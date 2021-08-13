open Ast
open Asm.Isa
open Util.Env
open Util.Names
open Util.Err

(* [call_runtime] adjusts the stack pointer and calls
   a runtime function, then adjusts the stack pointer back *)
let call_runtime (func : string) (si : int) =
  let stack_base = si - 1 in
  [
    Addi (stack_base, SP, None); Call (func, None); Subi (stack_base, SP, None);
  ]

(* [compile_un_op] generates code for a unary operator.
   Note: this assumes the operand value is in the a register. *)
let compile_un_op (op : un_op) =
  match op with
  | BNot -> [ Not (A, None) ]
  | LNot ->
      let make_true = gensym "make_true" in
      let continue = gensym "continue" in
      [
        Cmpi (Reg A, Imm 0, None);
        Je (make_true, None);
        Mvi (0, A, None);
        Jmp (continue, None);
        Label (make_true, None);
        Inr (A, None);
        Label (continue, None);
      ]

(* [compile_bin_op] generates code for a binary operator
   Note: assumes left operand is in the a register, right
   is in the b register *)
let compile_bin_op (op : bin_op) (si : int) =
  (* [comparison] generates code for a binary comparison
     operator, given a function that creates the corresponding
     jump instruction *)
  let comparison (operation : string) (make_jmp : string -> instr) =
    let success = gensym operation in
    let continue = gensym "continue" in
    [
      Cmp (A, B, None);
      make_jmp success;
      Mvi (0, A, None);
      Jmp (continue, None);
      Label (success, None);
      Mvi (1, A, None);
      Label (continue, None);
    ]
  in
  match op with
  | Plus -> [ Add (B, A, None) ]
  | Minus -> [ Sub (B, A, None) ]
  | Mult -> call_runtime "runtime_multiply" si @ [ Mov (C, A, None) ]
  | Div -> call_runtime "runtime_divide" si @ [ Mov (C, A, None) ]
  | Mod -> call_runtime "runtime_divide" si
  | BAnd -> [ And (B, A, None) ]
  | BOr -> [ Or (B, A, None) ]
  | BXor -> [ Xor (B, A, None) ]
  | Gt -> comparison "greater" (fun success -> Jg (success, None))
  | Lt -> comparison "less" (fun success -> Jl (success, None))
  | Gte -> comparison "greater_than_eq" (fun success -> Jge (success, None))
  | Lte -> comparison "less_than_eq" (fun success -> Jle (success, None))
  | UnsignedGt | UnsignedLt | UnsignedGte | UnsignedLte ->
      (* TODO: *)
      failwith "unimplemented!"
  | Eq -> comparison "equal" (fun success -> Je (success, None))
  | Neq -> comparison "not_equal" (fun success -> Jne (success, None))
  | LAnd | LOr ->
      raise
        (InternalError
           "attemped to compile logical and/or as normal binary operator")

(* [compile_expr] generates instructions for the given expression
   in a given environment and stack index *)
let rec compile_expr (expression : expr) (bindings : int env) (si : int)
    (defns : func_defn list) : instr list =
  (* [compile_log_bin_op] generates code for logical and/or,
     given a function that produces the correct kind of
     jump to the end of the logical operator's code *)
  let compile_log_bin_op (left : expr) (right : expr)
      (create_jmp : string -> instr) =
    let continue = gensym "continue" in
    compile_expr left bindings si defns
    @ [ Cmpi (Reg A, Imm 0, None); create_jmp continue ]
    @ compile_expr right bindings si defns
    @ [ Label (continue, None) ]
  in
  match expression with
  | NumLiteral (n, _) -> [ Mvi (n, A, None) ]
  | CharLiteral (c, _) ->
      (* TODO: *)
      failwith "unimplemented!"
  | Var (x, _) -> (
      match Env.find_opt x bindings with
      | Some x_si -> [ Lds (x_si, A, None) ]
      | None -> raise (InternalError "compiler: unbound variable"))
  | UnOp (op, operand, _) ->
      compile_expr operand bindings si defns @ compile_un_op op
  | BinOp (LAnd, left, right, _) ->
      compile_log_bin_op left right (fun continue -> Je (continue, None))
  | BinOp (LOr, left, right, _) ->
      compile_log_bin_op left right (fun continue -> Jne (continue, None))
  | BinOp (op, left, right, _) ->
      compile_expr right bindings si defns
      @ [ Sts (A, si, None) ]
      @ compile_expr left bindings (si + 1) defns
      @ [ Lds (si, B, None) ]
      @ compile_bin_op op si
  | Call (func, args, _) ->
      let stack_base = si - 1 in
      (args
      |> List.mapi (fun i arg ->
             let arg_si = si + i + 1 in
             compile_expr arg bindings arg_si defns @ [ Sts (A, arg_si, None) ])
      |> List.concat)
      @ [
          Addi (stack_base, SP, None);
          Call (function_label func, None);
          Subi (stack_base, SP, None);
        ]
  | Deref (e, _) ->
      (* TODO: *)
      failwith "unimplemented!"
  | AddrOf (lv, _) ->
      (* TODO: *)
      failwith "unimplemented!"
  | Assign (lv, expr, _) ->
      (* TODO: *)
      failwith "unimplemented!"
      (* (
          match Env.find_opt name bindings with
          | Some name_si ->
              compile_expr expr bindings si defns @ [ Sts (A, name_si, None) ]
          | None -> raise (InternalError "compiler: assign before initialize")) *)
  | Cast (typ, _, _) ->
      raise
        (InternalError
           (Printf.sprintf "encountered cast in compiler (casting to %s)"
              (string_of_ty typ)))
  | SInr _ | SDcr _ | SUpdate _ | SSubscript _ ->
      raise
        (InternalError
           (Printf.sprintf "encountered sugar expression in compiler: %s"
              (describe_expr expression)))

(* [compile_cond] generates instructions for specifically compiling
   conditions in ifs and whiles*)
and compile_cond (cond : expr) (condition_failed : string) (bindings : int env)
    (si : int) (defns : func_defn list) : instr list =
  (* For non-comparison expressions in condition-position, we just compile them
     normally and jump to condition_failed if the expr's value is 0. *)
  let default_compile_cond _ =
    compile_expr cond bindings si defns
    @ [ Cmpi (Reg A, Imm 0, None); Je (condition_failed, None) ]
  in
  match cond with
  | BinOp (op, left, right, _) -> (
      (* When comparison ops are in condition-position, we eliminate the
         need for two cmps and two jmps by doing a single cmp and choosing
         the right kind of jmp (for the comparison op) to the condition_failed
         label. *)
      let compile_comparison (make_jump : string -> instr) =
        compile_expr right bindings si defns
        @ [ Sts (A, si, None) ]
        @ compile_expr left bindings (si + 1) defns
        @ [ Lds (si, B, None) ]
        @ [ Cmp (A, B, None); make_jump condition_failed ]
      in
      match op with
      | Gt -> compile_comparison (fun label -> Jle (label, None))
      | Gte -> compile_comparison (fun label -> Jl (label, None))
      | Lt -> compile_comparison (fun label -> Jge (label, None))
      | Lte -> compile_comparison (fun label -> Jg (label, None))
      | UnsignedGt | UnsignedGte | UnsignedLt | UnsignedLte ->
          (* TODO: *)
          failwith "unimplemented!"
      | Eq -> compile_comparison (fun label -> Jne (label, None))
      | Neq -> compile_comparison (fun label -> Je (label, None))
      | _ -> default_compile_cond ())
  | _ -> default_compile_cond ()

(* [compile_stmt] generates instructions for a single statement,
   in a given environment and stack index *)
and compile_stmt (statement : stmt) (bindings : int env) (si : int)
    (defns : func_defn list) (ignore_asserts : bool) : instr list =
  match statement with
  | Declare (name, _, init, scope, _) ->
      (* if variable is initialized, generate code for that *)
      let initialization =
        match init with
        | None -> []
        | Some init ->
            compile_expr init bindings si defns @ [ Sts (A, si, None) ]
      in
      let ext_env = Env.add name si bindings in
      initialization
      @ compile_stmt_list scope ext_env (si + 1) defns ignore_asserts
  | ArrayDeclare _ ->
      (* TODO: *)
      failwith "unimplemented!"
  | Block (stmt_list, _) ->
      compile_stmt_list stmt_list bindings si defns ignore_asserts
  | ExprStmt (expr, _) -> compile_expr expr bindings si defns
  | Exit (expr, _) ->
      (match expr with
      | Some expr -> compile_expr expr bindings si defns @ [ Out (A, None) ]
      | None -> [])
      @ [ Hlt None ]
  | Return (expr, _) ->
      (match expr with
      | Some expr -> compile_expr expr bindings si defns
      | None -> [])
      @ [ Ret None ]
  | PrintDec (expr, _) ->
      compile_expr expr bindings si defns @ [ Out (A, None) ]
  | Assert _ when ignore_asserts -> []
  | Assert (expr, _) ->
      compile_expr expr bindings si defns @ call_runtime "runtime_assert" si
  | If (cond, thn, _) ->
      let condition_failed = gensym "condition_failed" in
      compile_cond cond condition_failed bindings si defns
      @ compile_stmt_list thn bindings si defns ignore_asserts
      @ [ Label (condition_failed, None) ]
  | IfElse (cond, thn, els, _) ->
      let condition_failed = gensym "condition_failed" in
      let end_else = gensym "end_else" in
      compile_cond cond condition_failed bindings si defns
      @ compile_stmt_list thn bindings si defns ignore_asserts
      @ [ Jmp (end_else, None); Label (condition_failed, None) ]
      @ compile_stmt_list els bindings si defns ignore_asserts
      @ [ Label (end_else, None) ]
  | While (cond, body, _) ->
      let start_while = gensym "start_while" in
      let condition_failed = gensym "condition_failed" in
      [ Label (start_while, None) ]
      @ compile_cond cond condition_failed bindings si defns
      @ compile_stmt_list body bindings si defns ignore_asserts
      @ [ Jmp (start_while, None); Label (condition_failed, None) ]

(* [compile_stmt_list] generates instructions for a list of statements,
   in an environment and stack index *)
and compile_stmt_list (statements : stmt list) (bindings : int env) (si : int)
    (defns : func_defn list) (ignore_asserts : bool) : instr list =
  statements
  |> List.concat_map (fun stmt ->
         compile_stmt stmt bindings si defns ignore_asserts)

(* [compile_func_defn] generates instructions for a function definition *)
and compile_func_defn (defn : func_defn) (defns : func_defn list)
    (ignore_asserts : bool) : instr list =
  (*
     | ...       |
     | arg 2     | si=3
     | arg 1     | si=2
     | arg 0     | si=1
     | ret addr  | <-- SP
  *)
  let bindings, si =
    List.fold_left
      (fun (env, si) (name, _) -> (Env.add name si env, si + 1))
      (Env.empty, 1) defn.params
  in
  [ Label (function_label defn.name, None) ]
  @ compile_stmt_list defn.body bindings si defns ignore_asserts
  @
  match defn.ctrl_reaches_end with
  | Some reaches_end ->
      if reaches_end then
        match defn.return_ty with
        | Void -> [ Ret None ]
        | _ -> raise (InternalError "control reached end of non-void function")
      else []
  | None ->
      raise
        (InternalError
           "tried to compile function without checking if control can reach \
            its end")

(* [compile] generates instructions for a complete program *)
and compile ?(ignore_asserts = false) (program : prog) : instr list =
  compile_stmt_list program.main.body Env.empty 1 program.funcs ignore_asserts
  @ (match program.main.ctrl_reaches_end with
    | Some reaches_end -> if reaches_end then [ Hlt None ] else []
    | None ->
        raise
          (InternalError
             "tried to compile main without checking if control can reach its \
              end"))
  @ List.concat_map
      (fun defn -> compile_func_defn defn program.funcs ignore_asserts)
      program.funcs
  @ Runtime.runtime program ~ignore_asserts
