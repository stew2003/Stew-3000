open Ast
open Asm.Isa
open Util.Env
open Util.Names
open Util.Err

(* [lookup_var] extracts stack index from a given variable in the bindings.
   Errors if the variable is unbound *)
let lookup_var (name : string) (bindings : int env) : int =
  match Env.find_opt name bindings with
  | Some si -> si
  | None ->
      raise
        (InternalError (Printf.sprintf "compiler: unbound variable %s" name))

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

(* [is_compare] determines if a bin_op is a comparison operator *)
let is_compare (op : bin_op) : bool =
  match op with
  | Gt | Gte | Lt | Lte | UnsignedGt | UnsignedGte | UnsignedLt | UnsignedLte
  | Eq | Neq ->
      true
  | _ -> false

type which_const = LeftConst | RightConst

(* [compile_bin_op] generates code for a binary operator *)
let rec compile_bin_op (op : bin_op) (left : expr) (right : expr)
    (bindings : int env) (si : int) (defns : func_defn list) =
  (* [label_from_bin_op] creates a label that is relevant to the given bin_op *)
  let label_from_bin_op (op : bin_op) : string =
    String.map (function ' ' -> '_' | c -> c) (describe_bin_op op)
  in
  (* [comparison] generates code for a binary comparison
     operator, given a function that creates the corresponding
     jump instruction *)
  let comparison (op : bin_op) =
    let success = gensym (label_from_bin_op op) in
    let continue = gensym "continue" in
    let jmp_success =
      match op with
      | Gt -> Jg (success, None)
      | Gte -> Jge (success, None)
      | Lt -> Jl (success, None)
      | Lte -> Jle (success, None)
      | UnsignedGt -> Ja (success, None)
      | UnsignedGte -> Jae (success, None)
      | UnsignedLt -> Jb (success, None)
      | UnsignedLte -> Jbe (success, None)
      | Eq -> Je (success, None)
      | Neq -> Jne (success, None)
      | _ ->
          raise
            (InternalError
               "compiler: tried to get jump of non-comparison operator")
    in
    [
      jmp_success;
      Mvi (0, A, None);
      Jmp (continue, None);
      Label (success, None);
      Mvi (1, A, None);
      Label (continue, None);
    ]
  in
  match (left, right) with
  | NumLiteral (const, _), non_const | non_const, NumLiteral (const, _) -> (
      let which =
        match left with NumLiteral _ -> LeftConst | _ -> RightConst
      in
      compile_expr non_const bindings si defns
      @
      match op with
      | Plus -> [ Addi (const, A, None) ]
      | Minus -> (
          match which with
          | LeftConst ->
              [ Mvi (const, B, None); Sub (A, B, None); Mov (B, A, None) ]
          | RightConst -> [ Subi (const, A, None) ])
      | Mult ->
          [ Mvi (const, B, None) ]
          @ call_runtime "runtime_multiply" si
          @ [ Mov (C, A, None) ]
      | Div ->
          (match which with
          | LeftConst -> [ Mov (A, B, None); Mvi (const, A, None) ]
          | RightConst -> [ Mvi (const, B, None) ])
          @ call_runtime "runtime_divide" si
          @ [ Mov (C, A, None) ]
      | Mod ->
          (match which with
          | LeftConst -> [ Mov (A, B, None); Mvi (const, A, None) ]
          | RightConst -> [ Mvi (const, B, None) ])
          @ call_runtime "runtime_divide" si
      | BAnd -> [ Ani (const, A, None) ]
      | BOr -> [ Ori (const, A, None) ]
      | BXor -> [ Xri (const, A, None) ]
      | op when is_compare op ->
          [
            (match which with
            | LeftConst -> Cmpi (Imm const, Reg A, None)
            | RightConst -> Cmpi (Reg A, Imm const, None));
          ]
          @ comparison op
      | _ ->
          raise
            (InternalError
               (Printf.sprintf "compiler: got invalid bin_op: %s"
                  (describe_bin_op op))))
  | _ -> (
      compile_expr right bindings si defns
      @ [ Sts (A, si, None) ]
      @ compile_expr left bindings (si + 1) defns
      @ [ Lds (si, B, None) ]
      @
      match op with
      | Plus -> [ Add (B, A, None) ]
      | Minus -> [ Sub (B, A, None) ]
      | Mult -> call_runtime "runtime_multiply" si @ [ Mov (C, A, None) ]
      | Div -> call_runtime "runtime_divide" si @ [ Mov (C, A, None) ]
      | Mod -> call_runtime "runtime_divide" si
      | BAnd -> [ And (B, A, None) ]
      | BOr -> [ Or (B, A, None) ]
      | BXor -> [ Xor (B, A, None) ]
      | op when is_compare op -> [ Cmp (A, B, None) ] @ comparison op
      | _ ->
          raise
            (InternalError
               (Printf.sprintf "compiler: got invalid bin_op: %s"
                  (describe_bin_op op))))

(* [compile_cond] generates instructions for specifically compiling
   conditions in ifs and whiles*)
and compile_cond (cond : expr) (condition_failed : string) (bindings : int env)
    (si : int) (defns : func_defn list) : instr list =
  match cond with
  | BinOp (op, left, right, _) when is_compare op -> (
      (* When comparison ops are in condition-position, we eliminate the
         need for two cmps and two jmps by doing a single cmp and choosing
         the right kind of jmp (for the comparison op) to the condition_failed
         label. If one of the operands is also an immediate, we use a cmpi
         instruction to eliminate extra stack operations *)
      (match (left, right) with
      | NumLiteral (const, _), non_const ->
          compile_expr non_const bindings si defns
          @ [ Cmpi (Imm const, Reg A, None) ]
      | non_const, NumLiteral (const, _) ->
          compile_expr non_const bindings si defns
          @ [ Cmpi (Reg A, Imm const, None) ]
      | _ ->
          compile_expr right bindings si defns
          @ [ Sts (A, si, None) ]
          @ compile_expr left bindings (si + 1) defns
          @ [ Lds (si, B, None) ]
          @ [ Cmp (A, B, None) ])
      @
      (* NOTE: the opposite jumps are used here, because they are
         jumps to the condition_failed label. *)
      match op with
      | Gt -> [ Jle (condition_failed, None) ]
      | Gte -> [ Jl (condition_failed, None) ]
      | Lt -> [ Jge (condition_failed, None) ]
      | Lte -> [ Jg (condition_failed, None) ]
      | UnsignedGt -> [ Jbe (condition_failed, None) ]
      | UnsignedGte -> [ Jb (condition_failed, None) ]
      | UnsignedLt -> [ Jae (condition_failed, None) ]
      | UnsignedLte -> [ Ja (condition_failed, None) ]
      | Eq -> [ Jne (condition_failed, None) ]
      | Neq -> [ Je (condition_failed, None) ]
      | _ ->
          raise
            (InternalError "compile: got impossible non-comparison operator"))
  | _ ->
      (* For non-comparison expressions in condition-position, we just compile them
         normally and jump to condition_failed if the expr's value is 0. *)
      compile_expr cond bindings si defns
      @ [ Cmpi (Reg A, Imm 0, None); Je (condition_failed, None) ]

(* [compile_expr] generates instructions for the given expression
   in a given environment and stack index *)
and compile_expr ?(value_ignored = false) (expression : expr)
    (bindings : int env) (si : int) (defns : func_defn list) : instr list =
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
  | CharLiteral _ ->
      raise
        (InternalError "compiler: checker did not replace character literal")
  | Var (x, _) -> [ Lds (lookup_var x bindings, A, None) ]
  | UnOp (op, operand, _) ->
      compile_expr operand bindings si defns @ compile_un_op op
  | BinOp (LAnd, left, right, _) ->
      compile_log_bin_op left right (fun continue -> Je (continue, None))
  | BinOp (LOr, left, right, _) ->
      compile_log_bin_op left right (fun continue -> Jne (continue, None))
  | BinOp (op, left, right, _) -> compile_bin_op op left right bindings si defns
  | Call (func, args, _) ->
      let stack_base = si - 1 in
      (args
      |> List.mapi (fun i arg ->
             let arg_si = si + i + 1 in
             match arg with
             | NumLiteral (num, _) -> [ Stsi (num, arg_si, None) ]
             | _ ->
                 compile_expr arg bindings arg_si defns
                 @ [ Sts (A, arg_si, None) ])
      |> List.concat)
      @ [
          Addi (stack_base, SP, None);
          Call (function_label func, None);
          Subi (stack_base, SP, None);
        ]
  | Deref (expr, _) -> compile_expr expr bindings si defns @ [ Ld (A, A, None) ]
  | AddrOf (lv, _) -> (
      match lv with
      | Var (name, _) ->
          [ Mov (SP, A, None); Addi (lookup_var name bindings, A, None) ]
      | Deref (expr, _) -> compile_expr expr bindings si defns
      | _ ->
          raise
            (InternalError "compiler: tried to take address of a non l-value"))
  | Assign (lv, expr, _) -> (
      match lv with
      | Var (name, _) -> (
          let var_si = lookup_var name bindings in
          match expr with
          | NumLiteral (num, _) when value_ignored ->
              [ Stsi (num, var_si, None) ]
          | _ -> compile_expr expr bindings si defns @ [ Sts (A, var_si, None) ]
          )
      | Deref (dest, _) -> (
          let compiled_dest = compile_expr dest bindings si defns in
          match expr with
          | NumLiteral (num, _) ->
              if value_ignored then
                compiled_dest @ [ Mvi (num, B, None); St (B, A, None) ]
              else
                compiled_dest
                @ [ Mov (A, B, None); Mvi (num, A, None); St (A, B, None) ]
          | _ ->
              compiled_dest
              @ [ Sts (A, si, None) ]
              @ compile_expr expr bindings (si + 1) defns
              @ [ Lds (si, B, None); St (A, B, None) ])
      | _ -> raise (InternalError "compiler: assignment of invalid l-value"))
  (* when the value is ignored anyway, convert postfix increment/decrement into
     prefix, since it takes less code. *)
  | (PostfixInr (lv, loc) | PostfixDcr (lv, loc)) as postfix when value_ignored
    ->
      let desugared =
        Desugar.desugar_expr
          (match postfix with
          | PostfixInr _ -> SPrefixInr (lv, loc)
          | _ -> SPrefixDcr (lv, loc))
      in
      compile_expr ~value_ignored desugared bindings si defns
  | PostfixInr (lv, _) | PostfixDcr (lv, _) -> (
      (* [make_crement] constructs the appropriate 'crement instruction, depending on
         whether this is a postfix INcrement or DEcrement. *)
      let make_crement (reg : register) : instr =
        match expression with
        | PostfixInr _ -> Inr (reg, None)
        | _ -> Dcr (reg, None)
      in
      match lv with
      | Var (name, _) ->
          let name_si = lookup_var name bindings in
          [
            Lds (name_si, A, None);
            Mov (A, B, None);
            make_crement B;
            Sts (B, name_si, None);
          ]
      | Deref (expr, _) ->
          compile_expr expr bindings si defns
          @ [
              Ld (A, B, None);
              Mov (B, C, None);
              make_crement C;
              St (C, A, None);
              Mov (B, A, None);
            ]
      | _ ->
          raise
            (InternalError "postfix increment/decrement got invalid l-value"))
  | Cast (typ, _, _) ->
      raise
        (InternalError
           (Printf.sprintf "encountered cast in compiler (casting to %s)"
              (string_of_ty typ)))
  | SPrefixInr _ | SPrefixDcr _ | SUpdate _ | SSubscript _ ->
      raise
        (InternalError
           (Printf.sprintf "encountered sugar expression in compiler: %s"
              (describe_expr expression)))

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
        | Some (NumLiteral (num, _)) -> [ Stsi (num, si, None) ]
        | Some init ->
            compile_expr init bindings si defns @ [ Sts (A, si, None) ]
      in
      let ext_env = Env.add name si bindings in
      initialization
      @ compile_stmt_list scope ext_env (si + 1) defns ignore_asserts
  | ArrayDeclare (name, _, size, init, scope, _) ->
      (* extract exactly how much to allocate for the array from its size
         expression (which should be a constant at this point) *)
      let allocation_amount =
        match size with
        | Some (NumLiteral (value, _)) -> value
        | _ -> raise (InternalError "compiler: underspecified array")
      in
      (* if initializer given, generate code for each expression and
         move its value into the array *)
      (match init with
      | Some exprs ->
          exprs
          |> List.mapi (fun i expr ->
                 match expr with
                 | NumLiteral (num, _) -> [ Stsi (num, si + i + 1, None) ]
                 | _ ->
                     compile_expr expr bindings (si + i + 1) defns
                     @ [ Sts (A, si + i + 1, None) ])
          |> List.concat
      | None -> [])
      (* create pointer to beginning of the array (stored at si, points at si+1) *)
      @ [ Mov (SP, A, None); Addi (si + 1, A, None); Sts (A, si, None) ]
      (* compile scope with array name bound to pointer to first element,
         give si that is past the array's allocation *)
      @ compile_stmt_list scope (Env.add name si bindings)
          (si + allocation_amount + 1)
          defns ignore_asserts
  | Block (stmt_list, _) ->
      compile_stmt_list stmt_list bindings si defns ignore_asserts
  | ExprStmt (expr, _) ->
      compile_expr ~value_ignored:true expr bindings si defns
  | Exit (expr, _) ->
      (match expr with
      | Some (NumLiteral (num, _)) -> [ Outi (num, None) ]
      | Some expr -> compile_expr expr bindings si defns @ [ Out (A, None) ]
      | None -> [])
      @ [ Hlt None ]
  | Return (expr, _) ->
      (match expr with
      | Some expr -> compile_expr expr bindings si defns
      | None -> [])
      @ [ Ret None ]
  | PrintDec (expr, _) -> (
      match expr with
      | NumLiteral (num, _) -> [ Outi (num, None) ]
      | _ -> compile_expr expr bindings si defns @ [ Out (A, None) ])
  | PrintLcd (expr, _) ->
      compile_expr expr bindings si defns @ call_runtime "runtime_print_lcd" si
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
  | Loop (body, _) ->
      let start_loop = gensym "start_loop" in
      [ Label (start_loop, None) ]
      @ compile_stmt_list body bindings si defns ignore_asserts
      @ [ Jmp (start_loop, None) ]
  | NopStmt _ -> []

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
  let runtime_init, runtime_subroutines =
    Runtime.runtime program ~ignore_asserts
  in
  runtime_init
  @ [ Label ("user_program_start", None) ]
  @ compile_stmt_list program.main.body Env.empty 1 program.funcs ignore_asserts
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
  @ runtime_subroutines
