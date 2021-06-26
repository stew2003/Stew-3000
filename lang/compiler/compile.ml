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
let rec compile_un_op (op : un_op) = match op with BNot -> [ Not (A, None) ]

(* [compile_bin_op] generates code for a binary operator
  Note: assumes left operand is in the a register, right
  is in the b register *)
and compile_bin_op (op : bin_op) (si : int) =
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
  | Eq -> comparison "equal" (fun success -> Je (success, None))
  | Neq -> comparison "not_equal" (fun success -> Jne (success, None))

(* [compile_expr] generates instructions for the given expression 
  in a given environment and stack index *)
and compile_expr (expression : expr) (bindings : int env) (si : int)
    (defns : func_defn list) : instr list =
  match expression with
  | Num (n, _) -> [ Mvi (n, A, None) ]
  | Var (x, _) -> (
      match Env.find_opt x bindings with
      | Some x_si -> [ Lds (x_si, A, None) ]
      | None -> raise (InternalError "compiler: unbound variable"))
  | UnOp (op, operand, _) ->
      compile_expr operand bindings si defns @ compile_un_op op
  | BinOp (op, left, right, _) ->
      compile_expr right bindings si defns
      @ [ Sts (A, si, None) ]
      @ compile_expr left bindings (si + 1) defns
      @ [ Lds (si, B, None) ]
      @ compile_bin_op op si
  | LogOp (op, _) -> compile_log_op op bindings si defns
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

(* [compile_log_op] generates code for a logical operator
  (and/or/not) *)
and compile_log_op (op : log_op) (bindings : int env) (si : int)
    (defns : func_defn list) : instr list =
  (* [log_bin_op] generates code for logical and/or,
     given a function that produces the correct kind of
     jump to the end of the logical operator's code *)
  let log_bin_op (left : expr) (right : expr) (create_jmp : string -> instr) =
    let continue = gensym "continue" in
    compile_expr left bindings si defns
    @ [ Cmpi (Reg A, Imm 0, None); create_jmp continue ]
    @ compile_expr right bindings si defns
    @ [ Label (continue, None) ]
  in
  match op with
  | LNot operand ->
      let make_true = gensym "make_true" in
      let continue = gensym "continue" in
      compile_expr operand bindings si defns
      @ [
          Cmpi (Reg A, Imm 0, None);
          Je (make_true, None);
          Mvi (0, A, None);
          Jmp (continue, None);
          Label (make_true, None);
          Inr (A, None);
          Label (continue, None);
        ]
  | LAnd (left, right) ->
      log_bin_op left right (fun continue -> Je (continue, None))
  | LOr (left, right) ->
      log_bin_op left right (fun continue -> Jne (continue, None))

(* [compile_stmt] generates instructions for a single statement,  
  in a given environment and stack index *)
and compile_stmt (statement : stmt) (bindings : int env) (si : int)
    (defns : func_defn list) : instr list =
  match statement with
  | Let (name, _, value, scope, _) ->
      let ext_env = Env.add name si bindings in
      compile_expr value bindings si defns
      @ [ Sts (A, si, None) ]
      @ compile_stmt_list scope ext_env (si + 1) defns
  | Assign (name, expr, _) -> (
      match Env.find_opt name bindings with
      | Some name_si ->
          compile_expr expr bindings si defns @ [ Sts (A, name_si, None) ]
      | None -> raise (InternalError "compiler: assign before initialize"))
  | Block (stmt_list, _) -> compile_stmt_list stmt_list bindings si defns
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
  | Inr (name, _) -> (
      match Env.find_opt name bindings with
      | Some name_si ->
          [ Lds (name_si, A, None); Inr (A, None); Sts (A, name_si, None) ]
      | None -> raise (InternalError "compiler: increment before initialize"))
  | Dcr (name, _) -> (
      match Env.find_opt name bindings with
      | Some name_si ->
          [ Lds (name_si, A, None); Dcr (A, None); Sts (A, name_si, None) ]
      | None -> raise (InternalError "compiler: decrement before initialize"))
  | Assert (expr, _) ->
      compile_expr expr bindings si defns @ call_runtime "runtime_assert" si
  | If (cond, thn, _) ->
      let condition_failed = gensym "condition_failed" in
      compile_expr cond bindings si defns
      @ [ Cmpi (Reg A, Imm 0, None); Je (condition_failed, None) ]
      @ compile_stmt_list thn bindings si defns
      @ [ Label (condition_failed, None) ]
  | IfElse (cond, thn, els, _) ->
      let condition_failed = gensym "condition_failed" in
      let end_else = gensym "end_else" in
      compile_expr cond bindings si defns
      @ [ Cmpi (Reg A, Imm 0, None); Je (condition_failed, None) ]
      @ compile_stmt_list thn bindings si defns
      @ [ Jmp (end_else, None); Label (condition_failed, None) ]
      @ compile_stmt_list els bindings si defns
      @ [ Label (end_else, None) ]
  | While (cond, body, _) ->
      let start_while = gensym "start_while" in
      let condition_failed = gensym "condition_failed" in
      [ Label (start_while, None) ]
      @ compile_expr cond bindings si defns
      @ [ Cmpi (Reg A, Imm 0, None); Je (condition_failed, None) ]
      @ compile_stmt_list body bindings si defns
      @ [ Jmp (start_while, None); Label (condition_failed, None) ]

(* [compile_stmt_list] generates instructions for a list of statements,
  in an environment and stack index *)
and compile_stmt_list (statements : stmt list) (bindings : int env) (si : int)
    (defns : func_defn list) : instr list =
  statements
  |> List.map (fun stmt -> compile_stmt stmt bindings si defns)
  |> List.concat

(* [compile_func_defn] generates instructions for a function definition *)
and compile_func_defn (defn : func_defn) (defns : func_defn list) : instr list =
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
  @ compile_stmt_list defn.body bindings si defns
  (* TODO: only include this ret if ctrl_reaches_end is set
     and the function is void. *)
  @ [ Ret None ]

(* [compile] generates instructions for a complete program *)
and compile (program : prog) : instr list =
  compile_stmt_list program.main.body Env.empty 1 program.funcs
  (* TODO: only include this hlt if ctrl_reaches_end is set
     and main is void. *)
  @ [ Hlt None ]
  @ List.concat_map
      (fun defn -> compile_func_defn defn program.funcs)
      program.funcs
  @ Runtime.runtime program
