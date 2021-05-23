open Ast
open Asm.Isa
open Asm
open Core.In_channel
open Core.Exn
open Util

(* [from_file] reads a file containing an asm implementation
  of runtime functionality and parses it into a list of instructions. *)
let from_file (filename : string) : instr list =
  let path = Printf.sprintf "compiler/runtime/%s" filename in
  try
    let source = read_all path in
    try Parser.parse source
    with Parser.AsmParseError (msg, loc) ->
      Printf.eprintf "%s: %s\n%s\n"
        (Colors.error "Error parsing runtime functions from `" ^ filename ^ "`")
        msg
        (Srcloc.string_of_src_loc loc source);
      exit 1
  with err ->
    Printf.eprintf "Error with getting runtime functions from `%s`: %s\n"
      filename (to_string err);
    exit 1

(* Implementation of multiplication *)
let runtime_multiply = from_file "multiply.3000.s"

(* Implementation of division *)
let runtime_divide = from_file "divide.3000.s"

(* Helpful functionality for dealing with signed operands to mult/div *)
let runtime_sign_utils = from_file "sign_utils.3000.s"

(* [check_expr] determines if the given expression contains a 
  sub-expression that satisfies the given predicate. *)
let rec check_expr (exp : expr) (pred : expr -> bool) : bool =
  (* true if either the expression or its subexpressions yield true *)
  pred exp
  ||
  match exp with
  | UnOp (_, operand, _) | LogOp (LNot operand, _) -> check_expr operand pred
  | BinOp (_, left, right, _)
  | LogOp (LAnd (left, right), _)
  | LogOp (LOr (left, right), _) ->
      check_expr left pred || check_expr right pred
  | Call (_, args, _) ->
      List.map (fun arg -> check_expr arg pred) args
      |> List.fold_left ( || ) false
  | _ -> false

(* [check_stmt] determines if the given statement contains an 
  expression that satisfies the given predicate *)
and check_stmt (stmt : stmt) (pred : expr -> bool) : bool =
  match stmt with
  | Let (_, _, value, body, _) ->
      check_expr value pred || check_stmt_list body pred
  | Assign (_, value, _) -> check_expr value pred
  | If (cond, thn, _) -> check_expr cond pred || check_stmt_list thn pred
  | IfElse (cond, thn, els, _) ->
      check_expr cond pred || check_stmt_list thn pred
      || check_stmt_list els pred
  | Block (stmts, _) -> check_stmt_list stmts pred
  | Return (Some value, _) -> check_expr value pred
  | ExprStmt (value, _) -> check_expr value pred
  | While (cond, body, _) -> check_expr cond pred || check_stmt_list body pred
  | PrintDec (value, _) -> check_expr value pred
  | _ -> false

(* [check_stmt_list] determines if the given statement list 
  contains an expression that satisfies the predicate *)
and check_stmt_list (stmts : stmt list) (pred : expr -> bool) : bool =
  stmts
  |> List.map (fun stmt -> check_stmt stmt pred)
  |> List.fold_left ( || ) false

(* [check_program] determines if the program contains an 
  expression that satisfies the given predicate *)
and check_program (pgrm : prog) (pred : expr -> bool) : bool =
  List.map (fun defn -> check_stmt_list defn.body pred) pgrm.funcs
  |> List.fold_left ( || ) false
  || check_stmt_list pgrm.main.body pred

(* [runtime] constructs the runtime code necessary for a given 
  program. Usually, this will be empty, but if the program requires
  special runtime functionality (multiplication, division, ...) this
  will contribute those subroutines *)
let runtime (program : prog) : instr list =
  let uses_mult =
    check_program program (function
      | BinOp (Mult, _, _, _) -> true
      | _ -> false)
  in
  let uses_div =
    check_program program (function BinOp (Div, _, _, _) -> true | _ -> false)
  in
  (if uses_mult || uses_div then runtime_sign_utils else [])
  @ (if uses_mult then runtime_multiply else [])
  @ if uses_div then runtime_divide else []
