open Compiler.Ast
open Compiler.Prettyprint
open OUnit2

(* [norm_l_value_locs] normalizes source locations for an l-value *)
let rec norm_l_value_locs (lv : l_value) : l_value =
  match lv with
  | LVar (name, _) -> LVar (name, None)
  | LDeref (e, _) -> LDeref (e, None)

(* [norm_expr_locs] normalizes source locations in an expression *)
and norm_expr_locs (exp : expr) : expr =
  match exp with
  | NumLiteral (n, _) -> NumLiteral (n, None)
  | CharLiteral (c, _) -> CharLiteral (c, None)
  | Var (id, _) -> Var (id, None)
  | UnOp (op, e, _) -> UnOp (op, norm_expr_locs e, None)
  | BinOp (op, l, r, _) -> BinOp (op, norm_expr_locs l, norm_expr_locs r, None)
  | Call (fn, args, _) -> Call (fn, List.map norm_expr_locs args, None)
  | Deref (e, _) -> Deref (norm_expr_locs e, None)
  | AddrOf (lv, _) -> AddrOf (norm_l_value_locs lv, None)
  | Cast (typ, e, _) -> Cast (typ, norm_expr_locs e, None)

(* [norm_stmt_locs] normalizes source locations in a statement *)
and norm_stmt_locs (stmt : stmt) : stmt =
  match stmt with
  | Declare (id, typ, value, body, _) ->
      Declare
        ( id,
          typ,
          (match value with
          | None -> None
          | Some value -> Some (norm_expr_locs value)),
          norm_stmt_list_locs body,
          None )
  | ArrayDeclare (name, typ, size, init, body, _) ->
      ArrayDeclare
        ( name,
          typ,
          Option.map norm_expr_locs size,
          Option.map (fun exprs -> List.map norm_expr_locs exprs) init,
          norm_stmt_list_locs body,
          None )
  | Assign (lv, exp, _) ->
      Assign (norm_l_value_locs lv, norm_expr_locs exp, None)
  | If (cond, thn, _) -> If (norm_expr_locs cond, norm_stmt_list_locs thn, None)
  | IfElse (cond, thn, els, _) ->
      IfElse
        ( norm_expr_locs cond,
          norm_stmt_list_locs thn,
          norm_stmt_list_locs els,
          None )
  | Block (stmts, _) -> Block (norm_stmt_list_locs stmts, None)
  | Return (Some e, _) -> Return (Some (norm_expr_locs e), None)
  | Return (None, _) -> Return (None, None)
  | ExprStmt (e, _) -> ExprStmt (norm_expr_locs e, None)
  | While (cond, body, _) ->
      While (norm_expr_locs cond, norm_stmt_list_locs body, None)
  | PrintDec (e, _) -> PrintDec (norm_expr_locs e, None)
  | Inr (lv, _) -> Inr (norm_l_value_locs lv, None)
  | Dcr (lv, _) -> Dcr (norm_l_value_locs lv, None)
  | Exit (Some e, _) -> Exit (Some (norm_expr_locs e), None)
  | Exit (None, _) -> Exit (None, None)
  | Assert (e, _) -> Assert (e, None)

(* [norm_stmt_list_locs] normalizes source locations in a statement list *)
and norm_stmt_list_locs (stmts : stmt list) : stmt list =
  List.map norm_stmt_locs stmts

(* [norm_func_locs] normalizes source locations in a function definition *)
and norm_func_locs (func : func_defn) : func_defn =
  { func with body = norm_stmt_list_locs func.body; loc = None }

and norm_define_locs (define : pp_define) : pp_define =
  { define with expression = norm_expr_locs define.expression; loc = None }

(* [norm_prog_locs] replaces all source locations in a program with None *)
and norm_prog_locs (pgrm : prog) : prog =
  let { defines; funcs; main } = pgrm in
  {
    defines = List.map norm_define_locs defines;
    funcs = List.map norm_func_locs funcs;
    main = norm_func_locs main;
  }

(* [assert_prog_eq] asserts that an actual program equals an expected program,
   and uses pretty printing to display a message if they are not. *)
let assert_prog_eq (expected : prog) (actual : prog) =
  assert_equal expected actual ~printer:pretty_print

(* [parse_norm] is a wrapper around parsing a source string and normalizing its
   source locations. *)
let parse_norm (source : string) : prog =
  source |> Compiler.Parser.parse |> norm_prog_locs
