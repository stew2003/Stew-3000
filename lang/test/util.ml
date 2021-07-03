open Compiler.Ast

(* [norm_expr_locs] normalizes source locations in an expression *)
let rec norm_expr_locs (exp : expr) =
  match exp with
  | Num (n, _) -> Num (n, None)
  | Var (id, _) -> Var (id, None)
  | UnOp (op, e, _) -> UnOp (op, norm_expr_locs e, None)
  | BinOp (op, l, r, _) -> BinOp (op, norm_expr_locs l, norm_expr_locs r, None)
  | LogOp (LNot e, _) -> LogOp (LNot (norm_expr_locs e), None)
  | LogOp (LAnd (l, r), _) ->
      LogOp (LAnd (norm_expr_locs l, norm_expr_locs r), None)
  | LogOp (LOr (l, r), _) ->
      LogOp (LOr (norm_expr_locs l, norm_expr_locs r), None)
  | Call (fn, args, _) -> Call (fn, List.map norm_expr_locs args, None)

(* [norm_stmt_locs] normalizes source locations in a statement *)
and norm_stmt_locs (stmt : stmt) =
  match stmt with
  | Let (id, typ, value, body, _) ->
      Let (id, typ, norm_expr_locs value, norm_stmt_list_locs body, None)
  | Assign (id, exp, _) -> Assign (id, norm_expr_locs exp, None)
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
  | Inr (name, _) -> Inr (name, None)
  | Dcr (name, _) -> Dcr (name, None)
  | Exit (Some e, _) -> Exit (Some (norm_expr_locs e), None)
  | Exit (None, _) -> Exit (None, None)
  | Assert (e, _) -> Assert (e, None)

(* [norm_stmt_list_locs] normalizes source locations in a statement list *)
and norm_stmt_list_locs (stmts : stmt list) = List.map norm_stmt_locs stmts

(* [norm_func_locs] normalizes source locations in a function definition *)
and norm_func_locs (func : func_defn) =
  { func with body = norm_stmt_list_locs func.body; loc = None }

and norm_define_locs (define : pp_define) =
  { define with expression = norm_expr_locs define.expression; loc = None }

(* [norm_prog_locs] replaces all source locations in a program with None *)
and norm_prog_locs (pgrm : prog) =
  let { defines; funcs; main } = pgrm in
  {
    defines = List.map norm_define_locs defines;
    funcs = List.map norm_func_locs funcs;
    main = norm_func_locs main;
  }
