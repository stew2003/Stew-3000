open Ast

let rec expand_in_expr (define : pp_define) (exp : expr) : expr =
  match exp with
  (* check if this var is the same as that of the define *)
  | Var (name, loc) ->
      if name = define.var then define.expression else Var (name, loc)
  | Num _ -> exp
  | UnOp (op, expr, loc) -> UnOp (op, expand_in_expr define expr, loc)
  | BinOp (op, left, right, loc) ->
      BinOp (op, expand_in_expr define left, expand_in_expr define right, loc)
  | LogOp (LNot expr, loc) -> LogOp (LNot (expand_in_expr define expr), loc)
  | LogOp (LAnd (left, right), loc) ->
      LogOp (LAnd (expand_in_expr define left, expand_in_expr define right), loc)
  | LogOp (LOr (left, right), loc) ->
      LogOp (LOr (expand_in_expr define left, expand_in_expr define right), loc)
  | Call (name, args, loc) ->
      Call (name, List.map (fun arg -> expand_in_expr define arg) args, loc)

and expand_in_stmt (define : pp_define) (stmt : stmt) : stmt =
  match stmt with
  | Let (name, typ, expr, body, loc) ->
      Let
        ( name,
          typ,
          expand_in_expr define expr,
          expand_in_stmt_list define body,
          loc )
  | Assign (name, expr, loc) -> Assign (name, expand_in_expr define expr, loc)
  | If (cond, thn, loc) ->
      If (expand_in_expr define cond, expand_in_stmt_list define thn, loc)
  | IfElse (cond, thn, els, loc) ->
      IfElse
        ( expand_in_expr define cond,
          expand_in_stmt_list define thn,
          expand_in_stmt_list define els,
          loc )
  | Block (body, loc) -> Block (expand_in_stmt_list define body, loc)
  | Return (Some expr, loc) -> Return (Some (expand_in_expr define expr), loc)
  | ExprStmt (expr, loc) -> ExprStmt (expand_in_expr define expr, loc)
  | While (cond, body, loc) ->
      While (expand_in_expr define cond, expand_in_stmt_list define body, loc)
  | PrintDec (expr, loc) -> PrintDec (expand_in_expr define expr, loc)
  | Exit (Some expr, loc) -> Exit (Some (expand_in_expr define expr), loc)
  | Assert (expr, loc) -> Assert (expand_in_expr define expr, loc)
  | _ -> stmt

and expand_in_stmt_list (define : pp_define) (stmts : stmt list) : stmt list =
  List.map (fun stmt -> expand_in_stmt define stmt) stmts

and expand_in_defn (define : pp_define) (defn : func_defn) : func_defn =
  { defn with body = expand_in_stmt_list define defn.body }

and expand_in_defines (define : pp_define) (defines : pp_define list) :
    pp_define list =
  match defines with
  | [] -> []
  | { var; expression; loc } :: rest ->
      { var; expression = expand_in_expr define expression; loc }
      :: expand_in_defines define rest

(* [preprocess] consumes a program and produces a version of it 
  with all uses of preprocessor #define directives expanded into
  their corresponding expressions in the ast. *)
let rec preprocess (pgrm : prog) : prog =
  match pgrm.defines with
  | [] -> pgrm
  | define :: rest ->
      preprocess
        {
          defines = expand_in_defines define rest;
          funcs = List.map (fun func -> expand_in_defn define func) pgrm.funcs;
          main = expand_in_defn define pgrm.main;
        }
