open Ast

(* [expand_in_expr] expands a #define directive in an expression. *)
let rec expand_in_expr (define : pp_define) (exp : expr) : expr =
  match exp with
  (* check if this var is the same as that of the define *)
  | Var (name, loc) ->
      if name = define.var then define.expression else Var (name, loc)
  | NumLiteral _ -> exp
  | CharLiteral _ -> exp
  | UnOp (op, expr, loc) -> UnOp (op, expand_in_expr define expr, loc)
  | BinOp (op, left, right, loc) ->
      BinOp (op, expand_in_expr define left, expand_in_expr define right, loc)
  | Call (name, args, loc) ->
      Call (name, List.map (fun arg -> expand_in_expr define arg) args, loc)
  | Deref (e, loc) -> Deref (expand_in_expr define e, loc)
  | AddrOf (e, loc) -> AddrOf (expand_in_expr define e, loc)
  | Cast (typ, e, loc) -> Cast (typ, expand_in_expr define e, loc)
  | Assign (dest, expr, loc) ->
      Assign (expand_in_expr define dest, expand_in_expr define expr, loc)
  | PostfixInr (lv, loc) -> PostfixInr (expand_in_expr define lv, loc)
  | PostfixDcr (lv, loc) -> PostfixDcr (expand_in_expr define lv, loc)
  | SPrefixInr (e, loc) -> SPrefixInr (expand_in_expr define e, loc)
  | SPrefixDcr (e, loc) -> SPrefixDcr (expand_in_expr define e, loc)
  | SUpdate (dest, amount, op, loc) ->
      SUpdate (expand_in_expr define dest, expand_in_expr define amount, op, loc)
  | SSubscript (arr, idx, loc) ->
      SSubscript (expand_in_expr define arr, expand_in_expr define idx, loc)

(* [expand_in_stmt] expands a #define directive in a single statement. *)
and expand_in_stmt (define : pp_define) (stmt : stmt) : stmt =
  match stmt with
  | Declare (name, typ, expr, body, loc) ->
      Declare
        ( name,
          typ,
          Option.map (fun e -> expand_in_expr define e) expr,
          expand_in_stmt_list define body,
          loc )
  | ArrayDeclare (name, typ, size, init, body, loc) ->
      ArrayDeclare
        ( name,
          typ,
          Option.map (fun s -> expand_in_expr define s) size,
          Option.map
            (fun exprs -> List.map (fun e -> expand_in_expr define e) exprs)
            init,
          expand_in_stmt_list define body,
          loc )
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
  | Return _ | Exit _ -> stmt

(* [expand_in_stmt_list] expands a #define directive in every statement
   in a list. *)
and expand_in_stmt_list (define : pp_define) (stmts : stmt list) : stmt list =
  List.map (fun stmt -> expand_in_stmt define stmt) stmts

(* [expand_in_defn] expands a #define directive in a function definition. *)
and expand_in_defn (define : pp_define) (defn : func_defn) : func_defn =
  { defn with body = expand_in_stmt_list define defn.body }

(* [expand_in_defines] expands a #define directive in a list of other
   #define directives. *)
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
