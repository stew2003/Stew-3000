open Ast
open Util.Env

(* [desugar] replaces syntactic sugar in the given program with equivalent
    expressions made up of non-sugar features. *)
let desugar (pgrm : prog) : prog =
  let rec desugar_expr (expr : expr) =
    match expr with
    (* sugar expressions *)
    | SInr (e, loc) ->
        let e_desugar = desugar_expr e in
        (* e++ ==> e = e + 1 *)
        Assign
          (e_desugar, BinOp (Plus, e_desugar, NumLiteral (1, loc), loc), loc)
    | SDcr (e, loc) ->
        let e_desugar = desugar_expr e in
        (* e-- ==> e = e - 1 *)
        Assign
          (e_desugar, BinOp (Minus, e_desugar, NumLiteral (1, loc), loc), loc)
    | SUpdate (dest, amount, op, loc) ->
        let dest_desugar = desugar_expr dest in
        let amount_desugar = desugar_expr amount in
        (* dest op= amount ==> dest = dest op amount *)
        Assign (dest_desugar, BinOp (op, dest_desugar, amount_desugar, loc), loc)
    | SSubscript (arr, idx, loc) ->
        let arr_desugar = desugar_expr arr in
        let idx_desugar = desugar_expr idx in
        (* a[i] ==> *(a + i) *)
        Deref (BinOp (Plus, arr_desugar, idx_desugar, loc), loc)
    (* recursively desugar sub-expressions *)
    | UnOp (op, operand, loc) -> UnOp (op, desugar_expr operand, loc)
    | BinOp (op, left, right, loc) ->
        BinOp (op, desugar_expr left, desugar_expr right, loc)
    | Call (name, args, loc) -> Call (name, List.map desugar_expr args, loc)
    | Deref (e, loc) -> Deref (desugar_expr e, loc)
    | AddrOf (e, loc) -> AddrOf (desugar_expr e, loc)
    | Cast (typ, e, loc) -> Cast (typ, desugar_expr e, loc)
    | Assign (dest, e, loc) -> Assign (desugar_expr dest, desugar_expr e, loc)
    (* expressions with no sub-expressions *)
    | NumLiteral _ | CharLiteral _ | Var _ -> expr
  and desugar_stmt (stmt : stmt) : stmt =
    match stmt with
    | Declare (name, typ, init, scope, loc) ->
        Declare
          ( name,
            typ,
            Option.map (fun init -> desugar_expr init) init,
            desugar_stmt_list scope,
            loc )
    | ArrayDeclare (name, typ, size, init, scope, loc) ->
        ArrayDeclare
          ( name,
            typ,
            Option.map (fun size -> desugar_expr size) size,
            Option.map (fun init -> List.map desugar_expr init) init,
            desugar_stmt_list scope,
            loc )
    | If (cond, thn, loc) -> If (desugar_expr cond, desugar_stmt_list thn, loc)
    | IfElse (cond, thn, els, loc) ->
        IfElse
          (desugar_expr cond, desugar_stmt_list thn, desugar_stmt_list els, loc)
    | While (cond, body, loc) ->
        While (desugar_expr cond, desugar_stmt_list body, loc)
    | Block (scope, loc) -> Block (desugar_stmt_list scope, loc)
    | Return (Some e, loc) -> Return (Some (desugar_expr e), loc)
    | ExprStmt (e, loc) -> ExprStmt (desugar_expr e, loc)
    | PrintDec (e, loc) -> PrintDec (desugar_expr e, loc)
    | Exit (Some e, loc) -> Exit (Some (desugar_expr e), loc)
    | Assert (e, loc) -> Assert (desugar_expr e, loc)
    | Return _ | Exit _ -> stmt
  and desugar_stmt_list (stmts : stmt list) : stmt list =
    List.map desugar_stmt stmts
  and desugar_func_defn (defn : func_defn) : func_defn =
    { defn with body = desugar_stmt_list defn.body }
  in
  {
    pgrm with
    funcs = List.map desugar_func_defn pgrm.funcs;
    main = desugar_func_defn pgrm.main;
  }
