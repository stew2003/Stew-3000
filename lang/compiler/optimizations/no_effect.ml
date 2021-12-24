open Ast
open Util.Err
open Warnings

(* FIXME: This is double warning things because of no-effect statements
   left behind from the elim-unused pass. Their locations are tied to the variable
   declarations that were removed. *)

(* [eliminate_no_effect] removes any statements in a program which have no effect. *)
let eliminate_no_effect ?(emit_warning : compiler_warn_handler = fun _ -> ())
    (pgrm : prog) : prog =
  (* [expr_has_no_effect] determines if an expression has no effect. *)
  let rec expr_has_no_effect (expr : expr) : bool =
    match expr with
    | NumLiteral _ | CharLiteral _ | Var _ -> true
    | UnOp (_, exp, _)
    | Deref (exp, _)
    | AddrOf (exp, _)
    | Cast (_, exp, _)
    | PostfixInr (exp, _)
    | PostfixDcr (exp, _) ->
        expr_has_no_effect exp
    | BinOp (_, left, right, _) | Assign (left, right, _) ->
        expr_has_no_effect left && expr_has_no_effect right
    | Call (name, args, _) -> (
        match lookup name pgrm.funcs with
        | Some defn ->
            (* A function call has no effect if the function body has no effect
               and neither do any of the arguments. *)
            stmt_list_has_no_effect defn.body && expr_list_has_no_effect args
        | None ->
            raise
              (InternalError
                 (Printf.sprintf
                    "encountered unbound function in no effect elimination: %s"
                    name)))
    | SPrefixInr _ | SPrefixDcr _ | SUpdate _ | SSubscript _ ->
        raise
          (InternalError
             (Printf.sprintf
                "encountered sugar expression in no effect elimination: %s"
                (describe_expr expr)))
  (* [stmt_has_no_effect] determines if a statement has no effect. *)
  and stmt_has_no_effect (stmt : stmt) : bool =
    match stmt with
    | Declare _ | ArrayDeclare _ -> false
    (* | Declare (_, _, init, scope, _) ->
           (match init with None -> true | Some init -> expr_has_no_effect init)
           && stmt_list_has_no_effect scope
       | ArrayDeclare (_, _, _, init, scope, _) ->
           (match init with
           | None -> true
           | Some exprs -> expr_list_has_no_effect exprs)
           && stmt_list_has_no_effect scope *)
    | If (cond, thn, _) ->
        expr_has_no_effect cond && stmt_list_has_no_effect thn
    | IfElse (cond, thn, els, _) ->
        expr_has_no_effect cond
        && stmt_list_has_no_effect thn
        && stmt_list_has_no_effect els
    | While (cond, body, _) ->
        expr_has_no_effect cond && stmt_list_has_no_effect body
    (* Loop causes non-termination, which is a side-effect. Return causes
       change in control flow, and exit/assert can cause machine to halt.
       Print functions cause observable output. *)
    | Loop _ | Return _ | Exit _ | Assert _ | PrintDec _ | PrintLcd _ -> false
    | Block (body, _) -> stmt_list_has_no_effect body
    | ExprStmt (exp, _) -> expr_has_no_effect exp
    | NopStmt _ -> true
  (* [stmt_list_has_no_effect] determines if a list of statements has no side effect. *)
  and stmt_list_has_no_effect (stmts : stmt list) : bool =
    List.fold_left (fun acc stmt -> acc && stmt_has_no_effect stmt) true stmts
  (* [expr_list_has_no_effect] determines if no expression in a list has a side effect. *)
  and expr_list_has_no_effect (exprs : expr list) : bool =
    List.fold_left (fun acc exp -> acc && expr_has_no_effect exp) true exprs
  (* [elim_stmt_list] removes statements from a list which do not have any effect. *)
  and elim_stmt_list (stmts : stmt list) : stmt list =
    match stmts with
    | [] -> []
    | stmt :: rest ->
        if stmt_has_no_effect stmt then (
          emit_warning (StmtHasNoEffect stmt);
          elim_stmt_list rest)
        else
          (match stmt with
          | Declare (var, typ, init, scope, loc) ->
              Declare (var, typ, init, elim_stmt_list scope, loc)
          | ArrayDeclare (var, typ, size, init, scope, loc) ->
              ArrayDeclare (var, typ, size, init, elim_stmt_list scope, loc)
          | If (cond, thn, loc) -> If (cond, elim_stmt_list thn, loc)
          | IfElse (cond, thn, els, loc) ->
              IfElse (cond, elim_stmt_list thn, elim_stmt_list els, loc)
          | While (cond, body, loc) -> While (cond, elim_stmt_list body, loc)
          | Loop (body, loc) -> Loop (elim_stmt_list body, loc)
          | Block (body, loc) -> Block (elim_stmt_list body, loc)
          | Return _ | ExprStmt _ | PrintDec _ | PrintLcd _ | Exit _ | Assert _
          | NopStmt _ ->
              stmt)
          :: elim_stmt_list rest
  (* [elim_defn] removes any statements that have no effect from the given
     function's body. *)
  and elim_defn (defn : func_defn) : func_defn =
    { defn with body = elim_stmt_list defn.body }
  in
  {
    pgrm with
    main = elim_defn pgrm.main;
    funcs = List.map elim_defn pgrm.funcs;
  }
