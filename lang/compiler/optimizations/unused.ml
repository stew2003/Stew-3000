open Ast
open Warnings
open Util.Err

(* [var_is_used] determines if a variable name is referenced at all
    in a given scope. *)
let var_is_used (var_to_find : string) (scope : stmt list) : bool =
  (* [used_in_stmt] determines if var_to_find is used in a given statement. *)
  let rec used_in_stmt (stmt : stmt) : bool =
    match stmt with
    | Declare (var, _, init, scope, _) -> (
        match init with
        | Some exp when used_in_expr exp -> true
        | _ ->
            (* If this declaration shadows the variable to find, stop searching *)
            if var = var_to_find then false else used_in_stmt_list scope)
    | ArrayDeclare (var, _, _, init, scope, _) -> (
        (* NOTE: size must be constant and therefore doesn't need to be checked. *)
        match init with
        | Some exprs when used_in_expr_list exprs -> true
        | _ -> if var = var_to_find then false else used_in_stmt_list scope)
    | If (cond, thn, _) -> used_in_expr cond || used_in_stmt_list thn
    | IfElse (cond, thn, els, _) ->
        used_in_expr cond || used_in_stmt_list thn || used_in_stmt_list els
    | While (cond, body, _) -> used_in_expr cond || used_in_stmt_list body
    | Loop (body, _) | Block (body, _) -> used_in_stmt_list body
    | Return (exp, _) | Exit (exp, _) -> (
        match exp with None -> false | Some exp -> used_in_expr exp)
    | ExprStmt (exp, _) | PrintDec (exp, _) | PrintLcd (exp, _) | Assert (exp, _)
      ->
        used_in_expr exp
    | NopStmt _ -> false
  (* [used_in_expr] determines if var_to_find is used in a given expression. *)
  and used_in_expr (expr : expr) : bool =
    match expr with
    | Var (var, _) -> var = var_to_find
    | UnOp (_, exp, _)
    | Deref (exp, _)
    | AddrOf (exp, _)
    | Cast (_, exp, _)
    | PostfixInr (exp, _)
    | PostfixDcr (exp, _) ->
        used_in_expr exp
    | BinOp (_, left, right, _) | Assign (left, right, _) ->
        used_in_expr left || used_in_expr right
    | Call (_, args, _) -> used_in_expr_list args
    | NumLiteral _ | CharLiteral _ -> false
    | SPrefixInr _ | SPrefixDcr _ | SUpdate _ | SSubscript _ ->
        raise
          (InternalError
             (Printf.sprintf
                "encountered sugar expression in unused variable elimination: \
                 %s"
                (describe_expr expr)))
  (* [used_in_expr_list] determines if var_to_find is used within a list of expressions *)
  and used_in_expr_list (exprs : expr list) : bool =
    List.fold_left (fun acc exp -> acc || used_in_expr exp) false exprs
  (* [used_in_stmt_list] determines if var_to_find is used within a given scope. *)
  and used_in_stmt_list (stmts : stmt list) : bool =
    List.fold_left (fun acc stmt -> acc || used_in_stmt stmt) false stmts
  in
  used_in_stmt_list scope

(* [eliminate_unused_vars] eliminates unused variable declarations from a program. *)
let eliminate_unused_vars ?(emit_warning : compiler_warn_handler = fun _ -> ())
    (pgrm : prog) : prog =
  (* Note: The below functions do not recur on sub-expressions, only
     sub-statements. This is because variable declaration only occurs in statements
     (which can't be nested within expressions) and thus unused variables can only
     occur within statements. *)

  (* [elim_unused_stmt] eliminates unused vars in a single statement. *)
  let rec elim_unused_stmt (stmt : stmt) : stmt =
    match stmt with
    | Declare (var, typ, init, scope, loc) ->
        if var_is_used var scope then
          Declare (var, typ, init, elim_unused_stmt_list scope, loc)
        else (
          emit_warning (UnusedVar (var, loc));
          (* TODO: For now, replace the declaration with its initializer, if any. In
             the future, we can detect if the initializer can be omitted entirely
             but it might have a side effect. *)
          Block
            ( (match init with
              | None -> NopStmt loc
              | Some exp -> ExprStmt (exp, loc))
              :: elim_unused_stmt_list scope,
              loc ))
    | ArrayDeclare (var, typ, size, init, scope, loc) ->
        if var_is_used var scope then
          ArrayDeclare (var, typ, size, init, elim_unused_stmt_list scope, loc)
        else (
          emit_warning (UnusedVar (var, loc));
          match init with
          | None -> NopStmt loc
          | Some exprs ->
              (* TODO: See above. *)
              Block
                ( List.map (fun e -> ExprStmt (e, loc)) exprs
                  @ elim_unused_stmt_list scope,
                  loc ))
    | If (cond, thn, loc) -> If (cond, elim_unused_stmt_list thn, loc)
    | IfElse (cond, thn, els, loc) ->
        IfElse (cond, elim_unused_stmt_list thn, elim_unused_stmt_list els, loc)
    | While (cond, body, loc) -> While (cond, elim_unused_stmt_list body, loc)
    | Loop (body, loc) -> Loop (elim_unused_stmt_list body, loc)
    | Block (body, loc) -> Block (elim_unused_stmt_list body, loc)
    | Return _ | ExprStmt _ | PrintDec _ | PrintLcd _ | Exit _ | Assert _
    | NopStmt _ ->
        stmt
  (* [elim_unused_stmt_list] eliminates unused vars in a list of statements. *)
  and elim_unused_stmt_list (stmts : stmt list) : stmt list =
    List.map elim_unused_stmt stmts
  (* [elim_unused_defn] eliminates unused vars in a function definition. *)
  and elim_unused_defn (defn : func_defn) : func_defn =
    { defn with body = elim_unused_stmt_list defn.body }
  in
  {
    pgrm with
    main = elim_unused_defn pgrm.main;
    funcs = List.map elim_unused_defn pgrm.funcs;
  }
