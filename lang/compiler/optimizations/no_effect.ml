open Ast
open Warnings
open Util.Err

(* ========= Elimination of statements with no effect ========= *)

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
  | BinOp (_, left, right, _) ->
      expr_has_no_effect left && expr_has_no_effect right
  (* We can't trace function calls without more sophisticated analysis
     that prevents us from entering a cycle in the program's call graph.
     So, assume that all calls have side-effects. *)
  | Call _ -> false
  (* Assignment has the effect of updating the value of a variable. *)
  | Assign _ -> false
  | SPrefixInr _ | SPrefixDcr _ | SUpdate _ | SSubscript _ ->
      raise
        (InternalError
           (Printf.sprintf
              "encountered sugar expression in no effect elimination: %s"
              (describe_expr expr)))

(* [stmt_has_no_effect] determines if a statement has no effect. *)
and stmt_has_no_effect (stmt : stmt) : bool =
  match stmt with
  (* Whether declarations have any effect is determined by whether or not
     they are used in their scope, which will be handled by unused variable analysis *)
  | Declare _ | ArrayDeclare _ -> false
  (* Loop causes non-termination, which is a side-effect. Return causes
       change in control flow, and exit/assert can cause machine to halt.
       Print functions cause observable output. *)
  | Loop _ | Return _ | Exit _ | Assert _ | PrintDec _ | PrintLcd _ -> false
  | NopStmt _ -> true
  (* Everything else recurs on sub expressions/statements *)
  | If (cond, thn, _) -> expr_has_no_effect cond && stmt_list_has_no_effect thn
  | IfElse (cond, thn, els, _) ->
      expr_has_no_effect cond
      && stmt_list_has_no_effect thn
      && stmt_list_has_no_effect els
  | While (cond, body, _) ->
      expr_has_no_effect cond && stmt_list_has_no_effect body
  | Block (body, _) -> stmt_list_has_no_effect body
  | ExprStmt (exp, _) -> expr_has_no_effect exp

(* [expr_list_has_no_effect] determines if _all_ the expressions in a list
   do not have any side effects. *)
and expr_list_has_no_effect (exprs : expr list) : bool =
  List.fold_left (fun acc exp -> acc && expr_has_no_effect exp) true exprs

(* [stmt_list_has_no_effect] determines if a list of statements has no side effect. *)
and stmt_list_has_no_effect (stmts : stmt list) : bool =
  List.fold_left (fun acc stmt -> acc && stmt_has_no_effect stmt) true stmts

(* [eliminate_no_effect] removes any statements in a program which have no effect. *)
let eliminate_no_effect ?(emit_warning : compiler_warn_handler = fun _ -> ())
    (pgrm : prog) : prog =
  (* [elim_stmt_list] removes statements from a list which do not have any effect. *)
  let rec elim_stmt_list (stmts : stmt list) : stmt list =
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

(* ========== Elimination of unused variable declarations ========== *)

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

  (* [elim_unused_stmt_list] eliminates unused vars in a list of statements. *)
  let rec elim_unused_stmt_list (stmts : stmt list) : stmt list =
    match stmts with
    | [] -> []
    | stmt :: rest ->
        (match stmt with
        | Declare (var, typ, init, scope, loc) ->
            if var_is_used var scope then
              [ Declare (var, typ, init, elim_unused_stmt_list scope, loc) ]
            else (
              emit_warning (UnusedVar (var, loc));
              (match init with
              | None -> []
              | Some exp ->
                  if expr_has_no_effect exp then [] else [ ExprStmt (exp, loc) ])
              @ elim_unused_stmt_list scope)
        | ArrayDeclare (var, typ, size, init, scope, loc) ->
            if var_is_used var scope then
              [
                ArrayDeclare
                  (var, typ, size, init, elim_unused_stmt_list scope, loc);
              ]
            else (
              emit_warning (UnusedVar (var, loc));
              (match init with
              | None -> []
              | Some exprs ->
                  if expr_list_has_no_effect exprs then []
                  else List.map (fun e -> ExprStmt (e, loc)) exprs)
              @ elim_unused_stmt_list scope)
        | If (cond, thn, loc) -> [ If (cond, elim_unused_stmt_list thn, loc) ]
        | IfElse (cond, thn, els, loc) ->
            [
              IfElse
                (cond, elim_unused_stmt_list thn, elim_unused_stmt_list els, loc);
            ]
        | While (cond, body, loc) ->
            [ While (cond, elim_unused_stmt_list body, loc) ]
        | Loop (body, loc) -> [ Loop (elim_unused_stmt_list body, loc) ]
        | Block (body, loc) -> [ Block (elim_unused_stmt_list body, loc) ]
        | Return _ | ExprStmt _ | PrintDec _ | PrintLcd _ | Exit _ | Assert _
        | NopStmt _ ->
            [ stmt ])
        @ elim_unused_stmt_list rest
  (* [elim_unused_defn] eliminates unused vars in a function definition. *)
  and elim_unused_defn (defn : func_defn) : func_defn =
    { defn with body = elim_unused_stmt_list defn.body }
  in
  {
    pgrm with
    main = elim_unused_defn pgrm.main;
    funcs = List.map elim_unused_defn pgrm.funcs;
  }

(* [eliminate_unused_vars_and_no_effects] eliminates unused variables and
   statements which have no effect from a program. *)
let eliminate_unused_vars_and_no_effects
    ?(emit_warning : compiler_warn_handler = fun _ -> ()) (pgrm : prog) : prog =
  (* [iteratively_eliminate] repeatedly applies elimination of no-effect
     statements and then elimination of unused variables to a program, until
     the optimizations cease to change the program. *)
  let rec iteratively_eliminate (pre_opt_pgrm : prog) : prog =
    let pgrm = pre_opt_pgrm |> eliminate_no_effect |> eliminate_unused_vars in
    if pgrm <> pre_opt_pgrm then iteratively_eliminate pgrm else pgrm
  in
  (* First, eliminate unused variables and emit warnings *)
  let pgrm = eliminate_unused_vars ~emit_warning pgrm in
  (* Next, eliminate statements which have no effect and emit warnings. *)
  let pgrm = eliminate_no_effect ~emit_warning pgrm in
  (* Finally, repeatedly apply these optimizations until the program can't be
     further optimized. Warnings are not emitted here because variables used
     in the program might end up unused after elimination of certain statements,
     which could cause confusion. *)
  iteratively_eliminate pgrm
