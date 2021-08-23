open Ast
open Warnings

(* [can_pass_stmt] determines if execution can pass a given statement. For
   instance, the exit() built-in halts the program, and therefore is impossible
   to pass. *)
let rec can_pass_stmt (stmt : stmt) : bool =
  match stmt with
  | Declare (_, _, _, scope, _) | ArrayDeclare (_, _, _, _, scope, _) ->
      can_pass_stmt_list scope
  | IfElse (_, thn, els, _) -> can_pass_stmt_list thn || can_pass_stmt_list els
  | Block (scope, _) -> can_pass_stmt_list scope
  (* Return and exit *cannot* be passed. Neither can loop because it loops
     forever (NOTE: if we implement break, this should change!) *)
  | Return _ | Exit _ | Loop _ -> false
  (* All of the following statements can be passed. *)
  | If _ | ExprStmt _ | While _ | PrintDec _ | PrintLcd _ | Assert _ | NopStmt _
    ->
      true

(* [can_pass_stmt_list] determines if a given list of statements can be passed. *)
and can_pass_stmt_list (stmts : stmt list) : bool =
  match stmts with
  | [] -> true
  | stmt :: rest ->
      if can_pass_stmt stmt then can_pass_stmt_list rest else false

(* [eliminate_dead_code] removes dead / unused code from the given program. *)
let eliminate_dead_code ?(emit_warning : compiler_warn_handler = fun _ -> ())
    (pgrm : prog) : prog =
  let rec dce_stmt (stmt : stmt) : stmt =
    (* TODO: warnings! *)
    match stmt with
    (* conditional construct with constant conditions can cause dead code *)
    | If (cond, body, loc) -> (
        match cond with
        | NumLiteral (cond_value, _) ->
            if cond_value = 0 then NopStmt loc
            else Block (dce_stmt_list body, loc)
        | _ -> If (cond, dce_stmt_list body, loc))
    | IfElse (cond, thn, els, loc) -> (
        match cond with
        | NumLiteral (cond_value, _) ->
            if cond_value = 0 then Block (dce_stmt_list els, loc)
            else Block (dce_stmt_list thn, loc)
        | _ -> IfElse (cond, dce_stmt_list thn, dce_stmt_list els, loc))
    | While (cond, body, loc) -> (
        match cond with
        | NumLiteral (0, _) -> NopStmt loc
        | _ -> Loop (dce_stmt_list body, loc))
    (* recursively eliminate dead code in sub-statements *)
    | Loop (body, loc) -> Loop (dce_stmt_list body, loc)
    | Block (body, loc) -> Block (dce_stmt_list body, loc)
    | Declare (name, typ, init, scope, loc) ->
        Declare (name, typ, init, dce_stmt_list scope, loc)
    | ArrayDeclare (name, typ, size, init, scope, loc) ->
        ArrayDeclare (name, typ, size, init, dce_stmt_list scope, loc)
    | Return _ | Exit _ | ExprStmt _ | PrintDec _ | PrintLcd _ | Assert _
    | NopStmt _ ->
        stmt
  and dce_stmt_list (stmts : stmt list) : stmt list =
    match stmts with
    | [] -> []
    | stmt :: rest -> (
        let stmt_dce = dce_stmt stmt in
        match stmt_dce with
        | Block ([], _) | NopStmt _ -> dce_stmt_list rest
        | _ ->
            if can_pass_stmt stmt_dce then stmt_dce :: dce_stmt_list rest
            else [ stmt_dce ])
  and dce_func_defn (defn : func_defn) : func_defn =
    { defn with body = dce_stmt_list defn.body }
  in
  {
    pgrm with
    funcs = List.map dce_func_defn pgrm.funcs;
    main = dce_func_defn pgrm.main;
  }
