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

(* [eliminate_unused] eliminates unused functions in the given program. *)
let eliminate_unused ?(emit_warning : compiler_warn_handler = fun _ -> ())
    (pgrm : prog) : prog =
  (* [defn_is_used] determines if a function definition is used by
     checking for calls to it. *)
  let defn_is_used (defn : func_defn) (pgrm : prog) : bool =
    check_for_expr pgrm (function
      | Call (name, _, _) when name = defn.name -> true
      | _ -> false)
  in
  (* [eliminate_unused_defns] eliminates function definitions that are never
     used by the given program. *)
  let rec eliminate_unused_defns (defns : func_defn list) : func_defn list =
    match defns with
    | [] -> []
    | defn :: rest ->
        if defn_is_used defn pgrm then defn :: eliminate_unused_defns rest
        else (
          emit_warning (UnusedFunction defn);
          eliminate_unused_defns rest)
  in
  { pgrm with funcs = eliminate_unused_defns pgrm.funcs }

(* [eliminate_dead_code] removes dead / unused code from the given program. *)
let eliminate_dead_code ?(emit_warning : compiler_warn_handler = fun _ -> ())
    (pgrm : prog) : prog =
  (* [dce_stmt] eliminates dead code in a single statement. *)
  let rec dce_stmt (stmt : stmt) : stmt =
    match stmt with
    (* conditional construct with constant conditions can cause dead code *)
    | If (cond, body, loc) -> (
        match cond with
        | NumLiteral (cond_value, _) ->
            emit_warning (ConstantCondition (cond_value, stmt));
            if cond_value = 0 then (
              emit_warning (DeadCode body);
              NopStmt loc)
            else Block (dce_stmt_list body, loc)
        | _ -> If (cond, dce_stmt_list body, loc))
    | IfElse (cond, thn, els, loc) -> (
        match cond with
        | NumLiteral (cond_value, _) ->
            emit_warning (ConstantCondition (cond_value, stmt));
            if cond_value = 0 then (
              emit_warning (DeadCode thn);
              Block (dce_stmt_list els, loc))
            else (
              emit_warning (DeadCode els);
              Block (dce_stmt_list thn, loc))
        | _ -> IfElse (cond, dce_stmt_list thn, dce_stmt_list els, loc))
    | While (cond, body, loc) -> (
        match cond with
        | NumLiteral (cond_value, _) ->
            if cond_value = 0 then (
              (* NOTE: only warn if condition is always 0, as while (1) is a
                 valid way to get an infinite loop. *)
              emit_warning (ConstantCondition (0, stmt));
              emit_warning (DeadCode body);
              NopStmt loc)
            else
              (* Convert while with truthy condition into an unconditional loop *)
              Loop (dce_stmt_list body, loc)
        | _ -> While (cond, dce_stmt_list body, loc))
    (* recursively eliminate dead code in sub-statements *)
    | Loop (body, loc) -> Loop (dce_stmt_list body, loc)
    | Block (body, loc) -> Block (dce_stmt_list body, loc)
    | Declare (name, typ, init, scope, loc) ->
        Declare (name, typ, init, dce_stmt_list scope, loc)
    | ArrayDeclare (name, typ, size, init, scope, loc) ->
        ArrayDeclare (name, typ, size, init, dce_stmt_list scope, loc)
    (* no change *)
    | Return _ | Exit _ | ExprStmt _ | PrintDec _ | PrintLcd _ | Assert _
    | NopStmt _ ->
        stmt
  (* [dce_stmt_list] eliminates dead code in a list of statements. If it
     encounters a statement that cannot be passed, it will elide all the
     following statements. It also recursively invokes DCE on substatements *)
  and dce_stmt_list (stmts : stmt list) : stmt list =
    match stmts with
    | [] -> []
    | stmt :: rest -> (
        let stmt_dce = dce_stmt stmt in
        match stmt_dce with
        | Block ([], _) | NopStmt _ -> dce_stmt_list rest
        | _ ->
            if can_pass_stmt stmt_dce then stmt_dce :: dce_stmt_list rest
            else (
              (match rest with [] -> () | _ -> emit_warning (DeadCode rest));
              [ stmt_dce ]))
  (* [dce_func_defn] performs dead code elimination on a function definition
     by processing its body of statements. *)
  and dce_func_defn (defn : func_defn) : func_defn =
    { defn with body = dce_stmt_list defn.body }
  in
  (* First, eliminate unused definitions and emit warnings *)
  let pgrm = eliminate_unused ~emit_warning pgrm in
  (* Eliminate dead code *)
  let pgrm =
    {
      pgrm with
      funcs = List.map dce_func_defn pgrm.funcs;
      main = dce_func_defn pgrm.main;
    }
  in
  (* Again, eliminate unused functions which may have become unused because
     of DCE. This time, don't emit warnings, however, because it might
     confuse the user. *)
  eliminate_unused pgrm
