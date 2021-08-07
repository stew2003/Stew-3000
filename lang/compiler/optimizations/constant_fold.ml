open Ast
open Warnings
open Util.Err

(* [fold_expr] performs constant folding on a given expression. *)
let rec fold_expr ?(emit_warning : compiler_warn_handler = fun _ -> ())
    (exp : expr) : expr =
  match exp with
  | UnOp (op, expr, loc) -> (
      let folded_operand = fold_expr expr ~emit_warning in
      match folded_operand with
      | Num (n, _) -> (
          match op with
          | BNot -> Num (lnot n, loc)
          | LNot -> Num ((if n = 0 then 1 else 0), loc))
      | _ -> UnOp (op, folded_operand, loc))
  | BinOp (op, left, right, loc) -> (
      let bool_to_3000_bool (b : bool) : int = if b then 1 else 0 in
      match
        (op, fold_expr left ~emit_warning, fold_expr right ~emit_warning)
      with
      | LAnd, Num (left_value, _), folded_right ->
          (* When left is false, fold to 0; otherwise fold to righthand value *)
          if left_value = 0 then Num (0, loc) else folded_right
      | LOr, Num (left_value, _), folded_right ->
          (* When left is truthy, fold to it; otherwise, fold to righthand value *)
          if left_value <> 0 then Num (left_value, loc) else folded_right
      | _, Num (left_value, left_loc), Num (right_value, right_loc) -> (
          match op with
          | Plus -> Num (left_value + right_value, loc)
          | Minus -> Num (left_value - right_value, loc)
          | Mult -> Num (left_value * right_value, loc)
          | Div | Mod ->
              if right_value = 0 then (
                emit_warning (DivisionByZero exp);
                (* do not perform the division, stop folding *)
                BinOp
                  ( op,
                    Num (left_value, left_loc),
                    Num (right_value, right_loc),
                    loc ))
              else
                Num
                  ( (match op with Div -> ( / ) | _ -> ( mod ))
                      left_value right_value,
                    loc )
          | BAnd -> Num (left_value land right_value, loc)
          | BOr -> Num (left_value lor right_value, loc)
          | BXor -> Num (left_value lxor right_value, loc)
          | Gt -> Num (bool_to_3000_bool (left_value > right_value), loc)
          | Lt -> Num (bool_to_3000_bool (left_value < right_value), loc)
          | Gte -> Num (bool_to_3000_bool (left_value >= right_value), loc)
          | Lte -> Num (bool_to_3000_bool (left_value <= right_value), loc)
          | Eq -> Num (bool_to_3000_bool (left_value = right_value), loc)
          | Neq -> Num (bool_to_3000_bool (left_value <> right_value), loc)
          | LAnd | LOr ->
              raise
                (InternalError
                   "unreachable match arm reached in constant folding"))
      | _, folded_left, folded_right ->
          BinOp (op, folded_left, folded_right, loc))
  | Call (name, args, loc) ->
      Call (name, List.map (fun arg -> fold_expr arg ~emit_warning) args, loc)
  | Num _ | Var _ -> exp

(* [fold_stmt] performs constant folding on a single statement. *)
and fold_stmt ?(emit_warning : compiler_warn_handler = fun _ -> ())
    (stmt : stmt) : stmt =
  match stmt with
  | Let (name, typ, expr, scope, loc) ->
      Let
        ( name,
          typ,
          fold_expr expr ~emit_warning,
          fold_stmt_list scope ~emit_warning,
          loc )
  | Assign (name, expr, loc) -> Assign (name, fold_expr expr ~emit_warning, loc)
  | If (cond, body, loc) -> (
      match fold_expr cond ~emit_warning with
      | Num (cond_value, _) ->
          if cond_value = 0 then Block ([], loc)
          else Block (fold_stmt_list body ~emit_warning, loc)
      | folded_cond -> If (folded_cond, fold_stmt_list body ~emit_warning, loc))
  | IfElse (cond, thn, els, loc) -> (
      match fold_expr cond ~emit_warning with
      | Num (cond_value, _) ->
          if cond_value = 0 then Block (fold_stmt_list els ~emit_warning, loc)
          else Block (fold_stmt_list thn ~emit_warning, loc)
      | folded_cond ->
          IfElse
            ( folded_cond,
              fold_stmt_list thn ~emit_warning,
              fold_stmt_list els ~emit_warning,
              loc ))
  | Block (body, loc) -> Block (fold_stmt_list body ~emit_warning, loc)
  | Return (Some expr, loc) -> Return (Some (fold_expr expr ~emit_warning), loc)
  | ExprStmt (expr, loc) -> ExprStmt (fold_expr expr ~emit_warning, loc)
  | While (cond, body, loc) -> (
      match fold_expr cond ~emit_warning with
      | Num (0, _) -> Block ([], loc)
      | folded_cond ->
          While (folded_cond, fold_stmt_list body ~emit_warning, loc))
  | PrintDec (expr, loc) -> PrintDec (fold_expr expr ~emit_warning, loc)
  | Exit (Some expr, loc) -> Exit (Some (fold_expr expr ~emit_warning), loc)
  | Assert (expr, loc) -> Assert (fold_expr expr ~emit_warning, loc)
  | Return _ | Exit _ | Inr _ | Dcr _ -> stmt

(* [fold_stmt_list] performs constant folding on a list of statements. *)
and fold_stmt_list ?(emit_warning : compiler_warn_handler = fun _ -> ())
    (stmts : stmt list) : stmt list =
  List.map (fun stmt -> fold_stmt stmt ~emit_warning) stmts

(* [fold_func_defn] performs constant folding on a function definition. *)
and fold_func_defn ?(emit_warning : compiler_warn_handler = fun _ -> ())
    (defn : func_defn) : func_defn =
  { defn with body = fold_stmt_list defn.body ~emit_warning }

(* [constant_fold] replaces any constant expression in the given program
    with the constant it evaluates to. *)
and constant_fold ?(emit_warning : compiler_warn_handler = fun _ -> ())
    (pgrm : prog) : prog =
  {
    pgrm with
    main = fold_func_defn pgrm.main ~emit_warning;
    funcs = List.map (fun func -> fold_func_defn func ~emit_warning) pgrm.funcs;
  }
