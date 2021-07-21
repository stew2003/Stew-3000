open Ast
open Util.Err

(* [fold_expr] performs constant folding on a given expression. *)
let rec fold_expr (exp : expr) : expr =
  match exp with
  | UnOp (op, expr, loc) -> (
      let folded_operand = fold_expr expr in
      match folded_operand with
      | Num (n, _) -> (
          match op with
          | BNot -> Num (lnot n, loc)
          | LNot -> Num ((if n = 0 then 1 else 0), loc))
      | _ -> UnOp (op, folded_operand, loc))
  | BinOp (op, left, right, loc) -> (
      let bool_to_3000_bool (b : bool) : int = if b then 1 else 0 in
      match (op, fold_expr left, fold_expr right) with
      | LAnd, Num (left_value, _), folded_right ->
          (* When left is false, fold to 0; otherwise fold to righthand value *)
          if left_value = 0 then Num (0, loc) else folded_right
      | LOr, Num (left_value, _), folded_right ->
          (* When left is truthy, fold to it; otherwise, fold to righthand value *)
          if left_value <> 0 then Num (left_value, loc) else folded_right
      | _, Num (left_value, _), Num (right_value, _) -> (
          match op with
          | Plus -> Num (left_value + right_value, loc)
          | Minus -> Num (left_value - right_value, loc)
          | Mult -> Num (left_value * right_value, loc)
          | Div -> Num (left_value / right_value, loc)
          | Mod -> Num (left_value mod right_value, loc)
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
  | Call (name, args, loc) -> Call (name, List.map fold_expr args, loc)
  | Num _ | Var _ -> exp

(* [fold_stmt] performs constant folding on a single statement. *)
and fold_stmt (stmt : stmt) : stmt =
  match stmt with
  | Let (name, typ, expr, scope, loc) ->
      Let (name, typ, fold_expr expr, fold_stmt_list scope, loc)
  | Assign (name, expr, loc) -> Assign (name, fold_expr expr, loc)
  | If (cond, body, loc) -> (
      match fold_expr cond with
      | Num (cond_value, _) ->
          if cond_value = 0 then Block ([], loc)
          else Block (fold_stmt_list body, loc)
      | folded_cond -> If (folded_cond, fold_stmt_list body, loc))
  | IfElse (cond, thn, els, loc) -> (
      match fold_expr cond with
      | Num (cond_value, _) ->
          if cond_value = 0 then Block (fold_stmt_list els, loc)
          else Block (fold_stmt_list thn, loc)
      | folded_cond ->
          IfElse (folded_cond, fold_stmt_list thn, fold_stmt_list els, loc))
  | Block (body, loc) -> Block (fold_stmt_list body, loc)
  | Return (Some expr, loc) -> Return (Some (fold_expr expr), loc)
  | ExprStmt (expr, loc) -> ExprStmt (fold_expr expr, loc)
  | While (cond, body, loc) -> (
      match fold_expr cond with
      | Num (0, _) -> Block ([], loc)
      | folded_cond -> While (folded_cond, fold_stmt_list body, loc))
  | PrintDec (expr, loc) -> PrintDec (fold_expr expr, loc)
  | Exit (Some expr, loc) -> Exit (Some (fold_expr expr), loc)
  | Assert (expr, loc) -> Assert (fold_expr expr, loc)
  | Return _ | Exit _ | Inr _ | Dcr _ -> stmt

(* [fold_stmt_list] performs constant folding on a list of statements. *)
and fold_stmt_list (stmts : stmt list) : stmt list = List.map fold_stmt stmts

(* [fold_func_defn] performs constant folding on a function definition. *)
and fold_func_defn (defn : func_defn) : func_defn =
  { defn with body = fold_stmt_list defn.body }

(* [constant_fold] replaces any constant expression in the given program
    with the constant it evaluates to.  *)
and constant_fold (pgrm : prog) : prog =
  {
    pgrm with
    main = fold_func_defn pgrm.main;
    funcs = List.map fold_func_defn pgrm.funcs;
  }