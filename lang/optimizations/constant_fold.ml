open Compiler.Ast

let rec fold_expr (exp : expr) : expr =
  match exp with
  | UnOp (op, expr, loc) -> (
      let folded_operand = fold_expr expr in
      match folded_operand with
      | Num (n, _) -> ( match op with BNot -> Num (lnot n, loc))
      | _ -> UnOp (op, folded_operand, loc))
  | BinOp (op, left, right, loc) -> (
      let bool_to_3000_bool (b : bool) : int = if b then 1 else 0 in
      match (fold_expr left, fold_expr right) with
      | Num (left_value, _), Num (right_value, _) -> (
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
          | Neq -> Num (bool_to_3000_bool (left_value <> right_value), loc))
      | folded_left, folded_right -> BinOp (op, folded_left, folded_right, loc))
  | LogOp (op, loc) -> (
      match op with
      | LAnd (left, right) -> (
          match (fold_expr left, fold_expr right) with
          | Num (left_value, _), Num (right_value, _) ->
              if left_value = 0 then Num (0, loc) else Num (right_value, loc)
          | folded_left, folded_right ->
              LogOp (LAnd (folded_left, folded_right), loc))
      | LOr (left, right) -> (
          match (fold_expr left, fold_expr right) with
          | Num (left_value, _), Num (right_value, _) ->
              if left_value <> 0 then Num (left_value, loc)
              else Num (right_value, loc)
          | folded_left, folded_right ->
              LogOp (LOr (folded_left, folded_right), loc))
      | LNot operand -> (
          match fold_expr operand with
          | Num (operand_value, _) ->
              if operand_value = 0 then Num (1, loc) else Num (0, loc)
          | folded_operand -> LogOp (LNot folded_operand, loc)))
  | Call (name, args, loc) -> Call (name, List.map fold_expr args, loc)
  | _ -> exp

and fold_stmt (stmt : stmt) : stmt =
  match stmt with
  | Let (name, typ, expr, scope, loc) ->
      Let (name, typ, fold_expr expr, scope, loc)
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
  | _ -> stmt

and fold_stmt_list (stmts : stmt list) : stmt list = List.map fold_stmt stmts

and fold_func_defn (defn : func_defn) : func_defn =
  { defn with body = fold_stmt_list defn.body }

and constant_fold (pgrm : prog) : prog =
  {
    pgrm with
    main = fold_func_defn pgrm.main;
    funcs = List.map fold_func_defn pgrm.funcs;
  }
