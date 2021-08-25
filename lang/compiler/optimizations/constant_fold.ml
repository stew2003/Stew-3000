open Ast
open Warnings
open Util.Err
open Util.Srcloc

type fold_err = FoldsToUnrepresentable of int

(* [string_of_fold_err] converts a folding error into a string explaining it. *)
let string_of_fold_err (err : fold_err) : string =
  match err with
  | FoldsToUnrepresentable n ->
      Printf.sprintf "expression evaluates to unrepresentable value: %d" n

exception ConstantFoldErr of fold_err with_loc_opt

(* [validate_folded_value] checks that a folded constant is still
   representable, and errors if not. If it is, the value is returned. *)
let validate_folded_value (n : int) (loc : maybe_loc) : int =
  if n < -128 || n > 255 then
    raise (ConstantFoldErr (FoldsToUnrepresentable n, loc));
  n

(* [fold_expr] performs constant folding on a given expression.
   NOTE: character literals are not explicitly folded here because they
   should be replaced by their number literal equivalents in the checking phase. *)
let rec fold_expr (exp : expr) (emit_warning : compiler_warn_handler) : expr =
  match exp with
  | UnOp (op, expr, loc) -> (
      let folded_operand = fold_expr expr emit_warning in
      match folded_operand with
      | NumLiteral (n, _) -> (
          match op with
          | BNot -> NumLiteral (lnot n, loc)
          | LNot -> NumLiteral ((if n = 0 then 1 else 0), loc))
      | _ -> UnOp (op, folded_operand, loc))
  | BinOp (op, left, right, loc) -> (
      let bool_to_3000_bool (b : bool) : int = if b then 1 else 0 in
      match (op, fold_expr left emit_warning, fold_expr right emit_warning) with
      | LAnd, NumLiteral (left_value, _), folded_right ->
          (* When left is false, fold to 0; otherwise fold to righthand value *)
          if left_value = 0 then NumLiteral (0, loc) else folded_right
      | LOr, NumLiteral (left_value, _), folded_right ->
          (* When left is truthy, fold to it; otherwise, fold to righthand value *)
          if left_value <> 0 then NumLiteral (left_value, loc) else folded_right
      | _, NumLiteral (left_value, left_loc), NumLiteral (right_value, right_loc)
        -> (
          match op with
          | Plus ->
              NumLiteral
                (validate_folded_value (left_value + right_value) loc, loc)
          | Minus ->
              NumLiteral
                (validate_folded_value (left_value - right_value) loc, loc)
          | Mult ->
              NumLiteral
                (validate_folded_value (left_value * right_value) loc, loc)
          | Div | Mod ->
              if right_value = 0 then (
                emit_warning (DivisionByZero exp);
                (* do not perform the division, stop folding *)
                BinOp
                  ( op,
                    NumLiteral (left_value, left_loc),
                    NumLiteral (right_value, right_loc),
                    loc ))
              else
                NumLiteral
                  ( validate_folded_value
                      ((match op with Div -> ( / ) | _ -> ( mod ))
                         left_value right_value)
                      loc,
                    loc )
          | BAnd -> NumLiteral (left_value land right_value, loc)
          | BOr -> NumLiteral (left_value lor right_value, loc)
          | BXor -> NumLiteral (left_value lxor right_value, loc)
          | Gt | UnsignedGt ->
              NumLiteral (bool_to_3000_bool (left_value > right_value), loc)
          | Lt | UnsignedLt ->
              NumLiteral (bool_to_3000_bool (left_value < right_value), loc)
          | Gte | UnsignedGte ->
              NumLiteral (bool_to_3000_bool (left_value >= right_value), loc)
          | Lte | UnsignedLte ->
              NumLiteral (bool_to_3000_bool (left_value <= right_value), loc)
          | Eq -> NumLiteral (bool_to_3000_bool (left_value = right_value), loc)
          | Neq ->
              NumLiteral (bool_to_3000_bool (left_value <> right_value), loc)
          | LAnd | LOr ->
              raise
                (InternalError
                   "unreachable match arm reached in constant folding"))
      | _, folded_left, folded_right ->
          BinOp (op, folded_left, folded_right, loc))
  | Call (name, args, loc) ->
      Call (name, List.map (fun arg -> fold_expr arg emit_warning) args, loc)
  | Deref (e, loc) -> Deref (fold_expr e emit_warning, loc)
  | AddrOf (e, loc) -> AddrOf (fold_expr e emit_warning, loc)
  | Cast (typ, e, loc) -> Cast (typ, fold_expr e emit_warning, loc)
  | Assign (dest, expr, loc) ->
      Assign (fold_expr dest emit_warning, fold_expr expr emit_warning, loc)
  | PostfixInr (lv, loc) -> PostfixInr (fold_expr lv emit_warning, loc)
  | PostfixDcr (lv, loc) -> PostfixDcr (fold_expr lv emit_warning, loc)
  | NumLiteral _ | CharLiteral _ | Var _ -> exp
  | SPrefixInr _ | SPrefixDcr _ | SUpdate _ | SSubscript _ ->
      raise
        (InternalError
           (Printf.sprintf
              "encountered sugar expression in constant folding: %s"
              (describe_expr exp)))

(* [fold_stmt] performs constant folding on a single statement. *)
and fold_stmt (stmt : stmt) (emit_warning : compiler_warn_handler) : stmt =
  match stmt with
  | Declare (name, typ, expr, scope, loc) ->
      Declare
        ( name,
          typ,
          (match expr with
          | None -> None
          | Some expr -> Some (fold_expr expr emit_warning)),
          fold_stmt_list scope emit_warning,
          loc )
  | ArrayDeclare (name, typ, size, init, scope, loc) ->
      ArrayDeclare
        ( name,
          typ,
          Option.map (fun size -> fold_expr size emit_warning) size,
          Option.map
            (fun exprs -> List.map (fun e -> fold_expr e emit_warning) exprs)
            init,
          fold_stmt_list scope emit_warning,
          loc )
  | If (cond, body, loc) ->
      If (fold_expr cond emit_warning, fold_stmt_list body emit_warning, loc)
  | IfElse (cond, thn, els, loc) ->
      IfElse
        ( fold_expr cond emit_warning,
          fold_stmt_list thn emit_warning,
          fold_stmt_list els emit_warning,
          loc )
  | Block (body, loc) -> Block (fold_stmt_list body emit_warning, loc)
  | Return (Some expr, loc) -> Return (Some (fold_expr expr emit_warning), loc)
  | ExprStmt (expr, loc) -> ExprStmt (fold_expr expr emit_warning, loc)
  | While (cond, body, loc) ->
      While (fold_expr cond emit_warning, fold_stmt_list body emit_warning, loc)
  | Loop (body, loc) -> Loop (fold_stmt_list body emit_warning, loc)
  | PrintDec (expr, loc) -> PrintDec (fold_expr expr emit_warning, loc)
  | PrintLcd (expr, loc) -> PrintLcd (fold_expr expr emit_warning, loc)
  | Exit (Some expr, loc) -> Exit (Some (fold_expr expr emit_warning), loc)
  | Assert (expr, loc) -> Assert (fold_expr expr emit_warning, loc)
  | Return _ | Exit _ | NopStmt _ -> stmt

(* [fold_stmt_list] performs constant folding on a list of statements. *)
and fold_stmt_list (stmts : stmt list) (emit_warning : compiler_warn_handler) :
    stmt list =
  List.map (fun stmt -> fold_stmt stmt emit_warning) stmts

(* [fold_func_defn] performs constant folding on a function definition. *)
and fold_func_defn (defn : func_defn) (emit_warning : compiler_warn_handler) :
    func_defn =
  { defn with body = fold_stmt_list defn.body emit_warning }

(* [constant_fold] replaces any constant expression in the given program
    with the constant it evaluates to. *)
and constant_fold ?(emit_warning : compiler_warn_handler = fun _ -> ())
    (pgrm : prog) : prog =
  {
    pgrm with
    main = fold_func_defn pgrm.main emit_warning;
    funcs = List.map (fun func -> fold_func_defn func emit_warning) pgrm.funcs;
  }
