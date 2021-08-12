open Ast
open Util.Srcloc
open Util.Env
open Util.Err
open Printf
open Warnings
open Optimizations.Constant_fold

(* A type constraint indicates whether the type of a given expression
   is constrained to a particular type, or if it could be various types. *)
type type_constraint = Unconstrained | ConstrainedTo of ty

type check_err =
  (* When control flow can reach the end of a non-void function *)
  | CtrlReachesEndOfNonVoid of string
  (* When a void function returns a value, or a non-void returns without a value *)
  | MismatchedReturn of string * ty
  (* When a variable is referenced without being bound first *)
  | UnboundVariable of string
  (* When a function is called without a definition *)
  | UndefinedFunction of string
  (* A type did not match the expected type (expression, expected type, actual type) *)
  | TypeError of expr * ty * ty
  (* Expression cannot be this type (but we don't expect it to be a particular type) *)
  | InvalidTypeError of expr * ty
  (* Operation with two types that should be the same but aren't *)
  | TypeMismatch of string * expr * ty * expr * ty
  (* Main was typed as non-void *)
  | NonVoidMain
  (* Something not a function definition was annotated as void *)
  | NonFunctionAnnotatedAsVoid of string
  (* Mismatched number of arguments supplied to a function call (name, expected #, actual #) *)
  | ArityMismatch of string * int * int
  (* Multiple function definitions using the same name *)
  | MultipleDefinitions of string
  (* Returned from main *)
  | ReturnInMain
  (* Program contains a constant value outside the representable range *)
  | UnrepresentableConstant of int
  (* A non-pointer type was dereferenced *)
  | DerefNonPointer of type_constraint
  (* A void pointer was dereferenced *)
  | DerefVoidPointer
  (* L-values must be variables or pointer dereferences *)
  | InvalidLValue of expr
  (* A (void) cast was made *)
  | CastToVoid
  (* An array's size declaration was non-constant (string is name of array) *)
  | NonConstantArraySize of string
  (* An array was declared with neither a size nor initializer *)
  | UnderspecifiedArray of string
  (* An array was initialized with more elements than its size can hold (name, size, # initialized elements) *)
  | TooLargeInitializer of string * int * int

exception CheckError of check_err with_loc_opt

(* [string_of_check_err] turns a check error into a printable string. *)
let string_of_check_err = function
  | CtrlReachesEndOfNonVoid name ->
      sprintf "control can reach the end of non-void function `%s`." name
  | MismatchedReturn (name, return_ty) ->
      sprintf "the %s function `%s` must return %s." (string_of_ty return_ty)
        name
        (if return_ty = Void then "nothing"
        else sprintf "a value of type %s" (string_of_ty return_ty))
  | UnboundVariable var -> sprintf "variable `%s` is unbound." var
  | UndefinedFunction name -> sprintf "function `%s` is undefined." name
  | TypeError (e, expected, actual) ->
      sprintf
        "the type of this %s expression was expected to be %s, but was %s."
        (describe_expr e) (string_of_ty expected) (string_of_ty actual)
  | InvalidTypeError (e, actual) ->
      sprintf "expected this %s expression to not have type %s."
        (describe_expr e) (string_of_ty actual)
  | TypeMismatch (op_name, left, left_ty, right, right_ty) ->
      sprintf
        "operation %s expected the %s expression (of type %s) to be the same \
         type as the %s expression (of type %s)"
        op_name (describe_expr left) (string_of_ty left_ty)
        (describe_expr right) (string_of_ty right_ty)
  | NonVoidMain -> "the `main` function must have return type void."
  | NonFunctionAnnotatedAsVoid name ->
      sprintf "`%s` is not a function and therefore cannot have type void." name
  | ArityMismatch (name, expected, actual) ->
      sprintf "function `%s` expects %d argument%s, but got %d." name expected
        (if expected = 1 then "" else "s")
        actual
  | MultipleDefinitions name ->
      sprintf "function `%s` has multiple definitions." name
  | ReturnInMain ->
      "cannot return from main function. Consider using exit() instead."
  | UnrepresentableConstant n -> sprintf "unrepresentable constant: %d" n
  | DerefNonPointer constr ->
      sprintf "dereference of non-pointer value (%s)"
        (match constr with
        | Unconstrained -> "unconstrained type"
        | ConstrainedTo t -> string_of_ty t)
  | DerefVoidPointer -> "cannot dereference void pointer"
  | InvalidLValue e ->
      sprintf "invalid l-value: %s (must be variable or dereference)"
        (describe_expr e)
  | CastToVoid -> "cannot cast to void type"
  | NonConstantArraySize arr -> sprintf "array `%s` must have constant size" arr
  | UnderspecifiedArray arr ->
      sprintf "array `%s` must have either a size or initializer" arr
  | TooLargeInitializer (arr, size, elements) ->
      sprintf "array `%s` (size %d) cannot be initialized with %d elements" arr
        size elements

(* [type_check] checks a function definition for type errors, raising an
   error if they are discovered. *)
let type_check (defn : func_defn) (defns : func_defn list)
    (emit_warning : compiler_warn_handler) : func_defn =
  (* [expect_non_void] checks that an expression is non-void type
     and raises an error if it is. *)
  let expect_non_void (exp : expr) (exp_ty_constraint : type_constraint) =
    match exp_ty_constraint with
    | ConstrainedTo Void ->
        raise (CheckError (InvalidTypeError (exp, Void), loc_from_expr exp))
    | _ -> ()
  in
  (* [lookup_var] looks up the type of a variable in the type environment
     and returns it, or errors if the variable is unbound *)
  let lookup_var (name : string) (env : ty env) (loc : maybe_loc) : ty =
    match Env.find_opt name env with
    | Some typ ->
        if typ = Void then
          raise (InternalError "found void variable type in type environment");
        typ
    | None -> raise (CheckError (UnboundVariable name, loc))
  in
  (* [type_check_l_value] checks an expression that is intended to be used as
     an l-value, so it must be backed by a memory location. NOTE: Valid l-values
     always must have a constrained type, because they are backed by memory, which
     could only be obtained through variable declaration or array declaration (both
     of which require type annotations). So this function returns a type, not a
     type constraint. *)
  let rec type_check_l_value (exp : expr) (env : ty env) (loc : maybe_loc) :
      ty * expr =
    match type_check_expr exp env with
    | ConstrainedTo t, (Var _ as lv_tc) | ConstrainedTo t, (Deref _ as lv_tc) ->
        (t, lv_tc)
    | Unconstrained, Var _ | Unconstrained, Deref _ ->
        raise (InternalError "l-value with unconstrained type encountered")
    | _, lv -> raise (CheckError (InvalidLValue lv, loc))
  (* [type_check_expr] calculates the type of the given expression,
     or raises an error if it violates type rules. *)
  and type_check_expr (exp : expr) (env : ty env) : type_constraint * expr =
    match exp with
    | NumLiteral (n, loc) ->
        (* all numbers are 8-bit *)
        if n < -128 || n > 255 then
          raise (CheckError (UnrepresentableConstant n, loc));
        (* int is the only signed type, otherwise it could be any *)
        if n < 0 then (ConstrainedTo Int, exp) else (Unconstrained, exp)
    | CharLiteral (c, loc) ->
        (* char literals are converted into num literals *)
        (ConstrainedTo Char, NumLiteral (Char.code c, loc))
    | Var (name, loc) -> (ConstrainedTo (lookup_var name env loc), exp)
    | UnOp (LNot, expr, loc) ->
        (* expression needs to be type checked and cannot be void *)
        let expr_ty, expr_tc = type_check_expr expr env in
        expect_non_void expr expr_ty;
        (* log ops are always interpreted as ints *)
        (ConstrainedTo Int, UnOp (LNot, expr_tc, loc))
    | UnOp (op, expr, loc) ->
        (* operand can be any non-void type *)
        let expr_ty, expr_tc = type_check_expr expr env in
        expect_non_void expr expr_ty;
        (* interpret result of unop as same type as operand *)
        (expr_ty, UnOp (op, expr_tc, loc))
    | BinOp (log_bin_op, left, right, loc)
      when log_bin_op = LAnd || log_bin_op = LOr ->
        (* left and right should type check internally *)
        let left_ty, left_tc = type_check_expr left env in
        let right_ty, right_tc = type_check_expr right env in
        (* neither left nor right can be void *)
        expect_non_void left left_ty;
        expect_non_void right right_ty;
        (* log ops are always interpreted as ints *)
        (ConstrainedTo Int, BinOp (log_bin_op, left_tc, right_tc, loc))
    | BinOp (op, left, right, loc) ->
        (* left and right should type check internally *)
        let left_ty, left_tc = type_check_expr left env in
        let right_ty, right_tc = type_check_expr right env in

        (* Compute the type to interpret both operands as. If one operand is
           constrained, the contrained type is taken. *)
        let operand_ty =
          match (left_ty, right_ty) with
          | ConstrainedTo t1, ConstrainedTo t2 when t1 = t2 -> ConstrainedTo t1
          | ConstrainedTo t, Unconstrained | Unconstrained, ConstrainedTo t ->
              ConstrainedTo t
          | Unconstrained, Unconstrained -> Unconstrained
          | ConstrainedTo t1, ConstrainedTo t2 ->
              raise
                (CheckError
                   (TypeMismatch (describe_bin_op op, left, t1, right, t2), loc))
        in
        (* neither left nor right can be void *)
        expect_non_void left left_ty;
        expect_non_void right right_ty;
        (* Convert an operator into its unsigned equivalent if it has one. *)
        let op_to_unsigned_op (op : bin_op) : bin_op =
          match op with
          | Gt -> UnsignedGt
          | Lt -> UnsignedLt
          | Gte -> UnsignedGte
          | Lte -> UnsignedLte
          | _ -> op
        in
        (* comparison operators evaluate to an int, other bin ops evaluate
           to same type as the operands *)
        let out_ty, typed_op =
          match op with
          | Gt | Lt | Gte | Lte | UnsignedGt | UnsignedLt | UnsignedGte
          | UnsignedLte | Eq | Neq -> (
              ( ConstrainedTo Int,
                (* Int is the only signed type, convert all others to use
                   unsigned equivalents *)
                match operand_ty with
                | ConstrainedTo Int -> op
                | _ -> op_to_unsigned_op op ))
          | Plus | Minus | Mult | Div | Mod | BAnd | BOr | BXor ->
              (operand_ty, op)
          | LAnd | LOr ->
              raise
                (InternalError
                   "unreachable match arm in checker reached with logical \
                    and/or")
        in
        (out_ty, BinOp (typed_op, left_tc, right_tc, loc))
    | Call (name, args, loc) -> (
        match lookup name defns with
        | Some called_defn -> (
            try
              let args_tc =
                List.map2
                  (fun (_, param_ty) arg ->
                    let arg_ty, arg_tc = type_check_expr arg env in

                    (* annotated param type and arg type should match if arg
                       is constrained, or arg should be unconstrained *)
                    match arg_ty with
                    | ConstrainedTo t when t <> param_ty ->
                        raise
                          (CheckError
                             (TypeError (arg, param_ty, t), loc_from_expr arg))
                    | _ -> arg_tc)
                  called_defn.params args
              in
              (* function call has type of function's return type *)
              (ConstrainedTo called_defn.return_ty, Call (name, args_tc, loc))
              (* Invalid_argument here means param/arg list were of different
                 lengths, which is arity mismatch error. *)
            with Invalid_argument _ ->
              raise
                (CheckError
                   ( ArityMismatch
                       ( called_defn.name,
                         List.length called_defn.params,
                         List.length args ),
                     loc )))
        | None -> raise (CheckError (UndefinedFunction name, loc)))
    | Deref (expr, loc) -> (
        (* NOTE: Design choice: dereferences must take an explicit pointer type *)
        match type_check_expr expr env with
        | ConstrainedTo (Pointer inner), deref_expr_tc ->
            if inner = Void then raise (CheckError (DerefVoidPointer, loc));
            (ConstrainedTo inner, Deref (deref_expr_tc, loc))
        | type_constr, _ ->
            raise (CheckError (DerefNonPointer type_constr, loc)))
    | AddrOf (lv, loc) ->
        let lv_type, lv_tc = type_check_l_value lv env loc in
        (ConstrainedTo (Pointer lv_type), AddrOf (lv_tc, loc))
    | Cast (typ, expr, loc) ->
        if typ = Void then raise (CheckError (CastToVoid, loc));
        let _, expr_tc = type_check_expr expr env in
        (* constrain this expression to the casted type, and just pass
           through the type checked expression (cast is not reproduced) *)
        (ConstrainedTo typ, expr_tc)
  (* [type_check_stmt] checks a single statement for type errors *)
  and type_check_stmt (stmt : stmt) (env : ty env) : stmt =
    (* [ensure_type_satisfies] checks that a given expression type checks
       to a constraint that is satisfied by the expected constraint.
       Returns the type checked expr *)
    let ensure_type_satisfies (expected : type_constraint) (expr : expr)
        (loc : maybe_loc) : expr =
      match (expected, type_check_expr expr env) with
      | ConstrainedTo expected_ty, (ConstrainedTo actual_ty, _)
        when actual_ty <> expected_ty ->
          raise (CheckError (TypeError (expr, expected_ty, actual_ty), loc))
      | _, (_, expr_tc) -> expr_tc
    in
    match stmt with
    | Declare (name, typ, init, scope, loc) ->
        (* cannot annotate a variable as having void type *)
        if typ = Void then
          raise (CheckError (NonFunctionAnnotatedAsVoid name, loc));
        (* if supplied, initialization expression must type check
           and match the annotated type. *)
        let init_tc =
          match init with
          | None -> None
          | Some expr ->
              Some (ensure_type_satisfies (ConstrainedTo typ) expr loc)
        in
        (* typecheck scope with env binding name->typ *)
        let ext_env = Env.add name typ env in
        let scope_tc = type_check_stmt_list scope ext_env in
        Declare (name, typ, init_tc, scope_tc, loc)
    | ArrayDeclare (name, typ, size, init, scope, loc) as decl ->
        (* cannot declare array of void types *)
        if typ = Void then
          raise (CheckError (NonFunctionAnnotatedAsVoid name, loc));

        (* Checks an array size expression by type checking it, then folding
           it to a constant value. *)
        let extract_and_check_size (size_expr : expr) : int * expr =
          let _, size_tc = type_check_expr size_expr env in
          match fold_expr size_tc emit_warning with
          | NumLiteral (size, _) as const_size_expr ->
              if size = 0 then emit_warning (ZeroSizeArray (name, decl));
              (size, const_size_expr)
          | _ ->
              raise
                (CheckError (NonConstantArraySize name, loc_from_expr size_expr))
        in
        (* Checks an array initializer by type checking each expression and
           ensuring they comply with the array's annotated type *)
        let check_initializer (exprs : expr list) : expr list =
          if List.length exprs = 0 then
            emit_warning (EmptyInitializer (name, decl));
          List.map
            (fun e ->
              (* every element in the initializer must satisfy the array's annotated type *)
              ensure_type_satisfies (ConstrainedTo typ) e (loc_from_expr e))
            exprs
        in

        let size_tc, init_tc =
          match (size, init) with
          (* only size specified *)
          | Some size, None ->
              let _, size_tc = extract_and_check_size size in
              (Some size_tc, None)
          (* only initializer specified *)
          | None, Some exprs -> (None, Some (check_initializer exprs))
          (* both size and initializer specified *)
          | Some size, Some exprs ->
              let const_size, size_tc = extract_and_check_size size in
              let exprs_tc = check_initializer exprs in
              let num_exprs = List.length exprs in
              (* initializer exceeds declared size *)
              if num_exprs > const_size then
                raise
                  (CheckError
                     (TooLargeInitializer (name, const_size, num_exprs), loc));

              (Some size_tc, Some exprs_tc)
          (* no size, no initializer *)
          | None, None -> raise (CheckError (UnderspecifiedArray name, loc))
        in
        (* typecheck scope with array name bound to pointer to element type *)
        let ext_env = Env.add name (Pointer typ) env in
        let scope_tc = type_check_stmt_list scope ext_env in

        ArrayDeclare (name, typ, size_tc, init_tc, scope_tc, loc)
    | Assign (lv, expr, loc) ->
        let lv_type, lv_tc = type_check_l_value lv env loc in
        let expr_tc = ensure_type_satisfies (ConstrainedTo lv_type) expr loc in
        Assign (lv_tc, expr_tc, loc)
    | Inr (lv, loc) ->
        let _, lv_tc = type_check_l_value lv env loc in
        Inr (lv_tc, loc)
    | Dcr (lv, loc) ->
        let _, lv_tc = type_check_l_value lv env loc in
        Dcr (lv_tc, loc)
    | If (cond, thn, loc) ->
        let cond_ty, cond_tc = type_check_expr cond env in
        expect_non_void cond cond_ty;
        If (cond_tc, type_check_stmt_list thn env, loc)
    | IfElse (cond, thn, els, loc) ->
        let cond_ty, cond_tc = type_check_expr cond env in
        expect_non_void cond cond_ty;
        IfElse
          ( cond_tc,
            type_check_stmt_list thn env,
            type_check_stmt_list els env,
            loc )
    | While (cond, body, loc) ->
        let cond_ty, cond_tc = type_check_expr cond env in
        expect_non_void cond cond_ty;
        While (cond_tc, type_check_stmt_list body env, loc)
    | Block (scope, loc) -> Block (type_check_stmt_list scope env, loc)
    | Return (maybe_expr, loc) ->
        let expr_tc =
          match maybe_expr with
          | Some expr ->
              (* returning an expression from a void function: bad *)
              if defn.return_ty = Void then
                raise
                  (CheckError (MismatchedReturn (defn.name, defn.return_ty), loc));
              (* returned expression should match return type of function *)
              Some
                (ensure_type_satisfies (ConstrainedTo defn.return_ty) expr loc)
          | None ->
              (* returning nothing from a non-void function: bad *)
              if defn.return_ty <> Void then
                raise
                  (CheckError (MismatchedReturn (defn.name, defn.return_ty), loc));
              None
        in
        Return (expr_tc, loc)
    | Exit (maybe_expr, loc) ->
        let expr_tc =
          match maybe_expr with
          | Some expr ->
              (* must exit with integer expression if given *)
              Some (ensure_type_satisfies (ConstrainedTo Int) expr loc)
          | None -> None
        in
        Exit (expr_tc, loc)
    | PrintDec (expr, loc) ->
        (* must print only signed integers on decimal display *)
        PrintDec (ensure_type_satisfies (ConstrainedTo Int) expr loc, loc)
    | ExprStmt (expr, loc) ->
        (* as long as expression internally type checks, we are good *)
        let _, expr_tc = type_check_expr expr env in
        ExprStmt (expr_tc, loc)
    | Assert (cond, loc) ->
        let cond_ty, cond_tc = type_check_expr cond env in
        expect_non_void cond cond_ty;
        Assert (cond_tc, loc)
  (* [type_check_stmt_list] checks a list of statements for type errors *)
  and type_check_stmt_list (stmts : stmt list) (env : ty env) : stmt list =
    List.map (fun stmt -> type_check_stmt stmt env) stmts
  in
  (* ensure all parameters are annotated as non-void *)
  List.iter
    (fun (name, typ) ->
      if typ = Void then
        raise (CheckError (NonFunctionAnnotatedAsVoid name, defn.loc)))
    defn.params;

  (* construct environment binding function parameters to their annotated types *)
  let env = Util.Env.of_list defn.params in

  (* type check the definition's body *)
  { defn with body = type_check_stmt_list defn.body env }

(* [ctrl_reaches_end] determines if the control flow of a function's
   body can reach the end of the function without encountering a return. *)
let ctrl_reaches_end (defn : func_defn) : bool =
  let rec can_pass_stmt (stmt : stmt) : bool =
    match stmt with
    | Declare (_, _, _, scope, _) | ArrayDeclare (_, _, _, _, scope, _) ->
        ctrl_reaches_end_stmt_list scope
    | IfElse (_, thn, els, _) ->
        ctrl_reaches_end_stmt_list thn || ctrl_reaches_end_stmt_list els
    | Block (scope, _) -> ctrl_reaches_end_stmt_list scope
    (* Return and exit *cannot* be passed. *)
    | Return _ | Exit _ -> false
    (* All of the following statements can be passed. *)
    | Assign _ | If _ | ExprStmt _ | While _ | PrintDec _ | Inr _ | Dcr _
    | Assert _ ->
        true
  and ctrl_reaches_end_stmt_list (stmts : stmt list) : bool =
    match stmts with
    | [] -> true
    | stmt :: rest ->
        if can_pass_stmt stmt then ctrl_reaches_end_stmt_list rest else false
  in
  ctrl_reaches_end_stmt_list defn.body

(* [check_funcs_are_unique] checks that function names are unique *)
let rec check_funcs_are_unique (defns : func_defn list) =
  match defns with
  | [] -> ()
  | defn :: rest -> (
      match List.filter (fun d -> d.name = defn.name) rest with
      | [] -> check_funcs_are_unique rest
      | dup :: _ -> raise (CheckError (MultipleDefinitions dup.name, dup.loc)))

(* [check_for_returns_in_main] checks that there are no return statements
   within the main function, and errors if there are. *)
let check_for_returns_in_main (main : func_defn) =
  check_for_stmt { defines = []; funcs = []; main } (function
    | Return (_, loc) -> raise (CheckError (ReturnInMain, loc))
    | _ -> false)
  |> ignore

(* [check] performs all validity checking on the input program,
   throwing an error if it finds problems. If it does not error,
   it returns a processed version of the input program which has
   casts removed and certain operators replaced to correspond with
   the types they are used with. *)
let check ?(emit_warning : compiler_warn_handler = fun _ -> ()) (pgrm : prog) :
    prog =
  (* main must have return type void *)
  if pgrm.main.return_ty <> Void then
    raise (CheckError (NonVoidMain, pgrm.main.loc));

  (* main cannot have return statements *)
  check_for_returns_in_main pgrm.main;

  (* function definitions must have unique names *)
  check_funcs_are_unique pgrm.funcs;

  let check_defn (defn : func_defn) : func_defn =
    (* check whether control can reach the end of each definition *)
    let reaches_end = ctrl_reaches_end defn in
    defn.ctrl_reaches_end <- Some reaches_end;

    (* control must not reach end of non-void function *)
    if reaches_end && defn.return_ty <> Void then
      raise (CheckError (CtrlReachesEndOfNonVoid defn.name, defn.loc));

    (* type check the function definition *)
    type_check defn pgrm.funcs emit_warning
  in
  {
    pgrm with
    main = check_defn pgrm.main;
    funcs = List.map check_defn pgrm.funcs;
  }
