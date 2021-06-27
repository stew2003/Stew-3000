open Ast
open Util.Srcloc
open Printf

type check_err =
  (* When control flow can reach the end of a non-void function *)
  | CtrlReachesEndOfNonVoid of func_defn
  (* When a void function returns a value, or a non-void returns without a value *)
  | MismatchedReturn of func_defn
  (* When a variable is referenced without being bound first *)
  | UnboundVariable of string
  (* When a function is called without a definition *)
  | UndefinedFunction of string
  (* (expression, expected type, actual type) *)
  | TypeError of expr * ty * ty
  (* expression cannot be this type *)
  | InvalidTypeError of expr * ty
  (* operation with two types that should be the same but aren't *)
  | TypeMismatch of string * expr * ty * expr * ty
  (* main was typed as non-void *)
  | NonVoidMain
  (* variable was annotated as void *)
  | VoidVar of string
  (* mismatched number of arguments supplied to a function call *)
  | ArityMismatch of string * int * int

exception CheckError of check_err with_loc_opt

(* [string_of_check_err] turns a check error into a printable string. *)
let string_of_check_err = function
  | CtrlReachesEndOfNonVoid defn ->
      sprintf "Control can reach the end of non-void function `%s`." defn.name
  | MismatchedReturn defn ->
      sprintf "The %s function `%s` must return %s."
        (string_of_ty defn.return_ty)
        defn.name
        (if defn.return_ty = Void then "nothing."
        else sprintf "a value of type %s." (string_of_ty defn.return_ty))
  | UnboundVariable var -> sprintf "The variable `%s` is unbound." var
  | UndefinedFunction name -> sprintf "The function `%s` is undefined." name
  | TypeError (e, expected, actual) ->
      sprintf
        "The type of this %s expression was expected to be %s, but was %s."
        (describe_expr e) (string_of_ty expected) (string_of_ty actual)
  | InvalidTypeError (e, actual) ->
      sprintf "Expected this %s expression to not have type %s."
        (describe_expr e) (string_of_ty actual)
  | TypeMismatch (op_name, left, left_ty, right, right_ty) ->
      sprintf
        "Operation %s expected the %s expression (of type %s) to be the same \
         type as the %s expression (of type %s)"
        op_name (describe_expr left) (string_of_ty left_ty)
        (describe_expr right) (string_of_ty right_ty)
  | NonVoidMain -> "The main function must have return type void."
  | VoidVar name -> sprintf "The variable `%s` cannot have type void." name
  | ArityMismatch (name, expected, actual) ->
      sprintf "The function `%s` expects %d arguments, but got %d." name
        expected actual

(* [ctrl_reaches_end] determines if the control flow of a function's
  body can reach the end of the function without encountering a return. *)
let ctrl_reaches_end (defn : func_defn) : bool =
  let rec can_pass_stmt (stmt : stmt) : bool =
    match stmt with
    | Let (_, _, _, scope, _) -> ctrl_reaches_end_stmt_list scope
    | IfElse (_, thn, els, _) ->
        ctrl_reaches_end_stmt_list thn || ctrl_reaches_end_stmt_list els
    | Block (scope, _) -> ctrl_reaches_end_stmt_list scope
    | Return _ | Exit _ -> false
    | _ -> true
  and ctrl_reaches_end_stmt_list (stmts : stmt list) : bool =
    match stmts with
    | [] -> true
    | stmt :: rest ->
        if can_pass_stmt stmt then ctrl_reaches_end_stmt_list rest else false
  in
  ctrl_reaches_end_stmt_list defn.body

(* [type_check] checks the program for type errors, raising an 
  error if they are discovered. *)
let type_check (defn : func_defn) (defns : func_defn list) =
  let expect_non_void (exp : expr) (exp_ty : ty) =
    if exp_ty = Void then
      raise (CheckError (InvalidTypeError (exp, exp_ty), loc_from_expr exp))
  in

  let lookup_var (name : string) (env : ty env) (loc : maybe_loc) : ty =
    match Env.find_opt name env with
    | Some typ ->
        if typ = Void then
          raise (InternalError "found void variable type in type environment");
        typ
    | None -> raise (CheckError (UnboundVariable name, loc))
  in

  let rec type_check_expr (exp : expr) (env : ty env) : ty =
    match expr with
    | Num _ -> Int
    | Var (name, loc) -> lookup_var name env loc
    | UnOp (_, expr, _) ->
        let expr_ty = type_check_expr expr env in
        expect_non_void expr expr_ty;
        expr_ty
    | BinOp (op, left, right, loc) -> (
        let left_expr_ty = type_check_expr left env in
        expect_non_void left left_expr_ty;
        let right_expr_ty = type_check_expr right env in
        expect_non_void right right_expr_ty;
        if left_expr_ty <> right_expr_ty then
          raise
            (CheckError
               ( TypeMismatch
                   (describe_bin_op op, left, left_expr_ty, right, right_expr_ty),
                 loc ));
        match op with
        | Gt | Lt | Gte | Lte | Eq | Neq -> Int
        | _ -> left_expr_ty)
    | LogOp (log_op, loc) ->
        (* TODO: check how C implements this *)
        failwith "not implemented"
    | Call (name, args, loc) -> (
        match lookup name defns with
        | Some called_defn -> (
            try
              List.iter2
                (fun (_, param_ty) arg ->
                  let arg_ty = type_check_expr arg env in
                  (* annotated param type and arg type should match *)
                  if param_ty <> arg_ty then
                    raise
                      (CheckError
                         (TypeError (arg, param_ty, arg_ty), loc_from_expr arg)))
                called_defn.params args
            with Invalid_argument ->
              raise
                (CheckError
                   (ArityMismatch
                      ( called_defn.name,
                        List.length called_defn.params,
                        List.length args ))))
        | None -> CheckError (UndefinedFunction name, loc))
  and type_check_stmt (stmt : stmt) (env : ty env) =
    match stmt with
    | Let (name, typ, expr, scope, loc) ->
        (* cannot annotate a variable as having void type *)
        if typ = Void then raise (CheckError (VoidVar name), loc);
        let expr_ty = type_check_expr expr env in
        (* type of expression must match annotated type *)
        if expr_ty <> typ then
          raise (CheckError (TypeError (expr, typ, expr_ty), loc));
        let ext_env = Env.add name typ env in
        type_check_stmt_list scope ext_env
    | Assign (name, expr, loc) ->
        let typ = lookup_var name env loc in
        let expr_ty = type_check_expr expr env in
        if expr_ty <> typ then
          raise (CheckError (TypeError (expr, typ, expr_ty), loc))
    | Inr (name, loc) | Dcr (name, loc) -> lookup_var name env loc
    | If (cond, thn, _) ->
        let cond_ty = type_check_expr cond env in
        expect_non_void cond cond_ty;
        type_check_stmt_list thn env
    | IfElse (cond, thn, els, _) ->
        let cond_ty = type_check_expr cond env in
        expect_non_void cond cond_ty;
        type_check_stmt_list thn env;
        type_check_stmt_list els env
    | While (cond, body, _) ->
        let cond_ty = type_check_expr cond env in
        expect_non_void cond cond_ty;
        type_check_stmt_list body env
    | Block (scope, _) -> type_check_stmt_list scope env
    | Return (maybe_expr, loc) -> (
        match maybe_expr with
        | Some expr ->
            if defn.return_ty = Void then
              raise (CheckError (MismatchedReturn defn), loc);
            let expr_ty = type_check_expr expr env in
            if defn.return_ty <> expr_ty then
              raise
                (CheckError (TypeError (expr, defn.return_ty, expr_ty), loc))
        | None ->
            if defn.return_ty <> Void then
              raise (CheckError (MismatchedReturn defn), loc))
    | Exit (maybe_expr, loc) -> (
        match maybe_expr with
        | Some expr ->
            let expr_ty = type_check_expr expr env in
            if expr_ty <> Int then
              raise (CheckError (TypeError (expr, Int, expr_ty), loc))
        | None -> ())
    | PrintDec (expr, loc) ->
        let expr_ty = type_check_expr expr env in
        if expr_ty <> Int then
          raise (CheckError (TypeError (expr, Int, expr_ty), loc))
    | ExprStmt (expr, _) -> type_check_expr expr env
    | Assert (cond, _) ->
        let cond_ty = type_check_expr cond env in
        expect_non_void cond cond_ty
  and type_check_stmt_list (stmts : stmt list) (env : ty env) =
    List.iter (fun stmt -> type_check_stmt stmt env) stmts
  in

  (* TODO: check defn parameters for non-void *)
  type_check_stmt_list defn.body Env.empty

(* [check] performs all validity checking on the input program,
  throwing an error if it finds problems. *)
(* TODO: check for returns in main *)
let check (pgrm : prog) =
  (* main must have return type void *)
  if pgrm.main.return_ty <> Void then raise (CheckError NonVoidMain);
  (* check whether control can reach the end of each definition *)
  List.iter
    (fun defn ->
      defn.ctrl_reaches_end <- ctrl_reaches_end defn;
      if defn.ctrl_reaches_end && defn.return_ty <> Void then
        raise (CheckError (CtrlReachesEndOfNonVoid defn)))
    (pgrm.main :: pgrm.funcs)
