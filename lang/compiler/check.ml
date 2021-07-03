open Ast
open Util.Srcloc
open Util.Env
open Util.Err
open Printf

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

exception CheckError of check_err with_loc_opt

(* [string_of_check_err] turns a check error into a printable string. *)
let string_of_check_err = function
  | CtrlReachesEndOfNonVoid name ->
      sprintf "Control can reach the end of non-void function `%s`." name
  | MismatchedReturn (name, return_ty) ->
      sprintf "The %s function `%s` must return %s." (string_of_ty return_ty)
        name
        (if return_ty = Void then "nothing"
        else sprintf "a value of type %s" (string_of_ty return_ty))
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
  | NonFunctionAnnotatedAsVoid name ->
      sprintf "`%s` is not a function and therefore cannot have type void." name
  | ArityMismatch (name, expected, actual) ->
      sprintf "The function `%s` expects %d argument%s, but got %d." name
        expected
        (if expected = 1 then "" else "s")
        actual
  | MultipleDefinitions name ->
      sprintf "The function `%s` has multiple definitions." name
  | ReturnInMain ->
      "Cannot return from main function. Consider using exit() instead."

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

(* [type_check] checks a function definition for type errors, raising an 
  error if they are discovered. *)
let type_check (defn : func_defn) (defns : func_defn list) =
  (* [expect_non_void] checks that an expression is non-void type
     and raises an error if it is. *)
  let expect_non_void (exp : expr) (exp_ty : ty) =
    if exp_ty = Void then
      raise (CheckError (InvalidTypeError (exp, exp_ty), loc_from_expr exp))
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
  (* [type_check_expr] calculates the type of the given expression,
     or raises an error if it violates type rules. *)
  let rec type_check_expr (exp : expr) (env : ty env) : ty =
    match exp with
    | Num _ -> Int
    | Var (name, loc) -> lookup_var name env loc
    | UnOp (_, expr, _) ->
        (* operand can be any non-void type *)
        let expr_ty = type_check_expr expr env in
        expect_non_void expr expr_ty;
        expr_ty
    | BinOp (op, left, right, loc) -> (
        (* left and right should type check internally *)
        let left_expr_ty = type_check_expr left env in
        let right_expr_ty = type_check_expr right env in
        (* left and right operand types must match *)
        if left_expr_ty <> right_expr_ty then
          raise
            (CheckError
               ( TypeMismatch
                   (describe_bin_op op, left, left_expr_ty, right, right_expr_ty),
                 loc ));
        (* neither left nor right can be void *)
        expect_non_void left left_expr_ty;
        expect_non_void right right_expr_ty;
        (* comparison operators evaluate to an int, other bin ops evaluate
           to same type as the operands *)
        match op with
        | Gt | Lt | Gte | Lte | Eq | Neq -> Int
        | _ -> left_expr_ty)
    | LogOp (log_op, _) -> (
        match log_op with
        | LNot expr ->
            (* expression needs to be type checked and cannot be void *)
            expect_non_void expr (type_check_expr expr env);
            (* log ops are always ints *)
            Int
        | LAnd (left, right) | LOr (left, right) ->
            (* left and right should type check internally *)
            let left_expr_ty = type_check_expr left env in
            let right_expr_ty = type_check_expr right env in
            (* neither left nor right can be void *)
            expect_non_void left left_expr_ty;
            expect_non_void right right_expr_ty;
            (* log ops are always ints *)
            Int)
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
                called_defn.params args;
              (* function call has type of function's return type *)
              called_defn.return_ty
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
  (* [type_check_stmt] checks a single statement for type errors *)
  and type_check_stmt (stmt : stmt) (env : ty env) =
    match stmt with
    | Let (name, typ, expr, scope, loc) ->
        (* cannot annotate a variable as having void type *)
        if typ = Void then
          raise (CheckError (NonFunctionAnnotatedAsVoid name, loc));
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
    | Inr (name, loc) | Dcr (name, loc) ->
        (* lookup will throw unbound error if var is unbound *)
        lookup_var name env loc |> ignore
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
            (* returning an expression from a void function: bad *)
            if defn.return_ty = Void then
              raise
                (CheckError (MismatchedReturn (defn.name, defn.return_ty), loc));
            (* returned expression should match return type of function *)
            let expr_ty = type_check_expr expr env in
            if defn.return_ty <> expr_ty then
              raise
                (CheckError (TypeError (expr, defn.return_ty, expr_ty), loc))
        | None ->
            (* returning nothing from a non-void function: bad *)
            if defn.return_ty <> Void then
              raise
                (CheckError (MismatchedReturn (defn.name, defn.return_ty), loc))
        )
    | Exit (maybe_expr, loc) -> (
        match maybe_expr with
        | Some expr ->
            let expr_ty = type_check_expr expr env in
            (* must exit with integer expression if given *)
            if expr_ty <> Int then
              raise (CheckError (TypeError (expr, Int, expr_ty), loc))
        | None -> ())
    | PrintDec (expr, loc) ->
        let expr_ty = type_check_expr expr env in
        (* must print only integers on decimal display *)
        if expr_ty <> Int then
          raise (CheckError (TypeError (expr, Int, expr_ty), loc))
    | ExprStmt (expr, _) ->
        (* as long as expression internally type checks, we are good *)
        type_check_expr expr env |> ignore
    | Assert (cond, _) ->
        let cond_ty = type_check_expr cond env in
        expect_non_void cond cond_ty
  and type_check_stmt_list (stmts : stmt list) (env : ty env) =
    List.iter (fun stmt -> type_check_stmt stmt env) stmts
  in
  (* ensure all parameters are annotated as non-void *)
  List.iter
    (fun (name, typ) ->
      if typ = Void then
        raise (CheckError (NonFunctionAnnotatedAsVoid name, defn.loc)))
    defn.params;

  (* construct environment binding function parameters to their annotated types *)
  let env = Util.Env.of_list defn.params in

  (* type check the defintion's body *)
  type_check_stmt_list defn.body env

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
  throwing an error if it finds problems. *)
let check (pgrm : prog) =
  (* main must have return type void *)
  if pgrm.main.return_ty <> Void then
    raise (CheckError (NonVoidMain, pgrm.main.loc));

  (* main cannot have return statements *)
  check_for_returns_in_main pgrm.main;

  (* function definitions must have unique names *)
  check_funcs_are_unique pgrm.funcs;

  pgrm.main :: pgrm.funcs
  |> List.iter (fun defn ->
         (* check whether control can reach the end of each definition *)
         let reaches_end = ctrl_reaches_end defn in
         defn.ctrl_reaches_end <- Some reaches_end;

         (* control must not reach end of non-void function *)
         if reaches_end && defn.return_ty <> Void then
           raise (CheckError (CtrlReachesEndOfNonVoid defn.name, defn.loc));

         (* type check the function definition *)
         type_check defn pgrm.funcs)
