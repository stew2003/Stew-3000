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
  (* When a variable is used before initializing: assign/inr/dcr *)
  | UseBeforeInit of string
  (* (expression, expected type, actual type) *)
  | TypeError of expr * ty * ty

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
  | UseBeforeInit var ->
      sprintf "The variable `%s` must be initialized before it can be used." var
  | TypeError (e, expected, actual) ->
      sprintf
        "The type of this %s expression was expected to be %s, but was %s."
        (describe_expr e) (string_of_ty expected) (string_of_ty actual)

(* [ctrl_reaches_end] determines if the control flow of a function's
  body can reach the end of the function without encountering a return. *)
let ctrl_reaches_end (_defn : func_defn) : bool = failwith "not implemented"

(* [type_check] checks the program for type errors, raising an 
  error if they are discovered. *)
let type_check (_pgrm : prog) = failwith "not implemented"

(* [check] performs all validity checking on the input program,
  throwing an error if it finds problems. *)
let check (_pgrm : prog) = failwith "not implemented"
