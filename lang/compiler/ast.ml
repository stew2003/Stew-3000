open Util.Srcloc

type ty = Void | Int

type un_op = BNot

type bin_op =
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | BAnd
  | BOr
  | BXor
  | Gt
  | Lt
  | Gte
  | Lte
  | Eq
  | Neq

type log_op = LNot of expr | LAnd of expr * expr | LOr of expr * expr

and expr =
  | Num of int * maybe_loc
  | Var of string * maybe_loc
  | UnOp of un_op * expr * maybe_loc
  | BinOp of bin_op * expr * expr * maybe_loc
  | LogOp of log_op * maybe_loc
  | Call of string * expr list * maybe_loc

type stmt =
  | Let of string * ty * expr * stmt list * maybe_loc
  | Assign of string * expr * maybe_loc
  | If of expr * stmt list * maybe_loc
  | IfElse of expr * stmt list * stmt list * maybe_loc
  | Block of stmt list * maybe_loc
  | Return of expr option * maybe_loc
  | ExprStmt of expr * maybe_loc
  | While of expr * stmt list * maybe_loc
  (* sends to decimal display *)
  | PrintDec of expr * maybe_loc
  (* Inr/Dcr mutate a variable, incrementing/decrementing *)
  | Inr of string * maybe_loc
  | Dcr of string * maybe_loc
  (* Halts program, emitting expr to decimal display *)
  | Exit of expr option * maybe_loc
  | Assert of expr * maybe_loc

type func_defn = {
  name : string;
  params : (string * ty) list;
  body : stmt list;
  return_ty : ty;
  (* Can control flow reach the end of this function's body? This is
     initialized to None by the parser, but is set during checking *)
  mutable ctrl_reaches_end : bool option;
  loc : maybe_loc;
}

(* Represents a preprocessor definition, which binds a name to an expression. *)
type pp_define = { var : string; expression : expr; loc : maybe_loc }

(* main is a special function with void return/no args
  whose body is what is run when the program is run *)
type prog = {
  defines : pp_define list;
  funcs : func_defn list;
  main : func_defn;
}

(* [lookup] finds Some defn that matches the given name *)
let lookup (name : string) (defns : func_defn list) : func_defn option =
  List.find_opt (fun (defn : func_defn) -> defn.name = name) defns

(* [string_of_ty] turns a type into a string *)
let string_of_ty (t : ty) : string =
  match t with Void -> "void" | Int -> "int"

(* [describe_bin_op] returns a description of a binary operator *)
let describe_bin_op (op : bin_op) : string =
  match op with
  | Plus -> "addition"
  | Minus -> "subtraction"
  | Mult -> "multiplication"
  | Div -> "division"
  | Mod -> "modulus"
  | BAnd -> "bitwise and"
  | BOr -> "bitwise or"
  | BXor -> "bitwise xor"
  | Gt -> "greater than"
  | Lt -> "less than"
  | Gte -> "greater than or equal"
  | Lte -> "less than or equal"
  | Eq -> "equality"
  | Neq -> "inequality"

(* [describe_un_op] returns a description of a unary operator *)
let describe_un_op (op : un_op) : string = match op with BNot -> "bitwise not"

(* [describe_log_op] returns a description of a logical operator *)
let describe_log_op (op : log_op) : string =
  match op with
  | LNot _ -> "logical not"
  | LAnd _ -> "logical and"
  | LOr _ -> "logical or"

(* [describe_expr] returns an abstract description of a given expression. *)
let describe_expr (e : expr) : string =
  match e with
  | Num _ -> "number"
  | Var _ -> "variable"
  | UnOp (op, _, _) -> describe_un_op op
  | BinOp (op, _, _, _) -> describe_bin_op op
  | LogOp (op, _) -> describe_log_op op
  | Call _ -> "function call"

(* [loc_from_expr] extracts the source location from an expression *)
let loc_from_expr (exp : expr) : maybe_loc =
  match exp with
  | Num (_, loc)
  | Var (_, loc)
  | UnOp (_, _, loc)
  | BinOp (_, _, _, loc)
  | LogOp (_, loc)
  | Call (_, _, loc) ->
      loc

(* [check_for_expr] determines if the program contains an 
  expression that satisfies the given predicate *)
let check_for_expr (pgrm : prog) (pred : expr -> bool) : bool =
  (* [check_expr] determines if the given expression contains a
     sub-expression that satisfies the given predicate. *)
  let rec check_expr (exp : expr) (pred : expr -> bool) : bool =
    (* true if either the expression or its subexpressions yield true *)
    pred exp
    ||
    match exp with
    | UnOp (_, operand, _) | LogOp (LNot operand, _) -> check_expr operand pred
    | BinOp (_, left, right, _)
    | LogOp (LAnd (left, right), _)
    | LogOp (LOr (left, right), _) ->
        check_expr left pred || check_expr right pred
    | Call (_, args, _) ->
        List.map (fun arg -> check_expr arg pred) args
        |> List.fold_left ( || ) false
    | _ -> false
  (* [check_stmt] determines if the given statement contains an
     expression that satisfies the given predicate *)
  and check_stmt (stmt : stmt) (pred : expr -> bool) : bool =
    (* NOTE: We are checking for sub-expressions here so we
       only examine statements that contain sub-expressions. *)
    match stmt with
    | Let (_, _, value, body, _) ->
        check_expr value pred || check_stmt_list body pred
    | Assign (_, value, _) -> check_expr value pred
    | If (cond, thn, _) -> check_expr cond pred || check_stmt_list thn pred
    | IfElse (cond, thn, els, _) ->
        check_expr cond pred || check_stmt_list thn pred
        || check_stmt_list els pred
    | Block (stmts, _) -> check_stmt_list stmts pred
    | Return (Some value, _) -> check_expr value pred
    | ExprStmt (value, _) -> check_expr value pred
    | While (cond, body, _) -> check_expr cond pred || check_stmt_list body pred
    | PrintDec (value, _) -> check_expr value pred
    | Exit (Some e, _) -> check_expr e pred
    | Assert (e, _) -> check_expr e pred
    | _ -> false
  (* [check_stmt_list] determines if the given statement list
     contains an expression that satisfies the predicate *)
  and check_stmt_list (stmts : stmt list) (pred : expr -> bool) : bool =
    stmts
    |> List.map (fun stmt -> check_stmt stmt pred)
    |> List.fold_left ( || ) false
  in
  List.map
    (fun defn -> check_stmt_list defn.body pred)
    (pgrm.funcs @ [ pgrm.main ])
  |> List.fold_left ( || ) false

(* [check_for_stmt] checks a given program for a statement 
  that satisfies the given predicate *)
let check_for_stmt (pgrm : prog) (pred : stmt -> bool) : bool =
  (* [check_stmt] checks a statement for the presence of a statement
     that satisfies the given predicate *)
  let rec check_stmt (stmt : stmt) (pred : stmt -> bool) : bool =
    pred stmt
    ||
    (* NOTE: We are checking for sub-statements here so only
       check the statements that have them. *)
    match stmt with
    | Let (_, _, _, body, _) -> check_stmt_list body pred
    | If (_, thn, _) -> check_stmt_list thn pred
    | IfElse (_, thn, els, _) ->
        check_stmt_list thn pred || check_stmt_list els pred
    | Block (stmts, _) -> check_stmt_list stmts pred
    | While (_, body, _) -> check_stmt_list body pred
    | _ -> false
  (* [check_stmt_list] checks a list of statements for one that satisfies
     the given predicate *)
  and check_stmt_list (stmts : stmt list) (pred : stmt -> bool) : bool =
    stmts
    |> List.map (fun stmt -> check_stmt stmt pred)
    |> List.fold_left ( || ) false
  in
  List.map
    (fun defn -> check_stmt_list defn.body pred)
    (pgrm.funcs @ [ pgrm.main ])
  |> List.fold_left ( || ) false
