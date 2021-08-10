open Util.Srcloc

(* Types:
    - void: for function return types
    - int: 8 bit signed integer (two's complement)
    - unsigned: 8 bit unsigned integer
    - char: 8 bit ASCII character
    - pointer: 8 bit memory address
*)
type ty = Void | Int | Unsigned | Char | Pointer of ty

type un_op = BNot | LNot

type bin_op =
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | BAnd
  | BOr
  | BXor
  | LAnd
  | LOr
  | Gt
  | Lt
  | Gte
  | Lte
  | UnsignedGt
  | UnsignedLt
  | UnsignedGte
  | UnsignedLte
  | Eq
  | Neq

type l_value = LVar of string * maybe_loc | LDeref of expr * maybe_loc

and expr =
  | NumLiteral of int * maybe_loc
  | CharLiteral of char * maybe_loc
  | Var of string * maybe_loc
  | UnOp of un_op * expr * maybe_loc
  | BinOp of bin_op * expr * expr * maybe_loc
  | Call of string * expr list * maybe_loc
  | Deref of expr * maybe_loc
  | AddrOf of l_value * maybe_loc
  | Cast of ty * expr * maybe_loc

type stmt =
  | Declare of string * ty * expr option * stmt list * maybe_loc
  | Assign of l_value * expr * maybe_loc
  (* Inr/Dcr mutate a variable, incrementing/decrementing *)
  | Inr of l_value * maybe_loc
  | Dcr of l_value * maybe_loc
  | If of expr * stmt list * maybe_loc
  | IfElse of expr * stmt list * stmt list * maybe_loc
  | While of expr * stmt list * maybe_loc
  | Block of stmt list * maybe_loc
  | Return of expr option * maybe_loc
  | ExprStmt of expr * maybe_loc
  (* sends to decimal display *)
  | PrintDec of expr * maybe_loc
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
let rec string_of_ty (t : ty) : string =
  match t with
  | Void -> "void"
  | Int -> "int"
  | Unsigned -> "unsigned"
  | Char -> "char"
  | Pointer typ -> Printf.sprintf "%s *" (string_of_ty typ)

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
  | LAnd -> "logical and"
  | LOr -> "logical or"
  | Gt -> "greater than"
  | Lt -> "less than"
  | Gte -> "greater than or equal"
  | Lte -> "less than or equal"
  | UnsignedGt -> "unsigned greater than"
  | UnsignedLt -> "unsigned less than"
  | UnsignedGte -> "unsigned greater than or equal"
  | UnsignedLte -> "unsigned less than or equal"
  | Eq -> "equality"
  | Neq -> "inequality"

(* [describe_un_op] returns a description of a unary operator *)
let describe_un_op (op : un_op) : string =
  match op with BNot -> "bitwise not" | LNot -> "logical not"

(* [describe_expr] returns an abstract description of a given expression. *)
let describe_expr (e : expr) : string =
  match e with
  | NumLiteral _ -> "number"
  | CharLiteral _ -> "character"
  | Var _ -> "variable"
  | UnOp (op, _, _) -> describe_un_op op
  | BinOp (op, _, _, _) -> describe_bin_op op
  | Call _ -> "function call"
  | Deref _ -> "dereference"
  | AddrOf _ -> "address-of"
  | Cast _ -> "cast"

(* [loc_from_expr] extracts the source location from an expression *)
let loc_from_expr (exp : expr) : maybe_loc =
  match exp with
  | NumLiteral (_, loc)
  | CharLiteral (_, loc)
  | Var (_, loc)
  | UnOp (_, _, loc)
  | BinOp (_, _, _, loc)
  | Call (_, _, loc)
  | Deref (_, loc)
  | AddrOf (_, loc)
  | Cast (_, _, loc) ->
      loc

(* [check_for_expr] determines if the program contains an
   expression that satisfies the given predicate *)
let check_for_expr (pgrm : prog) (pred : expr -> bool) : bool =
  (* [check_l_value] determines if the given l-value contains an
     expression that satisfies the predicate *)
  let rec check_l_value (lv : l_value) (pred : expr -> bool) : bool =
    match lv with LDeref (e, _) -> pred e | LVar _ -> false
  (* [check_expr] determines if the given expression contains a
     sub-expression that satisfies the given predicate. *)
  and check_expr (exp : expr) (pred : expr -> bool) : bool =
    (* true if either the expression or its subexpressions yield true *)
    pred exp
    ||
    match exp with
    | UnOp (_, operand, _) -> check_expr operand pred
    | BinOp (_, left, right, _) -> check_expr left pred || check_expr right pred
    | Call (_, args, _) ->
        List.map (fun arg -> check_expr arg pred) args
        |> List.fold_left ( || ) false
    | Deref (e, _) -> check_expr e pred
    | AddrOf (lv, _) -> check_l_value lv pred
    | Cast (_, e, _) -> check_expr e pred
    | NumLiteral _ | CharLiteral _ | Var _ -> false
  (* [check_stmt] determines if the given statement contains an
     expression that satisfies the given predicate *)
  and check_stmt (stmt : stmt) (pred : expr -> bool) : bool =
    (* NOTE: We are checking for sub-expressions here so we
       only examine statements that contain sub-expressions. *)
    match stmt with
    | Declare (_, _, value, body, _) ->
        (match value with None -> false | Some value -> check_expr value pred)
        || check_stmt_list body pred
    | Assign (lv, value, _) -> check_l_value lv pred || check_expr value pred
    | Inr (lv, _) | Dcr (lv, _) -> check_l_value lv pred
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
    | Return _ | Exit _ -> false
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
    | Declare (_, _, _, body, _) -> check_stmt_list body pred
    | If (_, thn, _) -> check_stmt_list thn pred
    | IfElse (_, thn, els, _) ->
        check_stmt_list thn pred || check_stmt_list els pred
    | Block (stmts, _) -> check_stmt_list stmts pred
    | While (_, body, _) -> check_stmt_list body pred
    | Assign _ | Return _ | ExprStmt _ | PrintDec _ | Inr _ | Dcr _ | Exit _
    | Assert _ ->
        false
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
