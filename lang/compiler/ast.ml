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
  mutable ctrl_reaches_end : bool;
  loc : maybe_loc;
}

(* main is a special function with void return/no args
  whose body is what is run when the program is run *)
type prog = { funcs : func_defn list; main : func_defn }

(* [lookup] finds Some defn that matches the given name *)
let lookup (name : string) (defns : func_defn list) : func_defn option =
  List.find_opt (fun defn -> defn.name = name) defns

(* [string_of_ty] turns a type into a string *)
let string_of_ty (t : ty) : string =
  match t with Void -> "void" | Int -> "int"

(* [describe_expr] returns an abstract description of a
  given expression. *)
let describe_expr (e : expr) : string =
  let describe_un_op (op : un_op) : string =
    match op with BNot -> "bitwise not"
  in
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
  in
  let describe_log_op (op : log_op) : string =
    match op with
    | LNot _ -> "logical not"
    | LAnd _ -> "logical and"
    | LOr _ -> "logical or"
  in
  match e with
  | Num _ -> "number"
  | Var _ -> "variable"
  | UnOp (op, _, _) -> describe_un_op op
  | BinOp (op, _, _, _) -> describe_bin_op op
  | LogOp (op, _) -> describe_log_op op
  | Call _ -> "function call"
