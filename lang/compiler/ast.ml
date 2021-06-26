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
  loc : maybe_loc;
}

(* main is a special function with void return/no args
  whose body is what is run when the program is run *)
type prog = { funcs : func_defn list; main : func_defn }

(* [lookup] finds Some defn that matches the given name *)
let lookup (name : string) (defns : func_defn list) : func_defn option =
  List.find_opt (fun defn -> defn.name = name) defns
