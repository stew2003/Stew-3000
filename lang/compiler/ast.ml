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
  | Num of int * src_loc
  | Var of string * src_loc
  | UnOp of un_op * expr * src_loc
  | BinOp of bin_op * expr * expr * src_loc
  | LogOp of log_op * src_loc
  | Call of string * expr list * src_loc

type stmt =
  | Let of string * ty * expr * stmt list * src_loc
  | Assign of string * expr * src_loc
  | If of expr * stmt list * src_loc
  | IfElse of expr * stmt list * stmt list * src_loc
  | Block of stmt list * src_loc
  | Return of expr option * src_loc
  | ExprStmt of expr * src_loc
  | While of expr * stmt list * src_loc
  (* sends to decimal display *)
  | PrintDec of expr * src_loc
  (* Inr/Dcr mutate a variable, incrementing/decrementing *)
  | Inr of string * src_loc
  | Dcr of string * src_loc
  (* Halts program, emitting expr to decimal display *)
  | Exit of expr option * src_loc
  | Assert of expr * src_loc

type func_defn = {
  name : string;
  params : (string * ty) list;
  body : stmt list;
  return_ty : ty;
  loc : src_loc;
}

(* main is a special function with void return/no args
  whose body is what is run when the program is run *)
type prog = { funcs : func_defn list; main : func_defn }

(* [lookup] finds Some defn that matches the given name *)
let lookup (func : string) (defns : func_defn list) : func_defn option =
  try Some (List.find (fun defn -> defn.name = func) defns)
  with Not_found -> None
