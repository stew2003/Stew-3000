type bin_op =
  | Plus
  | Minus
  | Mult
  | Div
  | BAnd
  | BOr
  | BXor
  | LAnd
  | LOr
  | Gt
  | Lt
  | Gte
  | Lte
  | Eq

type un_op = BNot | LNot

type expr =
  | Num of int
  | Var of string
  | BinOp of bin_op * expr * expr
  | UnOp of un_op * expr
  | Call of string * expr list

type stmt =
  | Let of string * expr * stmt list
  | Assign of string * expr
  | If of expr * stmt list
  | IfElse of expr * stmt list * stmt list
  | Block of stmt list
  | Return of expr
  | ExprStmt of expr
  | While of expr * stmt list
  (* sends to decimal display *)
  | Print of expr
  | Inr of string
  | Dcr of string

type func_defn = { name : string; params : string list; body : stmt list }

type prog = { funcs : func_defn list; main : stmt list }
