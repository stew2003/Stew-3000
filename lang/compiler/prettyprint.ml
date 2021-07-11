open Ast
open Printf

(* [indent] indents with spaces at a given level *)
let rec indent (lvl : int) = if lvl = 0 then "" else "  " ^ indent (lvl - 1)

let rec pretty_print_expr (exp : expr) (is_sub_expr : bool) : string =
  match exp with
  | Num (n, _) -> sprintf "%d" n
  | Var (name, _) -> name
  | UnOp (op, operand, _) ->
      pretty_print_un_op op (pretty_print_expr operand true)
  | BinOp (op, left, right, _) ->
      pretty_print_bin_op op
        (pretty_print_expr left true)
        (pretty_print_expr right true)
        is_sub_expr
  | LogOp (op, _) -> pretty_print_log_op op is_sub_expr
  | Call (name, args, _) ->
      sprintf "%s(%s)" name
        (List.map (fun arg -> pretty_print_expr arg false) args
        |> String.concat ", ")

and pretty_print_un_op (op : un_op) (pretty_operand : string) : string =
  match op with BNot -> sprintf "~%s" pretty_operand

and pretty_print_bin_op (op : bin_op) (pretty_left : string)
    (pretty_right : string) (is_sub_expr : bool) : string =
  let pretty_bin_op = function
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "%"
    | BAnd -> "&"
    | BOr -> "|"
    | BXor -> "^"
    | Gt -> ">"
    | Lt -> "<"
    | Gte -> ">="
    | Lte -> "<="
    | Eq -> "=="
    | Neq -> "!="
  in
  let pretty_body =
    sprintf "%s %s %s" pretty_left (pretty_bin_op op) pretty_right
  in
  if is_sub_expr then "(" ^ pretty_body ^ ")" else pretty_body

and pretty_print_log_op (op : log_op) (is_sub_expr : bool) : string =
  match op with
  | LNot expr -> sprintf "!%s" (pretty_print_expr expr true)
  | LAnd (left, right) ->
      let pretty_land =
        sprintf "%s && %s"
          (pretty_print_expr left true)
          (pretty_print_expr right true)
      in
      if is_sub_expr then "(" ^ pretty_land ^ ")" else pretty_land
  | LOr (left, right) ->
      let pretty_lor =
        sprintf "%s || %s"
          (pretty_print_expr left true)
          (pretty_print_expr right true)
      in
      if is_sub_expr then "(" ^ pretty_lor ^ ")" else pretty_lor

and pretty_print_stmt (stmt : stmt) (indent_level : int) : string =
  match stmt with
  | Let (name, typ, expr, scope, _) ->
      sprintf "%s %s = %s;\n%s" (pretty_print_type typ) name
        (pretty_print_expr expr false)
        (pretty_print_stmt_list scope indent_level)
  | Assign (name, expr, _) ->
      sprintf "%s = %s;" name (pretty_print_expr expr false)
  | If (cond, thn, _) ->
      sprintf "if (%s) %s"
        (pretty_print_expr cond false)
        (pretty_print_block thn indent_level)
  | IfElse (cond, thn, els, _) ->
      sprintf "if (%s) %s else %s"
        (pretty_print_expr cond false)
        (pretty_print_block thn indent_level)
        (pretty_print_block els indent_level)
  | While (cond, body, _) ->
      sprintf "while (%s) %s"
        (pretty_print_expr cond false)
        (pretty_print_block body indent_level)
  | Block (body, _) -> pretty_print_block body indent_level
  | Return (Some expr, _) -> sprintf "return %s;" (pretty_print_expr expr false)
  | Return (None, _) -> "return;"
  | Exit (Some expr, _) -> sprintf "exit(%s);" (pretty_print_expr expr false)
  | Exit (None, _) -> "exit();"
  | ExprStmt (expr, _) -> sprintf "%s;" (pretty_print_expr expr false)
  | PrintDec (expr, _) -> sprintf "print(%s);" (pretty_print_expr expr false)
  | Inr (name, _) -> sprintf "%s++;" name
  | Dcr (name, _) -> sprintf "%s--;" name
  | Assert (expr, _) -> sprintf "assert(%s);" (pretty_print_expr expr false)

and pretty_print_stmt_list (stmts : stmt list) (indent_level : int) : string =
  stmts
  |> List.map (fun stmt ->
         sprintf "%s%s" (indent indent_level)
           (pretty_print_stmt stmt indent_level))
  |> String.concat "\n"

and pretty_print_block (block : stmt list) (indent_level : int) : string =
  match block with
  | [] -> "{}"
  | _ ->
      sprintf "{\n%s\n%s}"
        (pretty_print_stmt_list block (indent_level + 1))
        (indent indent_level)

and pretty_print_type (typ : ty) : string =
  match typ with Void -> "void" | Int -> "int"

and pretty_print_func_defn (defn : func_defn) : string =
  let printed_params =
    List.map
      (fun (name, typ) -> sprintf "%s %s" (pretty_print_type typ) name)
      defn.params
    |> String.concat ", "
  in
  sprintf "%s %s(%s) %s"
    (pretty_print_type defn.return_ty)
    defn.name printed_params
    (pretty_print_block defn.body 0)

and pretty_print_define (define : pp_define) : string =
  sprintf "#define %s %s" define.var (pretty_print_expr define.expression false)

(* [pretty_print] takes a program and produces a string that is 
  the program's text formatted nicely.  *)
let pretty_print (pgrm : prog) : string =
  let defines =
    List.map pretty_print_define pgrm.defines |> String.concat "\n"
  in
  [ defines ]
  @ List.map pretty_print_func_defn pgrm.funcs
  @ [ pretty_print_func_defn pgrm.main ]
  |> String.concat "\n\n"
