open Ast
open Printf

(* [indent] returns a string representing the given level of indentation. *)
let rec indent (lvl : int) = if lvl = 0 then "" else "  " ^ indent (lvl - 1)

(* [pretty_print_un_op] converts a unary operator into a pretty-printed string. *)
let pretty_print_un_op ?(is_nested_in_op = false) (op : un_op)
    (pretty_operand : string) : string =
  let pretty_un_op = function BNot -> "~" | LNot -> "!" in
  sprintf
    (if is_nested_in_op then "%s%s" else "%s(%s)")
    (pretty_un_op op) pretty_operand

(* [pretty_bin_op] converts a bin_op into its surface syntax form *)
let pretty_bin_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | BAnd -> "&"
  | BOr -> "|"
  | BXor -> "^"
  | Gt | UnsignedGt -> ">"
  | Lt | UnsignedLt -> "<"
  | Gte | UnsignedGte -> ">="
  | Lte | UnsignedLte -> "<="
  | Eq -> "=="
  | Neq -> "!="
  | LAnd -> "&&"
  | LOr -> "||"

(* [wrap_if_nested] wraps a string in parentheses if it is nested in
   another expression *)
let wrap_if_nested (s : string) (nested : bool) : string =
  sprintf (if nested then "(%s)" else "%s") s

(* [pretty_print_bin_op] converts a binary operator expression into a pretty-printed string. *)
let pretty_print_bin_op ?(is_nested_in_op = false) (op : bin_op)
    (pretty_left : string) (pretty_right : string) : string =
  wrap_if_nested
    (sprintf "%s %s %s" pretty_left (pretty_bin_op op) pretty_right)
    is_nested_in_op

(* [pretty_print_expr] converts an expression into a pretty-printed string. *)
let rec pretty_print_expr ?(is_nested_in_op = false) (exp : expr) : string =
  match exp with
  | NumLiteral (n, _) -> sprintf "%d" n
  | CharLiteral (c, _) -> sprintf "'%c'" c
  | Var (name, _) -> name
  | UnOp (op, operand, _) ->
      pretty_print_un_op op (pretty_print_expr operand ~is_nested_in_op:true)
  | BinOp (op, left, right, _) ->
      pretty_print_bin_op op
        (pretty_print_expr left ~is_nested_in_op:true)
        (pretty_print_expr right ~is_nested_in_op:true)
        ~is_nested_in_op
  | Call (name, args, _) ->
      sprintf "%s(%s)" name
        (List.map pretty_print_expr args |> String.concat ", ")
  | Deref (e, _) -> sprintf "*%s" (pretty_print_expr ~is_nested_in_op:true e)
  | AddrOf (e, _) -> sprintf "&%s" (pretty_print_expr ~is_nested_in_op:true e)
  | Cast (typ, e, _) ->
      sprintf "(%s)%s" (pretty_print_type typ)
        (pretty_print_expr ~is_nested_in_op:true e)
  | Assign (dest, expr, _) ->
      wrap_if_nested
        (sprintf "%s = %s" (pretty_print_expr dest)
           (pretty_print_expr ~is_nested_in_op:true expr))
        is_nested_in_op
  | PostfixInr (lv, _) ->
      sprintf "%s++" (pretty_print_expr ~is_nested_in_op:true lv)
  | PostfixDcr (lv, _) ->
      sprintf "%s--" (pretty_print_expr ~is_nested_in_op:true lv)
  | SPrefixInr (lv, _) ->
      sprintf "++%s" (pretty_print_expr ~is_nested_in_op:true lv)
  | SPrefixDcr (lv, _) ->
      sprintf "--%s" (pretty_print_expr ~is_nested_in_op:true lv)
  | SUpdate (dest, amount, op, _) ->
      wrap_if_nested
        (sprintf "%s %s= %s" (pretty_print_expr dest) (pretty_bin_op op)
           (pretty_print_expr amount))
        is_nested_in_op
  | SSubscript (arr, idx, _) ->
      sprintf "%s[%s]"
        (pretty_print_expr ~is_nested_in_op:true arr)
        (pretty_print_expr ~is_nested_in_op:false idx)

(* [pretty_print_stmt] converts a single statement into a pretty-printed string. *)
and pretty_print_stmt (stmt : stmt) (indent_level : int) : string =
  match stmt with
  | Declare (name, typ, expr, scope, _) ->
      let initialization =
        match expr with
        | None -> ""
        | Some expr ->
            sprintf " = %s" (pretty_print_expr ~is_nested_in_op:true expr)
      in
      sprintf "%s %s%s;\n%s" (pretty_print_type typ) name initialization
        (pretty_print_stmt_list scope indent_level)
  | ArrayDeclare (name, typ, size, init, scope, _) ->
      let pretty_size =
        match size with None -> "" | Some size -> pretty_print_expr size
      in
      let pretty_init =
        match init with
        | None -> ""
        | Some exprs ->
            sprintf " = { %s }"
              (List.map pretty_print_expr exprs |> String.concat ", ")
      in
      sprintf "%s %s[%s]%s;\n%s" (pretty_print_type typ) name pretty_size
        pretty_init
        (pretty_print_stmt_list scope indent_level)
  | If (cond, thn, _) ->
      sprintf "if (%s) %s" (pretty_print_expr cond)
        (pretty_print_block thn indent_level)
  | IfElse (cond, thn, els, _) ->
      sprintf "if (%s) %s else %s" (pretty_print_expr cond)
        (pretty_print_block thn indent_level)
        (pretty_print_block els indent_level)
  | While (cond, body, _) ->
      sprintf "while (%s) %s" (pretty_print_expr cond)
        (pretty_print_block body indent_level)
  | Loop (body, _) -> sprintf "loop %s" (pretty_print_block body indent_level)
  | Block (body, _) -> pretty_print_block body indent_level
  | Return (Some expr, _) -> sprintf "return %s;" (pretty_print_expr expr)
  | Return (None, _) -> "return;"
  | Exit (Some expr, _) -> sprintf "exit(%s);" (pretty_print_expr expr)
  | Exit (None, _) -> "exit();"
  | ExprStmt (expr, _) -> sprintf "%s;" (pretty_print_expr expr)
  | PrintDec (expr, _) -> sprintf "print(%s);" (pretty_print_expr expr)
  | PrintLcd (expr, _) -> sprintf "print_lcd(%s);" (pretty_print_expr expr)
  | Assert (expr, _) -> sprintf "assert(%s);" (pretty_print_expr expr)
  | NopStmt _ -> ""

(* [pretty_print_stmt_list] converts a statement list into a pretty-printed string. *)
and pretty_print_stmt_list (stmts : stmt list) (indent_level : int) : string =
  stmts
  |> List.map (fun stmt ->
         sprintf "%s%s" (indent indent_level)
           (pretty_print_stmt stmt indent_level))
  |> String.concat "\n"

(* [pretty_print_block] converts a block (statement list that is curly-brace
   delimited) into a pretty-printed string. *)
and pretty_print_block (block : stmt list) (indent_level : int) : string =
  match block with
  | [] -> "{}"
  | _ ->
      sprintf "{\n%s\n%s}"
        (pretty_print_stmt_list block (indent_level + 1))
        (indent indent_level)

(* [pretty_print_type] converts a type into a pretty-printed string. *)
and pretty_print_type (typ : ty) : string = string_of_ty typ

(* [pretty_print_func_defn] converts a function definition into a pretty-printed string. *)
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

(* [pretty_print_define] converts a #define directive into a pretty-printed string. *)
and pretty_print_define (define : pp_define) : string =
  sprintf "#define %s %s" define.var (pretty_print_expr define.expression)

(* [pretty_print] takes a program and produces a string that is
   the program's text formatted nicely. *)
let pretty_print (pgrm : prog) : string =
  let defines =
    List.map pretty_print_define pgrm.defines |> String.concat "\n"
  in
  [ defines ]
  @ List.map pretty_print_func_defn pgrm.funcs
  @ [ pretty_print_func_defn pgrm.main ]
  |> List.filter (fun s -> s <> "")
  |> String.concat "\n\n"
