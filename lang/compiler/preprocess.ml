open Ast

let expand_in_expr (define : pp_define) (exp : expr) : expr =
  failwith "not implemented"

let expand_in_stmt (define : pp_define) (stmt : stmt) : stmt =
  failwith "not implemented"

let expand_in_defn (define : pp_define) (defn : func_defn) : func_defn =
  failwith "not implemented"

let expand_in_defines (define : pp_define) (defines : pp_define list) :
    pp_define list =
  failwith "not implemented"

let expand_in_prog (define : pp_define) (pgrm : prog) : prog =
  failwith "not implemented"

(* [preprocess] consumes a program and produces a version of it 
  with all uses of preprocessor #define directives expanded into
  their corresponding expressions in the ast. *)
let preprocess (pgrm : prog) : prog = failwith "not implemented"
