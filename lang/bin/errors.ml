open Core
open Util

(* [print_arbitrary_err] prints an arbitrary exception type 
    as a string error message to stderr *)
let print_arbitrary_err (error : exn) =
  Printf.eprintf "%s: %s\n" (Colors.error "Error") (Exn.to_string error)

(* [print_err] prints an error to stderr, given a type, message,
    and source location string *)
let print_err (err_type : string) (msg : string) (loc : string) =
  Printf.eprintf "%s: %s\n%s\n" err_type msg loc
