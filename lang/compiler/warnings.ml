open Util.Srcloc
open Ast
open Printf

(* Warnings that may be emitted by the compiler (generally speaking). *)
type compiler_warn =
  | DivisionByZero of expr
  | ZeroSizeArray of string * stmt
  | EmptyInitializer of string * stmt
  | ConstantCondition of int * stmt
  | DeadCode of stmt list
  | UnusedFunction of func_defn
  | UnusedVar of string * maybe_loc

(* Type for a function that is passed into the compiler to handle warnings. *)
type compiler_warn_handler = compiler_warn -> unit

(* [message_of_compiler_warn] converts a compiler warning into a tuple of
    strings (message, extra, help). *)
let message_of_compiler_warn (warning : compiler_warn) (source_text : string)
    (source_filename : string) : string * string option * string option =
  match warning with
  | DivisionByZero div_expr ->
      ( "division by zero (undefined behavior)",
        Option.map
          (fun loc -> string_of_src_loc loc source_text source_filename)
          (loc_from_expr div_expr),
        None )
  | ZeroSizeArray (arr, decl) ->
      ( sprintf "array `%s` has zero size" arr,
        Option.map
          (fun loc -> string_of_src_loc loc source_text source_filename)
          (loc_from_stmt decl),
        None )
  | EmptyInitializer (arr, decl) ->
      ( sprintf "array `%s` has an empty initializer" arr,
        Option.map
          (fun loc -> string_of_src_loc loc source_text source_filename)
          (loc_from_stmt decl),
        Some "consider omitting the initializer entirely" )
  | ConstantCondition (cond_value, stmt) ->
      ( sprintf "condition is a constant value (always %d)" cond_value,
        Option.map
          (fun loc -> string_of_src_loc loc source_text source_filename)
          (loc_from_stmt stmt),
        None )
  | DeadCode stmts ->
      let span_loc =
        List.fold_left
          (fun loc stmt ->
            match (loc, loc_from_stmt stmt) with
            | None, Some loc | Some loc, None -> Some loc
            | Some so_far, Some loc -> Some (span so_far loc)
            | None, None -> None)
          None stmts
      in
      ( "dead code",
        Option.map
          (fun loc -> string_of_src_loc loc source_text source_filename)
          span_loc,
        Some "this code is unreachable, consider removing it" )
  | UnusedFunction defn ->
      ( sprintf "unused function `%s`" defn.name,
        Option.map
          (fun loc -> string_of_src_loc loc source_text source_filename)
          defn.loc,
        None )
  | UnusedVar (var, loc) ->
      ( sprintf "unused variable `%s`" var,
        Option.map
          (fun loc -> string_of_src_loc loc source_text source_filename)
          loc,
        None )
