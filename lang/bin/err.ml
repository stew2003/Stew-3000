open Util

(* [print_err] prints an error to stderr, given a type, message,
    and source location string *)
let print_err (err_type : string) (msg : string) (loc : string option) =
  Printf.eprintf "%s: %s\n%s" (Colors.error err_type) msg
    (match loc with None -> "" | Some loc -> Printf.sprintf "%s\n" loc)

(* [print_arbitrary_err] prints an arbitrary exception type 
    as a string error message to stderr *)
let print_arbitrary_err (error : exn) =
  print_err "Error" (Core.Exn.to_string error) None

(* [try_read_source] attempst to read an entire file and return its
  contents, but handles any errors that occur in doing so and exits. *)
let try_read_source (filename : string) : string =
  try Core.In_channel.read_all filename
  with err ->
    print_arbitrary_err err;
    exit 1

(* [handle_err] handles an arbitrary error (via printing) that might have 
  originated in parsing, assembling, emulating, compiling, etc. It could also 
  be a system error or otherwise that was not thrown by our code. *)
let handle_err (error : exn) (source_text : string) =
  match error with
  | Asm.Parser.AsmParseError (msg, loc) ->
      print_err "Asm Parse Error" msg
        (Some (Srcloc.string_of_src_loc loc source_text))
  | Asm.Assemble.AssembleError (err, maybe_loc) ->
      print_err "Assembler Error"
        (Asm.Assemble.string_of_asm_err err)
        (Some (Srcloc.string_of_maybe_loc maybe_loc source_text))
  | Compiler.Parser.CompilerParseError (msg, loc) ->
      print_err "Parse Error" msg
        (Some (Srcloc.string_of_maybe_loc loc source_text))
  | Compiler.Check.CheckError (err, maybe_loc) ->
      print_err "Check Error"
        (Compiler.Check.string_of_check_err err)
        (Some (Srcloc.string_of_maybe_loc maybe_loc source_text))
  | Emulator.EmulatorError (err, maybe_loc) ->
      print_err "Emulator Error"
        (Emulator.string_of_emu_err err)
        (Some (Srcloc.string_of_maybe_loc maybe_loc source_text))
  | Err.InternalError msg -> print_err "Internal Error" msg None
  | err -> print_arbitrary_err err
