open Core
open Asm
open Emulator
open Util
open Printf
open Err

(* command-line interface for emulator *)
let command =
  Command.basic ~summary:"Runs the given asm file in the Stew 3000 emulator"
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: Filename.arg_type)
      and verbosity =
        flag "-v" (optional int) ~doc:"verbosity level of output logging"
      in
      fun () ->
        let verbosity = match verbosity with None -> 0 | Some v -> v in
        try
          (* read input file into string *)
          let source_text = In_channel.read_all filename in
          try
            (* parse input program *)
            let instrs = Parser.parse source_text in
            (* emulate and print final state *)
            let final_state = emulate instrs verbosity in
            printf "%s\n" (Colors.bold "Halted via hlt!");
            printf "%s\n" (string_of_stew_3000 final_state)
          with
          | Parser.AsmParseError (msg, loc) ->
              print_err
                (Colors.error "Error Parsing Asm")
                msg
                (Srcloc.string_of_src_loc loc source_text)
          | EmulatorError (err, maybe_loc) ->
              print_err
                (Colors.error "Emulator Error")
                (string_of_emu_err err)
                (Srcloc.string_of_maybe_loc maybe_loc source_text)
          | err -> print_arbitrary_err err
        with err -> print_arbitrary_err err)

let () = Command.run ~version:"1.0" command
