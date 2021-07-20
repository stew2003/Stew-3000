open Core
open Asm
open Emulator
open Err
open Util
open Printf

(* command-line interface for emulator *)
let command =
  Command.basic ~summary:"Runs the given asm file in the Stew 3000 emulator"
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: Filename.arg_type)
      and verbosity =
        flag "-v" (optional int) ~doc:"verbosity level of output logging"
      and db_mode = flag "-3db" no_arg ~doc:"emulate in debug mode"
      and warn = flag "-warn" no_arg ~doc:"emit warnings" in
      fun () ->
        (* debug mode puts verbosity at max *)
        let verbosity =
          if db_mode then 2 else match verbosity with None -> 0 | Some v -> v
        in
        (* read input file into string *)
        let source_text = try_read_source filename in
        try
          (* parse input program *)
          let instrs = Parser.parse source_text in
          (* emulate and print final state *)
          let final_state = emulate instrs ~verbosity ~db_mode ~warn in
          printf "%s\n" (Colors.bold "Halted via hlt!");
          printf "%s\n" (Emulator__Machine.string_of_stew_3000 final_state)
        with err -> handle_err err source_text filename)

let () = Command.run ~version:"1.0" command
