open Core
open Asm
open Emulator
open Util
open Printf

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
          let text = In_channel.read_all filename in
          (* parse input program *)
          let instrs = Parser.parse text in
          (* emulate and print final state *)
          let final_state = emulate_program instrs verbosity in
          printf "%s\n" (Colors.bold "Halted via hlt!");
          printf "%s\n" (string_of_stew_3000 final_state)
        with
        | Parser.AsmParseError msg ->
            eprintf "%s: %s\n" (Colors.error "Error Parsing Asm") msg
        | EmulatorError err ->
            eprintf "%s: %s\n"
              (Colors.error "Emulator Error")
              (string_of_emu_err err)
        | err -> eprintf "%s: %s\n" (Colors.error "Error") (Exn.to_string err))

let () = Command.run ~version:"1.0" command
