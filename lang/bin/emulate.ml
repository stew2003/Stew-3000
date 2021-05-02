open Core
open Asm
open Emulator
open Util

(* command-line interface for emulator *)
let command =
  Command.basic ~summary:"Runs the given asm file in the Stew 3000 emulator"
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: Filename.arg_type) in
      fun () ->
        try
          (* read input file into string *)
          let text = In_channel.read_all filename in
          (* parse input program *)
          let instrs = Parser.parse text in
          (* emulate and print final state *)
          let final_state = emulate_program instrs in
          Printf.printf "%s\n" (string_of_stew_3000 final_state)
        with
        | Parser.AsmParseError msg ->
            Printf.eprintf "%s: %s\n" (Colors.error "Error Parsing Asm") msg
        | EmulatorError err ->
            Printf.eprintf "%s: %s\n"
              (Colors.error "Emulator Error")
              (string_of_emu_err err)
        | err ->
            Printf.eprintf "%s: %s\n" (Colors.error "Error") (Exn.to_string err))

let () = Command.run ~version:"1.0" command
