open Core
open Asm
open Emulator

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
            Printf.eprintf "error parsing asm: %s\n" msg
        | EmulatorError err ->
            Printf.eprintf "emulator error: %s\n" (string_of_emu_err err)
        | err -> Printf.eprintf "error: %s\n" (Exn.to_string err))

let () = Command.run ~version:"1.0" command
