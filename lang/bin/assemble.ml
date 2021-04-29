open Core
open Asm

(* command-line interface for assembler *)
let command =
  Command.basic ~summary:"Assembles the given file into a binary"
    Command.Let_syntax.(
      let%map_open asm_filename = anon ("asm_filename" %: Filename.arg_type)
      and binary_filename = anon ("binary_filename" %: Filename.arg_type) in
      fun () ->
        try
          (* read input file into string *)
          let text = In_channel.read_all asm_filename in
          (* parse and assemble input program *)
          let instrs = Parser.parse text in
          let assembled = Assemble.assemble instrs in
          (* write assembled binary to out file *)
          let out = Out_channel.create binary_filename in
          Out_channel.output_bytes out assembled;
          Out_channel.close out
        with
        | Assemble.AssembleError err ->
            Printf.eprintf "assembler error: %s\n"
              (Assemble.string_of_asm_err err)
        | Parser.AsmParseError msg ->
            Printf.eprintf "error parsing asm: %s\n" msg
        | err -> Printf.eprintf "error: %s\n" (Exn.to_string err))

let () = Command.run ~version:"1.0" command
