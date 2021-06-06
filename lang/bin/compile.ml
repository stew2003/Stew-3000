open Core
open Compiler
open Asm.Isa
open Asm.Assemble
open Util
open Err

(* command-line interface for compiler *)
let command =
  Command.basic ~summary:"Compiles the given source file."
    Command.Let_syntax.(
      let%map_open src_file = anon ("source_filename" %: Filename.arg_type)
      and target_file = anon ("target_filename" %: Filename.arg_type)
      and emit_binary =
        flag "-bin" (optional string)
          ~doc:"binary_file emit compiled binary to file"
      in
      fun () ->
        try
          (* read input file into string *)
          let source_text = In_channel.read_all src_file in
          try
            let pgrm = Parser.parse source_text in
            let instrs = Compile.compile pgrm in

            (* write generated asm to target file *)
            Out_channel.write_all target_file
              ~data:(string_of_instr_list instrs);

            (* write binary also if requested *)
            match emit_binary with
            | None -> ()
            | Some bin_file ->
                (* assemble and write binary *)
                let assembled = assemble instrs in
                let bin_out = Out_channel.create bin_file in
                Out_channel.output_bytes bin_out assembled;
                Out_channel.close bin_out
          with
          (* TODO: handle other errors that might occur during compilation *)
          | Parser.CompilerParseError (msg, loc) ->
              print_err
                (Colors.error "Parse Error")
                msg
                (Srcloc.string_of_maybe_loc loc source_text)
          | AssembleError (err, maybe_loc) ->
              print_err
                (Colors.error "Assembler Error")
                (string_of_asm_err err)
                (Srcloc.string_of_maybe_loc maybe_loc source_text)
          | err -> print_arbitrary_err err
        with err -> print_arbitrary_err err)

let () = Command.run ~version:"1.0" command
