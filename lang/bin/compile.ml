open Core
open Compiler
open Asm.Isa
open Asm.Assemble
open Compiler.Optimizations
open Compiler.Warnings
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
        (* read input file into string *)
        let source_text = try_read_source src_file in
        try
          let pgrm = Parser.parse source_text in
          let pgrm = Preprocess.preprocess pgrm in

          (* On warnings, print them *)
          let warning_handler (w : compiler_warn) =
            print_warning (message_of_compiler_warn w source_text src_file)
          in

          Check.check pgrm;
          let pgrm =
            Constant_fold.constant_fold ~emit_warning:warning_handler pgrm
          in

          (* TEMP: pretty print the ast after preprocessing as sanity check *)
          (* Printf.printf "%s\n" (Prettyprint.pretty_print pgrm); *)
          let instrs = Compile.compile pgrm in

          (* write generated asm to target file *)
          Out_channel.write_all target_file ~data:(string_of_instr_list instrs);

          (* write binary also if requested *)
          match emit_binary with
          | None -> ()
          | Some bin_file ->
              (* assemble and write binary *)
              let assembled = assemble instrs in
              let bin_out = Out_channel.create bin_file in
              Out_channel.output_bytes bin_out assembled;
              Out_channel.close bin_out
        with err -> handle_err err source_text src_file)

let () = Command.run ~version:"1.0" command
