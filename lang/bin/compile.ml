open Core
open Compiler
open Asm.Isa
open Asm.Assemble
open Asm.Warnings
open Compiler.Optimizations
open Compiler.Warnings
open Compiler.Prettyprint
open Err
open Util

(* command-line interface for compiler *)
let command =
  Command.basic ~summary:"Compiles the given source file."
    Command.Let_syntax.(
      let%map_open src_file = anon ("source_filename" %: Filename.arg_type)
      and target_file = anon ("target_filename" %: Filename.arg_type)
      and emit_binary =
        flag "-bin" (optional string)
          ~doc:"binary_file emit compiled binary to file"
      and ignore_asserts =
        flag "-ignore-asserts" no_arg ~doc:"do not generate code for asserts"
      and disable_opt =
        flag "-disable-opt" no_arg ~doc:"do not apply extra optimizations"
      and show_opt_ast =
        flag "-show-ast" (optional string)
          ~doc:
            "stage print the optimized AST after a given stage in the pipeline \
             (preprocess, desugar, check, opt)"
      and no_code_gen =
        flag "-no-code-gen" no_arg ~doc:"aborts before code generation"
      and no_warnings =
        flag "-no-warn" no_arg ~doc:"do not print compiler warnings"
      in

      fun () ->
        (* [print_ast_if_stage_matches] prints the given program AST if the
           stage indicated by argument show_opt_ast matches the given stage. *)
        let print_ast_if_stage_matches (stage : string) (pgrm : Ast.prog) =
          match show_opt_ast with
          | None -> ()
          | Some chosen_stage ->
              if String.equal stage chosen_stage then (
                Printf.printf "%s"
                  (Colors.bold
                     (Printf.sprintf "==== AST at stage: %s ====\n" stage));
                Printf.printf "%s\n" (pretty_print pgrm))
        in

        (* read input file into string *)
        let source_text = try_read_source src_file in
        try
          let pgrm = Parser.parse source_text in
          let pgrm = Preprocess.preprocess pgrm in
          print_ast_if_stage_matches "preprocess" pgrm;

          let pgrm = Desugar.desugar pgrm in
          print_ast_if_stage_matches "desugar" pgrm;

          (* On warnings, print them *)
          let warning_handler (w : compiler_warn) =
            if not no_warnings then
              print_warning (message_of_compiler_warn w source_text src_file)
          in

          let pgrm = Check.check ~emit_warning:warning_handler pgrm in
          print_ast_if_stage_matches "check" pgrm;

          let pgrm =
            if disable_opt then pgrm
            else Constant_fold.constant_fold ~emit_warning:warning_handler pgrm
          in
          let pgrm =
            if disable_opt then pgrm
            else
              Dead_code_elimination.eliminate_dead_code
                ~emit_warning:warning_handler pgrm
          in

          print_ast_if_stage_matches "opt" pgrm;

          (* not doing code generation, stop here *)
          if no_code_gen then exit 0;

          let instrs = Compile.compile pgrm ~ignore_asserts in

          (* optimize the generated instructions *)
          let instrs =
            if disable_opt then instrs else Peephole.peephole_optimize instrs
          in

          (* write generated asm to target file *)
          Out_channel.write_all target_file ~data:(string_of_instr_list instrs);

          (* print message about generated code *)
          Printf.printf "%s `%s` ==> `%s` (%d instructions)\n"
            (Colors.success "Success!")
            src_file target_file (List.length instrs);

          (* write binary also if requested *)
          match emit_binary with
          | None -> ()
          | Some bin_file ->
              (* assemble and write binary *)
              let assembled =
                assemble instrs ~emit_warning:(fun w ->
                    print_warning (message_of_asm_warn w "" ""))
              in
              let bin_out = Out_channel.create bin_file in
              Out_channel.output_bytes bin_out assembled;
              Out_channel.close bin_out;

              (* print message about assembled binary *)
              Printf.printf "%s `%s` (%d instructions) ==> `%s` (%d bytes)\n"
                (Colors.success "Success!")
                target_file (List.length instrs) bin_file
                (Bytes.length assembled)
        with err -> handle_err err source_text src_file)

let () = Command.run ~version:"1.1.0" command
