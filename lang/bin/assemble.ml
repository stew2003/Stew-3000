open Core
open Asm
open Util
open Err
open Asm.Isa

(* command-line interface for assembler *)
let command =
  (* [print_bytes] writes a byte array to stdout in hexadecimal form,
     with 8 bytes per line *)
  let display_bytes (b : bytes) =
    Bytes.mapi b ~f:(fun i byte ->
        Printf.printf "%02x " (int_of_char byte);
        if (i + 1) mod 8 = 0 then Printf.printf "\n" else ();
        byte)
    |> ignore;
    (* if didn't reach end of line, no final newline was
       printed, so print it here *)
    if Bytes.length b mod 8 <> 0 then Printf.printf "\n" else ()
  in
  (* [display_side_by_side] prints the given instructions and binary
     from assembling side by side *)
  let display_side_by_side (instrs : instr list) (binary : int list list) =
    let string_of_byte_line (line : int list) =
      List.map line ~f:(fun byte -> Printf.sprintf "%02x" (byte land 0xff))
      |> String.concat ~sep:" "
    in
    List.map2 instrs binary ~f:(fun ins byte_line ->
        Printf.printf "%6s | %s\n"
          (string_of_byte_line byte_line)
          (string_of_instr ins))
    |> ignore
  in
  Command.basic ~summary:"Assembles the given file into a binary"
    Command.Let_syntax.(
      let%map_open asm_filename = anon ("asm_filename" %: Filename.arg_type)
      and binary_filename = anon ("binary_filename" %: Filename.arg_type)
      and side_by_side =
        flag "-side-by-side" no_arg ~doc:"print asm and binary side by side"
      in
      fun () ->
        try
          (* read input file into string *)
          let source_text = In_channel.read_all asm_filename in
          try
            (* parse and assemble input program *)
            let instrs = Parser.parse source_text in
            let _, unflattened, assembled =
              Assemble.assemble_with_rich_info instrs
            in
            (* write assembled binary to out file *)
            let out = Out_channel.create binary_filename in
            Out_channel.output_bytes out assembled;
            Out_channel.close out;

            (* print message and display assembled bytes *)
            Printf.printf "%s `%s` (%d instructions) ==> `%s` (%d bytes)\n"
              (Colors.success "Success!")
              asm_filename (List.length instrs) binary_filename
              (Bytes.length assembled);
            display_bytes assembled;
            (* print side-by-side view if indicated *)
            if side_by_side then (
              Printf.printf "%s\n" (Colors.br_cyan "Side by side:");
              display_side_by_side instrs unflattened)
            else ()
          with
          | Assemble.AssembleError (err, maybe_loc) ->
              print_err
                (Colors.error "Assembler Error")
                (Assemble.string_of_asm_err err)
                (Srcloc.string_of_maybe_loc maybe_loc source_text)
          | Parser.AsmParseError (msg, loc) ->
              print_err
                (Colors.error "Error Parsing Asm")
                msg
                (Srcloc.string_of_src_loc loc source_text)
          | err -> print_arbitrary_err err
        with err -> print_arbitrary_err err)

let () = Command.run ~version:"1.0" command
