open Core
open Asm
open Util

(* command-line interface for assembler *)
let command =
  (* [print_bytes] writes a byte array to stdout in hexadecimal form,
     with 8 bytes per line *)
  let printf_bytes (b : bytes) =
    Bytes.mapi b ~f:(fun i byte ->
        Printf.printf "%02x " (int_of_char byte);
        if (i + 1) mod 8 = 0 then Printf.printf "\n" else ();
        byte)
    |> ignore
  in
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
          Out_channel.close out;

          (* print message and display assembled bytes *)
          Printf.printf "%s `%s` (%d instructions) ==> `%s` (%d bytes)\n"
            (Colors.success "Success!")
            asm_filename (List.length instrs) binary_filename
            (Bytes.length assembled);
          printf_bytes assembled
        with
        | Assemble.AssembleError (err, maybe_loc) ->
            Printf.eprintf "%s: %s\n%s\n"
              (Colors.error "Assembler Error")
              (Assemble.string_of_asm_err err)
              (Srcloc.string_of_maybe_loc maybe_loc
                 (In_channel.read_all asm_filename))
        | Parser.AsmParseError msg ->
            Printf.eprintf "%s: %s\n" (Colors.error "Error Parsing Asm") msg
        | err ->
            Printf.eprintf "%s: %s\n" (Colors.error "Error") (Exn.to_string err))

let () = Command.run ~version:"1.0" command
