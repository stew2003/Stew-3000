open Asm.Isa

let string_of_instrs (instrs : instr list) =
  (* convert instrs to strings *)
  instrs |> List.map string_of_instr
  (* concat them all into one big string with newline separators *)
  |> String.concat "\n"

(* just an example of constructing/printing some asm *)
let () =
  print_endline
    (string_of_instrs
       [
         Label "entry";
         Mvi (20, A);
         Mvi (3, B);
         Cmp (A, B);
         Jl "less";
         Mvi (0, C);
         Jmp "continue";
         Label "less";
         Mvi (1, C);
         Label "continue";
         Out C;
         Hlt;
       ])
