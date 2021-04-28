open Asm.Isa

(* just an example of constructing/printing some asm *)
let () =
  print_endline
    (string_of_instr_list
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
