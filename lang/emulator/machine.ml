open Printf

(*  stew_3000 models the programmer-visible state of the machine
    NOTE: dec_disp_history is a record of every byte that has 
    been sent to the decimal display so far *)
type stew_3000 = {
  mutable a : int;
  mutable b : int;
  mutable c : int;
  mutable sp : int;
  mutable zflag : bool;
  mutable sflag : bool;
  mutable oflag : bool;
  mutable stack : int array;
  mutable dec_disp_history : int list;
  mutable pc : int;
  mutable halted : bool;
}

let string_of_stew_3000 (machine : stew_3000) : string =
  let string_of_dec_display (history : int list) =
    match history with
    | [] -> "(no output)"
    | _ -> List.map string_of_int history |> String.concat ", "
  in
  let string_of_stack_value (value : int) =
    if value = 0 then "__" else sprintf "%02x" (Numbers.as_8bit_unsigned value)
  in
  let string_of_stack (stack : int list) : string =
    List.mapi
      (fun i value ->
        if i mod 8 = 0 then
          sprintf "\n0x%02x: %s" i (string_of_stack_value value)
        else string_of_stack_value value)
      stack
    |> String.concat " "
  in
  (* [string_of_reg] formats a register's contents in a string,
     showing unsigned hex, and unsigned/signed decimal *)
  let string_of_reg (name : string) (contents : int) =
    let unsigned = Numbers.as_8bit_unsigned contents in
    let signed = Numbers.as_8bit_signed contents in
    sprintf "%2s: 0x%02x %4d %4d" name unsigned unsigned signed
  in
  let bool_to_int (b : bool) = if b then 1 else 0 in
  sprintf
    "== Registers ==\n\
     %s\n\
     %s\n\
     %s\n\
     %s\n\
     %s\n\
     %s\n\n\
     == Flags ==\n\
     zf: %d\n\
     sf: %d\n\
     of: %d\n\n\
     halted? %s\n\n\
     == Decimal Display History == (most recent last)\n\
     %s\n\n\
     == Stack ==%s\n"
    "     hex  uns  sig"
    (string_of_reg "a" machine.a)
    (string_of_reg "b" machine.b)
    (string_of_reg "c" machine.c)
    (string_of_reg "sp" machine.sp)
    (string_of_reg "pc" machine.pc)
    (bool_to_int machine.zflag)
    (bool_to_int machine.sflag)
    (bool_to_int machine.oflag)
    (if machine.halted then "yes" else "no")
    (string_of_dec_display machine.dec_disp_history)
    (string_of_stack (Array.to_list machine.stack))

let stack_size = 256

(* [new_stew_3000] constructs a new machine state in initial state *)
let new_stew_3000 _ : stew_3000 =
  {
    a = 0;
    b = 0;
    c = 0;
    sp = 0;
    zflag = false;
    sflag = false;
    oflag = false;
    stack = Array.make stack_size 0;
    dec_disp_history = [];
    pc = 0;
    halted = false;
  }
