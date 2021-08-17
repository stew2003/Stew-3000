open Printf

(* stew_3000 models the programmer-visible state of the machine
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
  mutable cflag : bool;
  mutable stack : int array;
  mutable dec_disp_history : int list;
  mutable pc : int;
  mutable halted : bool;
}

(* [num_as_hex_uns_sig] produces a string containing the given
   number printed as unsigned hex, unsigned decimal, and signed decimal *)
let num_as_hex_uns_sig (num : int) =
  let unsigned = Numbers.as_8bit_unsigned num in
  let signed = Numbers.as_8bit_signed num in
  sprintf "0x%02x %4d %4d" unsigned unsigned signed

let reg_header = "     hex  uns  sig"

(* [string_of_reg] formats a register's contents in a string,
   showing unsigned hex, and unsigned/signed decimal *)
let string_of_reg (name : string) (contents : int) =
  sprintf "%2s: %s" name (num_as_hex_uns_sig contents)

(* [string_of_all_regs] formats all registers for printing *)
let string_of_all_regs (machine : stew_3000) =
  [
    reg_header;
    string_of_reg "a" machine.a;
    string_of_reg "b" machine.b;
    string_of_reg "c" machine.c;
    string_of_reg "sp" machine.sp;
    string_of_reg "pc" machine.pc;
  ]
  |> String.concat "\n"

(* [string_of_flag] formats a flag's value in a string *)
let string_of_flag (name : string) (contents : bool) =
  let bool_to_int (b : bool) = if b then 1 else 0 in
  sprintf "%s: %d" name (bool_to_int contents)

(* [string_of_all_flags] formats all a machine's flags for printing *)
let string_of_all_flags (machine : stew_3000) =
  [
    string_of_flag "zf" machine.zflag;
    string_of_flag "sf" machine.sflag;
    string_of_flag "of" machine.oflag;
    string_of_flag "cf" machine.cflag;
  ]
  |> String.concat "\n"

(* [string_of_halted] produces "yes" if halted is true and "no" otherwise *)
let string_of_halted (halted : bool) : string = if halted then "yes" else "no"

(* [string_of_dec_display] produces a string of the decimal display history *)
let string_of_dec_display (history : int list) =
  match history with
  | [] -> "(no output)"
  | _ ->
      let last = List.length history - 1 in
      List.mapi
        (fun i value ->
          let value = Numbers.as_8bit_signed value in
          if (i + 1) mod 8 = 0 then sprintf "%d\n" value
          else if i = last then sprintf "%d" value
          else sprintf "%d, " value)
        history
      |> String.concat ""

(* [string_of_stack_at_addr] gets a string of the stack contents
   at a given address. The address is always interpreted as unsigned 8-bit *)
let string_of_stack_at_addr (stack : int array) (addr : int) =
  let stack = Array.to_list stack in
  let addr_as_unsigned = Numbers.as_8bit_unsigned addr in
  let contents = List.nth stack addr_as_unsigned in
  sprintf "stack[%d] = %s" addr_as_unsigned (num_as_hex_uns_sig contents)

(* [string_of_stack] constructs a string containing the entire contents of
   the stack *)
let string_of_stack (stack : int array) : string =
  let stack = Array.to_list stack in
  let string_of_stack_value (value : int) =
    if value = 0 then "__" else sprintf "%02x" (Numbers.as_8bit_unsigned value)
  in
  List.mapi
    (fun i value ->
      if i mod 8 = 0 then sprintf "\n0x%02x: %s" i (string_of_stack_value value)
      else string_of_stack_value value)
    stack
  |> String.concat " "

let string_of_stew_3000 (machine : stew_3000) : string =
  sprintf
    "== Registers ==\n\
     %s\n\n\
     == Flags ==\n\
     %s\n\n\
     halted? %s\n\n\
     == Decimal Display History == (most recent last)\n\
     %s\n\n\
     == Stack ==%s\n"
    (string_of_all_regs machine)
    (string_of_all_flags machine)
    (string_of_halted machine.halted)
    (string_of_dec_display machine.dec_disp_history)
    (string_of_stack machine.stack)

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
    cflag = false;
    stack = Array.make stack_size 0;
    dec_disp_history = [];
    pc = 0;
    halted = false;
  }
