open Ast
open Asm.Isa
open Util.Names

(* Implementation of multiplication *)
let runtime_multiply =
  let multiply = "runtime_multiply" in
  let loop = gensym "loop" in
  let done_loop = gensym "done" in
  (* ; runtime_multiply computes the product of a and b, leaving the result in c
     ;
     ; :: Implementation ::
     ; running_sum = 0
     ; counter = b
     ; while counter > 0
     ;   counter -= 1
     ;   running_sum += a
     ; product is running_sum
     ;
     ; Note on register allocation:
     ; a in a
     ; counter in b
     ; running_sum in c *)
  [ Label (multiply, None) ]
  @ [
      (* make both a and b positive *)
      Call ("runtime_normalize_signs", None);
      Sts (C, 1, None);
      Mvi (0, C, None);
      Label (loop, None);
      Cmpi (Reg B, Imm 0, None);
      Je (done_loop, None);
      Dcr (B, None);
      Add (A, C, None);
      Jmp (loop, None);
      Label (done_loop, None);
    ]
  (* ; set sign of product according to signs of a & b originally
     ; NOTE: set_result_sign will ret out of the call to mult *)
  @ [ Jmp ("runtime_set_result_sign", None) ]

(* Implementation of division *)
and runtime_divide =
  let divide = "runtime_divide" in
  let loop = gensym "loop" in
  let done_loop = gensym "done" in
  (* ; runtime_divide performs integer division of a / b, leaving the
     ; quotient in c, and remainder in a
     ;
     ; Note: quotient will be signed properly. remainder is always positive
     ; in this implementation
     ;
     ; :: Implementation ::
     ; left_over = num
     ; num_subtracts = 0
     ; while left_over >= denom:
     ;   left_over -= denom
     ;   num_subtracts += 1
     ;
     ; quotient is num_subtracts
     ; remainder is left_over *)
  [ Label (divide, None); Call ("runtime_normalize_signs", None) ]
  @ [
      (* store sign of quotient on stack, use c as counter *)
      Sts (C, 1, None);
      Mvi (0, C, None);
    ]
  @ [
      (* repeatedly subtract b from a *)
      Label (loop, None);
      Cmp (A, B, None);
      Jl (done_loop, None);
      Sub (B, A, None);
      Inr (C, None);
      Jmp (loop, None);
    ]
  @ [
      (* at this point, quotient is in c, remainder in a *)
      Label (done_loop, None);
      Jmp ("runtime_set_result_sign", None);
    ]

(* Helpful functionality for dealing with signed operands to mult/div *)
let runtime_normalize_signs =
  let normalize_signs = "runtime_normalize_signs" in
  let norm_a = gensym "norm_a" in
  let norm_b = gensym "norm_b" in
  let done_norm = gensym "done" in

  (* ; Makes both a and b positive, and sets c to non-zero if
     ; only *one* of a or b was negative. *)
  [
    Label (normalize_signs, None);
    Mvi (0, C, None);
    Label (norm_a, None);
    Cmpi (Reg A, Imm 0, None);
    Jge (norm_b, None);
    Not (A, None);
    Inr (A, None);
    Not (C, None);
    Label (norm_b, None);
    Cmpi (Reg B, Imm 0, None);
    Jge (done_norm, None);
    Not (B, None);
    Inr (B, None);
    Not (C, None);
    Label (done_norm, None);
    Ret None;
  ]

(* Helper for setting result sign of mult/div *)
let runtime_set_result_sign =
  let set_result_sign = "runtime_set_result_sign" in
  let no_change = gensym "no_change" in
  (* ; Sets the sign of c based on a value on the stack at index 1.
     ; If value is 0, leaves c alone, otherwise negates c
     ; NOTE: this expects to be jumped into, not called
     ; Assumes b can be clobbered. *)
  [
    Label (set_result_sign, None);
    Lds (1, B, None);
    Cmpi (Reg B, Imm 0, None);
    Je (no_change, None);
    Not (C, None);
    Inr (C, None);
    Label (no_change, None);
    Ret None;
  ]

(* Asserts that a condition is true and halts indicating error otherwise *)
let runtime_assert =
  let rt_assert = "runtime_assert" in
  let success = gensym "success" in
  (* ; Asserts that the value passed in the a register
     ; is not 0. If the value is found to be 0, we halt with
     ; -1 on the decimal display. *)
  [
    Label (rt_assert, None);
    Cmpi (Reg A, Imm 0, None);
    Jne (success, None);
    Mvi (-1, C, None);
    Out (C, None);
    Hlt None;
    Label (success, None);
    Ret None;
  ]

(* [check_for_expr] determines if the program contains an 
  expression that satisfies the given predicate *)
let check_for_expr (pgrm : prog) (pred : expr -> bool) : bool =
  (* [check_expr] determines if the given expression contains a
     sub-expression that satisfies the given predicate. *)
  let rec check_expr (exp : expr) (pred : expr -> bool) : bool =
    (* true if either the expression or its subexpressions yield true *)
    pred exp
    ||
    match exp with
    | UnOp (_, operand, _) | LogOp (LNot operand, _) -> check_expr operand pred
    | BinOp (_, left, right, _)
    | LogOp (LAnd (left, right), _)
    | LogOp (LOr (left, right), _) ->
        check_expr left pred || check_expr right pred
    | Call (_, args, _) ->
        List.map (fun arg -> check_expr arg pred) args
        |> List.fold_left ( || ) false
    | _ -> false
  (* [check_stmt] determines if the given statement contains an
     expression that satisfies the given predicate *)
  and check_stmt (stmt : stmt) (pred : expr -> bool) : bool =
    (* NOTE: We are checking for sub-expressions here so we
       only examine statements that contain sub-expressions. *)
    match stmt with
    | Let (_, _, value, body, _) ->
        check_expr value pred || check_stmt_list body pred
    | Assign (_, value, _) -> check_expr value pred
    | If (cond, thn, _) -> check_expr cond pred || check_stmt_list thn pred
    | IfElse (cond, thn, els, _) ->
        check_expr cond pred || check_stmt_list thn pred
        || check_stmt_list els pred
    | Block (stmts, _) -> check_stmt_list stmts pred
    | Return (Some value, _) -> check_expr value pred
    | ExprStmt (value, _) -> check_expr value pred
    | While (cond, body, _) -> check_expr cond pred || check_stmt_list body pred
    | PrintDec (value, _) -> check_expr value pred
    | Exit (Some e, _) -> check_expr e pred
    | Assert (e, _) -> check_expr e pred
    | _ -> false
  (* [check_stmt_list] determines if the given statement list
     contains an expression that satisfies the predicate *)
  and check_stmt_list (stmts : stmt list) (pred : expr -> bool) : bool =
    stmts
    |> List.map (fun stmt -> check_stmt stmt pred)
    |> List.fold_left ( || ) false
  in
  List.map
    (fun defn -> check_stmt_list defn.body pred)
    (pgrm.funcs @ [ pgrm.main ])
  |> List.fold_left ( || ) false

(* [check_for_stmt] checks a given program for a statement 
  that satisfies the given predicate *)
let check_for_stmt (pgrm : prog) (pred : stmt -> bool) : bool =
  (* [check_stmt] checks a statement for the presence of a statement
     that satisfies the given predicate *)
  let rec check_stmt (stmt : stmt) (pred : stmt -> bool) : bool =
    pred stmt
    ||
    (* NOTE: We are checking for sub-statements here so only
       check the statements that have them. *)
    match stmt with
    | Let (_, _, _, body, _) -> check_stmt_list body pred
    | If (_, thn, _) -> check_stmt_list thn pred
    | IfElse (_, thn, els, _) ->
        check_stmt_list thn pred || check_stmt_list els pred
    | Block (stmts, _) -> check_stmt_list stmts pred
    | While (_, body, _) -> check_stmt_list body pred
    | _ -> false
  (* [check_stmt_list] checks a list of statements for one that satisfies
     the given predicate *)
  and check_stmt_list (stmts : stmt list) (pred : stmt -> bool) : bool =
    stmts
    |> List.map (fun stmt -> check_stmt stmt pred)
    |> List.fold_left ( || ) false
  in
  List.map
    (fun defn -> check_stmt_list defn.body pred)
    (pgrm.funcs @ [ pgrm.main ])
  |> List.fold_left ( || ) false

let uses_mult (pgrm : prog) =
  check_for_expr pgrm (function BinOp (Mult, _, _, _) -> true | _ -> false)

let uses_div (pgrm : prog) =
  check_for_expr pgrm (function BinOp (Div, _, _, _) -> true | _ -> false)

let uses_mod (pgrm : prog) =
  check_for_expr pgrm (function BinOp (Mod, _, _, _) -> true | _ -> false)

let uses_assert (pgrm : prog) =
  check_for_stmt pgrm (function Assert _ -> true | _ -> false)

(* [conditionally_include] returns either the given code or an empty program,
  depending on whether the given condition is true or not. *)
let conditionally_include (code : instr list) (condition : bool) : instr list =
  if condition then code else []

(* [runtime] constructs the runtime code necessary for a given 
  program. Usually, this will be empty, but if the program requires
  special runtime functionality (multiplication, division, ...) this
  will contribute those subroutines *)
let runtime (program : prog) : instr list =
  let sign_utils = runtime_normalize_signs @ runtime_set_result_sign in
  let needs_mult_code = uses_mult program in

  (* modulus and division use the same division algorithm *)
  let needs_div_code = uses_div program || uses_mod program in

  conditionally_include sign_utils (needs_mult_code || needs_div_code)
  @ conditionally_include runtime_multiply needs_mult_code
  @ conditionally_include runtime_divide needs_div_code
  @ conditionally_include runtime_assert (uses_assert program)
