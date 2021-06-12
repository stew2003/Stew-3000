entry:
  mvi -4, a
  mvi 16, b
  call mult
  out c
  hlt

; Makes both a and b positive, and sets c to non-zero if 
; only *one* of a or b was negative. 
normalize_signs:
  mvi 0, c
norm_a:
  ; if a is negative, make it positive
  cmpi a, 0
  jge norm_b
  not a
  inr a
  not c
norm_b:
  ; if b is negative, make it positive
  cmpi b, 0
  jge done_norm
  not b
  inr b
  not c
done_norm:
  ret

; Sets the sign of c based on a value on the stack at index 1.
; If value is 0, leaves c alone, otherwise negates c
; NOTE: this expects to be jumped into, not called
; Assumes b can be clobbered.
set_result_sign:
  lds 1, b
  cmpi b, 0
  je no_sign_change
  not c
  inr c
no_sign_change:
  ret

; mult computes the product of a and b, leaving the result in c
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
; running_sum in c
mult:
  ; make a and b positive, keep sign info on stack
  call normalize_signs
  sts c, 1
  mvi 0, c
loop:
  cmpi b, 0
  je done_loop
  dcr b
  add a, c
  jmp loop
done_loop:
  ; set sign of product according to signs of a & b originally
  ; NOTE: set_result_sign will ret out of the call to mult
  jmp set_result_sign
