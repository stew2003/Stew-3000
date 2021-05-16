entry:
  mvi 87, a
  mvi 4, b
  call divide
  hlt

; divide performs integer division of a / b, leaving the
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
; remainder is left_over
divide:
  ; normalize signs to positive
  ; after checks, if c is 0 quotient should be positive,
  ; and otherwise quotient should be negative
  mvi 0, c
check_num:
  ; if numerator is negative, make it positive
  cmpi a, 0
  jge check_denom
  not a
  inr a
  not c
check_denom:
  ; if denom is negative, make it positive
  cmpi b, 0
  jge after_checks
  not b
  inr b
  not c
after_checks:
  sts c, 1 ; store sign of quotient on stack
  mvi 0, c ; now use c as a counter
loop:
  cmp a, b
  jl done
  sub b, a
  inr c
  jmp loop
done:
  ; quotient in c
  ; remainder in a
  lds 1, b
  cmpi b, 0
  je return ; if b=0, do not change quotient sign
  not c
  inr c
return:
  ret
