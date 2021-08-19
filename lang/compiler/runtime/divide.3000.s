; runtime_divide performs integer division of a / b, leaving the
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
runtime_divide:
  ; normalize signs to positive
  call runtime_normalize_signs
  sts c, 1 ; store sign of quotient on stack
  mov z, c ; now use c as a counter

  ; repeatedly subtract b from a
runtime_divide_loop:
  cmp a, b
  jl runtime_divide_done
  sub b, a
  inr c
  jmp runtime_divide_loop
runtime_divide_done:
  ; quotient in c
  ; remainder in a

  ; set sign of quotient based on signs of a and b
  ; NOTE: set_result_sign will ret from the call to divide
  jmp runtime_set_result_sign
