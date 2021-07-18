; runtime_multiply computes the product of a and b, leaving the result in c
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
runtime_multiply:
  ; make a and b positive, keep sign info on stack
  call runtime_normalize_signs
  sts c, 1
  mvi 0, c
runtime_multiply_loop:
  cmpi b, 0
  je runtime_multiply_done
  dcr b
  add a, c
  jmp runtime_multiply_loop
runtime_multiply_done:
  ; set sign of product according to signs of a & b originally
  ; NOTE: set_result_sign will ret out of the call to mult
  jmp runtime_set_result_sign
