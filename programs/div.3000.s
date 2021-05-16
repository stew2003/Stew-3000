entry:
  mvi 87, a
  mvi 4, b
  call divide
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
  call normalize_signs
  sts c, 1 ; store sign of quotient on stack
  mvi 0, c ; now use c as a counter

  ; repeatedly subtract b from a
loop:
  cmp a, b
  jl loop_done
  sub b, a
  inr c
  jmp loop
loop_done:
  ; quotient in c
  ; remainder in a

  ; load what the sign of quotient should be
  lds 1, b
  cmpi b, 0
  je divide_done
  not c
  inr c
divide_done:
  ret
