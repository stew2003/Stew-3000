entry:
  mvi 3, a
  mvi -17, b
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

; mult computes the product of a and b, leaving the result in c
;
; :: Implementation :: 
; running_sum = 0
; counter = b
; while counter > 0
;   counter -= 1
;   running_sum += a
; product is running_sum
mult:
  ; make a and b positive, keep sign info on stack
  call normalize_signs
  sts c, 1

  ; Note on register allocation:
  ; a in a
  ; counter in b
  ; running_sum in c
  mvi 0, c
loop:
  cmpi b, 0
  je done_loop
  dcr b
  add a, c
  jmp loop
done_loop:
  ; check what sign of product should be
  lds 1, a
  cmpi a, 0
  je mult_done
  not c
  inr c
mult_done:
  ret
