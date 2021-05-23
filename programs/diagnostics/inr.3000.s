entry:
  ; inr a
  mvi 40, a
  inr a
  mvi 41, b
  call assert_a_eq_b

  ; inr b
  mvi -28, b
  inr b
  mvi -27, a
  call assert_a_eq_b

  ; inr c
  mvi 116, c
  inr c
  mvi 117, a
  mov c, b
  call assert_a_eq_b

  ; inr sp
  inr sp
  ; NOTE: manually observe: sp = 1

  ; halt successfully
  mvi 0, c
  out c
  hlt

; assert_a_eq_b compares registers a and b, and
; halts with -1 on the decimal display if they 
; are not equal.
assert_a_eq_b:
  cmp a, b
  je assert_succeeded
  mvi -1, c
  out c
  hlt
assert_succeeded:
  ret
