entry:
  ; dcr a
  mvi 40, a
  dcr a
  mvi 39, b
  call assert_a_eq_b

  ; dcr b
  mvi -28, b
  dcr b
  mvi -29, a
  call assert_a_eq_b

  ; dcr c
  mvi 116, c
  dcr c
  mvi 115, a
  mov c, b
  call assert_a_eq_b

  ; dcr sp
  dcr sp
  ; NOTE: manually observe: sp = -1

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
