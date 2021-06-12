entry:
  ; addi byte, a
  mvi 16, a
  addi 20, a
  mvi 36, b
  call assert_a_eq_b

  ; addi byte, b
  mvi 47, b
  addi -17, b
  mvi 30, a
  call assert_a_eq_b

  ; addi byte, c
  mvi 100, c
  addi 14, c
  mvi 114, a
  mov c, b
  call assert_a_eq_b

  ; addi byte, sp
  addi 25, sp
  ; NOTE: manually observe: sp = 25

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
