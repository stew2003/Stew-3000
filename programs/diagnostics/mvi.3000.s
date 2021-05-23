entry:
  ; mvi byte, a
  mvi 12, a
  mvi 12, b
  call assert_a_eq_b

  ; mvi byte, b
  mvi -100, b
  mvi -100, a
  call assert_a_eq_b
  
  ; mvi byte, c
  mvi 127, c
  mvi 127, a
  mov c, b
  call assert_a_eq_b

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
