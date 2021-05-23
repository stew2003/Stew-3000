entry:
  ; add a, a
  mvi 10, a
  mvi 20, b
  add a, a
  call assert_a_eq_b

  ; add a, b
  mvi 15, a
  mvi 21, b
  add a, b
  mvi 36, a
  call assert_a_eq_b

  ; add a, c
  mvi 4, a
  mvi 26, c
  add a, c
  mvi 30, a
  mov c, b
  call assert_a_eq_b

  ; add b, a
  mvi 40, a
  mvi 12, b 
  add b, a
  mvi 52, b
  call assert_a_eq_b

  ; add b, b
  mvi -5, b
  add b, b
  mvi -10, a
  call assert_a_eq_b

  ; add b, c
  mvi 100, b
  mvi -20, c
  add b, c
  mvi 80, a
  mov c, b
  call assert_a_eq_b

  ; add c, a
  mvi 45, a
  mvi 30, c
  add c, a
  mvi 75, b
  call assert_a_eq_b

  ; add c, b 
  mvi 4, c
  mvi -18, b
  add c, b 
  mvi -14, a
  call assert_a_eq_b

  ; add c, c
  mvi 44, c
  add c, c
  mov c, b
  mvi 88, a
  call assert_a_eq_b

  ; add a, sp
  mvi 5, a
  add a, sp
  ; NOTE: manually observe: sp = 5

  ; add b, sp
  mvi 16, b
  add b, sp
  ; NOTE: manually observe: sp = 21

  ; add c, sp 
  mvi 100, c
  add c, sp
  ; NOTE: manually observe: sp = 121

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
