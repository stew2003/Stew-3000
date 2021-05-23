entry:
  ; mov a, b
  mvi 10, a
  mov a, b
  call assert_a_eq_b

  ; mov a, c
  mvi 12, a
  mov a, c
  mov c, b
  call assert_a_eq_b

  ; mov b, a
  mvi 125, b
  mov b, a
  call assert_a_eq_b

  ; mov b, c
  mvi -19, b
  mov b, c
  mov c, a
  call assert_a_eq_b

  ; mov c, a
  mvi 43, c
  mov c, a
  mvi 43, b
  call assert_a_eq_b

  ; mov c, b
  mvi 100, c
  mov c, b
  mvi 100, a
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
