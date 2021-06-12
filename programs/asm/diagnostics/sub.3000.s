entry:
  ; sub b, a
  mvi 10, a
  mvi 6, b
  sub b, a
  mvi 4, b
  call assert_a_eq_b

  ; sub c, a
  mvi 80, c
  mvi -10, a
  sub c, a
  mvi -90, b
  call assert_a_eq_b

  ; sub a, b
  mvi 12, a
  mvi 40, b
  sub a, b
  mvi 28, a
  call assert_a_eq_b

  ; sub c, b
  mvi 50, c
  mvi 35, b
  sub c, b
  mvi -15, a
  call assert_a_eq_b

  ; sub a, c
  mvi 14, a
  mvi 12, c
  sub a, c
  mov c, b
  mvi -2, a
  call assert_a_eq_b

  ; sub b, c
  mvi -15, b
  mvi -40, c
  sub b, c
  mov c, b
  mvi -25, a
  call assert_a_eq_b

  ; sub a, sp
  mvi 10, a
  sub a, sp
  ; NOTE: manually observe: sp = -10

  ; sub b, sp
  mvi -25, b
  sub b, sp
  ; NOTE: manually observe: sp = 15

  ; sub c, sp
  mvi 14, c
  sub c, sp
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
