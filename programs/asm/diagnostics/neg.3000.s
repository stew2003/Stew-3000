entry:
  ; neg a
  mvi -1, a
  neg a
  mvi 1, b
  call assert_a_eq_b

  ; neg b
  mvi 12, b
  neg b
  mvi -12, a
  call assert_a_eq_b

  ; neg c
  mvi 127, c
  neg c
  mvi -127, a
  mov c, b
  call assert_a_eq_b

  ; 0 negates to 0
  mvi 0, a
  neg a 
  mvi 0, b
  call assert_a_eq_b

  ; halt successfully
  outi 0
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
