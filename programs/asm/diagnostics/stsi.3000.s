entry:
  ; store 10 at addr sp+80
  stsi 10, 80
  lds 80, a
  mvi 10, b
  call assert_a_eq_b

  ; store -1 at addr sp+255
  stsi -1, 255
  lds 255, a
  mvi -1, b
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
