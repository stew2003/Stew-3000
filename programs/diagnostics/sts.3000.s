entry:
  ; set up memory
  ; (uses high stack indexes so call to assert_a_eq_b
  ; doesn't clobber stack with return address)
  ; si=10: 70
  ; si=11: 80
  ; si=12: 90
  mvi 70, a
  mvi 10, b
  st a, b
  mvi 80, a
  mvi 11, b
  st a, b
  mvi 90, a
  mvi 12, b
  st a, b

  ; clear registers
  mvi 0, a
  mvi 0, b
  mvi 0, c

  ; sts a, byte
  mvi 7, a
  sts a, 10
  lds 10, b
  call assert_a_eq_b

  ; sts b, byte
  mvi 8, b
  sts b, 11
  lds 11, a
  call assert_a_eq_b

  ; sts c, byte
  mvi 9, c
  sts c, 12
  lds 12, a
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
