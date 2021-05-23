entry:
  ; or b, a
  mvi 0b00001111, a
  mvi 0b10101010, b
  or b, a
  mvi 0b10101111, b
  call assert_a_eq_b

  ; or c, a
  mvi 0b11001010, c
  mvi 0b11110011, a
  or c, a
  mvi 0b11111011, b
  call assert_a_eq_b

  ; or a, b
  mvi 0b11110111, a
  mvi 0b00010010, b
  or a, b
  mvi 0b11110111, a
  call assert_a_eq_b

  ; or c, b
  mvi 0b10110011, c
  mvi 0b00011111, b
  or c, b
  mvi 0b10111111, a
  call assert_a_eq_b

  ; or a, c
  mvi 0b00000000, a
  mvi 0b11101111, c
  or a, c
  mvi 0b11101111, a
  mov c, b
  call assert_a_eq_b

  ; or b, c
  mvi 0b10010110, b
  mvi 0b11110000, c
  or b, c
  mvi 0b11110110, a
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
