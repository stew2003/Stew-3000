entry:
  ; and b, a
  mvi 0b00001111, a
  mvi 0b10101010, b
  and b, a
  mvi 0b00001010, b
  call assert_a_eq_b

  ; and c, a
  mvi 0b11001010, c
  mvi 0b11110011, a
  and c, a
  mvi 0b11000010, b
  call assert_a_eq_b

  ; and a, b
  mvi 0b11110111, a
  mvi 0b00010010, b
  and a, b
  mvi 0b00010010, a
  call assert_a_eq_b

  ; and c, b
  mvi 0b10110011, c
  mvi 0b00011111, b
  and c, b
  mvi 0b00010011, a
  call assert_a_eq_b

  ; and a, c
  mvi 0b00000000, a
  mvi 0b11101111, c
  and a, c
  mvi 0b00000000, a
  mov c, b
  call assert_a_eq_b

  ; and b, c
  mvi 0b10010110, b
  mvi 0b11110000, c
  and b, c
  mvi 0b10010000, a
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
