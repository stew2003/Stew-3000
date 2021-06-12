entry:
  ; xor b, a
  mvi 0b00001111, a
  mvi 0b10101010, b
  xor b, a
  mvi 0b10100101, b
  call assert_a_eq_b

  ; xor c, a
  mvi 0b11001010, c
  mvi 0b11110011, a
  xor c, a
  mvi 0b00111001, b
  call assert_a_eq_b

  ; xor a, b
  mvi 0b11110111, a
  mvi 0b00010010, b
  xor a, b
  mvi 0b11100101, a
  call assert_a_eq_b

  ; xor c, b
  mvi 0b10110011, c
  mvi 0b00011111, b
  xor c, b
  mvi 0b10101100, a
  call assert_a_eq_b

  ; xor a, c
  mvi 0b00000000, a
  mvi 0b11101111, c
  xor a, c
  mvi 0b11101111, a
  mov c, b
  call assert_a_eq_b

  ; xor b, c
  mvi 0b10010110, b
  mvi 0b11110000, c
  xor b, c
  mvi 0b01100110, a
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
