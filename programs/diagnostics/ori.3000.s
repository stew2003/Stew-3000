entry:
  ; ori byte, a
  mvi 0b00011011, a
  ori 0b11110101, a
  mvi 0b11111111, b
  call assert_a_eq_b

  ; ori byte, b
  mvi 0b11100101, b
  ori 0b11000111, b
  mvi 0b11100111, a
  call assert_a_eq_b

  ; ori byte, c
  mvi 0b00110011, c
  ori 0b11010010, c
  mvi 0b11110011, a
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
