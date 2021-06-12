entry:
  ; ani byte, a
  mvi 0b00011011, a
  ani 0b11110101, a
  mvi 0b00010001, b
  call assert_a_eq_b

  ; ani byte, b
  mvi 0b11100101, b
  ani 0b11111111, b
  mvi 0b11100101, a
  call assert_a_eq_b

  ; ani byte, c
  mvi 0b00110011, c
  ani 0b11010010, c
  mvi 0b00010010, a
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
