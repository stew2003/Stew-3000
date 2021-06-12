entry:
  ; set up memory
  ; 0x01: 15
  ; 0x02: 30
  ; 0x03: 60
  mvi 15, a
  sts a, 1
  mvi 30, a
  sts a, 2
  mvi 60, a
  sts a, 3

  ; bump sp to not clobber above memory contents
  addi 3, sp

  ; st a, a
  mvi 1, a
  st a, a
  ld a, b
  mvi 1, a
  call assert_a_eq_b

  ; st b, a
  mvi 100, b
  mvi 2, a
  st b, a
  ld a, a
  call assert_a_eq_b

  ; st c, a
  mvi 14, c
  mvi 2, a
  st c, a
  ld a, b
  mvi 14, a
  call assert_a_eq_b

  ; st a, b
  mvi 99, a
  mvi 3, b
  st a, b
  ld b, c
  mov c, b
  mvi 99, a
  call assert_a_eq_b

  ; st b, b
  mvi 2, b
  st b, b
  ld b, a
  mvi 2, b
  call assert_a_eq_b

  ; st c, b
  mvi 116, c
  mvi 1, b
  st c, b
  ld b, a
  mvi 116, b
  call assert_a_eq_b

  ; st a, c
  mvi 13, a
  mvi 3, c
  st a, c
  ld c, b
  mvi 13, a
  call assert_a_eq_b

  ; st b, c
  mvi 120, b
  mvi 3, c
  st b, c
  ld c, a
  mvi 120, b
  call assert_a_eq_b

  ; st c, c
  mvi 2, c
  st c, c
  ld c, a
  mvi 2, b
  call assert_a_eq_b

  ; restore sp
  subi 3, sp

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
