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

  ; ld a, a
  mvi 1, a
  ld a, a
  mvi 15, b
  call assert_a_eq_b

  ; ld b, a
  mvi 3, b
  ld b, a
  mvi 60, b
  call assert_a_eq_b

  ; ld c, a
  mvi 2, c
  ld c, a
  mvi 30, b
  call assert_a_eq_b

  ; ld a, b
  mvi 2, a
  ld a, b
  mvi 30, a
  call assert_a_eq_b

  ; ld b, b
  mvi 2, b
  ld b, b
  mvi 30, a
  call assert_a_eq_b

  ; ld c, b
  mvi 1, c
  ld c, b
  mvi 15, a
  call assert_a_eq_b

  ; ld a, c
  mvi 3, a
  ld a, c
  mvi 60, a
  mov c, b
  call assert_a_eq_b

  ; ld b, c
  mvi 1, b
  ld b, c
  mvi 15, a
  mov c, b
  call assert_a_eq_b

  ; ld c, c
  mvi 3, c
  ld c, c
  mvi 60, a
  mov c, b
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
