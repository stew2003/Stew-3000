entry:
  ; less
  mvi 40, a
  cmpi 39, a
  jle less_or_eq1
  call assert_not_reached
less_or_eq1:

  ; equal
  mvi 122, a
  cmpi 122, a
  jle less_or_eq2
  call assert_not_reached
less_or_eq2:

  ; greater
  mvi 99, a
  cmpi 104, a
  jle less_or_eq3
  jmp continue1
less_or_eq3:
  call assert_not_reached
continue1:

  ; halt successfully
  mvi 0, c
  out c
  hlt

; assert_not_reached asserts that this block
; of code is never run. If it is, it halts
; with -1 on the decimal display
assert_not_reached:
  mvi -1, c
  out c
  hlt