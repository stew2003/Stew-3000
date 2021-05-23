entry:
  ; less
  mvi 78, a
  cmpi 60, a
  jl less1
  call assert_not_reached
less1:

  ; equal
  mvi -49, a
  cmpi -49, a
  jl less2
  jmp continue1
less2:
  call assert_not_reached
continue1:

  ; greater
  mvi 100, a
  cmpi 114, a
  jl less3
  jmp continue2
less3:
  call assert_not_reached
continue2:

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