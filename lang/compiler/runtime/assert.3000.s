; runtime_assert asserts that the value passed to it in the a register is not 
; 0. If the value is found to be 0, we halt with -1 on the decimal display.
runtime_assert:
  cmpi a, 0
  jne assert_succeeded
  mvi -1, c
  out c
  hlt
assert_succeeded:
  ret
