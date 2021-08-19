; runtime_assert asserts that the value passed to it in the a register is not 
; 0. If the value is found to be 0, we halt with -1 on the decimal display.
runtime_assert:
  cmp a, z
  jne assert_succeeded
  outi -1
  hlt
assert_succeeded:
  ret
