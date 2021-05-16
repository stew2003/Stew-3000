; Makes both a and b positive, and sets c to non-zero if 
; only *one* of a or b was negative. 
runtime_normalize_signs:
  mvi 0, c
norm_a:
  ; if a is negative, make it positive
  cmpi a, 0
  jge norm_b
  not a
  inr a
  not c
norm_b:
  ; if b is negative, make it positive
  cmpi b, 0
  jge done_norm
  not b
  inr b
  not c
done_norm:
  ret

; Sets the sign of c based on a value on the stack at index 1.
; If value is 0, leaves c alone, otherwise negates c
; NOTE: this expects to be jumped into, not called
; Assumes b can be clobbered.
runtime_set_result_sign:
  lds 1, b
  cmpi b, 0
  je no_sign_change
  not c
  inr c
no_sign_change:
  ret
