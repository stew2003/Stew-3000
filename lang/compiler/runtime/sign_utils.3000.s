; Makes both a and b positive, and sets c to non-zero if 
; only *one* of a or b was negative. 
runtime_normalize_signs:
  mov z, c
runtime_normalize_signs_norm_a:
  ; if a is negative, make it positive
  cmp a, z
  jge runtime_normalize_signs_norm_b
  neg a
  not c
runtime_normalize_signs_norm_b:
  ; if b is negative, make it positive
  cmp b, z
  jge runtime_normalize_signs_done
  neg b
  not c
runtime_normalize_signs_done:
  ret

; Sets the sign of c based on a value on the stack at index 1.
; If value is 0, leaves c alone, otherwise negates c
; NOTE: this expects to be jumped into, not called
; Assumes b can be clobbered.
runtime_set_result_sign:
  lds 1, b
  cmp b, z
  je runtime_set_result_sign_no_change
  neg c
runtime_set_result_sign_no_change:
  ret
