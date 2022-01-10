entry:
  mvi 127, a
  call itoa
  hlt


; Prints the base-10 representation of a signed int to the LCD display.
; The number to format is passed in the A register.
; Note: Only works with values in the range [-127, 127] because 128 doesn't 
; play nicely.
itoa:
  cmp a, z
  jge itoa_positive
  did 45              ; Write '-' to LCD
itoa_positive:
itoa_inner:
  mvi 10, b           ; Divide by 10
  call runtime_divide ; Leaves quotient in c, remainder in a
  cmp c, z            ; Compare quotient with 0
  je itoa_display_remainder
  ; Quotient was non-zero--there are digits to print before this remainder,
  ; so make a recursive call to itoa to print them.

  ; Preserve remainder of initial division
  sts a, 1
  inr sp

  ; Call itoa on quotient (c)
  mov c, a
  call itoa_inner

  ; Restore original remainder
  dcr sp
  lds 1, a

itoa_display_remainder:
  ; Write (remainder + '0') to LCD
  addi 48, a
  dd a
  ret





















; ======================= Division =======================
; runtime_divide performs integer division of a / b, leaving the
; quotient in c, and remainder in a
;
; Note: quotient will be signed properly. remainder is always positive
; in this implementation
;
; :: Implementation ::
; left_over = num
; num_subtracts = 0
; while left_over >= denom:
;   left_over -= denom
;   num_subtracts += 1
;
; quotient is num_subtracts
; remainder is left_over
runtime_divide:
  ; normalize signs to positive
  call runtime_normalize_signs
  sts c, 1 ; store sign of quotient on stack
  mov z, c ; now use c as a counter

  ; repeatedly subtract b from a
runtime_divide_loop:
  cmp a, b
  jl runtime_divide_done
  sub b, a
  inr c
  jmp runtime_divide_loop
runtime_divide_done:
  ; quotient in c
  ; remainder in a

  ; set sign of quotient based on signs of a and b
  ; NOTE: set_result_sign will ret from the call to divide
  jmp runtime_set_result_sign
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