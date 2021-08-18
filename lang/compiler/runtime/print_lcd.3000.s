; Prints a string to the LCD display
;
; Expects a pointer to the first character of the string to 
; be passed in the A register.
runtime_print_lcd:
  ; read the character currently pointed to
  ld a, b

  ; stop when a null byte is reached
  cmp b, z
  je runtime_print_lcd_done

  dd b  ; output char on display
  inr a ; increment pointer to next char

  jmp runtime_print_lcd
runtime_print_lcd_done:
  ret