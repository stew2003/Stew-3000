; Initializes the LCD display
; See https://www.openhacks.com/uploadsproductos/eone-1602a1.pdf
runtime_lcd_init:
  ; function set (8-bit mode, 2-line display, 5x8 font)
  dic 0b00111000
  ; display on, cursor on, cursor blink on
  dic 0b00001111
  ; entry mode set: increment (write left-to-right), no display shift
  dic 0b00000110
  ; clear display
  dic 0b00000001