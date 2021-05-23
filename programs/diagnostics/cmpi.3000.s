entry:
  ; cmpi byte, a
  ; cmpi a, byte
  mvi 40, a
  cmpi a, 50
  ; NOTE: manually observe: zf = 0, sf = 1, of = 0
  cmpi 100, a
  ; NOTE: manually observe: zf = 0, sf = 0, of = 0

  ; cmpi byte, b
  ; cmpi b, byte
  mvi -21, b
  cmpi b, 16
  ; NOTE: manually observe: zf = 0, sf = 1, of = 0
  cmpi 42, b
  ; NOTE: manually observe: zf = 0, sf = 1, of = 0

  ; cmpi byte, c
  ; cmpi c, byte
  mvi 100, c
  cmpi c, 100
  ; NOTE: manually observe: zf = 1, sf = 0, of = 0
  cmpi -40, c
  ; NOTE: manually observe: zf = 0, sf = 0, of = 1

  ; halt successfully
  mvi 0, c
  out c
  hlt
