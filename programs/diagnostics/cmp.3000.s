entry:
  ; cmp a, b
  mvi 5, a
  mvi 10, b
  cmp a, b
  ; NOTE: manually observe: zf = 0, sf = 1, of = 0

  ; cmp a, c
  mvi 40, a
  mvi 6, c
  cmp a, c
  ; NOTE: manually observe: zf = 0, sf = 0, of = 0

  ; cmp b, a
  mvi 16, b
  mvi 16, a
  cmp b, a
  ; NOTE: manually observe: zf = 1, sf = 0, of = 0

  ; cmp b, c
  mvi -128, b
  mvi 50, c
  cmp b, c
  ; NOTE: manually observe: zf = 0, sf = 0, of = 1

  ; cmp c, a
  mvi 127, c
  mvi -10, a
  cmp c, a
  ; NOTE: manually observe: zf = 0, sf = 1, of = 1

  ; cmp c, b
  mvi 14, c
  mvi 100, b
  cmp c, b
  ; NOTE: manually observe: zf = 0, sf = 1, of = 0

  ; halt successfully
  mvi 0, c
  out c
  hlt
