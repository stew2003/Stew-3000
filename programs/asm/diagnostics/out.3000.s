entry:
  ; out a
  mvi 40, a
  out a
  ; NOTE: manually observe: dec display is 40

  ; out b
  mvi -15, b
  out b
  ; NOTE: manually observe: dec display is -15

  ; out c
  mvi 127, c
  out c
  ; NOTE: manually observe: dec display is 127

  ; halt successfully
  mvi 0, c
  out c
  hlt
