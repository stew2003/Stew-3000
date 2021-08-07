; entry:
;     call fun

; fun:
;     lds 0, a
;     hlt

entry:
    mvi 14, a
    sts a, 83
    lds 83, b
    out b
    hlt