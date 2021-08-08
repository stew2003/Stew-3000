entry:
    call fun

fun:
    lds 0, a
    out a
    hlt

; entry:
;     mvi 200, a
;     sts a, 83
;     lds 83, b
;     out b
;     hlt