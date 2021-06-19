; entry:
;     mvi 20, a
;     call fun
;     hlt

; fun:
;     addi 10, a
;     ret

entry:
    call fun

fun:
    lds 0, a
    hlt