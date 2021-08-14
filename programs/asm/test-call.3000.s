entry:
    call fun_1
    add a, b
    out b
    hlt

fun_1:
    mvi 40, a
    call fun_2
    ret

fun_2:
    mvi 5, b
    ret

