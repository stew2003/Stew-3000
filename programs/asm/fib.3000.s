entry:
    mvi 1, b
    out b

loop:
    add b, a
    cmpi a, 0
    jl stop
    out a
    mov a, c
    mov b, a
    mov c, b
    jmp loop

stop:
    hlt