entry:
    mvi 1, b
    out b

loop:
    add b, a
    cmpi a, 0
    jl stop
    out a
    call swap
    jmp loop

stop:
    hlt

swap: 
    mov a, c
    mov b, a
    mov c, b
    ret