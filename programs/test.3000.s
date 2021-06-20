entry:
    mvi 1, b
    out b

loop:
    add b, a
    out a
    mov a, c
    mov b, a
    mov c, b
    jmp loop