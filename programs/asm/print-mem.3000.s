; prints the contents of program memory to the 
; decimal display, byte by byte
    mvi 0, a
loop:
    ld a, b     ; load mem @ a into b
    out b       ; print b's contents
    inr a       ; increment address

    ; if overflowed, halt. otherwise loop
    cmpi a, 0
    jne loop
    hlt
