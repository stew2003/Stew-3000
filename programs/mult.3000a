entry:
    mvi 10, b
    mvi 5, c
    call mult
    out a
    hlt

; mult computes the product of b and c, 
; leaving the result in a
mult:
    mvi 0, a
    sts a, 1 ; running sum
    sts a, 2 ; counter
loop:
    ; store the running sum, load the counter
    sts a, 1
    lds 2, a

    ; cmp counter to c
    cmp a, c
    je done
    inr a ; increment counter

    ; store the counter, load the running sum
    sts a, 2
    lds 1, a

    ; add b to running sum another time
    add b, a
    jmp loop
done:
    ; restore sum before returning
    lds 1, a
    ret
