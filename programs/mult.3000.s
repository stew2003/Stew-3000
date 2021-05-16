entry:
    mvi -1, a
    mvi -1, b
    call mult
    out c
    hlt

; Makes both a and b positive, and sets c to non-zero if 
; only *one* of a or b was negative. 
normalize_signs:
  mvi 0, c
norm_a:
  ; if a is negative, make it positive
  cmpi a, 0
  jge norm_b
  not a
  inr a
  not c
norm_b:
  ; if b is negative, make it positive
  cmpi b, 0
  jge done_norm
  not b
  inr b
  not c
done_norm:
  ret

; mult computes the product of b and c, leaving the result in a
;
; :: Implementation :: 
; running_sum = 0
; counter = 0
; while counter != c
;   counter += 1
;   running_sum += b
; product is running_sum
mult:
    ; running sum at sp+1
    ; counter at sp+2
    ; product sign info at sp+3
    call normalize_signs
    sts c, 3
    mvi 0, c
    sts c, 2 ; init counter to 0
loop:
    ; store the running sum, load the counter
    sts c, 1
    lds 2, c

    ; cmp counter to c
    cmp c, b
    je done
    inr c ; increment counter

    ; store the counter, load the running sum
    sts c, 2
    lds 1, c

    ; add b to running sum another time
    add a, c
    jmp loop
done:
    ; restore sum before returning
    lds 1, c

    ; check what sign of product should be
    lds 3, a
    cmpi a, 0
    je no_sign_change
    not c
    inr c
no_sign_change:
    ret
