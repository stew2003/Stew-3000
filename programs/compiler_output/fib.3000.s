	mvi 0, a
	sts a, 1
start_while_17:
	mvi 11, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	jl less_19
	mvi 0, a
	jmp continue_20
less_19:
	mvi 1, a
continue_20:
	cmpi a, 0
	je condition_failed_18
	lds 1, a
	sts a, 3
	addi 1, sp
	call function_fib_rec
	subi 1, sp
	out a
	lds 1, a
	inr a
	sts a, 1
	jmp start_while_17
condition_failed_18:
	mvi 0, a
	sts a, 2
	mvi 1, a
	sts a, 3
	mvi 0, a
	sts a, 1
start_while_21:
	mvi 11, a
	sts a, 4
	lds 1, a
	lds 4, b
	cmp a, b
	jl less_23
	mvi 0, a
	jmp continue_24
less_23:
	mvi 1, a
continue_24:
	cmpi a, 0
	je condition_failed_22
	lds 3, a
	out a
	lds 2, a
	sts a, 4
	lds 3, a
	sts a, 2
	lds 3, a
	sts a, 5
	lds 4, a
	lds 5, b
	add b, a
	sts a, 3
	lds 1, a
	inr a
	sts a, 1
	jmp start_while_21
condition_failed_22:
	hlt
function_fib_rec:
	mvi 0, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	jge greater_than_eq_9
	mvi 0, a
	jmp continue_10
greater_than_eq_9:
	mvi 1, a
continue_10:
	addi 1, sp
	call runtime_assert
	subi 1, sp
	mvi 0, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	je equal_15
	mvi 0, a
	jmp continue_16
equal_15:
	mvi 1, a
continue_16:
	cmpi a, 0
	jne continue_12
	mvi 1, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	je equal_13
	mvi 0, a
	jmp continue_14
equal_13:
	mvi 1, a
continue_14:
continue_12:
	cmpi a, 0
	je condition_failed_11
	mvi 1, a
	ret
condition_failed_11:
	mvi 2, a
	sts a, 3
	lds 1, a
	lds 3, b
	sub b, a
	sts a, 3
	addi 1, sp
	call function_fib_rec
	subi 1, sp
	sts a, 2
	mvi 1, a
	sts a, 4
	lds 1, a
	lds 4, b
	sub b, a
	sts a, 4
	addi 2, sp
	call function_fib_rec
	subi 2, sp
	lds 2, b
	add b, a
	ret
runtime_assert:
	cmpi a, 0
	jne success_8
	mvi -1, c
	out c
	hlt
success_8:
	ret
