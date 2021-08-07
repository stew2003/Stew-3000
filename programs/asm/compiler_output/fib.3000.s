	mvi 0, a
	sts a, 1
start_while_8:
	mvi 11, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	jge condition_failed_9
	lds 1, a
	sts a, 3
	inr sp
	call function_fib_rec
	dcr sp
	out a
	lds 1, a
	inr a
	sts a, 1
	jmp start_while_8
condition_failed_9:
	mvi 0, a
	sts a, 2
	mvi 1, a
	sts a, 3
	mvi 0, a
	sts a, 1
start_while_10:
	mvi 11, a
	sts a, 4
	lds 1, a
	lds 4, b
	cmp a, b
	jge condition_failed_11
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
	jmp start_while_10
condition_failed_11:
	hlt
function_fib_rec:
	mvi 0, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	jge greater_than_eq_0
	mvi 0, a
	jmp continue_1
greater_than_eq_0:
	mvi 1, a
continue_1:
	inr sp
	call runtime_assert
	dcr sp
	mvi 0, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	je equal_6
	mvi 0, a
	jmp continue_7
equal_6:
	mvi 1, a
continue_7:
	cmpi a, 0
	jne continue_3
	mvi 1, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	je equal_4
	mvi 0, a
	jmp continue_5
equal_4:
	mvi 1, a
continue_5:
continue_3:
	cmpi a, 0
	je condition_failed_2
	mvi 1, a
	ret
condition_failed_2:
	mvi 2, a
	sts a, 3
	lds 1, a
	lds 3, b
	sub b, a
	sts a, 3
	inr sp
	call function_fib_rec
	dcr sp
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
	jne assert_succeeded
	mvi -1, c
	out c
	hlt
assert_succeeded:
	ret
