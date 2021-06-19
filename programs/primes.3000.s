	mvi 2, a
	sts a, 1
start_while_24:
	mvi 0, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	jg greater_27
	mvi 0, a
	jmp continue_28
greater_27:
	mvi 1, a
continue_28:
	cmpi a, 0
	je condition_failed_25
	lds 1, a
	sts a, 3
	addi 1, sp
	call function_is_prime
	subi 1, sp
	cmpi a, 0
	je condition_failed_26
	lds 1, a
	out a
condition_failed_26:
	lds 1, a
	inr a
	sts a, 1
	jmp start_while_24
condition_failed_25:
	hlt
function_is_prime:
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
	mvi 0, a
	ret
condition_failed_11:
	mvi 1, a
	sts a, 2
	lds 1, a
	lds 2, b
	sub b, a
	sts a, 2
start_while_17:
	mvi 1, a
	sts a, 3
	lds 2, a
	lds 3, b
	cmp a, b
	jg greater_22
	mvi 0, a
	jmp continue_23
greater_22:
	mvi 1, a
continue_23:
	cmpi a, 0
	je condition_failed_18
	mvi 0, a
	sts a, 3
	lds 2, a
	sts a, 4
	lds 1, a
	lds 4, b
	addi 3, sp
	call runtime_divide
	subi 3, sp
	lds 3, b
	cmp a, b
	je equal_20
	mvi 0, a
	jmp continue_21
equal_20:
	mvi 1, a
continue_21:
	cmpi a, 0
	je condition_failed_19
	mvi 0, a
	ret
condition_failed_19:
	lds 2, a
	dcr a
	sts a, 2
	jmp start_while_17
condition_failed_18:
	mvi 1, a
	ret
	ret
runtime_normalize_signs:
	mvi 0, c
norm_a_4:
	cmpi a, 0
	jge norm_b_5
	not a
	inr a
	not c
norm_b_5:
	cmpi b, 0
	jge done_6
	not b
	inr b
	not c
done_6:
	ret
runtime_set_result_sign:
	lds 1, b
	cmpi b, 0
	je no_change_7
	not c
	inr c
no_change_7:
	ret
runtime_divide:
	call runtime_normalize_signs
	sts c, 1
	mvi 0, c
loop_2:
	cmp a, b
	jl done_3
	sub b, a
	inr c
	jmp loop_2
done_3:
	jmp runtime_set_result_sign
runtime_assert:
	cmpi a, 0
	jne success_8
	mvi -1, c
	out c
	hlt
success_8:
	ret
