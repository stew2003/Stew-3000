	mvi 2, a
	sts a, 1
start_while_15:
	mvi 0, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	jg greater_18
	mvi 0, a
	jmp continue_19
greater_18:
	mvi 1, a
continue_19:
	cmpi a, 0
	je condition_failed_16
	lds 1, a
	sts a, 3
	addi 1, sp
	call function_is_prime
	subi 1, sp
	cmpi a, 0
	je condition_failed_17
	lds 1, a
	out a
condition_failed_17:
	lds 1, a
	inr a
	sts a, 1
	jmp start_while_15
condition_failed_16:
	hlt
function_is_prime:
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
	addi 1, sp
	call runtime_assert
	subi 1, sp
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
	mvi 0, a
	ret
condition_failed_2:
	mvi 1, a
	sts a, 2
	lds 1, a
	lds 2, b
	sub b, a
	sts a, 2
start_while_8:
	mvi 1, a
	sts a, 3
	lds 2, a
	lds 3, b
	cmp a, b
	jg greater_13
	mvi 0, a
	jmp continue_14
greater_13:
	mvi 1, a
continue_14:
	cmpi a, 0
	je condition_failed_9
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
	je equal_11
	mvi 0, a
	jmp continue_12
equal_11:
	mvi 1, a
continue_12:
	cmpi a, 0
	je condition_failed_10
	mvi 0, a
	ret
condition_failed_10:
	lds 2, a
	dcr a
	sts a, 2
	jmp start_while_8
condition_failed_9:
	mvi 1, a
	ret
runtime_normalize_signs:
	mvi 0, c
runtime_normalize_signs_norm_a:
	cmpi a, 0
	jge runtime_normalize_signs_norm_b
	not a
	inr a
	not c
runtime_normalize_signs_norm_b:
	cmpi b, 0
	jge runtime_normalize_signs_done
	not b
	inr b
	not c
runtime_normalize_signs_done:
	ret
runtime_set_result_sign:
	lds 1, b
	cmpi b, 0
	je runtime_set_result_sign_no_change
	not c
	inr c
runtime_set_result_sign_no_change:
	ret
runtime_divide:
	call runtime_normalize_signs
	sts c, 1
	mvi 0, c
runtime_divide_loop:
	cmp a, b
	jl runtime_divide_done
	sub b, a
	inr c
	jmp runtime_divide_loop
runtime_divide_done:
	jmp runtime_set_result_sign
runtime_assert:
	cmpi a, 0
	jne assert_succeeded
	mvi -1, c
	out c
	hlt
assert_succeeded:
	ret