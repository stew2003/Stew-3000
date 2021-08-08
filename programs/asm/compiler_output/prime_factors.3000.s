	mvi 118, a
	sts a, 1
start_while_9:
	mvi 1, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	je condition_failed_10
	lds 1, a
	sts a, 3
	inr sp
	call function_find_single_prime_factor
	dcr sp
	sts a, 2
	out a
	lds 2, a
	sts a, 3
	lds 1, a
	lds 3, b
	addi 2, sp
	call runtime_divide
	subi 2, sp
	mov c, a
	sts a, 1
	jmp start_while_9
condition_failed_10:
	hlt
function_find_single_prime_factor:
	mvi 2, a
	sts a, 2
start_while_0:
	lds 1, a
	sts a, 3
	lds 2, a
	lds 3, b
	cmp a, b
	jge condition_failed_1
	lds 2, a
	sts a, 4
	addi 2, sp
	call function_is_prime
	subi 2, sp
	cmpi a, 0
	je continue_3
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
	je equal_4
	mvi 0, a
	jmp continue_5
equal_4:
	mvi 1, a
continue_5:
continue_3:
	cmpi a, 0
	je condition_failed_2
	lds 2, a
	ret
condition_failed_2:
	lds 2, a
	inr a
	sts a, 2
	jmp start_while_0
condition_failed_1:
	lds 1, a
	ret
function_is_prime:
	mvi 1, a
	sts a, 2
	lds 1, a
	lds 2, b
	sub b, a
	sts a, 2
start_while_6:
	mvi 1, a
	sts a, 3
	lds 2, a
	lds 3, b
	cmp a, b
	jle condition_failed_7
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
	jne condition_failed_8
	mvi 0, a
	ret
condition_failed_8:
	lds 2, a
	dcr a
	sts a, 2
	jmp start_while_6
condition_failed_7:
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
