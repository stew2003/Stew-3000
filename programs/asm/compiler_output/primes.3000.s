	mvi 2, a
	sts a, 1
start_while_11:
	mov z, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	jle condition_failed_12
	lds 1, a
	sts a, 3
	inr sp
	call function_is_prime
	dcr sp
	cmp a, z
	je condition_failed_13
	lds 1, a
	out a
condition_failed_13:
	lds 1, a
	mov a, b
	inr b
	sts b, 1
	jmp start_while_11
condition_failed_12:
	hlt
function_is_prime:
	mov z, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	jge greater_than_eq_0
	mov z, a
	jmp continue_1
greater_than_eq_0:
	mvi 1, a
continue_1:
	inr sp
	call runtime_assert
	dcr sp
	mov z, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	je equal_6
	mov z, a
	jmp continue_7
equal_6:
	mvi 1, a
continue_7:
	cmp a, z
	jne continue_3
	mvi 1, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	je equal_4
	mov z, a
	jmp continue_5
equal_4:
	mvi 1, a
continue_5:
continue_3:
	cmp a, z
	je condition_failed_2
	mov z, a
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
	jle condition_failed_9
	mov z, a
	sts a, 3
	lds 2, a
	sts a, 4
	lds 1, a
	lds 4, b
	inr3 sp
	call runtime_divide
	dcr3 sp
	lds 3, b
	cmp a, b
	jne condition_failed_10
	mov z, a
	ret
condition_failed_10:
	lds 2, a
	mov a, b
	dcr b
	sts b, 2
	jmp start_while_8
condition_failed_9:
	mvi 1, a
	ret
runtime_normalize_signs:
	mov z, c
runtime_normalize_signs_norm_a:
	cmp a, z
	jge runtime_normalize_signs_norm_b
	neg a
	not c
runtime_normalize_signs_norm_b:
	cmp b, z
	jge runtime_normalize_signs_done
	neg b
	not c
runtime_normalize_signs_done:
	ret
runtime_set_result_sign:
	lds 1, b
	cmp b, z
	je runtime_set_result_sign_no_change
	neg c
runtime_set_result_sign_no_change:
	ret
runtime_divide:
	call runtime_normalize_signs
	sts c, 1
	mov z, c
runtime_divide_loop:
	cmp a, b
	jl runtime_divide_done
	sub b, a
	inr c
	jmp runtime_divide_loop
runtime_divide_done:
	jmp runtime_set_result_sign
runtime_assert:
	cmp a, z
	jne assert_succeeded
	outi -1
	hlt
assert_succeeded:
	ret
