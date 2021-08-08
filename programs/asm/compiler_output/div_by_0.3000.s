	mvi 0, a
	sts a, 1
	mvi 4, a
	lds 1, b
	addi 0, sp
	call runtime_divide
	subi 0, sp
	mov c, a
	sts a, 1
	mvi 6, a
	lds 1, b
	addi 0, sp
	call runtime_multiply
	subi 0, sp
	mov c, a
	out a
	hlt
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
runtime_multiply:
	call runtime_normalize_signs
	sts c, 1
	mvi 0, c
runtime_multiply_loop:
	cmpi b, 0
	je runtime_multiply_done
	dcr b
	add a, c
	jmp runtime_multiply_loop
runtime_multiply_done:
	jmp runtime_set_result_sign
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
