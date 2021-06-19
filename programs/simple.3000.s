	mvi 10, a
	sts a, 1
	mvi 40, a
	lds 1, b
	addi 0, sp
	call runtime_divide
	subi 0, sp
	out a
	hlt
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
