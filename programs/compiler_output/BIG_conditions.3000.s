	mvi 10, a
	sts a, 1
	mvi 15, a
	sts a, 2
	lds 1, a
	lds 2, b
	cmp a, b
	jl less_1
	mvi 0, a
	jmp continue_2
less_1:
	mvi 1, a
continue_2:
	cmpi a, 0
	je condition_failed_0
	mvi 1, a
	out a
condition_failed_0:
	hlt
