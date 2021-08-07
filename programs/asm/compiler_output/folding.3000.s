start_while_0:
	mvi 5, a
	cmpi a, 0
	je condition_failed_1
	mvi 50, a
	out a
	jmp start_while_0
condition_failed_1:
	hlt
