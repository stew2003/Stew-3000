start_while_9:
	mvi 5, a
	cmpi a, 0
	je condition_failed_10
	mvi 50, a
	out a
	jmp start_while_9
condition_failed_10:
	hlt
