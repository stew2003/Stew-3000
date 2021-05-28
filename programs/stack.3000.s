; fills up the stack by setting stack[i] = i
loop:
	st a, a
	inr a
	cmpi a, 0
	jne loop
	hlt
