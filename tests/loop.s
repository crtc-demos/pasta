$strcpy:
	movi	r4, #lo(dest)
	addhi	r4, #hi(dest)
	movi	r6, #lo(src)
	addhi	r6, #hi(src)
	movi	r5, #0
	movi	r7, #0
	movi	r3, #0
loop:
	ldr	r2, [r7:r6, r3]
	str	r2, [r5:r4, r3]
	addi	r3, #2
	ifnz	r2, loop
	rets
dest:
	data	#0
	data	#0
	data	#0
src:
	data	#10000
	data	#20000
	data	#0
