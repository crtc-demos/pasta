; unsigned multiply r0 by r1, output r1:r0 (16 x 16 -> 32)
umult_sihi:
	push	r8
	push	r7
	push	r6
	push	r5
	push	r4
	
	not	r2, r0
	mov	r3, r1
	movi	r4, #0
	movi	r8, #0
	movi	r1, #0		; result in r1:r8
	movi	r0, #16
loop:
	lsrx	r2, #1
	sbc	r5, r5		; -1 if low bit clear.
	and	r6, r3, r5
	and	r7, r4, r5
	add	r8, r8, r6
	add	r1, r1, r7
	add	r3, r3, r3
	lslx	r4, #1
	subi	r0, #1
	ifnz	r0, loop
	
	mov	r0, r8
	
	pop	r4
	pop	r5
	pop	r6
	pop	r7
	pop	r8
	rets

; unsigned multiply r0 by r1, output r0 (16 x 16 -> 16)
umult_hihi:
	push	r5
	push	r4

	movi	r2, #0
	movi	r5, #1
loop2:
	mov	r4, r1
	clz	r4
	lsl	r7, r0, r4
	add	r2, r2, r7
	lsl	r4, r5, r4
	xor	r1, r1, r4
	ifnz	r1, loop2
	mov	r0, r2

	pop	r4
	pop	r5
	rets
