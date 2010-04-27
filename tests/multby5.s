	; Define automatically-allocated chunk of zero-page locations.
	.temps $70..$7f
	
	.context multiply_xa_by_3
	; a 2-byte variable allocated from the above pool.
	.var2 tmp
multiply_xa_by_3:
	; original number in [x:a].
	sta %tmp
	stx %tmp + 1
	asl a
	rol %tmp + 1
	; now [$71:a] are the original number * 2.
	clc
	adc %tmp
	sta %tmp
	txa
	adc %tmp + 1
	tax
	lda %tmp
	; now [x:a] should be the original number * 3.
	rts
	.ctxend

	.context multiply_xa_by_5
	; two 2-byte variables.
	.var2 tmp1, tmp2
multiply_xa_by_5:
	sta %tmp1
	stx %tmp1 + 1
	jsr multiply_xa_by_3
	sta %tmp2
	stx %tmp2 + 1

	; multiply tmp1 by 2.
	lda %tmp1 + 1
	asl %tmp1
	rol a
	tax

	; add saved input * 3 value.
	lda %tmp1
	clc
	adc %tmp2
	sta %tmp1
	txa
	adc %tmp2 + 1
	tax
	lda %tmp1
	rts
	.ctxend
