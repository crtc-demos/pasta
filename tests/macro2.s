	.macro add res a b
foo:
	lda %a
	clc
	adc %b
	sta %res
	lda %a+1
	adc %b+1
	sta %res+1
	bcc foo
	.mend

	@add 16, 18, 20
	@add 16, bob, 20
