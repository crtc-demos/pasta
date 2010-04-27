	.temps $70..$71

	.context hello
	.var i, j, k
	lda %i
	clc
	adc %j
	sta $k
	.ctxend
