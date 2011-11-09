	.macro add res a b
	lda %a
	clc
	adc %b
	sta %res
	.mend

	@add {$70}, {($72),y}, {($74),y}
