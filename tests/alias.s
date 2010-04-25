	.alias	foo $ca
	.alias	bar $fe
	.alias  quux foo+bar

	lda #foo
	sta bar
	lda quux
