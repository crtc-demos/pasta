	.alias num $80000005

	.if num >=u $80000005
	lda #1
	.else
	lda #2
	.endif
