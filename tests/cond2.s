	.org $e00

	.if 1
	lda #65
	.else
	lda #70
	.endif

	.ifdef BLAH
	lda #72
	.elif .defined(BAR)
	lda #76
	.endif
