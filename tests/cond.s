	.org $e00

	.if 1
	lda #65
	.else
	lda #70
	.endif

	.ifdef FOO
	lda #65
	.elif 1
	lda #66
	.else
	lda #67
	.endif

	.ifdef FOO
	lda #65
	.elif 0
	lda #66
	.elif 1
	lda #67
	.else
	lda #68
	.endif
