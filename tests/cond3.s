	.ifdef HELLO
	lda #1
	.elif .defined(HELLO2)
	lda #2
	.else
	lda #4
	.endif

	.alias HELLO 15

HELLO:
foo:
	.ifdef foo
	lda #32
	.endif
