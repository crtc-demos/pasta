foo:
	.scope
	tax
	txs
foo2:
	lda #65
	bcc foo2
	.scend
	bne foo
	; should give an error
	beq foo2
