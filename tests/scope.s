foo:
	.scope
	tax
	txs
foo:
	lda #65
	bcc foo
	.scend
	bne foo
