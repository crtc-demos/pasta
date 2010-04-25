	.org $e00
	bcc long
	bra a_label
	.dsb 130, 0
a_label
	txa
long:
	rts
