	.macro load a
	lda %a
	.mend

	.macro load_imm imm
	@load {#%imm}
	.mend

	; We should pass down addrmode arguments without expanding them first.
	.macro load2 a
	@load %a
	.mend

	.macro load2_imm imm
	@load2 {#%imm}
	.mend

	.org $e00
	@load_imm 25
	@load2_imm 50
