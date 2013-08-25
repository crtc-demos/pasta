	.macro load a
	lda %a
	.mend

	.macro load_imm imm
	@load {#%imm}
	.mend

	; If a variable is used in any way other than writing it by itself,
	; addressing mode arguments cannot be used.
	.macro load2 a
	@load %a+1
	.mend

	.macro load2_imm imm
	@load2 {#%imm}
	.mend

	.org $e00
	@load_imm 25
	@load2_imm 50
