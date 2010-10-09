.alias oswrch $ffee

.macro vdu c
	lda #%c
	jsr oswrch
.mend

.macro mode n
	@vdu 22
	@vdu %n
.mend

.macro add res a b
	lda %a
	clc
	adc %b
	sta %res
	lda %a + 1
	adc %b + 1
	sta %res + 1
.mend

.macro add_b_twice res a b
	@add %res, %a, %b
	@add %res, %res, %b
.mend

.temps $70..$8f

.org $e00

entry:
.(

	; Works:
	@vdu 22
	@vdu 4

	; Barfs with: Fatal error: exception Insn.UnknownVariable("n")
	@mode 7

	; Works:
	@add $80, $82, $83

	; Barfs with: Fatal error: exception Insn.UnknownVariable("res")
	@add_b_twice $80, $82, $83

	rts

.)

