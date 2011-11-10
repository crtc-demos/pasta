	.macro roll target
	rol %target
	.mend

	.org $e00

	@roll {a}
	@roll $70
	@roll b
	; implicit accumulator addressing also works!
	@roll {}

b:
	.byte 0

