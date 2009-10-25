	.org $e00
	.temps $70..$8f

	.context foo
	.var x1, y1, z1
foo:
	rts
	.ctxend

	.context bar
	.var x2, y2, z2
bar:
	rts
	.ctxend

	.context main
	.var x3, y2, z3
main:
	jsr foo
	jsr bar
	rts
	.ctxend
