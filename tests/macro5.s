	.macro add addr foo
	lda %addr
	clc
	adc #%foo
	sta %addr
	.mend
	
	.macro copyto dst lo hi
	lda %lo
	sta %dst
	lda %hi
	sta %dst + 1
	@add {(%dst)}, 5
	.mend
	
	@add {($e0),y}, 5
	@add {($f0)}, 6
	@copyto $70, {#<500}, {#>500}
