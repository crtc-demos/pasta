; Flimsy Forth-like macros.

.alias oswrch $ffee
.alias osbyte $fff4
.alias osfind $ffce
.alias osgbpb $ffd1
.alias osfile $ffdd

.org $e00

; 2-byte (static) arguments.
.alias arg0 $74
.alias arg1 $76
.alias arg2 $78
.alias arg3 $7a
.alias arg4 $7c
.alias arg5 $7e

; 4-byte result
.alias result $70

; 8 temporaries (caller-save).
.alias tmp0 $80
.alias tmp1 $82
.alias tmp2 $84
.alias tmp3 $86
.alias tmp4 $88
.alias tmp5 $8a
.alias tmp6 $8c
.alias tmp7 $8e

.alias xsp $48

entry:
	pha
	phx
	phy
	lda #$40
	sta xsp
	;jsr test
	;jsr test2
	jsr test3
	ply
	plx
	pla
	rts

; Simple word-oriented RPN operations on an X-indexed stack in zero page.
; Stack grows downwards, and ToS is "full". Generally X should be initialised
; before operations. A,Y can be clobbered.

.macro init_sp
	ldx xsp
.mend

.macro drop
	inx
	inx
.mend

.macro drop2
	inx
	inx
	inx
	inx
.mend

.macro addw
	lda $02,x
	clc
	adc $00,x
	sta $02,x
	lda $03,x
	adc $01,x
	sta $03,x
	@drop
.mend

; Add a word constant to ToS.
.macro addwc cst
	lda $00,x
	clc
	adc #<%cst
	sta $00,x
	lda $01,x
	adc #>%cst
	sta $01,x
.mend

; Add a byte constant to ToS.

.macro addwcb cst
	lda $00,x
	clc
	adc #%cst
	sta $00,x
	bcc _skip
	inc $01,x
_skip:
.mend

; Increment ToS.

.macro incw
	inc $00,x
	bne _skip
	inc $01,x
_skip:
.mend

; Add ToS to [Y:A]

.macro add_ay
	clc
	adc $00,x
	pha
	tya
	adc $01,x
	tay
	pla
.mend

; Subtract two words on stack, result to stack.

.macro subw
	lda $02,x
	sec
	sbc $00,x
	sta $02,x
	lda $03,x
	sbc $01,x
	sta $03,x
	@drop
.mend

; Subtract word-size argument from ToS.

.macro subwc cst
	lda $00,x
	sec
	sbc #<%cst
	sta $00,x
	lda $01,x
	sbc #>%cst
	sta $01,x
.mend

; Subtract byte-size argument from ToS.

.macro subwcb cst
	lda $00,x
	sec
	sbc #%cst
	sta $00,x
	bcs _skip
	dec $01,x
_skip:
.mend

; Decrement ToS.

.macro decw
	dec $00,x
	lda $00,x
	cmp #255
	bne _skip
	dec $01,x
_skip:
.mend

; Subtract ToS from [Y:A]

.macro sub_ay
	sec
	sbc $00,x
	pha
	tya
	sbc $01,x
	tay
	pla
.mend

; Logic ops

.macro andw
	lda $00,x
	and $02,x
	sta $02,x
	lda $01,x
	and $03,x
	sta $03,x
	@drop
.mend

.macro orw
	lda $00,x
	ora $02,x
	sta $02,x
	lda $01,x
	ora $03,x
	sta $03,x
	@drop
.mend

.macro eorw
	lda $00,x
	eor $02,x
	sta $02,x
	lda $01,x
	eor $03,x
	sta $03,x
	@drop
.mend

; Shifts

.macro aslw
	asl $00,x
	rol $01,x
.mend

.macro aslw_cst cst
	ldy #%cst
	lda $01,x
_loop:
	asl $00,x
	rol
	dey
	bne _loop
	sta $01,x
.mend

.macro lsrw
	lsr $01,x
	ror $00,x
.mend

.macro lsrw_cst cst
	ldy #%cst
	lda $00,x
_loop:
	lsr $01,x
	ror
	dey
	bne _loop
	sta $00,x
.mend

.macro asrw
	lda $01,x
	cmp #$80
	ror
	ror $00,x
	sta $01,x
.mend

.macro asrw_cst cst
	ldy #%cst
	lda $01,x
_loop:
	cmp #$80
	ror
	ror $00,x
	dey
	bne _loop
	sta $01,x
.mend

; multiply.

.macro mulw
	jsr mulw_fn
.mend

mulw_fn:
	lda $00,x
	sta arg0
	lda $01,x
	sta arg0 + 1
	lda $02,x
	sta arg1
	lda $03,x
	sta arg1 + 1
	jsr mult_16_16
	@drop
	lda result
	sta $00,x
	lda result + 1
	sta $01,x
	rts

	; 16-bit multiply. Corrupts tmp0, tmp1, tmp2. Args in arg0, arg1.
	; Result in first two bytes of 'result'.
mult_16_16:
	.scope
	stz result
	stz result + 1
	
	lda arg0 + 1
	eor arg1 + 1
	sta tmp2

	; negate inputs if they are positive
	lda arg1 + 1
	bpl _bpos
	lda #0
	sec
	sbc arg1
	sta arg1
	lda #0
	sbc arg1 + 1
	sta arg1 + 1
_bpos:

	lda arg0 + 1
	bpl _apos
	lda #0
	sec
	sbc arg0
	sta arg0
	lda #0
	sbc arg0 + 1
	sta arg0 + 1
_apos:
	
	lda arg1
	sta tmp0 + 1
	lda arg1 + 1
	sta tmp1
	
	.scope
	lda #1
	sta tmp0
_lowbits:
	lda arg0
	and tmp0
	beq _nextbit
	lda result
	clc
	adc tmp0 + 1
	sta result
	lda result + 1
	adc tmp1
	sta result + 1
_nextbit:
	asl tmp0 + 1
	rol tmp1

	asl tmp0
	bne _lowbits
	.scend

	.scope
	lda #1
	sta tmp0
_highbits:
	lda arg0 + 1
	and tmp0
	beq _nextbit
	lda result + 1
	clc
	adc tmp1
	sta result + 1
_nextbit:
	asl tmp1
	
	asl tmp0
	bne _highbits

	lda tmp2
	bpl _done
	.scend

	; negate result
	lda #0
	sec
	sbc result
	sta result
	lda #0
	sbc result + 1
	sta result + 1
_done:
	rts	
	.scend

; Pop ToS, store at an absolute address.

.macro pop_to_abs addr
	lda $00,x
	sta %addr
	lda $01,x
	sta %addr + 1
	@drop
.mend

; Push contents of absolute address onto stack.

.macro push_from_abs addr
	dex
	dex
	lda %addr
	sta $00,x
	lda %addr + 1
	sta $01,x
.mend

; Push constant (or address) onto stack.

.macro push_cst cst
	dex
	dex
	lda #<%cst
	sta $00,x
	lda #>%cst
	sta $01,x
.mend

; Pop to indirected zero-page location.

.macro pop_to_ind ind
	lda $00,x
	sta (%ind)
	lda $01,x
	ldy #1
	sta (%ind),y
	@drop
.mend

; Push from indirected zero-page location.

.macro push_from_ind zp
	dex
	dex
	lda (%zp)
	sta $00,x
	ldy #1
	lda (%zp),y
	sta $01,x
.mend

; Pop/push to [Y:A]

.macro pop_to_ay
	lda $00,x
	ldy $01,x
	@drop
.mend

.macro push_from_ay
	dex
	dex
	sta $00,x
	sty $01,x
.mend

; Duplicate ToS.

.macro dup
	dex
	dex
	lda $02,x
	sta $00,x
	lda $03,x
	sta $01,x
.mend

; Duplicate an item from ARG places down the stack (1-based)

.macro pick stackoffset
	dex
	dex
	lda %stackoffset*2,x
	sta $00,x
	lda %stackoffset*2+1,x
	sta $01,x
.mend

.macro swap
	ldy $00,x
	lda $02,x
	sta $00,x
	sty $02,x
	ldy $01,x
	lda $03,x
	sta $01,x
	sty $03,x
.mend

; Comparisons. Jump to argument if condition true.

.macro if_eq dest
	lda $00,x
	cmp $02,x
	bne skip
	lda $01,x
	cmp $03,x
	bne skip
	@drop2
	jmp %dest
skip:
	@drop2
.mend

.macro if_ne dest
	lda $00,x
	cmp $02,x
	bne go
	lda $01,x
	cmp $03,x
	beq skip
go:
	@drop2
	jmp %dest
skip:
	@drop2
.mend

; this is if $03,$02 is greater-or-equal than $01,00.
; true if...
;   $03 > $01   (or $01 < $03)
;   || ($03 = $01 && $02 >= $00)   (or ... && !($02 < $00))

.macro if_geu dest
	lda $01,x
	cmp $03,x
	bcc go
	bne skip
	lda $02,x
	cmp $00,x
	bcc skip
go:
	@drop2
	jmp %dest
skip:
	@drop2
.mend

.macro if_leu dest
	lda $03,x
	cmp $01,x
	bcc go
	bne skip
	lda $00,x
	cmp $02,x
	bcc skip
go:
	@drop2
	jmp %dest
skip:
	@drop2
.mend

; $03,$02 is greater-than $01,$00.
; true if...
;   $03 > $01   (or $01 < $03)
;   || ($03 = $01 && $02 > $00)   (or ... && !($02 >= $00))

.macro if_gtu dest
	lda $01,x
	cmp $03,x
	bcc go
	bne _skip
	lda $00,x
	cmp $02,x
	bcs skip
go:
	@drop2
	jmp %dest
skip:
	@drop2
.mend

.macro if_ltu dest
	lda $03,x
	cmp $01,x
	bcc go
	bne skip
	lda $02,x
	cmp $00,x
	bcs skip
go:
	@drop2
	jmp %dest
skip:
	@drop2
.mend

; $03,$02 greater-or-equal than $01,$00, signed.
; true if...
;   $03 >s $01  (or $01 <s $03)
;   || ($03 = $01 && $02 >=s $00)   (or ... && !($02 <s $00))

; stolen from the tubes.

.macro if_ge dest
	lda $02,x
	cmp $00,x
	lda $03,x
	sbc $01,x
	bvc skip
	eor #$80
skip:
	bmi skip2
	@drop2
	jmp %dest
skip2:
	@drop2
.mend

.macro if_le dest
	lda $00,x
	cmp $02,x
	lda $01,x
	sbc $03,x
	bvc _skip
	eor #$80
skip:
	bmi _skip2
	@drop2
	jmp %dest
skip2:
	@drop2
.mend

.macro if_lt dest
	lda $02,x
	cmp $00,x
	lda $03,x
	sbc $01,x
	bvc skip
	eor #$80
skip:
	bpl skip2
	@drop2
	jmp %dest
skip2:
	@drop2
.mend

.macro if_gt dest
	lda $00,x
	cmp $02,x
	lda $01,x
	sbc $03,x
	bvc skip
	eor #$80
skip:
	bpl skip2
	@drop2
	jmp %dest
skip2:
	@drop2
.mend

.macro pr_string
	jsr pr_string_fn
.mend

pr_string_fn:
	.scope
	lda $00,x
	sta tmp0
	lda $01,x
	sta tmp0+1
	ldy #0
_loop:
	lda (tmp0),y
	beq _done
	jsr oswrch
	iny
	jmp _loop
_done:
	inx
	inx
	rts
	.scend

.macro pr_hex
	jsr pr_hex_fn
.mend

	; print hex digit in A. A corrupted.
hex_digit:
	.scope
	cmp #10
	bcc _digit
	clc
	adc #'a' - 10
	bra done
_digit:
	clc
	adc #'0'
done:
	jmp oswrch
	.scend

pr_hex_fn:
	.scope
	lda $01,x
	lsr
	lsr
	lsr
	lsr
	jsr hex_digit
	lda $01,x
	and #$f
	jsr hex_digit
	lda $00,x
	lsr
	lsr
	lsr
	lsr
	jsr hex_digit
	lda $00,x
	and #$f
	jsr hex_digit
	inx
	inx
	rts
	.scend

.macro pr_newl
	jsr pr_newl_fn
.mend

pr_newl_fn:
	.scope
	lda #10
	jsr oswrch
	lda #13
	jsr oswrch
	rts
	.scend

.macro vdu
	jsr vdu_fn
.mend

vdu_fn:
	lda $00,x
	inx
	inx
	jmp oswrch

; ( addr byte -- )

.macro storebyte
	lda $02,x
	sta tmp0
	lda $03,x
	sta tmp0+1
	lda $00,x
	sta (tmp0)
	@drop2
.mend

; ( addr -- byte )

.macro loadbyte
	lda $00,x
	sta tmp0
	lda $01,x
	sta tmp0+1
	lda ($00)
	sta $00,x
	stz $01,x
.mend

; ( base 8_bit_offset -- val )

.macro byte_lut
	lda $02,x
	sta tmp0
	lda $03,x
	sta tmp0 + 1
	ldy $00,x
	@drop
	lda (tmp0),y
	sta $00,x
	stz $01,x
.mend

; test code!

hellow:
	.asc "Hello world",0

done_str:
	.asc "Done now!",0

test:
	@init_sp

	@push_cst hellow
	@pr_string
	@pr_newl

	@push_cst 500
	@pr_hex
	@pr_newl

	@push_cst 100
	@subwcb 10
	@pr_hex
	@pr_newl

	@push_cst 10
	@push_cst 10
	@push_cst 10
	@addw
	@addw
	@decw
	@pr_hex
	@pr_newl

	@push_cst -10
_outer:
	@push_cst -10
_loop:
	@dup
	@pick 3
	@addw
	@pr_hex
	@pr_newl

	@incw
	@dup
	@push_cst 10
	@if_lt _loop

	@drop

	@incw
	@dup
	@push_cst 10
	@if_lt _outer

	@push_cst done_str
	@pr_string
	@pr_newl

	rts

test2:
	.scope
	lda #22
	jsr oswrch
	lda #1
	jsr oswrch
	
	@push_cst $3000
_loop:
	@dup
	@push_cst 255
	@storebyte
	
	@incw
	@dup
	@push_cst $8000
	@if_ltu _loop

	rts
	.scend

setmode:
	pha
	lda #22
	jsr oswrch
	pla
	jmp oswrch

; ( y -- addr )

row_start:
	@lsrw_cst 3
	@push_cst 320
	@mulw
	@push_cst $5800
	@addw
	rts

powers_of_two:
	.byte 128, 64, 32, 16, 8, 4, 2, 1

; ( x y -- )

drawpixel:
	.scope
	@dup
	; ( x y y )
	jsr row_start
	; ( x y row_start )
	@swap
	; ( x row_start y )
	@push_cst 7
	@andw
	; ( x row_start y&7 )
	@addw
	; ( x row_start )
	@swap
	; ( row_start x )
	@dup
	; ( row_start x x )
	@push_cst $fff8
	@andw
	; ( row_start x x&~7 )
	@swap
	@push_cst 7
	@andw
	; ( row_start x&~7 x&7 )
	@push_cst powers_of_two
	@swap
	; ( row_start x&~7 po2_base x&7 )
	@byte_lut
	; ( row_start x&~7 pixval )
	@pop_to_abs tmp3
	@addw
	@pop_to_abs tmp4
	lda (tmp4)
	ora tmp3
	sta (tmp4)
	.scend
	rts

test3:
	.scope
	lda #4
	jsr setmode
	
	@init_sp
		
	@push_cst 35
_loop:
	@push_cst 35
_loop2:
	@dup
	@pick 3
	jsr drawpixel
	
	@incw

	@dup
	@push_cst [320-35]
	@if_ltu _loop2
	@drop
	
	@incw

	@dup
	@push_cst [256-35]
	@if_ltu _loop
	
	rts
	.scend
