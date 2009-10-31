	.alias oswrch $ffee
	.alias osbyte $fff4
	.alias osfind $ffce
	.alias osgbpb $ffd1
	.alias osfile $ffdd

	; The "notemps" directive says that these functions are safe to call
	; from within a context. I.e., they use none of the automatically
	; allocated temporaries.
	.notemps oswrch, osbyte, osfind, osgbpb, osfile

	.org $e00

; 0x40-0x5f used for matrices, vector temps.

; 32 bytes.
	.alias m_mat $50

; 8 bytes.
	.alias vec_tmp $40

; 0x00-0x3f can be used as X-indexed stack.
	.alias xsp $48

; three 256-byte chunks, "byte-planed"
	.alias sqtab_0 $8000
	.alias sqtab_1 $8100
	.alias sqtab_2 $8200

	.alias logtab_addr $8300
	.alias exptab_addr $8b00

; two 64-byte halves for lo/hi, "byte-planed"
	.alias sintab_0 $9b00
	.alias sintab_1 $9b40

	; Declare ZP locations to use for automatically-allocated temporaries.
	.temps $70..$8f
	.temps $4a..$4f

entry:
	.(
	lda #2
	jsr setmode
	jsr setorigin
	jsr select_sram
	jsr load_sqtab
	jsr load_logtab
	jsr load_exptab
	jsr load_sintab
	jsr cls
	jsr test_render
	.(
rept:
	jsr test_line
	bra rept
	.)
	jsr select_old_lang
	rts
	.)

setmode:
	pha
	lda #22
	jsr oswrch
	pla
	jsr oswrch
	rts

centre:
	.byte 29
	.byte <640
	.byte >640
	.byte <512
	.byte >512

setorigin:
	.(
	ldx #0
loop:
	lda centre,x
	jsr oswrch
	inx
	cpx #5
	bne loop
	rts
	.)

	.context cls
cls:
	lda #12
	jmp oswrch
	.ctxend

old_lang:
	.byte 0

select_sram:
	.(
	lda $f4
	sta old_lang
	lda #4
	; must save to ram copy of ROMSEL first!
	sta $f4
	sta $fe30
	rts
	.)

select_old_lang:
	.(
	lda old_lang
	sta $f4
	sta $fe30
	rts
	.)

	; copy A * 256 bytes from $3000 to YX.
	; corrupts tmp1, tmp2

	.context copy_to_sram
	.var2 tmp1, tmp2

copy_to_sram:
	stx %tmp1
	sty %tmp1 + 1
	tax
	lda #<$3000
	sta %tmp2
	lda #>$3000
	sta %tmp2 + 1
	ldy #0
loop:
	lda (%tmp2),y
	sta (%tmp1),y
	iny
	bne loop
	inc %tmp2 + 1
	inc %tmp1 + 1
	dex
	bne loop
	rts
	.ctxend

osfile_blk:
	.dsb 18,0

	; load a file named YX to screen ram (temporary space) at $3000.
load_file:
	stx osfile_blk
	sty osfile_blk + 1
	stz osfile_blk + 6
	lda #<$3000
	sta osfile_blk + 2
	lda #>$3000
	sta osfile_blk + 3
	ldx #<osfile_blk
	ldy #>osfile_blk
	lda #$ff
	jmp osfile

load_sqtab:
	.(
	ldx #<sqtab_name
	ldy #>sqtab_name
	jsr load_file
	lda #>768
	ldx #<sqtab_0
	ldy #>sqtab_0
	jmp copy_to_sram
sqtab_name:
	.asc "sqtab",13
	.)

load_logtab:
	.(
	ldx #<logtab_name
	ldy #>logtab_name
	jsr load_file
	lda #>2048
	ldx #<logtab_addr
	ldy #>logtab_addr
	jmp copy_to_sram
logtab_name:
	.asc "logtab",13
	.)

load_exptab:
	.(
	ldx #<exptab_name
	ldy #>exptab_name
	jsr load_file
	lda #>4096
	ldx #<exptab_addr
	ldy #>exptab_addr
	jmp copy_to_sram
exptab_name:
	.asc "exptab",13
	.)

load_sintab:
	.(
	ldx #<sintab_name
	ldy #>sintab_name
	jsr load_file
	; actually only 128 bytes, so this copies too much.
	lda #1
	ldx #<sintab_0
	ldy #>sintab_0
	jmp copy_to_sram
sintab_name:
	.asc "sintab",13	
	.)

	; multiply ahi,alo by bhi,blo, result in result (3 bytes).
	; 'dumb' implementation.
	; corrupts A,X,Y.

	.context mult_16_16
	.var alo, ahi, blo, bhi
	.var result_neg
	.var3 result
	.var4 tmp

mult_16_16:
	stz %result
	stz %result + 1
	stz %result + 2
	
	lda %ahi
	eor %bhi
	sta %result_neg

	; negate inputs if they are positive
	.(
	lda %bhi
	bpl b_pos
	lda #0
	sec
	sbc %blo
	sta %blo
	lda #0
	sbc %bhi
	sta %bhi
b_pos:
	.)

	.(
	lda %ahi
	bpl a_pos
	lda #0
	sec
	sbc %alo
	sta %alo
	lda #0
	sbc %ahi
	sta %ahi
a_pos:
	.)
	
	lda %blo
	sta %tmp + 1
	lda %bhi
	sta %tmp + 2
	stz %tmp + 3
	
	lda #1
	sta %tmp
	.(
lowbits:
	lda %alo
	and %tmp
	beq nextbit
	lda %result
	clc
	adc %tmp + 1
	sta %result
	lda %result + 1
	adc %tmp + 2
	sta %result + 1
	lda %result + 2
	adc %tmp + 3
	sta %result + 2
nextbit:
	asl %tmp + 1
	rol %tmp + 2
	rol %tmp + 3

	asl %tmp
	bne lowbits
	.)

	lda #1
	sta %tmp
	.(
highbits:
	lda %ahi
	and %tmp
	beq nextbit
	lda %result + 1
	clc
	adc %tmp + 2
	sta %result + 1
	lda %result + 2
	adc %tmp + 3
	sta %result + 2
nextbit:
	asl %tmp + 2
	rol %tmp + 3
	
	asl %tmp
	bne highbits
	.)

	lda %result_neg
	bpl done

	; negate result
	lda #0
	sec
	sbc %result
	sta %result
	lda #0
	sbc %result + 1
	sta %result + 1
	lda #0
	sbc %result + 2
	sta %result + 2

done:
	rts
	.ctxend

	; multiply ahi,alo by bhi,blo. Latter must be -256..256.
	; Both values are signed. Result in 'result'.
	; corrupts alo,ahi,blo,bhi,tmp1

	.context mult_16_8
	.var alo, ahi, blo, bhi, tmp1
	.var3 result

mult_16_8:
	phy
	phx
	ldy #0
	bit %bhi
	.(
	bpl b_positive
	iny

	lda #0
	sec
	sbc %blo
	sta %blo
	lda #0
	sbc %bhi
	sta %bhi
b_positive:
	.)
	
	bit %ahi
	.(
	bpl a_positive
	iny

	lda #0
	sec
	sbc %alo
	sta %alo
	lda #0
	sbc %ahi
	sta %ahi
a_positive:
	.)

	sty %tmp1

	; Allow b=256 or b=-256 as special cases. (Condition a bit slack).
	lda %bhi
	cmp #1
	.(
	bne not_mult_256
	jmp mult_256
not_mult_256:
	.)
	
	ldx %alo
	ldy %blo
	lda sqtab_0, x
	clc
	adc sqtab_0, y
	sta %result
	lda sqtab_1, x
	adc sqtab_1, y
	sta %result + 1
	lda sqtab_2, x
	adc sqtab_2, y
	sta %result + 2
	; result[2-0] has alo^2 + b^2
	txa
	sec
	sbc %blo
	.(
	bcs alo_minus_b_positive
	eor #$ff
	inc
alo_minus_b_positive:
	.)
	tax
	; X has abs (alo - b).
	lda %result
	sec
	sbc sqtab_0, x
	sta %result
	lda %result + 1
	sbc sqtab_1, x
	sta %result + 1
	lda %result + 2
	sbc sqtab_2, x
	sta %result + 2
	; result[2-0] has alo^2 + b^2 - abs(alo-b)^2
	ldx %ahi
	ldy %blo
	; add (ahi^2) << 8
	lda %result + 1
	clc
	adc sqtab_0, x
	sta %result + 1
	lda %result + 2
	adc sqtab_1, x
	sta %result + 2
	; add (b^2) << 8
	lda %result + 1
	clc
	adc sqtab_0, y
	sta %result + 1
	lda %result + 2
	adc sqtab_1, y
	sta %result + 2
	; result[2-0] has (previous result) + (ahi^2 << 8) + (b^2 << 8)
	txa
	sec
	sbc %blo
	.(
	bcs ahi_minus_b_positive
	eor #$ff
	inc
ahi_minus_b_positive:
	.)
	tax
	; X has abs (ahi - b)
	lda %result + 1
	sec
	sbc sqtab_0, x
	sta %result + 1
	lda %result + 2
	sbc sqtab_1, x
	sta %result + 2
	; result[2-0] has (previous result) - abs(ahi-b)^2 << 8
	lsr %result + 2
	ror %result + 1
	ror %result

	; finally, should the answer be positive or negative?
	lsr %tmp1
	.(
	bcc done
	lda #0
	sec
	sbc %result
	sta %result
	lda #0
	sbc %result + 1
	sta %result + 1
	lda #0
	sbc %result + 2
	sta %result + 2
done:
	.)
	plx
	ply
	rts

mult_256:
	.(
	lsr %tmp1
	bcc m256_positive
	stz %result
	lda #0
	sec
	sbc %alo
	sta %result + 1
	lda #0
	sbc %ahi
	sta %result + 2
	plx
	ply
	rts
m256_positive:
	.)
	stz %result
	lda %alo
	sta %result + 1
	lda %ahi
	sta %result + 2
	plx
	ply
	rts
	.ctxend
	
	; Do scale * ahi,alo / bhi,blo. Result in 'result' (2 bytes). Inputs
	; corrupted.
	; A corrupted. X,Y preserved. "scale" is fixed at 32.
	
	.context scaled_div
	.var2 in_a, in_b, tmp1
	.var tmp2
	.var2 result
	
scaled_div:
	phy
	phx
	stz %tmp2
	bit %in_a + 1
	.(
	bpl apos
	lda #0
	sec
	sbc %in_a
	sta %in_a
	lda #0
	sbc %in_a + 1
	sta %in_a + 1
	inc %tmp2
apos:
	.)
	bit %in_b + 1
	.(
	bpl bpos
	lda #0
	sec
	sbc %in_b
	sta %in_b
	lda #0
	sbc %in_b + 1
	sta %in_b + 1
	inc %tmp2
bpos:
	.)
	lda %in_a + 1
	asl %in_a
	rol
	clc
	adc #>logtab_addr
	sta %in_a + 1
	lda (%in_a)
	sta %tmp1
	ldy #1
	lda (%in_a), y
	sta %tmp1 + 1
	lda %in_b + 1
	asl %in_b
	rol
	clc
	adc #>logtab_addr
	sta %in_b + 1
	lda %tmp1
	sec
	sbc (%in_b)
	sta %tmp1
	lda %tmp1 + 1
	sbc (%in_b), y
	; bias (+2048 bytes/1024 array elements)
	clc
	adc #8
	; exptab base
	clc
	adc #>exptab_addr
	sta %tmp1 + 1
	lsr %tmp2
	bcs result_negative
	lda (%tmp1)
	sta %result
	lda (%tmp1), y
	sta %result + 1
	plx
	ply
	rts
result_negative:
	lda #0
	sec
	sbc (%tmp1)
	sta %result
	lda #0
	sbc (%tmp1), y
	sta %result + 1
	plx
	ply
	rts
	.ctxend

	; Find the sin of the accumulator. 0 to 2*pi radians (full circle) are
	; represented as 0 to 255. Output is -256..+256, placed in (m_vec_p),y.
	; corrupts tmp1, A, X, Y.
	
	.context sin
	.var2 m_vec_p, tmp1
	
sin:
	phy
	pha
	and #$7f
	cmp #64
	bcc angle_0_to_halfpi
	bne angle_halfpi_to_pi
	; angle=64 is not stored in the table. Special case.
	ldx #<256
	ldy #>256
	jmp over_pi_check
angle_0_to_halfpi:
	tax
	jmp do_lookup
angle_halfpi_to_pi:
	eor #$7f
	tax
	inx
do_lookup:
	lda sintab_0,x
	ldy sintab_1,x
	tax
over_pi_check:
	; now low part of result in X, high part in Y
	pla
	bpl less_than_pi
	stx %tmp1
	sty %tmp1 + 1
	ply
	lda #0
	sec
	sbc %tmp1
	sta (%m_vec_p),y
	lda #0
	sbc %tmp1 + 1
	iny
	sta (%m_vec_p),y
	rts
less_than_pi:
	sty %tmp1
	txa
	ply
	sta (%m_vec_p),y
	iny
	lda %tmp1
	sta (%m_vec_p),y
	rts
	.ctxend

	.context cos
	; re-use the sin context for the arguments and return value for cos.
cos:
	clc
	adc #64
	jmp sin
	.ctxend

sin_result:
	.word 0

	.context sin_wave
	.var2 tmp2

	; test for sin function. Comically slow!
sin_wave:
	lda #<sin_result
	sta %sin.m_vec_p
	lda #>sin_result
	sta %sin.m_vec_p + 1
	stz %tmp2
	stz %tmp2 + 1
loop:
	lda %tmp2
	ldy #2
	jsr sin
	lda #25
	jsr oswrch
	lda #69
	jsr oswrch
	lda %tmp2
	sec
	sbc #<640
	php
	jsr oswrch
	plp
	lda %tmp2 + 1
	sbc #>640
	jsr oswrch
	ldy #2
	lda (%sin.m_vec_p),y
	jsr oswrch
	ldy #3
	lda (%sin.m_vec_p),y
	jsr oswrch
	
	inc %tmp2
	bne no_hi
	inc %tmp2 + 1
no_hi:
	
	lda %tmp2 + 1
	cmp #5
	bne loop
	rts
	.ctxend

rot_matrix:
	.dsb 32, 0

	; set up:
	; [  cos x  0  sin x   0 ]
	; [    0    1    0     0 ]
	; [ -sin x  0  cos x   0 ]
	; [    0    0    0     1 ]

	; Make rotation matrix about Y axis, from angle held in accumulator.
	; corrupts tmp1 (by calling sin/cos), A, X, Y.
	
	.context make_yrot_matrix
	
make_yrot_matrix:
	pha
	
	ldx #<rot_matrix
	stx %sin.m_vec_p
	ldx #>rot_matrix
	stx %sin.m_vec_p + 1
	
	; store "cos x"
	ldy #0
	jsr cos
	ldy #0
	lda (%sin.m_vec_p),y
	ldy #20
	sta (%sin.m_vec_p),y
	ldy #1
	lda (%sin.m_vec_p),y
	ldy #21
	sta (%sin.m_vec_p),y

	; store "sin x"
	pla
	ldy #16
	jsr sin
	; and "-sin x"
	ldy #16
	lda #0
	sec
	sbc (%sin.m_vec_p),y
	ldy #4
	sta (%sin.m_vec_p),y
	lda #0
	ldy #17
	sbc (%sin.m_vec_p),y
	ldy #5
	sta (%sin.m_vec_p),y

	; store ones (256s).
	lda #0
	ldy #10
	sta (%sin.m_vec_p),y
	ldy #30
	sta (%sin.m_vec_p),y
	lda #1
	ldy #11
	sta (%sin.m_vec_p),y
	ldy #31
	sta (%sin.m_vec_p),y

	; store zeros.
	ldy #2
	lda #0
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	ldy #6
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	iny
	; 8
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	ldy #12
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	iny
	; 14
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	ldy #18
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	ldy #22
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	iny
	; 24
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	iny
	; 26
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	iny
	; 28
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	iny
	
	rts
	.ctxend

	; set up:
	; [ 1    0      0    0 ]
	; [ 0  cos x  sin x  0 ]
	; [ 0 -sin x  cos x  0 ]
	; [ 0    0      0    1 ]

	; Make rotation matrix about Y axis, from angle held in accumulator.
	; corrupts tmp1 (by calling sin/cos), A, X, Y.
	
	.context make_xrot_matrix
	
make_xrot_matrix:
	pha
	
	ldx #<rot_matrix
	stx %sin.m_vec_p
	ldx #>rot_matrix
	stx %sin.m_vec_p + 1
	
	; store "cos x"
	ldy #10
	jsr cos
	ldy #10
	lda (%sin.m_vec_p),y
	ldy #20
	sta (%sin.m_vec_p),y
	ldy #11
	lda (%sin.m_vec_p),y
	ldy #21
	sta (%sin.m_vec_p),y

	; store "sin x"
	pla
	ldy #18
	jsr sin
	; and "-sin x"
	ldy #18
	lda #0
	sec
	sbc (%sin.m_vec_p),y
	ldy #12
	sta (%sin.m_vec_p),y
	lda #0
	ldy #19
	sbc (%sin.m_vec_p),y
	ldy #13
	sta (%sin.m_vec_p),y

	; store ones (256s).
	lda #0
	ldy #0
	sta (%sin.m_vec_p),y
	ldy #30
	sta (%sin.m_vec_p),y
	lda #1
	ldy #1
	sta (%sin.m_vec_p),y
	ldy #31
	sta (%sin.m_vec_p),y

	; store zeros.
	ldy #2
	lda #0
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	iny
	; 4
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	iny
	; 6
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	iny
	; 8
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	ldy #14
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	iny
	; 16
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	ldy #22
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	ldy #22
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	iny
	; 24
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	iny
	; 26
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	iny
	; 28
	sta (%sin.m_vec_p),y
	iny
	sta (%sin.m_vec_p),y
	iny
	
	rts
	.ctxend

points:
	.word -20, -20, -20, 1
	.word  20, -20, -20, 1
	.word -20,  20, -20, 1
	.word  20,  20, -20, 1
	.word -20, -20,  20, 1
	.word  20, -20,  20, 1
	.word -20,  20,  20, 1
	.word  20,  20,  20, 1

xpoints:
	.dsb 48, 0
xpoints_old:
	.dsb 48, 0
xpoints_tmp:
	.dsb 48, 0

	; for faces, define normal vector.
;faces:
;	.word  0,  0, -1, 1
;	.word  1,  0,  0, 1
;	.word  0,  0,  1, 1
;	.word -1,  0,  0, 1
;	.word  0, -1,  0, 1
;	.word  0,  1,  0, 1

;xfaces:
;	.dsb 36, 0

;     6__________7          __________    
;     /:        /|         /:	5    /|
;   2/________3/ |        /________ / |
;   |  :      |  |       |  :	 2 |  |
;   |  :      |  |       |3 :	   | 1|
;   | 4:______|__|5      |  :_0____|__| 
;   | /       | /        | /   4   | / 
;   |/________|/         |/________|/
;   0         1           	    

; pt0, pt1, pt2, visible.
corners:
	.byte 8*2, 8*0, 8*1, 0
	.byte 8*3, 8*1, 8*5, 0
	.byte 8*7, 8*5, 8*4, 0
	.byte 8*6, 8*4, 8*0, 0
	.byte 8*0, 8*4, 8*5, 0
	.byte 8*6, 8*2, 8*3, 0

	; for lines, define start point, end point, left face, right face.
lines:
	.byte 0, 1, 0, 4
	.byte 1, 5, 1, 4
	.byte 5, 4, 2, 4
	.byte 4, 0, 3, 4
	.byte 0, 2, 3, 0
	.byte 1, 3, 0, 1
	.byte 4, 6, 2, 3
	.byte 5, 7, 1, 2
	.byte 2, 3, 5, 0
	.byte 3, 7, 5, 1
	.byte 7, 6, 5, 2
	.byte 6, 2, 5, 3

; OpenGL perspective transformation P looks like:
; [ 2n/(r-l)     0       (r+l)/(r-l)      0      ]
; [    0      2n/(t-b)   (t+b)/(t-b)      0      ]
; [    0         0      -(f+n)/(f-n)  -2fn/(f-n) ]
; [    0         0           -1           0      ]
; plug in n = 1, f = 40, l = -20, r = 20, t = 16, b = -16
;
; Camera matrix C looks like:
; [ 1  0  0  vx ]
; [ 0  1  0  vy ]
; [ 0  0  1  vz ]
; [ 0  0  0  1  ]
; plug in e.g. vx = 0, vy = 0, vz = 20
;
; Screen matrix S looks like: (are z/w sensible?)
; [ 80  0  0  0 ]
; [  0 128 0  0 ]
; [  0  0  1  0 ]
; [  0  0  0  1 ]
;
; We want to transform camera, then perspective, then screen. This is:
; S . P . C
; or, (multiplied by 256 for fixed-point)
; [ 5120   0     0    0   ]
; [   0  10240   0    0   ]
; [   0    0   -262 -7842 ]
; [   0    0   -256 -5120 ]

; (This is written transposed.)
transformation:
        .word 768, 0, 0, 0
        .word 0, 1536, 0, 0
        .word 0, 0, -475, -256
        .word 0, 0, -5851, 20480

; Multiply a 4x4 matrix M (16-bit elements) by a column vector V
; (16 bit elements, but max. -256..256). Result loses 8 least significant bits.
; m_mat is (fixed) zero-page location, m_vec_p is pointer to vector,
; m_result_p is pointer to result.
; [ $50 $58 $60 $68 ] ( ($4c),0 )
; [ $52 $5a $62 $6a ] ( ($4c),2 )
; [ $54 $5c $64 $6c ] ( ($4c),4 )
; [ $56 $5e $66 $6e ] ( ($4c),6 )

	.context matrix_mult
	.var2 m_vec_p, m_result_p

matrix_mult:
	phy
	phx
	ldx #0
row:
	lda m_mat,x
	sta %mult_16_8.alo
	lda m_mat+1,x
	sta %mult_16_8.ahi
	lda (%m_vec_p)
	sta %mult_16_8.blo
	ldy #1
	lda (%m_vec_p),y
	sta %mult_16_8.bhi
	jsr mult_16_8
	txa
	tay
	lda %mult_16_8.result+1
	sta (%m_result_p),y
	lda %mult_16_8.result+2
	iny
	sta (%m_result_p),y
	
	lda m_mat+8,x
	sta %mult_16_8.alo
	lda m_mat+9,x
	sta %mult_16_8.ahi
	ldy #2
	lda (%m_vec_p),y
	sta %mult_16_8.blo
	iny
	lda (%m_vec_p),y
	sta %mult_16_8.bhi
	jsr mult_16_8
	txa
	tay
	lda (%m_result_p),y
	clc
	adc %mult_16_8.result+1
	sta (%m_result_p),y
	iny
	lda (%m_result_p),y
	adc %mult_16_8.result+2
	sta (%m_result_p),y
	
	lda m_mat+16,x
	sta %mult_16_8.alo
	lda m_mat+17,x
	sta %mult_16_8.ahi
	ldy #4
	lda (%m_vec_p),y
	sta %mult_16_8.blo
	iny
	lda (%m_vec_p),y
	sta %mult_16_8.bhi
	jsr mult_16_8
	txa
	tay
	lda (%m_result_p),y
	clc
	adc %mult_16_8.result+1
	sta (%m_result_p),y
	iny
	lda (%m_result_p),y
	adc %mult_16_8.result+2
	sta (%m_result_p),y
	
	lda m_mat+24,x
	sta %mult_16_8.alo
	lda m_mat+25,x
	sta %mult_16_8.ahi
	ldy #6
	lda (%m_vec_p),y
	sta %mult_16_8.blo
	iny
	lda (%m_vec_p),y
	sta %mult_16_8.bhi
	jsr mult_16_8
	txa
	tay
	lda (%m_result_p),y
	clc
	adc %mult_16_8.result+1
	sta (%m_result_p),y
	iny
	lda (%m_result_p),y
	adc %mult_16_8.result+2
	sta (%m_result_p),y
	
	inx
	inx
	
	cpx #8
	beq finished
	jmp row
finished:
	plx
	ply
	rts
	.ctxend

; exactly the same, but with a/b swapped for multiplication.
	.context matrix_mult_pre
	.var2 m_vec_p, m_result_p

matrix_mult_pre:
	phy
	phx
	ldx #0
row:
	lda m_mat,x
	sta %mult_16_8.blo
	lda m_mat+1,x
	sta %mult_16_8.bhi
	lda (%m_vec_p)
	sta %mult_16_8.alo
	ldy #1
	lda (%m_vec_p),y
	sta %mult_16_8.ahi
	jsr mult_16_8
	txa
	tay
	lda %mult_16_8.result+1
	sta (%m_result_p),y
	lda %mult_16_8.result+2
	iny
	sta (%m_result_p),y
	
	lda m_mat+8,x
	sta %mult_16_8.blo
	lda m_mat+9,x
	sta %mult_16_8.bhi
	ldy #2
	lda (%m_vec_p),y
	sta %mult_16_8.alo
	iny
	lda (%m_vec_p),y
	sta %mult_16_8.ahi
	jsr mult_16_8
	txa
	tay
	lda (%m_result_p),y
	clc
	adc %mult_16_8.result+1
	sta (%m_result_p),y
	iny
	lda (%m_result_p),y
	adc %mult_16_8.result+2
	sta (%m_result_p),y
	
	lda m_mat+16,x
	sta %mult_16_8.blo
	lda m_mat+17,x
	sta %mult_16_8.bhi
	ldy #4
	lda (%m_vec_p),y
	sta %mult_16_8.alo
	iny
	lda (%m_vec_p),y
	sta %mult_16_8.ahi
	jsr mult_16_8
	txa
	tay
	lda (%m_result_p),y
	clc
	adc %mult_16_8.result+1
	sta (%m_result_p),y
	iny
	lda (%m_result_p),y
	adc %mult_16_8.result+2
	sta (%m_result_p),y
	
	lda m_mat+24,x
	sta %mult_16_8.blo
	lda m_mat+25,x
	sta %mult_16_8.bhi
	ldy #6
	lda (%m_vec_p),y
	sta %mult_16_8.alo
	iny
	lda (%m_vec_p),y
	sta %mult_16_8.ahi
	jsr mult_16_8
	txa
	tay
	lda (%m_result_p),y
	clc
	adc %mult_16_8.result+1
	sta (%m_result_p),y
	iny
	lda (%m_result_p),y
	adc %mult_16_8.result+2
	sta (%m_result_p),y
	
	inx
	inx
	
	cpx #8
	beq finished
	jmp row
finished:
	plx
	ply
	rts
	.ctxend

	; copy matrix at (M_VEC_P) to M_MAT (the global transformation matrix).

	.context copy_matrix
	.var2 m_vec_p

copy_matrix:
	ldy #0
loop:
	lda (%m_vec_p),y
	sta m_mat,y
	iny
	cpy #32
	bne loop
	rts
	.ctxend

	; transpose matrix at (arg1). Result in (arg0).
	; corrupts A, X, Y
	
	.context transpose_matrix
	.var2 arg0, arg1
	.var tmp0, tmp1
	
transpose_matrix:
	ldy #0
	stz %tmp0
loop:
	; load lo byte
	lda (%arg1),y
	tax
	iny
	sty %tmp1
	; load hi byte
	lda (%arg1),y
	ldy %tmp0
	iny
	; store hi byte
	sta (%arg0),y
	txa
	dey
	; store lo byte
	sta (%arg0),y
	
	tya
	clc
	adc #8
	cmp #31
	bcc still_doing_column
	; if we get to the end of a column, we want to go back to the start
	; of the next column. Subtract 30.
	sec
	sbc #30
still_doing_column:
	sta %tmp0
	
	lda %tmp1
	iny
	cpy #32
	bne loop
	rts
	.ctxend

tmp_matrix:
	.dsb 32, 0

	; do TMP_MATRIX = M_MAT x ROT_MATRIX.

	.context postmultiply_matrix

postmultiply_matrix:
	lda #<rot_matrix
	sta %matrix_mult.m_vec_p
	lda #>rot_matrix
	sta %matrix_mult.m_vec_p + 1
	
	lda #<tmp_matrix
	sta %matrix_mult.m_result_p
	lda #>tmp_matrix
	sta %matrix_mult.m_result_p + 1
	
	jsr matrix_mult

	lda #<[rot_matrix + 8]
	sta %matrix_mult.m_vec_p
	lda #>[rot_matrix + 8]
	sta %matrix_mult.m_vec_p + 1
	
	lda #<[tmp_matrix + 8]
	sta %matrix_mult.m_result_p
	lda #>[tmp_matrix + 8]
	sta %matrix_mult.m_result_p + 1
	
	jsr matrix_mult

	lda #<[rot_matrix + 16]
	sta %matrix_mult.m_vec_p
	lda #>[rot_matrix + 16]
	sta %matrix_mult.m_vec_p + 1
	
	lda #<[tmp_matrix + 16]
	sta %matrix_mult.m_result_p
	lda #>[tmp_matrix + 16]
	sta %matrix_mult.m_result_p + 1
	
	jsr matrix_mult

	lda #<[rot_matrix + 24]
	sta %matrix_mult.m_vec_p
	lda #>[rot_matrix + 24]
	sta %matrix_mult.m_vec_p + 1
	
	lda #<[tmp_matrix + 24] 
	sta %matrix_mult.m_result_p
	lda #>[tmp_matrix + 24]
	sta %matrix_mult.m_result_p + 1
	
	jsr matrix_mult

	rts
	.ctxend

	.context transform_points

transform_points:
	; m_vec_p points to first point.
	lda #<points
	sta %matrix_mult.m_vec_p
	lda #>points
	sta %matrix_mult.m_vec_p+1

	lda #<vec_tmp
	sta %matrix_mult.m_result_p
	lda #>vec_tmp
	sta %matrix_mult.m_result_p+1

	; these stay live throughout the function. Make sure that they don't
	; get clobbered by calls to other contexts.

	.protect %matrix_mult.m_vec_p
	.protect %matrix_mult.m_result_p

	ldx #0
iter:
	jsr matrix_mult
	
	; dehomogenise X
	lda (%matrix_mult.m_result_p)
	sta %scaled_div.in_a
	ldy #1
	lda (%matrix_mult.m_result_p),y
	sta %scaled_div.in_a + 1
	ldy #6
	lda (%matrix_mult.m_result_p),y
	sta %scaled_div.in_b
	iny
	lda (%matrix_mult.m_result_p),y
	sta %scaled_div.in_b + 1
	jsr scaled_div
	lda %scaled_div.result
	sta xpoints,x
	inx
	lda %scaled_div.result+1
	sta xpoints,x
	inx
	
	; dehomogenise Y
	ldy #2
	lda (%matrix_mult.m_result_p),y
	sta %scaled_div.in_a
	iny
	lda (%matrix_mult.m_result_p),y
	sta %scaled_div.in_a + 1
	ldy #6
	lda (%matrix_mult.m_result_p),y
	sta %scaled_div.in_b
	iny
	lda (%matrix_mult.m_result_p),y
	sta %scaled_div.in_b + 1
	jsr scaled_div
	lda %scaled_div.result
	sta xpoints,x
	inx
	lda %scaled_div.result+1
	sta xpoints,x
	inx
	
	; store Z too?
	inx
	inx
	
	; move to next input point
	lda %matrix_mult.m_vec_p
	clc
	adc #8
	sta %matrix_mult.m_vec_p
	.(
	bcc nohi
	inc %matrix_mult.m_vec_p+1
nohi:
	.)
	
	cpx #48
	bcc iter
	
	rts
	.ctxend

	.context plot_xpoint_y

	; emit coordinates (x*8, y*4) for transformed point at Y-register
	; index into xpoints. A corrupted, X, Y preserved.
plot_xpoint_y:
	pha
	lda #25
	jsr oswrch
	pla
	jsr oswrch
	
	; output x
	lda xpoints,y
	jsr oswrch
	lda xpoints+1,y
	jsr oswrch

	; output y
	lda xpoints+2,y
	jsr oswrch
	lda xpoints+3,y
	jsr oswrch
	
	rts
	.ctxend

rotation_amount:
	.byte 0

	.context test_render
	.var first

test_render:
	lda #0
	sta %first

test_render_loop:
	lda #<transformation
	sta %copy_matrix.m_vec_p
	lda #>transformation
	sta %copy_matrix.m_vec_p + 1
	jsr copy_matrix

	lda rotation_amount
	jsr make_yrot_matrix
	jsr postmultiply_matrix
	
	; copy temp matrix back to m_mat.
	lda #<tmp_matrix
	sta %copy_matrix.m_vec_p
	lda #>tmp_matrix
	sta %copy_matrix.m_vec_p + 1
	jsr copy_matrix
	
	lda rotation_amount
	asl
	jsr make_xrot_matrix
	jsr postmultiply_matrix
	
	lda #<tmp_matrix
	sta %copy_matrix.m_vec_p
	lda #>tmp_matrix
	sta %copy_matrix.m_vec_p + 1
	jsr copy_matrix
	
	.(
	ldx #0
loop:
	lda xpoints,x
	sta xpoints_old,x
	inx
	cpx #48
	bne loop
	.)
	
	jsr transform_points
	; draw new object
	jsr draw_object

	jsr visibility

	lda %first
	beq is_first

	.(
	ldx #0
loop:
	lda xpoints, x
	sta xpoints_tmp, x
	lda xpoints_old, x
	sta xpoints, x
	inx
	cpx #48
	bne loop
	.)

	; undraw old object
	jsr draw_object

	.(
	ldx #0
loop:
	lda xpoints_tmp, x
	sta xpoints, x
	inx
	cpx #48
	bne loop
	.)

is_first:
	lda #1
	sta %first

	inc rotation_amount
	jmp test_render_loop
	
	rts
	.ctxend

	.context draw_object
	.var r_tmp1

draw_object:
	ldx #0
loop:
	lda lines,x
	; multiply by 6
	asl
	sta %r_tmp1
	asl
	clc
	adc %r_tmp1
	tay
	
	lda xpoints,y
	clc
	adc #80
	sta %v_line.x_start
	lda xpoints+2,y
	clc
	adc #128
	sta %v_line.y_start
	
	lda lines+1,x
	asl
	sta %r_tmp1
	asl
	clc
	adc %r_tmp1
	tay
	
	lda xpoints,y
	clc
	adc #80
	sta %v_line.x_end
	lda xpoints+2,y
	clc
	adc #128
	sta %v_line.y_end
	
	phx
	jsr v_line
	plx
	
	txa
	clc
	adc #4
	tax
	
	cmp #48
	bne loop
	rts
	.ctxend

	.context v_line

	.var y_start, y_end
	.var x_start, x_end
	.var2 xpos, xdelta
	.var2 tmp1, tmp2
	
v_line:
	.(
	lda %y_end
	cmp %y_start
	bcs right_way_up
	
	; flip Y
	lda %y_start
	ldx %y_end
	sta %y_end
	stx %y_start
	
	; flip X
	lda %x_start
	ldx %x_end
	sta %x_end
	stx %x_start
	
right_way_up:
	.)

	lda %x_end
	sec
	sbc %x_start
	pha
	lda #0
	sbc #0
	sta %scaled_div.in_a + 1
	pla
	asl
	rol %scaled_div.in_a + 1
	asl
	rol %scaled_div.in_a + 1
	asl
	rol %scaled_div.in_a + 1
	sta %scaled_div.in_a
	; now scaled_div.in_a is (x_end - x_start) * 8.
	
	lda %y_end
	sec
	sbc %y_start
	sta %scaled_div.in_b
	stz %scaled_div.in_b + 1
	
	jsr scaled_div
	
	lda %scaled_div.result
	sta %xdelta
	lda %scaled_div.result + 1
	sta %xdelta + 1
	
	lda %x_start
	sta %xpos + 1
	stz %xpos
	
	; now xpos, xdelta should be set correctly.
	
	lda %y_start
	and #255-7
	stz %tmp1

	; multiply by 64 to [A:%tmp1]
	lsr
	ror %tmp1
	lsr
	ror %tmp1
	
	; store to tmp2 (ypos & ~7) * 64
	sta %tmp2 + 1
	ldx %tmp1
	stx %tmp2
	
	; tmp1 is (ypos & ~7) * 16
	lsr
	ror %tmp1
	lsr
	ror %tmp1
	sta %tmp1 + 1

	; tmp2 = (ypos & ~7) * 80 + screen start
	lda %tmp1
	clc
	adc %tmp2
	sta %tmp2
	lda %tmp1 + 1
	adc %tmp2 + 1
	clc
	adc #$30
	sta %tmp2 + 1
	
	; row offset to Y
	lda %y_start
	and #7
	tay
	
loop:
	lda %xpos + 1
	stz %tmp1 + 1
	and #0xfe
	asl
	rol %tmp1 + 1
	asl
	rol %tmp1 + 1
	
	clc
	adc %tmp2
	sta %tmp1
	lda %tmp2 + 1
	adc %tmp1 + 1
	sta %tmp1 + 1
	
	lda %xpos + 1
	and #1
	tax
	lda pixmask, x
	eor (%tmp1), y
	sta (%tmp1), y
	
	lda %xpos
	clc
	adc %xdelta
	sta %xpos
	lda %xpos + 1
	adc %xdelta + 1
	sta %xpos + 1
	
	ldx %y_start
	inx
	cpx %y_end
	bcs done
	stx %y_start
	
	iny
	cpy #8
	bne loop
	
	ldy #0
	lda %tmp2
	clc
	adc #<640
	sta %tmp2
	lda %tmp2 + 1
	adc #>640
	sta %tmp2 + 1
	jmp loop
done:
	rts
	.ctxend

pixmask:
	.byte 0b00101010
	.byte 0b00010101

	.context test_line
test_line:
	lda #50
	sta %v_line.y_start
	lda #100
	sta %v_line.y_end
	lda #30
	sta %v_line.x_start
	lda #60
	sta %v_line.x_end
	
	jsr v_line

	rts
	.ctxend

	.context visibility
	.var2 tmp
	.var corner0, corner1, corner2

visibility:
	ldx #0
do_corner:
	; [ahi:alo] = x2 - x0
	ldy corners + 2, x
	lda xpoints, y
	sty %corner2
	ldy corners, x
	sty %corner0
	sec
	sbc xpoints, y
	sta %mult_16_8.alo
	ldy %corner2
	lda xpoints + 1, y
	ldy %corner0
	sbc xpoints + 1, y
	sta %mult_16_8.ahi
	
	; [bhi:blo] = y1 - y0
	ldy corners + 1, x
	lda xpoints + 2, y
	sty %corner1
	ldy %corner0
	sec
	sbc xpoints + 2, y
	sta %mult_16_8.blo
	ldy %corner1
	lda xpoints + 3, y
	ldy %corner0
	sbc xpoints + 3, y
	sta %mult_16_8.bhi
	
	jsr mult_16_8
	
	lda %mult_16_8.result
	sta %tmp
	lda %mult_16_8.result + 1
	sta %tmp + 1
	
	; [ahi:alo] = y2 - y0
	ldy %corner2
	lda xpoints + 2, y
	ldy %corner0
	sec
	sbc xpoints + 2, y
	sta %mult_16_8.alo
	ldy %corner2
	lda xpoints + 3, y
	ldy %corner0
	sbc xpoints + 3, y
	sta %mult_16_8.ahi
	
	; [bhi:blo] = x1 - x0
	ldy %corner1
	lda xpoints, y
	ldy %corner0
	sec
	sbc xpoints, y
	sta %mult_16_8.blo
	ldy %corner1
	lda xpoints + 1, y
	ldy %corner0
	sbc xpoints + 1, y
	sta %mult_16_8.bhi
	
	jsr mult_16_8
	
	; compare tmp, result signed less-than-or-equal
	.(
	lda %tmp
	cmp %mult_16_8.result
	lda %tmp + 1
	sbc %mult_16_8.result + 1
	bvc skip
	eor #$80
skip:
	bmi less
	stz corners + 3, x
	bra done
less:
	lda #1
	sta corners + 3, x
done:
	.)
	
	txa
	clc
	adc #4
	tax
	
	cpx #24
	.(
	beq done
	jmp do_corner
done:
	.)
	
	lda #30
	jsr oswrch
	
	ldx #0
print:
	lda corners + 3, x
	clc
	adc #'0'
	jsr oswrch
	txa
	clc
	adc #4
	tax
	cpx #24
	bne print
	
	rts
	.ctxend
