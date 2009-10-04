foo:
  lda #65
  dex
  bne foo
bar:
  lda something
  sta something+1
  lda #<3000+1
  ldx #>3000+1
  iny
  tsx
  tya
  beq bar
  rts

; a macro!
.macro moo
  lda #65
  jsr 65530
.mend

something:
  .byte 5
  .byte 0
  .byte -15
