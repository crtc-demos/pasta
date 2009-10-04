  ifz	r2, skip_not_zero_target
  push	r0	; dummy
  push	r1	; dummy
  push	r13
  push	r12
  push	r11
  lpra	r13:r12, not_zero_target_addr
  ldr	r11, [p6, #0]
  str	r11, [sp, #6]
  ldr	r11, [p6, #2]
  str	r11, [sp, #8]
  pop	r11
  pop	r12
  pop	r13
  ret
not_zero_target_addr:
  data	#lohalf(not_zero_target)
  data	#hihalf(not_zero_target)
skip_not_zero_target:

  mov r0, r1
not_zero_target:
