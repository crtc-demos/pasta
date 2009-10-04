(* 6502 instruction encodings.  *)

type addrmode = Accumulator
              | Absolute
              | Absolute_X
              | Absolute_Y
              | Immediate
              | Indirect
              | X_Indirect
              | Indirect_Y
              | Relative
              | Zeropage
              | Zeropage_X
              | Zeropage_Y
              | Implied
	      | Synth_lbra

type raw_addrmode = Raw_immediate of Expr.const_expr
                  | Raw_num of Expr.const_expr
		  | Raw_num_x of Expr.const_expr
		  | Raw_num_y of Expr.const_expr
		  | Raw_x_indirect of Expr.const_expr
		  | Raw_indirect_y of Expr.const_expr
		  | Raw_accumulator
		  | Raw_implied

type opcode = Adc | And | Asl | Bcc | Bcs | Beq | Bit | Bmi
            | Bne | Bpl | Brk | Bvc | Bvs | Clc | Cld | Cli
            | Clv | Cmp | Cpx | Cpy | Dec | Dex | Dey | Eor
            | Inc | Inx | Iny | Jmp | Jsr | Lda | Ldx | Ldy
            | Lsr | Nop | Ora | Pha | Php | Pla | Plp | Rol
            | Ror | Rti | Rts | Sbc | Sec | Sed | Sei | Sta
            | Stx | Sty | Tax | Tay | Txa | Tsx | Txs | Tya

type cycle = Fixed of int
           | Page_Plus of int
           | Branch_Plus of int

let insns =
  [Adc, Immediate,   0x69, 2, Fixed 2;
   Adc, Zeropage,    0x65, 2, Fixed 3;
   Adc, Zeropage_X,  0x75, 2, Fixed 4;
   Adc, Absolute,    0x6d, 3, Fixed 4;
   Adc, Absolute_X,  0x7d, 3, Page_Plus 4;
   Adc, Absolute_Y,  0x79, 3, Page_Plus 4;
   Adc, X_Indirect,  0x61, 2, Fixed 6;
   Adc, Indirect_Y,  0x71, 2, Page_Plus 5;
   
   And, Immediate,   0x29, 2, Fixed 2;
   And, Zeropage,    0x25, 2, Fixed 3;
   And, Zeropage_X,  0x35, 2, Fixed 4;
   And, Absolute,    0x2d, 3, Fixed 4;
   And, Absolute_X,  0x3d, 3, Page_Plus 4;
   And, Absolute_Y,  0x39, 3, Page_Plus 4;
   And, X_Indirect,  0x21, 2, Fixed 6;
   And, Indirect_Y,  0x31, 2, Page_Plus 5;
   
   Asl, Accumulator, 0x0a, 1, Fixed 2;
   Asl, Zeropage,    0x06, 2, Fixed 5;
   Asl, Zeropage_X,  0x16, 2, Fixed 6;
   Asl, Absolute,    0x0e, 3, Fixed 6;
   Asl, Absolute_X,  0x1e, 3, Fixed 7;
   
   Bcc, Relative,    0x90, 2, Branch_Plus 2;
   Bcs, Relative,    0xb0, 2, Branch_Plus 2;
   Beq, Relative,    0xf0, 2, Branch_Plus 2;
   Bmi, Relative,    0x30, 2, Branch_Plus 2;
   Bne, Relative,    0xd0, 2, Branch_Plus 2;
   Bpl, Relative,    0x10, 2, Branch_Plus 2;
   Bvc, Relative,    0x50, 2, Branch_Plus 2;
   Bvs, Relative,    0x70, 2, Branch_Plus 2;

   (* Synthetic long branch instructions. Not sure if these are a good idea. *)
   Bcc, Synth_lbra,  0x90, 5, Branch_Plus 2;
   Bcs, Synth_lbra,  0xb0, 5, Branch_Plus 2;
   Beq, Synth_lbra,  0xf0, 5, Branch_Plus 2;
   Bmi, Synth_lbra,  0x30, 5, Branch_Plus 2;
   Bne, Synth_lbra,  0xd0, 5, Branch_Plus 2;
   Bpl, Synth_lbra,  0x10, 5, Branch_Plus 2;
   Bvc, Synth_lbra,  0x50, 5, Branch_Plus 2;
   Bvs, Synth_lbra,  0x70, 5, Branch_Plus 2;

   Bit, Zeropage,    0x24, 2, Fixed 3;
   Bit, Absolute,    0x2c, 3, Fixed 4;
   
   Brk, Implied,     0x00, 1, Fixed 7;
   
   Clc, Implied,     0x18, 1, Fixed 2;
   Cld, Implied,     0xd8, 1, Fixed 2;
   Cli, Implied,     0x58, 1, Fixed 2;
   Clv, Implied,     0xb8, 1, Fixed 2;

   Cmp, Immediate,   0xc9, 2, Fixed 2;
   Cmp, Zeropage,    0xc5, 2, Fixed 3;
   Cmp, Zeropage_X,  0xd5, 2, Fixed 4;
   Cmp, Absolute,    0xcd, 3, Fixed 4;
   Cmp, Absolute_X,  0xdd, 3, Page_Plus 4;
   Cmp, Absolute_Y,  0xd9, 3, Page_Plus 4;
   Cmp, X_Indirect,  0xc1, 2, Fixed 6;
   Cmp, Indirect_Y,  0xd1, 2, Page_Plus 5;
   
   Cpx, Immediate,   0xe0, 2, Fixed 2;
   Cpx, Zeropage,    0xe4, 2, Fixed 3;
   Cpx, Absolute,    0xec, 3, Fixed 4;
   
   Cpy, Immediate,   0xc0, 2, Fixed 2;
   Cpy, Zeropage,    0xc4, 2, Fixed 3;
   Cpy, Absolute,    0xcc, 3, Fixed 4;
   
   Dec, Zeropage,    0xc6, 2, Fixed 5;
   Dec, Zeropage_X,  0xd6, 2, Fixed 6;
   Dec, Absolute,    0xce, 3, Fixed 3;
   Dec, Absolute_X,  0xde, 3, Fixed 7;
   
   Dex, Implied,     0xca, 1, Fixed 2;
   Dey, Implied,     0x88, 1, Fixed 2;

   Eor, Immediate,   0x49, 2, Fixed 2;
   Eor, Zeropage,    0x45, 2, Fixed 3;
   Eor, Zeropage_X,  0x55, 2, Fixed 4;
   Eor, Absolute,    0x4d, 3, Fixed 4;
   Eor, Absolute_X,  0x5d, 3, Page_Plus 4;
   Eor, Absolute_Y,  0x59, 3, Page_Plus 4;
   Eor, X_Indirect,  0x41, 2, Fixed 6;
   Eor, Indirect_Y,  0x51, 2, Page_Plus 5;

   Inc, Zeropage,    0xe6, 2, Fixed 5;
   Inc, Zeropage_X,  0xf6, 2, Fixed 6;
   Inc, Absolute,    0xee, 3, Fixed 6;
   Inc, Absolute_X,  0xfe, 3, Fixed 7;
   
   Inx, Implied,     0xe8, 1, Fixed 2;
   Iny, Implied,     0xc8, 1, Fixed 2;
   
   Jmp, Absolute,    0x4c, 3, Fixed 3;
   Jmp, Indirect,    0x6c, 3, Fixed 5;
   
   Jsr, Absolute,    0x20, 3, Fixed 6;
   
   Lda, Immediate,   0xa9, 2, Fixed 2;
   Lda, Zeropage,    0xa5, 2, Fixed 3;
   Lda, Zeropage_X,  0xb5, 2, Fixed 4;
   Lda, Absolute,    0xad, 3, Fixed 4;
   Lda, Absolute_X,  0xbd, 3, Page_Plus 4;
   Lda, Absolute_Y,  0xb9, 3, Page_Plus 4;
   Lda, X_Indirect,  0xa1, 2, Fixed 6;
   Lda, Indirect_Y,  0xb1, 2, Page_Plus 5;

   Ldx, Immediate,   0xa2, 2, Fixed 2;
   Ldx, Zeropage,    0xa6, 2, Fixed 3;
   Ldx, Zeropage_Y,  0xb6, 2, Fixed 4;
   Ldx, Absolute,    0xae, 3, Fixed 4;
   Ldx, Absolute_Y,  0xbe, 3, Page_Plus 4;
   
   Ldy, Immediate,   0xa0, 2, Fixed 2;
   Ldy, Zeropage,    0xa4, 2, Fixed 3;
   Ldy, Zeropage_X,  0xb4, 2, Fixed 4;
   Ldy, Absolute,    0xac, 3, Fixed 4;
   Ldy, Absolute_X,  0xbc, 3, Page_Plus 4;
   
   Lsr, Accumulator, 0x4a, 1, Fixed 2;
   Lsr, Zeropage,    0x46, 2, Fixed 5;
   Lsr, Zeropage_X,  0x56, 2, Fixed 6;
   Lsr, Absolute,    0x4e, 3, Fixed 6;
   Lsr, Absolute_X,  0x5e, 3, Fixed 7;
   
   Nop, Implied,     0xea, 1, Fixed 2;
   
   Ora, Immediate,   0x09, 2, Fixed 2;
   Ora, Zeropage,    0x05, 2, Fixed 3;
   Ora, Zeropage_X,  0x15, 2, Fixed 4;
   Ora, Absolute,    0x0d, 3, Fixed 4;
   Ora, Absolute_X,  0x1d, 3, Page_Plus 4;
   Ora, Absolute_Y,  0x19, 3, Page_Plus 4;
   Ora, X_Indirect,  0x01, 2, Fixed 6;
   Ora, Indirect_Y,  0x11, 2, Page_Plus 5;

   Pha, Implied,     0x48, 1, Fixed 3;
   Php, Implied,     0x08, 1, Fixed 3;
   Pla, Implied,     0x68, 1, Fixed 4;
   Plp, Implied,     0x28, 1, Fixed 4;

   Rol, Accumulator, 0x2a, 1, Fixed 2;
   Rol, Zeropage,    0x26, 2, Fixed 5;
   Rol, Zeropage_X,  0x36, 2, Fixed 6;
   Rol, Absolute,    0x2e, 3, Fixed 6;
   Rol, Absolute_X,  0x3e, 3, Fixed 7;
   
   Ror, Accumulator, 0x6a, 1, Fixed 2;
   Ror, Zeropage,    0x66, 2, Fixed 5;
   Ror, Zeropage_X,  0x76, 2, Fixed 6;
   Ror, Absolute,    0x6e, 3, Fixed 6;
   Ror, Absolute_X,  0x7e, 3, Fixed 7;

   Rti, Implied,     0x40, 1, Fixed 6;
   Rts, Implied,     0x60, 1, Fixed 6;

   Sbc, Immediate,   0xe9, 2, Fixed 2;
   Sbc, Zeropage,    0xe5, 2, Fixed 3;
   Sbc, Zeropage_X,  0xf5, 2, Fixed 4;
   Sbc, Absolute,    0xed, 3, Fixed 4;
   Sbc, Absolute_X,  0xfd, 3, Page_Plus 4;
   Sbc, Absolute_Y,  0xf9, 3, Page_Plus 4;
   Sbc, X_Indirect,  0xe1, 2, Fixed 6;
   Sbc, Indirect_Y,  0xf1, 2, Page_Plus 5;

   Sec, Implied,     0x38, 1, Fixed 2;
   Sed, Implied,     0xf8, 1, Fixed 2;
   Sei, Implied,     0x78, 1, Fixed 2;

   Sta, Zeropage,    0x85, 2, Fixed 3;
   Sta, Zeropage_X,  0x95, 2, Fixed 4;
   Sta, Absolute,    0x8d, 3, Fixed 4;
   Sta, Absolute_X,  0x9d, 3, Fixed 5;
   Sta, Absolute_Y,  0x99, 3, Fixed 5;
   Sta, X_Indirect,  0x81, 2, Fixed 6;
   Sta, Indirect_Y,  0x91, 2, Fixed 6;

   Stx, Zeropage,    0x86, 2, Fixed 3;
   Stx, Zeropage_Y,  0x96, 2, Fixed 4;
   Stx, Absolute,    0x8e, 3, Fixed 4;
   
   Sty, Zeropage,    0x84, 2, Fixed 3;
   Sty, Zeropage_X,  0x94, 2, Fixed 4;
   Sty, Absolute,    0x8c, 3, Fixed 4;
   
   Tax, Implied,     0xaa, 1, Fixed 2;
   Tay, Implied,     0xa8, 1, Fixed 2;
   Txa, Implied,     0x8a, 1, Fixed 2;
   Tya, Implied,     0x98, 1, Fixed 2;
   Tsx, Implied,     0xba, 1, Fixed 2;
   Txs, Implied,     0x9a, 1, Fixed 2]

type insn_info =
  {
    opcode : int;
    bytes : int;
    cycles : cycle
  }

let insns_hash =
  let ht = Hashtbl.create 100 in
  List.iter
    (fun (op, am, opc, sz, tm) ->
      let infos =
        { opcode = opc;
	  bytes = sz;
	  cycles = tm } in
      Hashtbl.add ht (op, am) infos)
    insns;
  ht

let insn_size op addrmode =
  let i = Hashtbl.find insns_hash (op, addrmode) in
  i.bytes

let insn_opcode op addrmode =
  let i = Hashtbl.find insns_hash (op, addrmode) in
  i.opcode

let raw_addrmode_expr_fn fn = function
    Raw_immediate e -> Raw_immediate (fn e)
  | Raw_num e -> Raw_num (fn e)
  | Raw_num_x e -> Raw_num_x (fn e)
  | Raw_num_y e -> Raw_num_y (fn e)
  | Raw_x_indirect e -> Raw_x_indirect (fn e)
  | Raw_indirect_y e -> Raw_indirect_y (fn e)
  | (Raw_accumulator | Raw_implied) as r -> r
