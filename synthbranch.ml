open M6502
open Insn

let invert_branch = function
    Bcc -> Bcs
  | Bcs -> Bcc
  | Beq -> Bne
  | Bne -> Beq
  | Bmi -> Bpl
  | Bpl -> Bmi
  | Bvc -> Bvs
  | Bvs -> Bvc
  | _ -> failwith "Not a branch"

let expand_synth_branch prog start_vpc env ~verbose =
  let lineno = ref (SourceLine 0) in
  let expanded, _ = Insn.fold_right_with_env
    (fun env insn (acc, vpc) ->
      match insn with
        Insn ((Bcc | Bcs | Beq | Bmi | Bne | Bpl | Bvc | Bvs) as opcode,
	      Synth_lbra, args) ->
	  let inv_opcode = invert_branch opcode in
	  if verbose then begin
	    Printf.fprintf stderr "Note: created synthetic long branch at %s\n"
	      (Insn.string_of_srcloc !lineno)
	  end;
	  Insn (Jmp, Absolute, args)
	  :: Insn (inv_opcode, Relative, [| Int32.add (Int32.of_int vpc) 5l |])
	  :: acc, vpc + Layout.insn_size env insn
      | Insn (Bra as opcode, Synth_lbra, args) ->
          if verbose then begin
	    Printf.fprintf stderr "Note: using jmp instead of bra at %s\n"
	      (Insn.string_of_srcloc !lineno)
	  end;
          Insn (Jmp, Absolute, args) :: acc, vpc + Layout.insn_size env insn
      | SourceLoc line ->
          lineno := line;
	  insn :: acc, vpc + Layout.insn_size env insn
      | _ -> insn :: acc, vpc + Layout.insn_size env insn)
    env
    prog
    ([], start_vpc) in
  expanded
