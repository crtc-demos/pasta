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

let expand_synth_branch prog env ~verbose =
  let lineno = ref (SourceLine 0) in
  Insn.fold_right_with_env
    (fun env insn acc ->
      match insn with
        Insn ((Bcc | Bcs | Beq | Bmi | Bne | Bpl | Bvc | Bvs) as opcode,
	      Synth_lbra, args) ->
	  let inv_opcode = invert_branch opcode in
	  if verbose then begin
	    Printf.fprintf stderr "Note: created synthetic long branch at %s\n"
	      (Insn.string_of_srcloc !lineno)
	  end;
	  Insn (Jmp, Absolute, args)
	  :: Insn (inv_opcode, Relative, [| 5l |])
	  :: acc
      | Insn (Bra as opcode, Synth_lbra, args) ->
          if verbose then begin
	    Printf.fprintf stderr "Note: using jmp instead of bra at %s\n"
	      (Insn.string_of_srcloc !lineno)
	  end;
          Insn (Jmp, Absolute, args) :: acc
      | SourceLoc line ->
          lineno := line;
	  insn :: acc
      | _ -> insn :: acc)
    env
    prog
    []
