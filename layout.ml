open Insn

let insn_size = function
    Label _ -> 0
  | Data (n, dl) -> n * List.length dl
  | Ascii al ->
      List.fold_right
	(fun part sz ->
          match part with
	    AscChar _ -> sz + 1
	  | AscString s -> sz + String.length s)
	al
	0
  | Insn (opc, adm, _) -> M6502.insn_size opc adm
  | _ -> failwith "Can't find size of insn"

(* Do layout. Convert raw (parsed) insns into "cooked" insns, ready for
   encoding. Also flatten out nested scopes, and reverse the program so it's in
   the "correct" order (with the head of the list as the first instruction).  *)

let layout env first_pass vpc_start insns =
  let insns', last_vpc = Insn.fold_left_with_env
    (fun env (insns, vpc) insn ->
      match insn with
        Raw_insn (opcode, raw_addrmode) ->
	  let addrmode, args =
	    addrmode_from_raw env first_pass vpc opcode raw_addrmode in
	  let insn_size = M6502.insn_size opcode addrmode in
	  Insn (opcode, addrmode, args) :: insns, vpc + insn_size
      | Label foo ->
          Env.replace env foo (Int32.of_int vpc);
	  insns, vpc
      | x -> x :: insns, vpc + (insn_size x))
    [env]
    ([], vpc_start)
    insns in
  insns', last_vpc

let iterate_layout vpc_start insns =
  let outer_env = Env.new_env () in
  let rec iter first_pass previous_cooked_insns =
    let cooked_insns, last_pc = layout outer_env first_pass vpc_start insns in
    if cooked_insns <> previous_cooked_insns then
      iter false cooked_insns
    else
      cooked_insns, last_pc, outer_env in
  iter true []
