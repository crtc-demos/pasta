open Insn

let insn_size = function
    Label _ -> 0
  | Data (n, _) -> n
  | Insn (opc, adm, _) -> M6502.insn_size opc adm
  | _ -> failwith "Can't find size of insn"

let layout vpc_start insns =
  let label_locs = Hashtbl.create 100 in
  let rec scan vpc = function
    [] -> vpc
  | insn::insns ->
      begin match insn with
        Label foo ->
	  (* Printf.fprintf stderr "Label '%s' at 0x%x\n" foo vpc; *)
	  Hashtbl.add label_locs foo (Int32.of_int vpc)
      | _ -> ()
      end;
      scan (vpc + insn_size insn) insns
  in
    let last_vpc = scan vpc_start insns in
    last_vpc, label_locs

let cook_insns env first_pass vpc_start insns =
  let insns', _ = List.fold_right
    (fun insn (insns, vpc) ->
      match insn with
        Raw_insn (opcode, raw_addrmode) ->
          let addrmode, args =
	    addrmode_from_raw env first_pass vpc opcode raw_addrmode in
	  let insn_size = M6502.insn_size opcode addrmode in
          Insn (opcode, addrmode, args) :: insns, vpc + insn_size
      | x -> x :: insns, vpc + (insn_size x))
    insns
    ([], 0) in
  insns'

let iterate_layout vpc_start insns =
  let rec iter first_pass env =
    let cooked_insns = cook_insns env first_pass vpc_start insns in
    let last_pc, new_env = layout vpc_start cooked_insns in
    if env <> new_env then
      iter false new_env
    else
      cooked_insns, last_pc, new_env in
  iter true (Hashtbl.create 1)
