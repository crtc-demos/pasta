open Insn

(* It'd probably be better to filter out the zero-sized directives before we
   get to here.  This works OK for now though.  *)

let rec insn_size env = function
    Label _ -> 0
  | DeclVars _ -> 0
  | Temps _ -> 0
  | NoTemps _ -> 0
  | Interf _ -> 0
  | Protect _ -> 0
  | Data (n, dl) -> n * List.length dl
  | Ascii al ->
      List.fold_right
	(fun part sz ->
          match part with
	    AscChar _ -> sz + 1
	  | AscString s -> sz + String.length s)
	al
	0
  | DataBlock (n, _) -> Int32.to_int (Expr.eval ~env n)
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
      | Alias (label, cexp) ->
          let cst = Expr.eval ~env cexp in
	  Env.replace env label cst;
	  insns, vpc
      | x -> x :: insns, vpc + (insn_size env x))
    [env]
    ([], vpc_start)
    insns in
  (* Stick context entry points into top-level environment (hack!)  *)
  Insn.iter_with_context
    (fun ctx insn ->
      match insn with
        Context (ht, ctxname, _) ->
	  begin try
	    let entry_pt = Env.find [ht] ctxname in
	    Env.replace [env] ctxname entry_pt
	  with Not_found ->
	    Printf.printf "No context entry point in '%s'\n" ctxname;
	    failwith "Boo."
	  end
      | _ -> ())
    insns;
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
