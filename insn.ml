open Expr
open M6502

type insn =
    Raw_insn of opcode * raw_addrmode
  | Insn of opcode * addrmode * int32 array
  | Label of string
  | Data of int * const_expr
  | Scope of (string, int32) Hashtbl.t * insn list
  | Macrodef of string * string list * insn list
  | Expmacro of string * const_expr list

(* A right-fold where scopes are invisible, but the environment stack gets
   updated as the tree is traversed.  *)

let rec fold_right_with_env fn outer_env insns acc =
  let rec foldr env insns acc =
    match insns with
      [] -> acc
    | Scope (inner_env, insns_in_scope)::is ->
	let inner_acc = foldr (inner_env::env) insns_in_scope acc in
	foldr env is inner_acc
    | i::is ->
	fn env i (foldr env is acc) in
  foldr outer_env insns acc

(* Similar, but folding left.  *)

let fold_left_with_env fn outer_env acc insns =
  let rec foldl env acc insns =
    match insns with
      [] -> acc
    | Scope (inner_env, insns_in_scope)::is ->
        let deeper_env = inner_env::env in
        let inner_acc = foldl deeper_env acc insns_in_scope in
	foldl env inner_acc is
    | i::is ->
	foldl env (fn env acc i) is in
  foldl outer_env acc insns

let has_addrmode op am =
  Hashtbl.mem insns_hash (op, am)

exception BadAddrmode

let addrmode_from_raw env first_pass vpc opcode am =
  let eval_addr n =
    try Expr.eval ~env n
    with Expr.Label_not_found _ as e ->
      (* Ignore missing labels on first pass, since we've not seen them all
         yet.  *)
      if first_pass then 0xffffl else raise e
  and rel_branch_good addr =
    let diff = (Int32.to_int addr) - vpc in
    diff >= -126 && diff < 130 in
  match am with
    Raw_immediate n ->
      if has_addrmode opcode Immediate then
	Immediate, [| eval_addr n |]
      else
        raise BadAddrmode
  | Raw_num n ->
      let addr = eval_addr n in
      if has_addrmode opcode Zeropage && addr < 0x100l then
        Zeropage, [| addr |]
      else if has_addrmode opcode Absolute then
        Absolute, [| addr |]
      else if has_addrmode opcode Relative then begin
        if rel_branch_good addr then
          Relative, [| addr |]
	else
	  Synth_lbra, [| addr |]
      end else
	raise BadAddrmode
  | Raw_num_x n ->
      let addr = eval_addr n in
      if has_addrmode opcode Zeropage_X && addr < 0x100l then
        Zeropage_X, [| addr |]
      else if has_addrmode opcode Absolute_X then
        Absolute_X, [| addr |]
      else
        raise BadAddrmode
  | Raw_num_y n ->
      let addr = eval_addr n in
      if has_addrmode opcode Zeropage_Y && addr < 0x100l then
        Zeropage_Y, [| addr |]
      else if has_addrmode opcode Absolute_Y then
        Absolute_Y, [| addr |]
      else
        raise BadAddrmode
  | Raw_x_indirect n ->
      if has_addrmode opcode X_Indirect then
        X_Indirect, [| eval_addr n |]
      else
        raise BadAddrmode
  | Raw_indirect_y n ->
      if has_addrmode opcode Indirect_Y then
        Indirect_Y, [| eval_addr n |]
      else
        raise BadAddrmode
  | Raw_accumulator ->
      if has_addrmode opcode Accumulator then
        Accumulator, [| |]
      else
        raise BadAddrmode
  | Raw_implied ->
      if has_addrmode opcode Implied then
        Implied, [| |]
      else if has_addrmode opcode Accumulator then
        Accumulator, [| |]
      else
        raise BadAddrmode

(* FIXME: Macros should expand as scopes. *)

let invoke_macros prog macros =
  List.fold_right
    (fun insn insns ->
      match insn with
        Expmacro (name, actual_args) ->
	  let (formal_args, body) = Hashtbl.find macros name in
	  List.fold_right
	    (fun body_insn insns ->
	      match body_insn with
		Raw_insn (opc, raw_addrmode) ->
	          let raw_addrmode' = M6502.raw_addrmode_expr_fn
		    (fun expr -> subst_macro_args expr formal_args actual_args)
		    raw_addrmode in
		  Raw_insn (opc, raw_addrmode') :: insns
	      | _ -> body_insn :: insns)
	    body
	    insns
      | _ -> insn :: insns)
    prog
    []
