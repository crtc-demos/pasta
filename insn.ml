open Expr
open M6502

type insn =
    Raw_insn of opcode * raw_addrmode
  | Insn of opcode * addrmode * int32 array
  | Label of string
  | Alias of string * const_expr
  | Data of int * const_expr list
  | Ascii of ascii_part list
  | DataBlock of const_expr * const_expr
  | Origin of const_expr
  | Scope of (string, int32) Hashtbl.t * insn list
  | Context of (string, int32) Hashtbl.t * string * insn list
  | Temps of temp_spec list
  | NoTemps of string list
  | Interf of string list * string list
  | Protect of string list list
  | Macrodef of string * string list * insn list
  | Expmacro of string * const_expr list
  | DeclVars of int * string list

and ascii_part = AscString of string
               | AscChar of const_expr

and temp_spec = OneTemp of const_expr
	      | TempRange of const_expr * const_expr

(* A right-fold where scopes are invisible, but the environment stack gets
   updated as the tree is traversed.  *)

let rec fold_right_with_env fn outer_env insns acc =
  let rec foldr env insns acc =
    match insns with
      [] -> acc
    | Scope (inner_env, insns_in_scope)::is ->
	let inner_acc = foldr (inner_env::env) insns_in_scope acc in
	foldr env is inner_acc
    | Context (inner_env, ctxname, insns_in_ctx)::is ->
        let inner_acc = foldr (inner_env::env) insns_in_ctx acc in
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
    | Context (inner_env, ctxname, insns_in_ctx)::is ->
	let deeper_env = inner_env::env in
	let inner_acc = foldl deeper_env acc insns_in_ctx in
	foldl env inner_acc is
    | i::is ->
	foldl env (fn env acc i) is in
  foldl outer_env acc insns

let iter_with_context fn insns =
  let rec iter ctx insns =
    match insns with
      [] -> ()
    | Scope (_, insns_in_scope)::is ->
        iter ctx insns_in_scope;
	iter ctx is
    | (Context (_, ctxname, insns_in_ctx) as i)::is ->
        iter (ctxname::ctx) insns_in_ctx;
	fn ctx i;
	iter ctx is
    | i::is ->
        fn ctx i;
	iter ctx is in
  iter [] insns

(* This map retains the program structure.  *)

let map_with_context fn insns =
  let rec map ctx insns =
    match insns with
      [] -> []
    | Context (ht, ctxname, insns_in_ctx)::is ->
        let new_insns = map (ctxname::ctx) insns_in_ctx in
	(fn ctx (Context (ht, ctxname, new_insns)))::(map ctx is)
    | Scope (ht, insns_in_scope)::is ->
        let new_insns = map ctx insns_in_scope in
	(fn ctx (Scope (ht, new_insns)))::(map ctx is)
    | i::is -> (fn ctx i)::(map ctx is) in
  map [] insns

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
  | Raw_indirect n ->
      if has_addrmode opcode Indirect then
        Indirect, [| eval_addr n |]
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

(* Expand macros once in PROG.  Note macros are expanded in nested scopes at
   their points of invocation.  *)

let rec invoke_macros_once prog macros =
  List.fold_right
    (fun insn (expanded, insns) ->
      match insn with
        Expmacro (name, actual_args) ->
	  let (formal_args, body) = Hashtbl.find macros name in
	  let expansion = List.fold_right
	    (fun body_insn insns ->
	      match body_insn with
		Raw_insn (opc, raw_addrmode) ->
	          let raw_addrmode' = M6502.map_raw_addrmode_expr
		    (fun expr -> subst_macro_args expr formal_args actual_args)
		    raw_addrmode in
		  Raw_insn (opc, raw_addrmode') :: insns
	      | _ -> body_insn :: insns)
	    body
	    [] in
	  true, Scope (Env.new_env (), expansion) :: insns
      | Scope (nested_env, body) ->
          let expanded', body' = invoke_macros_once body macros in
	  expanded', Scope (nested_env, body') :: insns
      | Context (nested_env, ctxname, body) ->
	  let expanded', body' = invoke_macros_once body macros in
	  expanded', Context (nested_env, ctxname, body') :: insns
      | _ -> expanded, insn :: insns)
    prog
    (false, [])

(* Iteratively expand macros in PROG.  *)

let rec invoke_macros prog macros =
  let expanded, prog' = invoke_macros_once prog macros in
  if expanded then
    invoke_macros prog' macros
  else
    prog'

exception UnhandledJump

let context_from_expr caller expr =
  match expr with
    Expr.ExLabel lab when Context.ctxs#mem [lab] -> [lab]
  | Expr.ExLabel lab when (Context.ctxs#get caller)#defines_label lab ->
      Printf.printf "Context %s has internal calls\n"
        (Context.to_string caller);
      raise Not_found
  | x ->
      Printf.printf "Context %s may not call %s\n" (Context.to_string caller)
        (Expr.to_string x);
      raise UnhandledJump

(* Find the dependencies of contexts upon other contexts.  *)

let find_dependencies prog =
  iter_with_context
    (fun ctxid i ->
      match ctxid, i with
        [], _ -> ()
      | _, Raw_insn (opcode, rawaddrmode) ->
	  begin match opcode, rawaddrmode with
	    Jsr, Raw_num dest
	  | Jmp, Raw_num dest ->
	      begin try
	        let ctx = Context.ctxs#get ctxid in
	        let dctxid = context_from_expr ctxid dest in
		if not (ctx#call_marked dctxid) then begin
		  Printf.printf "Context %s calls %s\n"
		    (Context.to_string ctxid) (Context.to_string dctxid);
		  ctx#calls_context dctxid
		end
	      with Not_found ->
	        ()
	      end
	  | Jmp, x ->
	      Printf.printf "Context %s has unsupported jump\n"
	        (Context.to_string ctxid);
	      raise UnhandledJump
	  | _ -> ()
	  end
      | _ -> ())
    prog

exception UnknownVariable of string

let lookup_var ctxname varname =
  let var =
    match varname with
      [single] ->
        begin try
	  (Context.ctxs#get ctxname)#get_var single
	with Not_found ->
	  raise (UnknownVariable single)
	end
    | [qual; name] ->
        begin try
	  (Context.ctxs#get [qual])#get_var name
	with Not_found ->
	  raise (UnknownVariable (qual ^ "." ^ name))
	end
    | _ -> failwith "Nested contexts aren't supported yet" in
  Expr.Int (Int32.of_int var#get_loc)

let substitute_vars prog =
  map_with_context
    (fun ctx i ->
      match i with
        Raw_insn (opc, rawadm) ->
	  let newrawadm = map_raw_addrmode_expr
	    (Expr.map_expr (function
	        VarRef v -> lookup_var ctx v
	      | x -> x))
	    rawadm in
	  Raw_insn (opc, newrawadm)
      | x -> x)
    prog
