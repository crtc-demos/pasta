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
  | SourceLoc of srcloc
  | IncludeFile of filename

and ascii_part = AscString of string
               | AscChar of const_expr

and temp_spec = OneTemp of const_expr
	      | TempRange of const_expr * const_expr

and srcloc = SourceLine of filename * int
           | SourceExpandedFromLine of filename * int * filename * int

and filename = string

let unknown_sourceline = SourceLine ("<unknown>", 0)

let string_of_srcloc = function
    SourceLine (filename, line) -> Printf.sprintf "%s:%d" filename line
  | SourceExpandedFromLine (filename, line, fromfile, fromline) ->
      Printf.sprintf "%s:%d, expanded from %s:%d" filename line
        fromfile fromline

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
  let srcloc = ref (SourceLine ("<unknown>", 0)) in
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
    | (SourceLoc line as i)::is ->
        srcloc := line;
	foldl env (fn env !srcloc acc i) is
    | i::is ->
	foldl env (fn env !srcloc acc i) is in
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

exception BadAddrmode of string

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
        raise (BadAddrmode "immediate")
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
	raise (BadAddrmode "zeropage or absolute")
  | Raw_num_x n ->
      let addr = eval_addr n in
      if has_addrmode opcode Zeropage_X && addr < 0x100l then
        Zeropage_X, [| addr |]
      else if has_addrmode opcode Absolute_X then
        Absolute_X, [| addr |]
      else
        raise (BadAddrmode "indexed X")
  | Raw_num_y n ->
      let addr = eval_addr n in
      if has_addrmode opcode Zeropage_Y && addr < 0x100l then
        Zeropage_Y, [| addr |]
      else if has_addrmode opcode Absolute_Y then
        Absolute_Y, [| addr |]
      else
        raise (BadAddrmode "indexed Y")
  | Raw_indirect n ->
      if has_addrmode opcode ZP_Indirect then
        ZP_Indirect, [| eval_addr n |]
      else if has_addrmode opcode Indirect then
        Indirect, [| eval_addr n |]
      else
        raise (BadAddrmode "indirect")
  | Raw_x_indirect n ->
      if has_addrmode opcode X_Indirect then
        X_Indirect, [| eval_addr n |]
      else if has_addrmode opcode X_Indirjmp then
        X_Indirjmp, [| eval_addr n |]
      else
        raise (BadAddrmode "X indirect")
  | Raw_indirect_y n ->
      if has_addrmode opcode Indirect_Y then
        Indirect_Y, [| eval_addr n |]
      else
        raise (BadAddrmode "indirect Y")
  | Raw_accumulator ->
      if has_addrmode opcode Accumulator then
        Accumulator, [| |]
      else
        raise (BadAddrmode "accumulator")
  | Raw_implied ->
      if has_addrmode opcode Implied then
        Implied, [| |]
      else if has_addrmode opcode Accumulator then
        Accumulator, [| |]
      else
        raise (BadAddrmode "implied")

(* Expand macros once in PROG.  Note macros are expanded to nested scopes at
   their points of invocation.  *)

let rec invoke_macros_once prog macros =
  let expandedfromline = ref 0
  and expandedfromfile = ref "<unknown>" in
  let expanded, out = List.fold_left
    (fun (expanded, insns) insn ->
      match insn with
        Expmacro (name, actual_args) ->
	  let (formal_args, body) = try
	    Hashtbl.find macros name
	  with Not_found ->
	    raise (Line.AssemblyError (
	      Printf.sprintf "Undefined macro '%s'" name,
	      Printf.sprintf "%s:%d" !expandedfromfile !expandedfromline))
	  in let infile = ref "<unknown>" and lineno = ref 0 in
	  begin try
	    let expansion = List.fold_left
	      (fun insns body_insn ->
		match body_insn with
		  Raw_insn (opc, raw_addrmode) ->
	            let raw_addrmode' = M6502.map_raw_addrmode_expr
		      (fun expr ->
		        subst_macro_args expr formal_args actual_args)
		      raw_addrmode in
		    Raw_insn (opc, raw_addrmode') :: insns
		| Expmacro (name, args) ->
		    let args' = List.map
		      (fun arg ->
		        subst_macro_args arg formal_args actual_args)
		      args in
		    Expmacro (name, args') :: insns
		| SourceLoc l ->
		    begin match l with
		      SourceLine (filename, line) ->
		        infile := filename;
        	        lineno := line;
		        SourceLoc (SourceExpandedFromLine (filename, line,
			           !expandedfromfile, !expandedfromline))
			  :: insns
		    | SourceExpandedFromLine (filename, line, _, _) ->
		        SourceLoc (SourceExpandedFromLine (filename, line,
				   !expandedfromfile, !expandedfromline))
			  :: insns
		    end
		| _ -> body_insn :: insns)
	      []
	      body in
	    true, Scope (Env.new_env (), List.rev expansion) :: insns
	  with Expr.UnknownMacroArg arg ->
	    raise (Line.AssemblyError (
	      Printf.sprintf "Unknown macro arg '%s'" arg,
	      Printf.sprintf "%s:%d" !infile !lineno))
	  | Expr.TooManyParams ->
	    raise (Line.AssemblyError (
	      Printf.sprintf "Too many parameters for macro '%s'" name,
	      Printf.sprintf "%s:%d (defined at %s:%d)" !expandedfromfile
	        !expandedfromline !infile !lineno))
	  | Expr.NotEnoughParams ->
	    raise (Line.AssemblyError (
	      Printf.sprintf "Not enough parameters for macro '%s'" name,
	      Printf.sprintf "%s:%d (defined at %s:%d)" !expandedfromfile
	        !expandedfromline !infile !lineno))
	  end
      | Scope (nested_env, body) ->
          let expanded', body' = invoke_macros_once body macros in
	  expanded || expanded', Scope (nested_env, body') :: insns
      | Context (nested_env, ctxname, body) ->
	  let expanded', body' = invoke_macros_once body macros in
	  expanded || expanded', Context (nested_env, ctxname, body') :: insns
      | SourceLoc l ->
          begin match l with
	    SourceLine (file, line) ->
	      expandedfromfile := file;
	      expandedfromline := line
	  | SourceExpandedFromLine (file, line, _, _) ->
	      expandedfromfile := file;
	      expandedfromline := line
	  end;
	  expanded, insn :: insns
      | _ -> expanded, insn :: insns)
    (false, [])
    prog in
  expanded, List.rev out

(* Iteratively expand macros in PROG.  *)

let rec invoke_macros prog macros =
  let expanded, prog' = invoke_macros_once prog macros in
  if expanded then
    invoke_macros prog' macros
  else
    prog'

exception UnhandledJump

let context_from_expr caller expr lineno =
  match expr with
    Expr.ExLabel lab when Context.ctxs#mem [lab] -> [lab]
  | Expr.ExLabel lab when (Context.ctxs#get caller)#defines_label lab ->
      begin match !Log.alloc_stream with
        None -> ()
      | Some fh ->
	  Printf.fprintf fh "Context '%s' has internal calls\n"
            (Context.to_string caller);
      end;
      raise Not_found
  | x ->
      raise (Line.AssemblyError ((Printf.sprintf
        "Context '%s' may not call '%s' (which is not in a context)"
	(Context.to_string caller)
        (Expr.to_string x)), string_of_srcloc lineno))

(* Find the dependencies of contexts upon other contexts.  *)

let find_dependencies prog =
  let lineno = ref unknown_sourceline in
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
	        let dctxid = context_from_expr ctxid dest !lineno in
		if not (ctx#call_marked dctxid) then begin
		  begin match !Log.alloc_stream with
		    None -> ()
		  | Some fh ->
		    Printf.fprintf fh "Context '%s' calls '%s'\n"
		      (Context.to_string ctxid) (Context.to_string dctxid)
		  end;
		  ctx#calls_context dctxid
		end
	      with Not_found ->
	        ()
	      end
	  | Jmp, x ->
	      raise (Line.AssemblyError ((Printf.sprintf
	        "Context '%s' has unsupported jump" (Context.to_string ctxid)),
		(string_of_srcloc !lineno)))
	  | _ -> ()
	  end
      | _, SourceLoc line ->
	  lineno := line
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
