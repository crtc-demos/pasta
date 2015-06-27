open Insn

(* It'd probably be better to filter out the zero-sized directives before we
   get to here.  This works OK for now though.  *)

let rec insn_size env = function
    Label _
  | DeclVars _
  | Temps _
  | NoTemps _
  | Interf _
  | Protect _
  | SourceLoc _ -> 0
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

let rec verify_declarations insns =
  let labels_aliases = Hashtbl.create 10
  and origin = Hashtbl.create 1
  and contexts = Hashtbl.create 10
  and macros = Hashtbl.create 10 in
  let add_once desc ht name sourceloc =
    if Hashtbl.mem ht name then
      raise (Line.AssemblyError
        ((Printf.sprintf "Multiple %s definition '%s'" desc name),
	 Insn.string_of_srcloc sourceloc));
    Hashtbl.add ht name () in
  let sourceloc = ref unknown_sourceline in
  let check = function
    SourceLoc sl -> sourceloc := sl
  | Label l -> add_once "label or alias" labels_aliases l !sourceloc
  | Alias (a, _) -> add_once "label or alias" labels_aliases a !sourceloc
  | Origin _ -> add_once "origin" origin ".org" !sourceloc
  | Context (_, name, inner) ->
      add_once "context" contexts name !sourceloc;
      verify_declarations inner
  | Scope (_, inner) ->
      verify_declarations inner
  | Macrodef (name, _, _) -> add_once "macro" macros name !sourceloc
  | _ -> () in
  List.iter check insns

(* Do a dummy layout pass to find aliases (exactly, with non-label-dependent
   values) or labels (defined vs. undefined only).  Multiple definition errors
   are not detected here.  *)

let layout_conditionals_1 frags defines first_pass =
  let rec build_conditional env lineno insns_in insns_out iter_again =
    match insns_in with
      [] -> List.rev insns_out, iter_again
    | i::is ->
        begin match i with
	  Label foo ->
	    Env.replace env foo Expr.UnknownVal;
	    build_conditional env lineno is (i::insns_out) iter_again
	| Alias (label, cexp) ->
            begin try
	      let cst = Expr.eval ~env cexp in
	      Env.replace env label (Expr.KnownVal cst);
	      build_conditional env lineno is (i::insns_out) iter_again
	    with
	      Expr.UnknownValue _ ->
	        Env.replace env label Expr.UnknownVal;
		build_conditional env lineno is (i::insns_out) iter_again
	    | Expr.Label_not_found _ ->
		build_conditional env lineno is (i::insns_out) true
	    end
	| CondBlock (cnd, tlst, flst) ->
            begin try
	      let cst = Expr.eval ~env cnd in
	      if cst <> 0l then
		build_conditional env lineno (tlst @ is) insns_out iter_again
	      else
		build_conditional env lineno (flst @ is) insns_out iter_again
	    with Expr.Label_not_found _ as e ->
	      if first_pass then
		build_conditional env lineno is (i::insns_out) true
	      else
	        raise e
	    end
	| Scope (inner_env, insns_in_scope) ->
	    let scope, iter_again' =
	      build_conditional (inner_env::env) lineno insns_in_scope []
				iter_again in
	    build_conditional env lineno is
			      (Scope (inner_env, scope)::insns_out)
			      iter_again'
	| Context (inner_env, ctxname, insns_in_ctx) ->
	    let ctx, iter_again' =
	      build_conditional (inner_env::env) lineno insns_in_ctx []
				iter_again in
	    build_conditional env lineno is
			      (Context (inner_env, ctxname, ctx)::insns_out)
			      iter_again'
	| Macrodef (nm, args, insns_in_def) ->
	    let mdef, iter_again' =
	      build_conditional env lineno insns_in_def [] iter_again in
	    build_conditional env lineno is
			      (Macrodef (nm, args, mdef)::insns_out)
			      iter_again'
	| IncludeFile f ->
	    Line.push_include f;
	    let parsed = Parse_file.parse_file f in
	    let included_insns, iter_again' =
	      build_conditional env lineno parsed [] iter_again in
	    Line.pop_include ();
	    build_conditional env lineno (included_insns @ is) insns_out
			      iter_again'
	| SourceLoc new_line ->
	    build_conditional env new_line is (i::insns_out) iter_again
	| _ -> build_conditional env lineno is (i::insns_out) iter_again
	end in
  build_conditional defines (SourceLine ("<unknown>", 0)) frags [] false

let layout_conditionals frags defines =
  let rec retry frags first_pass =
    let frags, iter_again = layout_conditionals_1 frags [defines] first_pass in
    if iter_again then
      retry frags false
    else
      frags in
  retry frags true

(* Do layout. Convert raw (parsed) insns into "cooked" insns, ready for
   encoding. Also flatten out nested scopes, and reverse the program so it's in
   the "correct" order (with the head of the list as the first instruction).  *)

let layout env first_pass vpc_start insns =
  let insns', last_vpc, iter_again = Insn.fold_left_with_env
    (fun env lineno (insns, vpc, iter_again) insn ->
      match insn with
        Raw_insn (opcode, raw_addrmode) ->
	  begin try
	    let addrmode, args =
	      addrmode_from_raw env first_pass vpc opcode raw_addrmode in
	    let insn_size = M6502.insn_size opcode addrmode in
	    Insn (opcode, addrmode, args) :: insns, vpc + insn_size, iter_again
	  with Insn.BadAddrmode am ->
	    raise (Line.AssemblyError
	      ((Printf.sprintf "Bad addressing mode '%s'" am),
	       (Insn.string_of_srcloc lineno)))
	  | Expr.Label_not_found lab ->
	    raise (Line.AssemblyError
	      ((Printf.sprintf "Label '%s' not found" lab),
	       Insn.string_of_srcloc lineno))
	  end
      | Label foo ->
          Env.replace env foo (Expr.KnownVal (Int32.of_int vpc));
	  insns, vpc, iter_again
      | Alias (label, cexp) ->
	  begin try
	    let cst = Expr.eval ~env cexp in
	    Env.replace env label (Expr.KnownVal cst);
	    insns, vpc, iter_again
	  with Expr.Label_not_found _ ->
	    insns, vpc, true
	  end
      | Data (size, cexplist) ->
	  let items' = List.map
	    (fun cexp ->
	      try
	        Expr.subst_labels ~env cexp
	      with Expr.UnknownValue _ | Expr.Label_not_found _ ->
	        cexp)
	    cexplist in
	  let data' = Data (size, items') in
	  data' :: insns, vpc + (insn_size env insn), iter_again
      | Ascii al ->
	  let items' = List.map
	    (function
	      AscString s -> AscString s
	    | AscChar cexp ->
		try
	          AscChar (Expr.subst_labels ~env cexp)
		with Expr.Label_not_found _ ->
	          AscChar cexp)
	    al in
	  let ascii' = Ascii items' in
	  ascii' :: insns, vpc + (insn_size env insn), iter_again
      | DataBlock (numexp, bvalexp) ->
	  let numexp' = try
	    Expr.subst_labels ~env numexp
	  with Expr.Label_not_found _ ->
	    numexp in
	  let bvalexp' = try
	    Expr.subst_labels ~env bvalexp
	  with Expr.Label_not_found _ ->
	    bvalexp in
	  let dblk' = DataBlock (numexp', bvalexp') in
	  dblk' :: insns, vpc + (insn_size env insn), iter_again
      | x -> x :: insns, vpc + (insn_size env x), iter_again)
    [env]
    ([], vpc_start, false)
    insns in
  let lineno = ref unknown_sourceline in
  (* Stick context entry points into top-level environment (hack!)  *)
  Insn.iter_with_context
    (fun ctx insn ->
      match insn with
        Context (ht, ctxname, _) ->
	  begin try
	    let entry_pt = Env.find [ht] ctxname in
	    Env.replace [env] ctxname entry_pt
	  with Not_found ->
	    raise (Line.AssemblyError ((Printf.sprintf
	      "No entry point in context '%s'" ctxname),
	      (Insn.string_of_srcloc !lineno)))
	  end
      | SourceLoc line ->
          lineno := line
      | _ -> ())
    insns;
  insns', last_vpc, iter_again

let iterate_layout vpc_start insns outer_env =
  let rec iter first_pass previous_cooked_insns =
    let cooked_insns, last_pc, iterate_again =
      layout outer_env first_pass vpc_start insns in
    if iterate_again || cooked_insns <> previous_cooked_insns then
      iter false cooked_insns
    else
      cooked_insns, last_pc, outer_env in
  iter true []
