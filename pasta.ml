let collect_insns prog =
  List.fold_right
    (fun frag acc ->
      match frag with
        Insn.Macrodef _
      | Insn.DeclVars _
      | Insn.Temps _
      | Insn.NoTemps _
      | Insn.Interf _ -> acc
      | x -> x :: acc)
    prog
    []

let collect_macros prog =
  let ht = Hashtbl.create 5 in
  List.iter
    (function
        Insn.Macrodef (name, formal_args, body) ->
          Hashtbl.add ht name (formal_args, body)
      | _ -> ())
    prog;
  ht

let collect_contexts prog =
  Insn.iter_with_context
    (fun ctx i ->
      Context.ctxs#add ctx;
      match i with
        Insn.Label lab -> (Context.ctxs#get ctx)#add_label lab
      | _ -> ())
    prog

(* For "notemps" declarations, add dummy contexts.  *)

let collect_notemps prog =
  Insn.iter_with_context
    (fun _ i ->
      match i with
        Insn.NoTemps labels ->
	  List.iter (fun lab -> Context.ctxs#add [lab]) labels
      | _ -> ())
    prog

let collect_vars prog =
  Insn.iter_with_context
    (fun ctx i ->
      match i with
        Insn.DeclVars (sz, names) ->
	  let ctxobj = Context.ctxs#get ctx in
	  List.iter
	    (fun name -> ctxobj#add_var name sz)
	    names
      | _ -> ())
    prog;
  ()

(* Collect explicit interference directives.  *)

let collect_interf prog =
  Insn.fold_left_with_env
    (fun _ _ intflist insn ->
      match insn with
        Insn.Interf (a, b) -> (a, b) :: intflist
      | _ -> intflist)
    []
    []
    prog

(* This really wants fold_with_context, but I'm too lazy to write it.  *)

let collect_protections prog =
  let plist = ref [] in
  Insn.iter_with_context
    (fun ctx insn ->
      match insn with
        Insn.Protect pl ->
	  List.iter (fun protect -> plist := (ctx, protect) :: !plist) pl
      | _ -> ())
    prog;
  !plist

let collect_temps prog pool env =
  List.iter
    (function
        Insn.Temps tempspecs ->
          List.iter (fun tspec -> pool#add_to_pool tspec env) tempspecs
      | _ -> ())
    prog

(* Find the origin.  Return (origin, prog') where prog' is the input program
   with .org directives removed.
   If this is called with "prog" in reverse order, the first .org will be the
   one used, and later settings will be ignored.  Could be an error instead.  *)

let extract_origin prog defines =
  List.fold_right
    (fun insn (org, insns) ->
      match insn with
        Insn.Origin x -> Int32.to_int (Expr.eval ~env:[defines] x), insns
      | x -> org, x::insns)
    prog
    (0, [])

let rec resolve_includes prog =
  List.fold_right
    (fun insn out_insns ->
      match insn with
        Insn.IncludeFile f ->
	  Line.push_include f;
	  let parsed = Parse_file.parse_file f in
	  let p_resolved = resolve_includes parsed in
	  Line.pop_include ();
	  p_resolved @ out_insns
      | Insn.Scope (ht, insns_in_scope) ->
          let inner_resolved = resolve_includes insns_in_scope in
	  Insn.Scope (ht, inner_resolved) :: out_insns
      | Insn.Context (ht, ctxname, insns_in_ctx) ->
          let inner_resolved = resolve_includes insns_in_ctx in
	  Insn.Context (ht, ctxname, inner_resolved) :: out_insns
      | _ -> insn :: out_insns)
    prog
    []

(*let resolve_conditionals frags defines =
  let rec conditionalise _ = function
    Insn.CondBlock (c, tlst, flst) ->
      let res = Expr.eval ~env:[defines] c in
      Insn.Scope
        (Env.new_env (),
	 if res <> 0l then
	   Insn.map_with_context conditionalise tlst
	 else
	   Insn.map_with_context conditionalise flst)
  | x -> x in
  Insn.map_with_context conditionalise frags*)

let _ =
  let infile = ref "" and outfile = ref "a.out"
  and allocdump = ref false
  and noisy = ref true
  and defines = Hashtbl.create 10 in
  let do_define def =
    try
      let eq = String.index def '=' in
      let sym = String.sub def 0 eq
      and cst = String.sub def (eq + 1) (String.length def - eq - 1) in
      Hashtbl.replace defines sym (Expr.KnownVal (Int32.of_string cst))
    with Not_found ->
      Hashtbl.replace defines def Expr.UnknownVal in
  let argspec =
    ["-o", Arg.Set_string outfile, "Set output file";
     "-a", Arg.Set allocdump, "Write allocation info dump";
     "-q", Arg.Clear noisy, "Be quiet about synthetic instructions";
     "-D", Arg.String do_define, "Define assembly-time constant from cmdline"]
  and usage = "Usage: pasta -o <output> <input>" in
  Arg.parse argspec (fun i -> infile := i) usage;
  if !infile = "" then begin
    Arg.usage argspec usage;
    exit 1
  end;
  let return_code = ref 0 in
  Line.current_file := !infile;
  let frags_toplevel = try
    Parse_file.parse_file !infile
  with Line.ParseError ->
    exit 1 in
  let alloc_filename = Log.alloc_filename !infile in
  if !allocdump then
    Log.open_alloc alloc_filename;
  begin try
    let frags = Layout.layout_conditionals frags_toplevel defines in
    let prog = collect_insns frags in
    Layout.verify_declarations frags;
    let macros = collect_macros frags in
    let prog = Insn.invoke_macros prog macros in
    let origin, prog = extract_origin prog defines in
    collect_contexts prog;
    collect_notemps frags;
    Insn.find_dependencies prog;
    Context.ctxs#transitive_closure;
    collect_vars frags;
    let igraph = Alloc.build_graph () in
    let igraph = Alloc.add_explicit_interf igraph (collect_interf frags) in
    let prots = collect_protections frags in
    let igraph = Alloc.add_protection igraph prots in
    begin match !Log.alloc_stream with
      None -> ()
    | Some fh -> Alloc.print_graph fh igraph
    end;
    let pool = new Temps.temps in
    collect_temps frags pool [defines];
    let spilled = Colour.alloc igraph pool in
    if spilled <> [] then begin
      Printf.fprintf stderr "Ran out of temps!\n";
      List.iter
	(fun (nctx, nvarname) ->
	  Printf.fprintf stderr "No space for: %s.%s\n" nctx#get_name nvarname)
	spilled;
      raise (Line.NonLineError "Allocation failure")
    end;
    begin match !Log.alloc_stream with
      None -> ()
    | Some fh -> Colour.dump_allocation fh
    end;
    let prog = Insn.substitute_vars prog in
    (* Now, prog is in "reverse" order, i.e. the head of the list contains the
       last instruction (and the head of each nested scope contains the last
       instruction of that scope.  *)
    let cooked_prog, _, env  = Layout.iterate_layout origin prog defines in
    (* Iterating layout puts the program in the correct order (i.e. with the
       head of the insn list as the start of the program).  *)
    let cooked_prog' = Synthbranch.expand_synth_branch cooked_prog origin [env]
			 ~verbose:!noisy in
    ignore (Encode.encode_prog origin [env] cooked_prog' !outfile)
  with Line.AssemblyError (err, line) ->
    Printf.fprintf stderr "%s at %s\n" err line;
    return_code := 1
  | Line.NonLineError err ->
    Printf.fprintf stderr "%s\n" err;
    return_code := 1
  | Line.ParseError ->
    return_code := 1
  end;
  Log.close_alloc ();
  if !return_code <> 0 then
    exit !return_code
