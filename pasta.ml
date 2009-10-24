let collect_insns prog =
  List.fold_right
    (fun frag acc ->
      match frag with
        Insn.Macrodef _
      | Insn.DeclVars _
      | Insn.Temps _ -> acc
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
        Insn.DeclVars (sz, names) -> ()
      | _ -> ())
    prog;
  ()

(* Find the origin.  Return (origin, prog') where prog' is the input program
   with .org directives removed.
   If this is called with "prog" in reverse order, the first .org will be the
   one used, and later settings will be ignored.  Could be an error instead.  *)

let extract_origin prog =
  List.fold_right
    (fun insn (org, insns) ->
      match insn with
        Insn.Origin x -> Int32.to_int (Expr.eval x), insns
      | x -> org, x::insns)
    prog
    (0, [])

let _ =
  let infile = ref "" and outfile = ref "a.out" in
  let argspec =
    ["-o", Arg.Set_string outfile, "Set output file"]
  and usage = "Usage: pasta -o <output> <input>" in
  Arg.parse argspec (fun i -> infile := i) usage;
  if !infile = "" then begin
    Arg.usage argspec usage;
    exit 1
  end;
  let inf = open_in !infile in
  let stdinbuf = Lexing.from_channel inf in
  let frags =
    try
      Parser.insn_seq Lexer.token stdinbuf
    with
      Parser.Error as exc ->
        Printf.fprintf stderr "Parse error at line %d: " (!Lexer.line_num);
	raise exc
    | Failure _ as exc ->
        Printf.fprintf stderr "Error at line %d: " (!Lexer.line_num);
	raise exc
    in
  let prog = collect_insns frags in
  let macros = collect_macros frags in
  let prog = Insn.invoke_macros prog macros in
  let origin, prog = extract_origin prog in
  collect_contexts frags;
  collect_notemps frags;
  Insn.find_dependencies frags;
  let contexts = collect_vars frags in
  (* Now, prog is in "reverse" order, i.e. the head of the list contains the
     last instruction (and the head of each nested scope contains the last
     instruction of that scope.  *)
  let cooked_prog, _, env  = Layout.iterate_layout origin prog in
  (* Iterating layout puts the program in the correct order (i.e. with the head
     of the insn list as the start of the program).  *)
  ignore (Encode.encode_prog origin [env] cooked_prog !outfile);
  close_in inf
