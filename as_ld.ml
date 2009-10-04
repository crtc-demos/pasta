let collect_insns prog =
  List.fold_right
    (fun frag acc ->
      match frag with
        Insn.Macrodef _ -> acc
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

let _ =
  let inf = open_in Sys.argv.(1) in
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
  let cooked_prog, _, env  = Layout.iterate_layout 0 prog in
  ignore (Encode.encode_prog 0 env (List.rev cooked_prog));
  close_in inf
