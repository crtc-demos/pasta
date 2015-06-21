let parse_file input_name =
  let inf = open_in input_name in
  let stdinbuf = Lexing.from_channel inf in
  let frags = try
    Parser.insn_seq Lexer.token stdinbuf
  with
    (* Not sure what circumstances this might still be thrown in.
       Maybe none.  *)
    Parser.Error as exc ->
      Printf.fprintf stderr "Parse error at %s:%d: " input_name
		     (!Line.line_num);
      raise exc
  | Line.ParseError as exc ->
      Printf.fprintf stderr "Parse error at %s:%d\n" input_name
		     (!Line.line_num);
      raise exc
  | Failure _ as exc ->
      Printf.fprintf stderr "Error at %s:%d: " input_name (!Line.line_num);
      raise exc in
  close_in inf;
  frags
