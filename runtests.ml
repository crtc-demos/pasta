(* Run tests for pasta.  *)

open Kaputt

exception Error_code of int
exception Bad_error

let invoke_pasta ?check_error:check_error_opt srcfile =
  let open Shell in
  let outfile = chop_suffix srcfile ".s" in
  let cleanup () =
    Pervasives.ignore (run (rm ~options:["-f"] [outfile; "pasta_err"])) in
  let cmd = command (Printf.sprintf "./pasta %s -o %s" srcfile outfile) in
  let redir_err = redirect_error cmd "pasta_err" in
  let rc = run redir_err in
  begin match check_error_opt with
    None -> ()
  | Some check_error ->
      let fh = open_in "pasta_err" in
      let errline = try input_line fh with End_of_file -> "" in
      let pass = check_error errline in
      close_in fh;
      if not pass then begin
        cleanup ();
	raise Bad_error
      end
  end;
  cleanup ();
  if rc <> 0 then raise (Error_code rc)

type simple_test =
  {
    title : string;
    form : [`Good | `Bad | `Check_err of string -> bool];
    src : string
  }

let match_regexp re str =
  let comp_re = Str.regexp re in
  Str.string_match comp_re str 0

let test_list = [
  { title = "aliases"; form = `Good; src = "tests/alias.s" };
  { title = "ascii"; form = `Good; src = "tests/ascii.s" };
  { title = "out-of-range indirect-y addressing";
    form = `Check_err (match_regexp "Address \\$100 out of range at [^:]+:3");
    src = "tests/bad-ind-y.s" };
  { title = "bad addressing mode"; form = `Check_err (match_regexp
      "Bad addressing mode 'zeropage or absolute' at [^:]+:3");
    src = "tests/badinsn.s" };
  { title = "call out of context"; form = `Check_err (match_regexp
      "Context 'foo' may not call 'bar' (which is not in a context) \
       at [^:]+:5");
    src = "tests/cannot-call.s" };
  { title = "contexts"; form = `Good; src = "tests/contexts.s" };
  { title = "indirect jump out of context"; form = `Check_err (match_regexp
      "Context 'foo' has unsupported jump at [^:]+:3");
    src = "tests/ctx-ind-jump.s" };
  { title = "data definitions"; form = `Good; src = "tests/data.s" };
  { title = "flimsy FORTHish macros"; form = `Good; src = "tests/flimsy.s" };
  { title = "indirect jumps"; form = `Good; src = "tests/ind-jump.s" };
  { title = "labels"; form = `Good; src = "tests/labels.s" };
  { title = "linefeeds/colons"; form = `Good; src = "tests/linefeed.s" };
  { title = "duplicate labels"; form = `Check_err (match_regexp
      "Multiple label or alias definition 'foo' at [^:]+:4");
    src = "tests/duplicate_labels.s" };
  { title = "duplicate macros"; form = `Check_err (match_regexp
      "Multiple macro definition 'foo' at [^:]+:5");
    src = "tests/duplicate_macros.s" };
  { title = "duplicate origin"; form = `Check_err (match_regexp
      "Multiple origin definition '.org' at [^:]+:2");
    src = "tests/duplicate_origin.s" };
  { title = "duplicate contexts"; form = `Check_err (match_regexp
      "Multiple context definition 'foo' at [^:]+:7");
    src = "tests/duplicate_ctx.s" };
  { title = "duplicate aliases"; form = `Check_err (match_regexp
      "Multiple label or alias definition 'foo' at [^:]+:2");
    src = "tests/duplicate_aliases.s" };
  { title = "dependent aliases"; form = `Good;
    src = "tests/dependent_aliases.s" };
  { title = "local data labels"; form = `Good;
    src = "tests/local_data_labels.s"};
  { title = "local dsb labels"; form = `Good;
    src = "tests/local_dsb_labels.s"};
  { title = "synthesized conditional branch"; form = `Good;
    src = "tests/long-cbra.s" };
  { title = "macros"; form = `Good; src = "tests/macro.s" };
  { title = "macros 2";
    form = `Check_err (match_regexp
      "Label 'bob' not found at [^:]+:3, expanded from [^:]+:14");
    src = "tests/macro2.s" };
  { title = "macros 3"; form = `Check_err (match_regexp
      "Unknown macro arg 'ret' at [^:]+:6"); src = "tests/macro3.s" };
  { title = "macros 4"; form = `Check_err (match_regexp
      "Label 'res' not found at [^:]+:6, expanded from [^:]+:13");
    src = "tests/macro4.s" };
  { title = "macros 5"; form = `Good; src = "tests/macro5.s" };
  { title = "macros 6"; form = `Good; src = "tests/macro6.s" };
  { title = "macros 7"; form = `Good; src = "tests/macro7.s" };
  { title = "x5 using contexts"; form = `Good; src = "tests/multby5.s" };
  { title = "context with no entry point"; form = `Check_err (match_regexp
      "No entry point in context 'foo' at [^:]+:2");
    src = "tests/no-entry-pt.s" };
  { title = "zero-page var overflow"; form = `Bad; src = "tests/no-space.s" };
  { title = "origin"; form = `Good; src = "tests/origin.s" };
  { title = "pastabug"; form = `Good; src = "tests/pastabug.s" };
  { title = "parse error"; form = `Check_err (match_regexp
      "Parse error at [^:]+:8"); src = "tests/perror.s" };
  { title = "precedence"; form = `Good; src = "tests/precedence.s" };
  { title = "rol variations"; form = `Good; src = "tests/rol.s" };
  { title = "shadowing definitions in scope"; form = `Good;
    src = "tests/scope.s" };
  { title = "labels invisible outside scope"; form = `Check_err (match_regexp
      "Label 'foo2' not found at [^:]+:11"); src = "tests/scope2.s" };
  { title = "misc test"; form = `Good; src = "tests/test.s" };
  { title = "addressing-mode macro args"; form = `Good;
    src = "tests/addrmode.s" };
  { title = "addressing-mode macro args 2"; form = `Check_err (match_regexp
      "Bad usage of macro address argument 'a' at [^:]+:12 (expanded from \
       [^:]+:16)");
    src = "tests/addrmode2.s" }
]

let _ =
  List.iter
    (fun stest ->
      Test.add_simple_test
        ~title:stest.title
	(fun () ->
	  match stest.form with
            `Bad -> Assertion.raises (fun () -> invoke_pasta stest.src)
	  | `Good -> Assertion.no_raise (fun () -> invoke_pasta stest.src)
	  | `Check_err erchk ->
	      Assertion.no_raise
	        (fun () ->
		  try invoke_pasta ~check_error:erchk stest.src
		  with (Error_code n as exc) -> if n = 0 then raise exc)))
    test_list

let _ =
  Test.launch_tests ()
