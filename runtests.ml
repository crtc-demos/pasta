(* Run tests for pasta.  *)

open Kaputt

exception Error_code of int

let invoke_pasta srcfile =
  let open Shell in
  let outfile = chop_suffix srcfile ".s" in
  let cmd = command (Printf.sprintf "./pasta %s -o %s" srcfile outfile) in
  let redir_err = redirect_error cmd "pasta_err" in
  let rc = run redir_err in
  Pervasives.ignore (run (rm ~options:["-f"] [outfile; "pasta_err"]));
  if rc <> 0 then raise (Error_code rc)

type simple_test =
  {
    title : string;
    form : [`Good | `Bad];
    src : string
  }

let test_list = [
  { title = "aliases"; form = `Good; src = "tests/alias.s" };
  { title = "ascii"; form = `Good; src = "tests/ascii.s" };
  { title = "out-of-range indirect-y addressing"; form = `Bad;
    src = "tests/bad-ind-y.s" };
  { title = "bad addressing mode"; form = `Bad; src = "tests/badinsn.s" };
  { title = "call out of context"; form = `Bad; src = "tests/cannot-call.s" };
  { title = "contexts"; form = `Good; src = "tests/contexts.s" };
  { title = "indirect jump out of context"; form = `Bad;
    src = "tests/ctx-ind-jump.s" };
  { title = "data definitions"; form = `Good; src = "tests/data.s" };
  { title = "flimsy FORTHish macros"; form = `Good; src = "tests/flimsy.s" };
  { title = "indirect jumps"; form = `Good; src = "tests/ind-jump.s" };
  { title = "labels"; form = `Good; src = "tests/labels.s" };
  { title = "linefeeds/colons"; form = `Good; src = "tests/linefeed.s" };
  { title = "duplicate labels"; form = `Bad; src = "tests/duplicate_labels.s" };
  { title = "local data labels"; form = `Good;
    src = "tests/local_data_labels.s"};
  { title = "synthesized conditional branch"; form = `Good;
    src = "tests/long-cbra.s" };
  { title = "macros"; form = `Good; src = "tests/macro.s" };
  { title = "macros 2"; form = `Bad; src = "tests/macro2.s" };
  { title = "macros 3"; form = `Bad; src = "tests/macro3.s" };
  { title = "macros 4"; form = `Bad; src = "tests/macro4.s" };
  { title = "macros 5"; form = `Good; src = "tests/macro5.s" };
  { title = "macros 6"; form = `Good; src = "tests/macro6.s" };
  { title = "macros 7"; form = `Good; src = "tests/macro7.s" };
  { title = "x5 using contexts"; form = `Good; src = "tests/multby5.s" };
  { title = "context with no entry point"; form = `Bad;
    src = "tests/no-entry-pt.s" };
  { title = "zero-page var overflow"; form = `Bad; src = "tests/no-space.s" };
  { title = "origin"; form = `Good; src = "tests/origin.s" };
  { title = "pastabug"; form = `Good; src = "tests/pastabug.s" };
  { title = "parse error"; form = `Bad; src = "tests/perror.s" };
  { title = "precedence"; form = `Good; src = "tests/precedence.s" };
  { title = "rol variations"; form = `Good; src = "tests/rol.s" };
  { title = "shadowing definitions in scope"; form = `Good;
    src = "tests/scope.s" };
  { title = "labels invisible outside scope"; form = `Bad;
    src = "tests/scope2.s" };
  { title = "misc test"; form = `Good; src = "tests/test.s" }
]

let _ =
  List.iter
    (fun stest ->
      Test.add_simple_test
        ~title:stest.title
	(fun () ->
	  match stest.form with
            `Bad -> Assertion.raises (fun () -> invoke_pasta stest.src)
	  | `Good -> Assertion.no_raise (fun () -> invoke_pasta stest.src)))
    test_list

let _ =
  Test.launch_tests ()
