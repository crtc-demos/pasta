open Context
open Var

module Node = struct
  type t = context * string

  let compare (c1, v1) (c2, v2) =
    let ctx_cmp = Pervasives.compare c1 c2 in
    if ctx_cmp <> 0 then
      ctx_cmp
    else
      compare v1 v2
end

module G = Graph.Make(Node)

let build_graph () =
  ctxs#fold
    (fun ctxname ctx acc -> ctx#fold_vars
      (fun var _ acc ->
        (* Interferes with stuff this context calls...  *)
        let others = ctx#fold_calls
	(fun dctxid acc ->
	  let dctx = ctxs#get dctxid in
	  dctx#fold_vars
	    (fun dvar _ acc -> G.add (ctx, var) (dctx, dvar) acc)
	    acc)
	acc in
	(* Also interfere with other variables in same context.  *)
	ctx#fold_vars
	  (fun dvar _ acc -> G.add (ctx, var) (ctx, dvar) acc)
	  others)
      acc)
    G.empty

exception BadVarRef of string

let split = function
    [ctx; var] -> ctx, var
  | x -> raise (BadVarRef (String.concat "." x))

(* Augment interference list with interferences marked with explicit directives
   in source.  Won't handle nested contexts.  This won't be necessary if I
   implement proper dataflow analysis.  *)

let add_explicit_interf graph intflist =
  List.fold_left
    (fun graph (a, b) ->
      let (ctx1id, var1) = split a and (ctx2id, var2) = split b in
      let ctx1 = ctxs#get [ctx1id] and ctx2 = ctxs#get [ctx2id] in
      G.add (ctx1, var1) (ctx2, var2) graph)
    graph
    intflist

(* Add protection for variables of a "foreign" context throughout a particular
   context.  This means that other function calls within the context should be
   prevented from clobbering those variables.  *)

let add_protection graph prots =
  List.fold_left
    (fun graph (inctxid, prot) ->
      let inctx = ctxs#get inctxid
      and protctxid, protvar = split prot in
      let protctx = ctxs#get [protctxid] in
      inctx#fold_calls
        (fun dctxid graph ->
	  let dctx = ctxs#get dctxid in
	  dctx#fold_vars
	    (fun varname _ graph ->
	      G.add (protctx, protvar) (dctx, varname) graph)
	    graph)
	graph)
    graph
    prots

let print_node (nctx, nvar) =
  Printf.sprintf "%s.%s" (nctx#get_name) nvar

let print_graph fh igraph =
  G.fold_node
    (fun src targs_i () ->
      let connected = G.connected_i targs_i in
      List.iter
        (fun targ -> Printf.fprintf fh "%s interferes with %s\n"
				    (print_node src) (print_node targ))
	connected)
    igraph
    ()
