(* A register allocator.  *)

module IntfGraph = Alloc.G

let neighbours_use nodes =
  List.fold_left
    (fun acc node ->
      let nctx, nvarname = node in
      try
	let nvar = nctx#get_var nvarname in
	(nvar#get_loc, nvar#get_size) :: acc
      with Not_found -> acc)
    []
    nodes

let alloc intf regpool =
  let min_degree graph =
    IntfGraph.fold_node
      (fun node edges lo ->
        match lo with
          None -> Some (node, IntfGraph.degree_i edges)
        | Some (_, lo_deg) ->
            let new_deg = IntfGraph.degree_i edges in
            if new_deg < lo_deg then Some (node, new_deg) else lo)
      graph
      None
  in let rec remove_nodes intf spilled =
    match min_degree intf with
      None -> spilled
    | Some (node, md) ->
        let without_node = IntfGraph.remove_node node intf in
        let spilled' = remove_nodes without_node spilled in
        (* The node's neighbours when it is put back in.  *)
        let neighbours = IntfGraph.connected node intf in
	let exclude_regs = neighbours_use neighbours in
        try
	  let (nctx, nvar) = node in
	  let var = nctx#get_var nvar in
          let chosen = regpool#choose_reg exclude_regs var#get_size in
	  var#set_loc chosen;
	  spilled'
        with
          Temps.Spill -> node :: spilled'
  in
    remove_nodes intf []

let dump_allocation fh =
  Context.ctxs#iter
    (fun ctxname ctx ->
      ctx#fold_vars
        (fun name var () ->
          try
	    Printf.fprintf fh "Var %s.%s (size %d) assigned to %x\n"
	      (Context.to_string ctxname) name var#get_size var#get_loc
	  with Not_found ->
	    Printf.fprintf fh "Variable %s.%s\n" (Context.to_string ctxname)
	      name)
	())
    
