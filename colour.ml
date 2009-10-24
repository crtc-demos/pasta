(* A register allocator.  *)

module IntfGraph = Alloc.G

type place = Spill | Allocate

let alloc intf regpool =
  let min_degree graph =
    IntfGraph.fold_node
      (fun node edges lo ->
       (* Printf.printf "Alloc: node %s\n" (Id.string_of_rop node); *)
        match lo with
          None -> Some (node, IntfGraph.degree_i edges)
        | Some (_, lo_deg) ->
            let new_deg = IntfGraph.degree_i edges in
            if new_deg < lo_deg then Some (node, new_deg) else lo)
      graph
      None
  in let rec remove_nodes intf' spilled =
    match min_degree intf' with
      None -> colouring, spilled
    | Some (node, md) ->
      (*  Printf.printf "Colour: node %s, degree %d\n"
          (Id.string_of_rop node) md; *)
        let without_node = IntfGraph.remove_node node intf' in
        let cols, spilled' =
          remove_nodes without_node colouring spilled
        in
          (* The node's neighbours when it is put back in.  *)
          let neighbours = IntfGraph.connected node intf'
          and all_neighbours = IntfGraph.connected node intf in
          try
	    let (nctx, nvar) = node in
	    let var = nctx#get_var nvar in
            let chosen = regpool#choose_reg var#get_size in
	    var#set_loc chosen;
	    spilled'
          with
            Temps.Spill -> node :: spilled'
  in
    remove_nodes intf []

