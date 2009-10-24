(* A functional graph. Needlessly generic.

   Should be able to handle the queries:

    * do nodes x and y have an edge between them?
    * how many neighbours does node x have?
    * get list of neighbours for a node

   And (non-destructive) operations:

    * Add edge x, y (allowing self-links)
    * Remove edge x, y
    * number of neighbours
    * delete node & all connections to/from it
  
   All operations are symmetric, i.e. this is an undirected graph.
*)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type node
    type edges
    type t
    val empty : t
    val is_empty : t -> bool
    val add : node -> node -> t -> t
    val add_node : node -> t -> t
    val remove : node -> node -> t -> t
    val has_edge : node -> node -> t -> bool
    val degree : node -> t -> int
    val remove_node : node -> t -> t
    val connected : node -> t -> node list
    val get_edges : node -> t -> edges
    val degree_i : edges -> int
    val connected_i : edges -> node list
    val fold_node : (node -> edges -> 'a -> 'a) -> t -> 'a -> 'a
  end

module Make(Ord: OrderedType) = struct
  type node = Ord.t
    
  module BSet = Set.Make(Ord)
  
  module BMap = Map.Make(Ord)
  
  type t = BSet.t BMap.t
  
  type edges = BSet.t
  
  let empty = BMap.empty
  
  let is_empty = BMap.is_empty
  
  let add_oneway a b intf =
    try
      let set = BMap.find a intf in
      let set' = BSet.add b set in
      BMap.add a set' intf
    with Not_found ->
      BMap.add a (BSet.singleton b) intf
  
  let add a b intf =
    if (Ord.compare a b) <> 0 then
      add_oneway a b (add_oneway b a intf)
    else
      add_oneway a b intf
  
  (* Add an unconnected node to the graph.  *)
  let add_node a intf =
    if not (BMap.mem a intf) then
      BMap.add a BSet.empty intf
    else
      intf
  
  (* Removes empty sets from the mapping.  *)
  let remove_oneway a b intf =
    let set = BMap.find a intf in
    let set' = BSet.remove b set in
    (* Add set of neighbours without A.  *)
      BMap.add a set' intf

  let remove a b intf =
    if (Ord.compare a b) <> 0 then
      remove_oneway a b (remove_oneway b a intf)
    else
      remove_oneway a b intf
  
  let has_edge a b intf =
    try
      let set = BMap.find a intf in
      BSet.mem b set
    with Not_found -> false
  
  let degree a graph =
    let set = BMap.find a graph in
    BSet.cardinal set
  
  let remove_node a graph =
    let set = BMap.find a graph in
    let in_removed = BSet.fold (fun e g -> remove_oneway e a g)
                               set graph in
    BMap.remove a in_removed
  
  let connected a graph =
    let set = BMap.find a graph in
    BSet.elements set
  
  let get_edges a graph =
    BMap.find a graph

  (* 'internal' degree: from the internal representation of a node's
     connectivity, return the degree.  *)
  let degree_i set = BSet.cardinal set

  (* 'internal' connected: from the internal representation of a node's
     connectivity, return a list of the connected edges.  *)
  let connected_i set = BSet.elements set
  
  let fold_node fn graph a =
    BMap.fold fn graph a
end

