module LabelSet = Set.Make(String)
module CtxSet = Set.Make (struct
			    type t = string list
			    let compare = compare
			  end)

let to_string ctx = String.concat "." ctx

class context name = object
  val name = to_string name
  val mutable labels = LabelSet.empty
  val mutable vars = Hashtbl.create 10
  val mutable calls = CtxSet.empty
  
  method add_label lab =
    labels <- LabelSet.add lab labels
  
  method add_var var sz =
    Hashtbl.add vars var (new Var.variable var sz)
  
  method get_var var =
    Hashtbl.find vars var
  
  method defines_label lab =
    LabelSet.mem lab labels
  
  method calls_context ctx =
    calls <- CtxSet.add ctx calls
 
  method call_marked dest =
    CtxSet.mem dest calls
 
  method fold_vars : 'a. (string -> Var.variable -> 'a -> 'a) -> 'a -> 'a =
    fun fn base -> Hashtbl.fold fn vars base
  
  method fold_calls : 'a. (CtxSet.elt -> 'a -> 'a) -> 'a -> 'a =
    fun fn base -> CtxSet.fold fn calls base
  
  method get_name = name
end

class contexts = object (self)
  val contexts = (Hashtbl.create 20 : (string list, context) Hashtbl.t)
  
  method add name =
    if not (Hashtbl.mem contexts name) then
      Hashtbl.add contexts name (new context name)
  
  method get name =
    try
      Hashtbl.find contexts name
    with Not_found ->
      self#add name;
      self#get name
  
  method mem name =
    Hashtbl.mem contexts name
  
  method iter fn =
    Hashtbl.iter fn contexts
  
  method fold : 'a. (string list -> context -> 'a -> 'a) -> 'a -> 'a =
    fun fn base -> Hashtbl.fold fn contexts base
  
  method transitive_closure =
    let rec iterate_closure () =
      let did_something = ref false in
      Hashtbl.iter
	(fun _ (ctx:context) ->
          ctx#fold_calls
	    (fun dctxid () -> 
	      let dctx = self#get dctxid in
	      dctx#fold_calls
		(fun ddctxid () ->
		  if not (ctx#call_marked ddctxid) then begin
		    ctx#calls_context ddctxid;
		    Printf.printf "Context %s reaches %s\n"
		      (ctx#get_name) (to_string ddctxid);
		    did_something := true
		  end)
		())
	    ())
	contexts;
      if !did_something then
        iterate_closure () in
    print_endline "Transitive closure...";
    iterate_closure ();
    print_endline "done."
end

let ctxs = new contexts
