module LabelSet = Set.Make(String)
module VarSet = Set.Make(String)

class context = object
  val mutable labels = LabelSet.empty
  val mutable vars = VarSet.empty
  
  method add_label lab =
    labels <- LabelSet.add lab labels
  
  method add_var var =
    vars <- VarSet.add var vars
    
  method defines_label lab =
    LabelSet.mem lab labels
end

class contexts = object (self)
  val contexts = (Hashtbl.create 20 : (string list, context) Hashtbl.t)
  
  method add name =
    if not (Hashtbl.mem contexts name) then
      Hashtbl.add contexts name (new context)
  
  method get name =
    try
      Hashtbl.find contexts name
    with Not_found ->
      self#add name;
      self#get name
  
  method mem name =
    Hashtbl.mem contexts name
end

let to_string ctx = String.concat "." ctx

let ctxs = new contexts
