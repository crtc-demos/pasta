class variable (name:string) (size:int) = object
  val name = name
  val size = size
  val mutable location = None
  
  method get_name = name

  method get_size = size
  
  method set_loc (loc:int) =
    location <- Some loc
  
  method get_loc =
    match location with
      None -> raise Not_found
    | Some x -> x
end
