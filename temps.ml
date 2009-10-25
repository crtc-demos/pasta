exception Spill

module Pool = Set.Make(struct type t = int let compare = compare end)

class temps = object
  val mutable pool = Pool.empty
  
  method add_to_pool tmpspec =
    begin match tmpspec with
      Insn.OneTemp x -> pool <- Pool.add (Expr.eval_int x) pool
    | Insn.TempRange (loex, hiex) ->
        let lo = Expr.eval_int loex
	and hi = Expr.eval_int hiex in
	for r = lo to hi do
	  pool <- Pool.add r pool
	done
    end
  
  method choose_reg exclusions sz =
    let rec scan rpool =
      match rpool with
        a::b::c::d::rest when sz = 4 && b = a + 1 && c = a + 2 && d = a + 3 ->
	  a
      | a::b::c::rest when sz = 3 && b = a + 1 && c = a + 2 ->
	  a
      | a::b::rest when sz = 2 && b = a + 1 ->
	  a
      | a::rest when sz = 1 ->
	  a
      | a::rest -> scan rest
      | [] -> raise Spill in
    let pool_minus_excluded = List.fold_left
      (fun set (alloc, size) ->
        Pool.filter (fun loc -> loc < alloc || loc >= alloc + size) set)
      pool
      exclusions in
    scan (Pool.elements pool_minus_excluded)
  
end
