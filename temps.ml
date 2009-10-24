exception Spill

class temps = object
  val mutable orig = []
  val mutable pool = []
  
  method add_to_pool tmpspec =
    begin match tmpspec with
      Insn.OneTemp x -> pool <- (Expr.eval_int x)::pool
    | Insn.TempRange (loex, hiex) ->
        let lo = Expr.eval_int loex
	and hi = Expr.eval_int hiex in
	for r = lo to hi do
	  pool <- r::pool
	done
    end;
    pool <- List.sort compare pool;
    orig <- pool
  
  method choose_reg sz =
    let rec scan rejected rpool =
      match rpool with
        a::b::c::d::rest when sz = 4 && b = a + 1 && c = a + 2 && d = a + 3 ->
	  a, rejected, rest
      | a::b::c::rest when sz = 3 && b = a + 1 && c = a + 2 ->
	  a, rejected, rest
      | a::b::rest when sz = 2 && b = a + 1 ->
	  a, rejected, rest
      | a::rest when sz = 1 ->
	  a, rejected, rest
      | a::rest -> scan (a::rejected) rest
      | [] -> raise Spill in
    let alloc, rejected, remainder = scan [] pool in
    pool <- rejected @ remainder;
    alloc
  
  method reset =
    pool <- orig
end
