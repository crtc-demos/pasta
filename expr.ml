type const_expr =
    Int of int32
  | Plus of const_expr * const_expr
  | Minus of const_expr * const_expr
  | Uminus of const_expr
  | LoByte of const_expr
  | HiByte of const_expr
  | ExLabel of string
  | MacroArg of string

exception Label_not_found of string

let rec map_expr fn = function
    (Int _ | ExLabel _ | MacroArg _) as e -> fn e
  | Plus (a, b) -> fn (Plus (map_expr fn a, map_expr fn b))
  | Minus (a, b) -> fn (Minus (map_expr fn a, map_expr fn b))
  | Uminus a -> fn (Uminus (map_expr fn a))
  | LoByte a -> fn (LoByte (map_expr fn a))
  | HiByte a -> fn (HiByte (map_expr fn a))

let subst_macro_args expr formal_args actual_args =
  let f2a = Hashtbl.create 5 in
  let rec build_hash form act =
    match form, act with
      [], [] -> ()
    | [], _ -> failwith "Too many actual parameters"
    | _, [] -> failwith "Not enough actual parameters"
    | f::forms, a::acts -> Hashtbl.add f2a f a; build_hash forms acts in
  build_hash formal_args actual_args;
  map_expr
    (function MacroArg name -> Hashtbl.find f2a name | x -> x)
    expr

let eval ?env expr =
  let rec eval' = function
      Int i -> i
    | Plus (a, b) -> Int32.add (eval' a) (eval' b)
    | Minus (a, b) -> Int32.sub (eval' a) (eval' b)
    | Uminus a -> Int32.neg (eval' a)
    | HiByte a -> Int32.logand (Int32.shift_right_logical (eval' a) 8) 0xffl
    | LoByte a -> Int32.logand (eval' a) 0xffl
    | ExLabel lab ->
	begin try
	  match env with
            None -> raise Not_found
	  | Some e -> Env.find e lab
	with Not_found ->
	  raise (Label_not_found lab)
	end
    | MacroArg _ -> failwith "Can't evaluate macro arg" in
  eval' expr

let eval_int ?env expr =
  Int32.to_int (eval ?env expr)
