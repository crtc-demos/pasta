type const_expr =
    Int of int32
  | Plus of const_expr * const_expr
  | Minus of const_expr * const_expr
  | Times of const_expr * const_expr
  | Divide of const_expr * const_expr
  | Uminus of const_expr
  | LoByte of const_expr
  | HiByte of const_expr
  | ExLabel of string
  | VarRef of string list

exception Label_not_found of string

let rec map_expr fn = function
    (Int _ | ExLabel _ | VarRef _) as e -> fn e
  | Plus (a, b) -> fn (Plus (map_expr fn a, map_expr fn b))
  | Minus (a, b) -> fn (Minus (map_expr fn a, map_expr fn b))
  | Times (a, b) -> fn (Times (map_expr fn a, map_expr fn b))
  | Divide (a, b) -> fn (Divide (map_expr fn a, map_expr fn b))
  | Uminus a -> fn (Uminus (map_expr fn a))
  | LoByte a -> fn (LoByte (map_expr fn a))
  | HiByte a -> fn (HiByte (map_expr fn a))

exception UnknownMacroArg of string

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
    (function
        VarRef [name] ->
	  begin try
	    Hashtbl.find f2a name
	  with Not_found ->
	    raise (UnknownMacroArg name)
	  end
      | x -> x)
    expr

exception UnknownVariable of string

let eval ?env expr =
  let rec eval' = function
      Int i -> i
    | Plus (a, b) -> Int32.add (eval' a) (eval' b)
    | Minus (a, b) -> Int32.sub (eval' a) (eval' b)
    | Times (a, b) -> Int32.mul (eval' a) (eval' b)
    | Divide (a, b) -> Int32.div (eval' a) (eval' b)
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
    | VarRef vl -> raise (UnknownVariable (String.concat "." vl)) in
  eval' expr

let eval_int ?env expr =
  Int32.to_int (eval ?env expr)

let to_string expr =
  let buf = Buffer.create 80 in
  let app s = Buffer.add_string buf s
  and appc c = Buffer.add_char buf c in
  let rec emit = function
    Int i -> app (Int32.to_string i)
  | Plus (a, b) -> appc '('; emit a; appc '+'; emit b; appc ')'
  | Minus (a, b) -> appc '('; emit a; appc '-'; emit b; appc ')'
  | Times (a, b) -> appc '('; emit a; appc '*'; emit b; appc ')'
  | Divide (a, b) -> appc '('; emit a; appc '/'; emit b; appc ')'
  | Uminus a -> app "-("; emit a; appc ')'
  | HiByte a -> app ">("; emit a; appc ')'
  | LoByte a -> app "<("; emit a; appc ')'
  | ExLabel lab -> app lab
  | VarRef vl -> app (String.concat "." vl) in
  emit expr;
  Buffer.contents buf
