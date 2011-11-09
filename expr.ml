type const_expr =
    Int of int32
  | Plus of const_expr * const_expr
  | Minus of const_expr * const_expr
  | Times of const_expr * const_expr
  | Divide of const_expr * const_expr
  | And of const_expr * const_expr
  | Or of const_expr * const_expr
  | Eor of const_expr * const_expr
  | Lshift of const_expr * const_expr
  | Rshift of const_expr * const_expr
  | Arshift of const_expr * const_expr
  | Uminus of const_expr
  | Not of const_expr
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
  | Not a -> fn (Not (map_expr fn a))
  | Arshift (a, b) -> fn (Arshift (map_expr fn a, map_expr fn b))
  | Rshift (a, b) -> fn (Rshift (map_expr fn a, map_expr fn b))
  | Lshift (a, b) -> fn (Lshift (map_expr fn a, map_expr fn b))
  | Eor (a, b) -> fn (Eor (map_expr fn a, map_expr fn b))
  | Or (a, b) -> fn (Or (map_expr fn a, map_expr fn b))
  | And (a, b) -> fn (And (map_expr fn a, map_expr fn b))

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
    | Not a -> Int32.lognot (eval' a)
    | Arshift (a, b) -> Int32.shift_right (eval' a) (Int32.to_int (eval' b))
    | Rshift (a, b) ->
        Int32.shift_right_logical (eval' a) (Int32.to_int (eval' b))
    | Lshift (a, b) -> Int32.shift_left (eval' a) (Int32.to_int (eval' b))
    | Eor (a, b) -> Int32.logxor (eval' a) (eval' b)
    | Or (a, b) -> Int32.logor (eval' a) (eval' b)
    | And (a, b) -> Int32.logand (eval' a) (eval' b)
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
  | Arshift (a, b) -> appc '('; emit a; app ">>>"; emit b; appc ')'
  | Rshift (a, b) -> appc '('; emit a; app ">>"; emit b; appc ')'
  | Lshift (a, b) -> appc '('; emit a; app ">>"; emit b; appc ')'
  | Eor (a, b) -> appc '('; emit a; appc '^'; emit b; appc ')'
  | Or (a, b) -> appc '('; emit a; appc '|'; emit b; appc ')'
  | And (a, b) -> appc '('; emit a; appc '&'; emit b; appc ')'
  | Uminus a -> app "-("; emit a; appc ')'
  | Not a -> app "~("; emit a; appc ')'
  | HiByte a -> app ">("; emit a; appc ')'
  | LoByte a -> app "<("; emit a; appc ')'
  | ExLabel lab -> app lab
  | VarRef vl -> app (String.concat "." vl) in
  emit expr;
  Buffer.contents buf
