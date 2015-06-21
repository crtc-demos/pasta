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
  | Eq of const_expr * const_expr
  | Ne of const_expr * const_expr
  | Ge of const_expr * const_expr
  | Gt of const_expr * const_expr
  | Le of const_expr * const_expr
  | Lt of const_expr * const_expr
  | Geu of const_expr * const_expr
  | Gtu of const_expr * const_expr
  | Leu of const_expr * const_expr
  | Ltu of const_expr * const_expr
  | Uminus of const_expr
  | Not of const_expr
  | LoByte of const_expr
  | HiByte of const_expr
  | ExLabel of string
  | VarRef of string list
  | Defined of string

exception Label_not_found of string

let rec map_expr fn = function
    (Int _ | ExLabel _ | VarRef _ | Defined _) as e -> fn e
  | Plus (a, b) -> fn (Plus (map_expr fn a, map_expr fn b))
  | Minus (a, b) -> fn (Minus (map_expr fn a, map_expr fn b))
  | Times (a, b) -> fn (Times (map_expr fn a, map_expr fn b))
  | Divide (a, b) -> fn (Divide (map_expr fn a, map_expr fn b))
  | Uminus a -> fn (Uminus (map_expr fn a))
  | LoByte a -> fn (LoByte (map_expr fn a))
  | HiByte a -> fn (HiByte (map_expr fn a))
  | Not a -> fn (Not (map_expr fn a))
  | Eq (a, b) -> fn (Eq (map_expr fn a, map_expr fn b))
  | Ne (a, b) -> fn (Ne (map_expr fn a, map_expr fn b))
  | Gt (a, b) -> fn (Gt (map_expr fn a, map_expr fn b))
  | Lt (a, b) -> fn (Lt (map_expr fn a, map_expr fn b))
  | Ge (a, b) -> fn (Ge (map_expr fn a, map_expr fn b))
  | Le (a, b) -> fn (Le (map_expr fn a, map_expr fn b))
  | Gtu (a, b) -> fn (Gtu (map_expr fn a, map_expr fn b))
  | Ltu (a, b) -> fn (Ltu (map_expr fn a, map_expr fn b))
  | Geu (a, b) -> fn (Geu (map_expr fn a, map_expr fn b))
  | Leu (a, b) -> fn (Leu (map_expr fn a, map_expr fn b))
  | Arshift (a, b) -> fn (Arshift (map_expr fn a, map_expr fn b))
  | Rshift (a, b) -> fn (Rshift (map_expr fn a, map_expr fn b))
  | Lshift (a, b) -> fn (Lshift (map_expr fn a, map_expr fn b))
  | Eor (a, b) -> fn (Eor (map_expr fn a, map_expr fn b))
  | Or (a, b) -> fn (Or (map_expr fn a, map_expr fn b))
  | And (a, b) -> fn (And (map_expr fn a, map_expr fn b))

exception UnknownVariable of string
exception UnknownValue of string

type symval = UnknownVal
            | KnownVal of int32

let ltu a b =
  (Int32.logxor a 0x80000000l) < (Int32.logxor b 0x80000000l)

let gtu a b =
  (Int32.logxor a 0x80000000l) > (Int32.logxor b 0x80000000l)

let leu a b =
  (Int32.logxor a 0x80000000l) <= (Int32.logxor b 0x80000000l)

let geu a b =
  (Int32.logxor a 0x80000000l) >= (Int32.logxor b 0x80000000l)

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
  | Eq (a, b) -> if (eval' a) = (eval' b) then 0xffffffffl else 0l
  | Ne (a, b) -> if (eval' a) <> (eval' b) then 0xffffffffl else 0l
  | Lt (a, b) -> if (eval' a) < (eval' b) then 0xffffffffl else 0l
  | Gt (a, b) -> if (eval' a) > (eval' b) then 0xffffffffl else 0l
  | Le (a, b) -> if (eval' a) <= (eval' b) then 0xffffffffl else 0l
  | Ge (a, b) -> if (eval' a) >= (eval' b) then 0xffffffffl else 0l
  | Ltu (a, b) -> if ltu (eval' a) (eval' b) then 0xffffffffl else 0l
  | Gtu (a, b) -> if gtu (eval' a) (eval' b) then 0xffffffffl else 0l
  | Leu (a, b) -> if leu (eval' a) (eval' b) then 0xffffffffl else 0l
  | Geu (a, b) -> if geu (eval' a) (eval' b) then 0xffffffffl else 0l
  | ExLabel lab ->
      begin try
	match env with
          None -> raise Not_found
	| Some e ->
	    begin match Env.find e lab with
	      KnownVal k -> k
	    | UnknownVal -> raise (UnknownValue lab)
	    end
      with Not_found ->
	raise (Label_not_found lab)
      end
  | VarRef vl -> raise (UnknownVariable (String.concat "." vl))
  | Defined lab ->
      begin match env with
	None -> 0l
      | Some e -> if Env.mem e lab then 0xffffffffl else 0l
      end in
  eval' expr

let eval_int ?env expr =
  Int32.to_int (eval ?env expr)

let subst_labels ?env expr =
  let rec subst' = function
    Int i -> Int i
  | Plus (a, b) -> Plus (subst' a, subst' b)
  | Minus (a, b) -> Minus (subst' a, subst' b)
  | Times (a, b) -> Times (subst' a, subst' b)
  | Divide (a, b) -> Divide (subst' a, subst' b)
  | Uminus a -> Uminus (subst' a)
  | HiByte a -> HiByte (subst' a)
  | LoByte a -> LoByte (subst' a)
  | Not a -> Not (subst' a)
  | Arshift (a, b) -> Arshift (subst' a, subst' b)
  | Rshift (a, b) -> Rshift (subst' a, subst' b)
  | Lshift (a, b) -> Lshift (subst' a, subst' b)
  | Eq (a, b) -> Eq (subst' a, subst' b)
  | Ne (a, b) -> Ne (subst' a, subst' b)
  | Ge (a, b) -> Ge (subst' a, subst' b)
  | Le (a, b) -> Le (subst' a, subst' b)
  | Gt (a, b) -> Gt (subst' a, subst' b)
  | Lt (a, b) -> Lt (subst' a, subst' b)
  | Defined l -> Defined l
  | Geu (a, b) -> Geu (subst' a, subst' b)
  | Leu (a, b) -> Leu (subst' a, subst' b)
  | Gtu (a, b) -> Gtu (subst' a, subst' b)
  | Ltu (a, b) -> Ltu (subst' a, subst' b)
  | Eor (a, b) -> Eor (subst' a, subst' b)
  | Or (a, b) -> Or (subst' a, subst' b)
  | And (a, b) -> And (subst' a, subst' b)
  | ExLabel lab ->
      begin try
        match env with
	  None -> raise Not_found
	| Some e ->
	    begin match Env.find e lab with
	      KnownVal k -> Int k
	    | UnknownVal -> raise (UnknownValue lab)
	    end
      with Not_found ->
        raise (Label_not_found lab)
      end
  | VarRef vl -> raise (UnknownVariable (String.concat "." vl)) in
  subst' expr

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
  | Eq (a, b) -> appc '('; emit a; appc '='; emit b; appc ')'
  | Ne (a, b) -> appc '('; emit a; app "!="; emit b; appc ')'
  | Lt (a, b) -> appc '('; emit a; appc '<'; emit b; appc ')'
  | Gt (a, b) -> appc '('; emit a; appc '>'; emit b; appc ')'
  | Le (a, b) -> appc '('; emit a; app "<="; emit b; appc ')'
  | Ge (a, b) -> appc '('; emit a; app ">="; emit b; appc ')'
  | Ltu (a, b) -> appc '('; emit a; app "<u"; emit b; appc ')'
  | Gtu (a, b) -> appc '('; emit a; app ">u"; emit b; appc ')'
  | Leu (a, b) -> appc '('; emit a; app "<=u"; emit b; appc ')'
  | Geu (a, b) -> appc '('; emit a; app ">=u"; emit b; appc ')'
  | Defined l -> app "defined("; app l; appc ')'
  | Uminus a -> app "-("; emit a; appc ')'
  | Not a -> app "~("; emit a; appc ')'
  | HiByte a -> app ">("; emit a; appc ')'
  | LoByte a -> app "<("; emit a; appc ')'
  | ExLabel lab -> app lab
  | VarRef vl -> app (String.concat "." vl) in
  emit expr;
  Buffer.contents buf
