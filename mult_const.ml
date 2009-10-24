(* Multiply by constant can be made from:

   - shift-left by n (n close to m*8 are more efficient)
   - addition of terms
   - subtraction of terms

  Current algorithm does a greedy search over the solution tree, and most likely
  will give non-optimal results quite a lot of the time.
  Support for repeated patterns of bits is probably possible to add, which will
  give improvements in some cases.
  Optimal solution in the general case is an open problem, apparently.
*)


type term = {
  dest : string;
  oper : op;
  bits : int32;
  cost : int;
  sum : bool
}

and op = Add of string * string
       | Sub of string * string
       | Shift of string * int
       | Src of string

let print_oper = function
    Add(a,b) -> a ^ " + " ^ b
  | Sub(a,b) -> a ^ " - " ^ b
  | Shift(a,b) -> a ^ " << " ^ (string_of_int b)
  | Src(a) -> a

let print_terms ts =
  List.iter
    (fun { dest = d; oper = o; cost = c; bits = b; sum = s } ->
       Printf.printf "%s := %s  (cost=%d, bits=%.8lx, sum=%b)\n"
         d (print_oper o) c b s)
    ts

let shift_cost n =
  let wholebytes = if n > 7 then 1 else 0 in
  abs (((n + 4) mod 8) - 4) + wholebytes
  
let oper_cost = function
    Shift (_, n) -> shift_cost n
  | Add _ -> 1
  | Sub _ -> 1
  | Src _ -> 0

let tmpno = ref 0

let mktmp () =
  incr tmpno;
  "$t" ^ (string_of_int !tmpno)

let lo_span const =
  let rec scan n lo count =
    let next_n = Int32.shift_left n 1 in
    let bitset = (Int32.logand const n) <> 0l in
    match lo with
      None ->
        let lo' = if bitset then Some n else None in scan next_n lo' 1
    | Some lo' ->
        if bitset then scan next_n lo (count + 1) else Some (lo', count)
  in
    if const = 0l then None else scan 1l None 0

let mask_out const lo_bit bits =
  let hi_bit = if bits > 31 then 0l else Int32.shift_left lo_bit bits in
  let mask = Int32.sub hi_bit lo_bit in
  Int32.logand const (Int32.lognot mask)

let span_list const =
  let rec build c =
    match lo_span c with
      None -> []
    | Some ((lo_bit, numbits) as e) -> e :: build (mask_out c lo_bit numbits)
  in
    build const

let ffs x =
  let step x mask n s =
    match (Int32.logand x mask) with
      0l -> Int32.shift_right_logical x s, n + s
    | _ -> x, n in
  if x = 0l then 32 else
  let x, n = step x 0x0000ffffl 1 16 in
  let x, n = step x 0x000000ffl n 8 in
  let x, n = step x 0x0000000fl n 4 in
  let x, n = step x 0x00000003l n 2 in
  n - (Int32.to_int (Int32.logand x 1l))

let popc x =
  let x = Int32.sub x (Int32.logand (Int32.shift_right_logical x 1)
                                    0x55555555l) in
  let x = Int32.add (Int32.logand x 0x33333333l)
                    (Int32.logand (Int32.shift_right_logical x 2)
                                  0x33333333l) in
  let x = Int32.logand (Int32.add x (Int32.shift_right_logical x 4))
                       0x0f0f0f0fl in
  let x = Int32.add x (Int32.shift_right_logical x 8) in
  let x = Int32.add x (Int32.shift_right_logical x 16) in
  Int32.to_int (Int32.logand x 0x3fl)

let minimise_shift_cost terms shift =
  let rec scan minimum fromterm shiftfrom terms =
    match terms with
      [] ->
        begin match minimum with
          Some m -> m, fromterm, shiftfrom
        | None -> failwith "Couldn't minimise shift cost"
        end
    | term::rest ->
        begin match term.oper with
          Shift (src, _) when (popc term.bits) = 1 ->
            let prev_shift = ffs term.bits in
            let diff = shift - prev_shift in
            if diff >= 0 then begin
              let curr = shift_cost diff in
              match minimum with
                None ->
                  scan (Some curr) term.dest diff rest
              | (Some minimum) as some_min ->
                  if curr < minimum then
                    scan (Some curr) term.dest diff rest
                  else
                    scan some_min fromterm shiftfrom rest
            end else
              scan minimum fromterm shiftfrom rest
        | Src src ->
            let srccost = shift_cost shift in
            begin match minimum with
              None -> scan (Some srccost) term.dest shift rest
            | (Some minimum) as some_min ->
                if srccost < minimum then
                  scan (Some srccost) term.dest shift rest
                else
                  scan some_min fromterm shiftfrom rest
            end
        | _ -> scan minimum fromterm shiftfrom rest
        end
  in
    scan None "" (-1) terms

let terms_from_split_span terms lo_bit bits =
  let rec build bit n =
    match n with
      0 -> []
    | n ->
        let shift_for_bit = ffs bit in
        let mincost, fromterm, shiftfrom =
          minimise_shift_cost terms shift_for_bit in
        let term = {
          dest = mktmp ();
          oper = Shift (fromterm, shiftfrom);
          cost = mincost;
          bits = bit;
          sum = true } in
        term :: build (Int32.shift_left bit 1) (n - 1)
  in
    build lo_bit bits

let terms_from_sub_span terms lo_bit bits =
  let hi_bit = Int32.shift_left lo_bit bits in
  let locost, fromlo, shiftlo =
    minimise_shift_cost terms (ffs lo_bit) in
  let loterm = {
    dest = mktmp ();
    oper = Shift (fromlo, shiftlo);
    cost = locost;
    bits = lo_bit;
    sum = false } in
  let hicost, fromhi, shifthi =
    minimise_shift_cost (loterm::terms) (ffs hi_bit) in
  let hiterm = {
    dest = mktmp ();
    oper = Shift (fromhi, shifthi);
    cost = hicost;
    bits = hi_bit;
    sum = false } in
  { dest = mktmp ();
    oper = Sub (hiterm.dest, loterm.dest);
    cost = 1;
    bits = Int32.sub hi_bit lo_bit;
    sum = true } :: hiterm :: [loterm]

let rec select_term terms const =
  let t = List.fold_right
    (fun term lo ->
      match term.oper with
        Src _ -> lo
      | _ -> if term.cost < lo.cost then term else lo)
    terms
    (List.hd terms)
  in
    if (Int32.logand const t.bits) = 0l || not t.sum then
      select_term (List.filter (fun x -> t <> x) terms) const
    else
      t

let select_sub_span spans const =
  let rec chomp maxi best_lobit best_bits = function
    [] -> best_lobit, best_bits
  | (lo_bit, bits)::rest ->
      let hi_bit = Int32.shift_left lo_bit bits in
      let mask = Int32.sub hi_bit lo_bit in
      let thisbite = popc (Int32.logand const mask) in
      if thisbite > maxi then
        chomp thisbite lo_bit bits rest
      else
        chomp maxi best_lobit best_bits rest
  in
    chomp 0 0l 0 spans

let print_span_list sl =
  List.iter
    (fun (lo_bit, bits) -> Printf.printf "lo: %.8lx  bits: %d\n" lo_bit bits)
    sl

let list_cost foo =
  List.fold_right (fun x sum -> x.cost + sum) foo 0

(* During build_terms, we don't know the cost of the additions at the end.
   That's not quite right, and is the reason for the "+1" fudge factor in the
   try_split function.  *)

let rec build_terms terms const cost =
  let try_split spans =
    let split_terms = List.flatten
      (List.map
        (fun (lo_bit, bits) -> terms_from_split_span terms lo_bit bits)
        spans) in
    let next_terms = split_terms @ terms in
    let least_cost = select_term next_terms const in
    let const_minus_bit = Int32.logand const (Int32.lognot least_cost.bits) in
    build_terms (least_cost::terms) const_minus_bit
                (cost + least_cost.cost + 1) in

  let try_sub spans =
    let lo_bit, bits = select_sub_span spans const in
    let sub_term = terms_from_sub_span terms lo_bit bits in
    let sub_part = List.hd sub_term in
    let const_minus_bits = Int32.logand const (Int32.lognot sub_part.bits) in
    build_terms (sub_term @ terms) const_minus_bits
                (cost + list_cost sub_term) in

  if const = 0l then
    terms, cost
  else begin
    let spans = span_list const in
    let split_terms, split_cost = try_split spans in
    let sub_terms, sub_cost = try_sub spans in
    if split_cost < sub_cost then
      split_terms, split_cost
    else
      sub_terms, sub_cost
  end

let src_term = { dest = mktmp (); oper = Src "src"; bits = 1l; cost = 0;
                 sum = false }

let mult_const const =
  let terms, _ = build_terms [src_term] const 0 in
  let sum = List.fold_right
    (fun item acc ->
      if item.sum then item::acc else acc)
    terms
    []
  in let rec sum_terms terms partial = function
    a::b::rest ->
      let newsum =
        { dest = mktmp ();
          oper = Add (a.dest, b.dest);
          cost = 1;
          bits = Int32.logor a.bits b.bits;
          sum = true } in
       sum_terms (newsum::terms) (newsum::partial) rest
  | a::rest ->
      sum_terms terms (a::partial) rest
  | [] -> terms, partial
  in let rec all_terms terms = function
    [] -> terms
  | [single] ->
      { dest = "dest";
        oper = Src single.dest;
        cost = 0;
        bits = single.bits;
        sum = false } :: terms
  | lots ->
      let terms', partial_sums = sum_terms terms [] lots in
      all_terms terms' partial_sums
  in
    List.rev (all_terms terms sum)

type eop =
    Eadd of eop * eop
  | Esub of eop * eop
  | Easl of eop
  | Elsr of eop
  | Etmp of int
  | Esrc
  | Edst

let gather_temps terms =
  let ht = Hashtbl.create 5
  and tmpno = ref 0 in
  let merge_temp tmp =
    if not (Hashtbl.mem ht tmp) && tmp.[0] = '$' then begin
      Hashtbl.add ht tmp (Etmp !tmpno);
      incr tmpno
    end in
  List.iter
    (fun term ->
      merge_temp term.dest;
      match term.oper with
        Add (a, b) -> merge_temp a; merge_temp b
      | Sub (a, b) -> merge_temp a; merge_temp b
      | Shift (a, _) -> merge_temp a
      | Src a -> merge_temp a)
    terms;
  ht

let conv_ops terms tmpht =
  let cnv x =
    match x with
      "src" -> Esrc
    | "dest" -> Edst
    | x -> Hashtbl.find tmpht x in
  List.fold_right
    (fun term acc ->
      let dst = cnv term.dest in
      match term.oper with
        Add (a, b) -> (dst, Eadd (cnv a, cnv b)) :: acc
      | Sub (a, b) -> (dst, Esub (cnv a, cnv b)) :: acc
      | Src a -> (dst, cnv a) :: acc
      | Shift (a, 0) -> (dst, cnv a) :: acc
      | Shift (a, 1) -> (dst, Easl (cnv a)) :: acc
      | Shift (a, 2) -> (dst, Easl dst) :: (dst, Easl (cnv a)) :: acc
      | Shift (a, 3) ->
          (dst, Easl dst) :: (dst, Easl dst) :: (dst, Easl (cnv a)) :: acc
      | Shift (a, 4) ->
          (dst, Easl dst) :: (dst, Easl dst) :: (dst, Easl dst)
	  :: (dst, Easl (cnv a)) :: acc)
    terms
    []

let mulc by =
  let terms = mult_const by in
  let tmpht = gather_temps terms in
  conv_ops terms tmpht
