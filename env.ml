let rec find htstack key =
  match htstack with
    [] -> raise Not_found
  | env::envs ->
      try
        Hashtbl.find env key
      with Not_found ->
        find envs key

let add htstack key valu =
  match htstack with
    [] -> raise Not_found
  | env::_ -> Hashtbl.add env key valu

let replace htstack key valu =
  match htstack with
    [] -> raise Not_found
  | env::_ -> Hashtbl.replace env key valu
