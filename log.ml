let alloc_stream = ref None

let alloc_filename fname =
  let chopped = Filename.chop_extension fname in
  chopped ^ ".alloc"

let open_alloc alloc_fname =
  alloc_stream := Some (open_out alloc_fname)

let close_alloc () =
  match !alloc_stream with
    None -> ()
  | Some fh -> close_out fh

let printf stream fmt =
  match !stream with
    None -> Obj.magic
  | Some fh -> Printf.fprintf fh fmt
