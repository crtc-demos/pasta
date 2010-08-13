let line_num = ref 1

let reset_line_num () =
  line_num := 1

let current_file = ref "<unknown>"

let file_stack = ref []

let push_include filename =
  file_stack := (!current_file, !line_num) :: !file_stack;
  current_file := filename;
  line_num := 1

let pop_include () =
  match !file_stack with
    [] -> failwith "Include file stack empty"
  | (oldfile, oldline)::rest ->
      line_num := oldline;
      current_file := oldfile;
      file_stack := rest

exception AssemblyError of string * string
exception NonLineError of string
