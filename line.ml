let line_num = ref 1

let reset_line_num () =
  line_num := 1

exception AssemblyError of string * string
