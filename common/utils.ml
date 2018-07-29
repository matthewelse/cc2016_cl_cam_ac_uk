open Core

(* read_int in the standard library is deprecated *)
let read_int () = 
    let _ = print_string "input> " in
    Int.of_string In_channel.(input_line_exn stdin)
