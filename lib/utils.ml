open Core

(* read_int is deprecated *)
let read_int () = Int.of_string In_channel.(input_line_exn stdin)
