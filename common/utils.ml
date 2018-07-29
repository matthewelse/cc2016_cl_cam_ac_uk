open Core

(* read_int in the standard library is deprecated *)
let read_int () = 
    print_string "input> ";
    Out_channel.flush stdout;
    Int.of_string In_channel.(input_line_exn stdin)
