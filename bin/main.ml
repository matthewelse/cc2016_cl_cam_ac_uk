open Core
open Async

open Common
open Frontend
open Interpreters
open Backend

let run input_file interpreters =
  let options : Options.t =
    { verbose_front= false
    ; verbose_tree= false
    ; verbose_back= false
    ; stack_max= 1000
    ; heap_max= 1000 }
  in
  let e = Front_end.front_end options input_file in
  List.iter interpreters ~f:(fun interpreter ->
      let result = interpreter options e in
      printf "%s\n" result )
  |> return

let i0 _options expr =
  Interp_0.interpret_top_level expr |> Interp_0.string_of_value

let i1 options expr =
  Interp_1.interpret options expr |> Interp_1.string_of_value

let i2 options expr =
  Interp_2.interpret options expr |> Interp_2.string_of_value

let i3 options expr =
  Interp_3.interpret options expr |> Interp_3.string_of_value

let i4 options expr = Backend.Jargon.compile options expr |> Interpreters.Jargon.interpret options |> Interpreters.Jargon.string_of_value

let interpreter = function
  | "I0" -> i0
  | "I1" -> i1
  | "I2" -> i2
  | "I3" -> i3
  | "I4" | "Jargon" | "JARGON" -> i4
  | i ->
      eprintf "Unknown interpreter %s" i ;
      Core.exit 1

let interpreter = Command.Arg_type.create interpreter

let run =
  let open Command.Let_syntax in
  let%map_open input_file = anon ("file" %: file)
  and interpreters =
    flag "-interpreter" (listed interpreter) ~doc:"Interpreter to use"
  in
  fun () -> run input_file interpreters

let command =
  Command.async ~summary:"slang compiler"
    ~readme:(fun () ->
      "Compiler for the slang language from Cambridge Computer Science Part \
       II compiler construction course." )
    run

let () = Command.run command
