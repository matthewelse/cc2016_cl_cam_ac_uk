open Core
open Async

open Common
open Frontend
open Interpreters
open Backend

let run input_file interpreters options =
  let e = Front_end.front_end options input_file in
  List.iter interpreters ~f:(fun interpreter ->
      let result = interpreter options e in
      printf "%s\n" result )
  |> return

let i0 _options expr =
  Compiler_0.compile expr
  |> Interp_0.interpret_top_level
  |> Interp_0.string_of_value

let i1 options expr =
  Interp_1.interpret options expr 
  |> Interp_1.string_of_value

let i2 options expr =
  Backend.Compiler_2.compile expr
  |> Interp_2.interpret options
  |> Interp_2.string_of_value

let i3 options expr =
  Backend.Compiler_3.compile options expr
  |> Interp_3.interpret options
  |> Common.Interp_3.string_of_value

let i4 options expr =
  Backend.Jargon.compile options expr
  |> Interpreters.Jargon.interpret options
  |> Interpreters.Jargon.Vm_state.string_of_value

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
  and interpreters = flag "-interpreter" (listed interpreter) ~doc:"Interpreter to use"
  and verbose_back = flag "-verbose-back" no_arg ~doc:"FLAG verbose back-end"
  and verbose_front = flag "-verbose-front" no_arg ~doc:"FLAG verbose front-end"
  and verbose_tree = flag "-verbose-tree" no_arg ~doc:"FLAG verbose tree"
  and stack_max = flag "-stack-max" (optional_with_default 1000 int) ~doc:"INT max heap size"
  and heap_max = flag "-heap-max" (optional_with_default 1000 int) ~doc:"INT max heap size"
  in
  let options : Options.t =
    { verbose_front
    ; verbose_tree
    ; verbose_back
    ; stack_max
    ; heap_max }
  in
  fun () -> run input_file interpreters options

let command =
  Command.async ~summary:"slang compiler"
    ~readme:(fun () ->
      "Compiler for the slang language from Cambridge Computer Science Part \
       II compiler construction course." )
    run

let () = Command.run command
