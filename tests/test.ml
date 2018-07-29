open Core

open Backend
open Frontend
open Interpreters
open Common

let i0 _options expr =
  Interp_0.interpret_top_level expr |> Interp_0.string_of_value

let i1 options expr =
  Interp_1.interpret options expr |> Interp_1.string_of_value

let i2 options expr =
  Interp_2.interpret options expr |> Interp_2.string_of_value

let i3 options expr =
  Interp_3.interpret options expr |> Interp_3.string_of_value

let i4 options expr = Backend.Jargon.compile options expr |> Interpreters.Jargon.interpret options |> Interpreters.Jargon.string_of_value

let run input_text =
  let interpreters = [i0; i1; i2; i3; i4] in
  let options : Options.t =
    { verbose_front= false
    ; verbose_tree= false
    ; verbose_back= false
    ; stack_max= 1000
    ; heap_max= 1000 }
  in
  let e = Front_end.front_end_with_string options input_text in
  List.iter interpreters ~f:(fun interpreter ->
      let result = interpreter options e in
      printf "%s\n" result )
