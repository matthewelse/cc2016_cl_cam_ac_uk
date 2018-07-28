open Lexing
open Common

let error action s =
  Errors.complain ("\nERROR in input with " ^ action ^ " : " ^ s ^ "\n")

let peek (options: Options.t) m e pp =
  if options.verbose_front then
    print_string
      ( m ^ ":\n"
      ^ (if options.verbose_tree then Pptree.pp_no_bracket else fun x -> x)
          (pp e)
      ^ "\n" )
  else ()

let parse_error lexbuf =
  let pos = lexbuf.lex_curr_p in
  let line = string_of_int pos.pos_lnum in
  let pos = string_of_int (pos.pos_cnum - pos.pos_bol + 1) in
  error "parsing" ("at line " ^ line ^ " position " ^ pos)

(* initialize lexer *)
let init_lexbuf file =
  let in_chan =
    try open_in file with _ ->
      error "initialize lexer" ("can't open file " ^ file)
  in
  let lexbuf = from_channel in_chan in
  let _ =
    lexbuf.lex_curr_p <- {pos_fname= file; pos_lnum= 1; pos_bol= 0; pos_cnum= 0}
  in
  lexbuf

(* parse input string *)
let parse options lexbuf =
  let e =
    try Parser.start Lexer.token lexbuf with Parsing.Parse_error ->
      parse_error lexbuf
  in
  let _ = peek options "Parsed result" e Past.string_of_expr in
  e

(* perform static checks *)
let check options e =
  let e' =
    try Static.check e with Errors.Error s -> error "static check" s
  in
  let _ = peek options "After static checks" e' Past.string_of_expr in
  e'

(* translate from Past.expr to Ast.expr *)
let translate (options: Options.t) e =
  let e' = Past_to_ast.translate_expr e in
  let _ = peek options "After translation" e' Ast.string_of_expr in
  e'

(* the front end *)
let front_end options file =
  translate options (check options (parse options (init_lexbuf file)))

let front_end_with_string options input =
  let lexbuf = from_string input in
  let expr = parse options lexbuf in
  let expr' = check options expr in
  translate options expr'

