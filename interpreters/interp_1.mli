open Common
open Frontend

type value

val interpret : Options.t -> Ast.expr -> value

val string_of_value : value -> string
