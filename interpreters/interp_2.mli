open Common
open Common.Interp_2

val interpret : Options.t -> code -> value

val string_of_value : value -> string
