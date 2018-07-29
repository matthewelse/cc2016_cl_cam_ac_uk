type oper = ADD | MUL | DIV | SUB | LT | AND | OR | EQB | EQI

type unary_oper = NEG | NOT | READ

let pp_uop = function NEG -> "-" | NOT -> "~" | READ -> "read"

let pp_bop = function
  | ADD -> "+"
  | MUL -> "*"
  | DIV -> "/"
  | SUB -> "-"
  | LT -> "<"
  | EQI -> "eqi"
  | EQB -> "eqb"
  | AND -> "&&"
  | OR -> "||"

let string_of_uop = pp_uop

let string_of_bop = pp_bop
