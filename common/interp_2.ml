type address = int

type var = string

type value =
  | REF of address
  | INT of int
  | BOOL of bool
  | UNIT
  | PAIR of value * value
  | INL of value
  | INR of value
  | CLOSURE of closure
  | REC_CLOSURE of code

and closure = (code * env)

and instruction =
  | PUSH of value
  | LOOKUP of var
  | UNARY of Oper.unary_oper
  | OPER of Oper.oper
  | ASSIGN
  | SWAP
  | POP
  | BIND of var
  | FST
  | SND
  | DEREF
  | APPLY
  | MK_PAIR
  | MK_INL
  | MK_INR
  | MK_REF
  | MK_CLOSURE of code
  | MK_REC of var * code
  | TEST of code * code
  | CASE of code * code
  | WHILE of code * code

and code = instruction list

and binding = (var * value)

and env = binding list
