open Core

type var = string

type address = int

type label = string

type location = label * address option

type value =
  | REF of address
  | INT of int
  | BOOL of bool
  | UNIT
  | PAIR of value * value
  | INL of value
  | INR of value
  | CLOSURE of location * env

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
  | RETURN
  | MK_PAIR
  | MK_INL
  | MK_INR
  | MK_REF
  | MK_CLOSURE of location
  | TEST of location
  | CASE of location
  | GOTO of location
  | LABEL of label
  | HALT

and binding = (var * value)

and env = binding list



let string_of_list sep f l =
  let inner = List.map l ~f |> String.concat ~sep in
  "[" ^ inner ^ "]"

let rec string_of_value = function
  | REF a -> "REF(" ^ string_of_int a ^ ")"
  | BOOL b -> string_of_bool b
  | INT n -> string_of_int n
  | UNIT -> "UNIT"
  | PAIR (v1, v2) -> "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"
  | INL v -> "inl(" ^ string_of_value v ^ ")"
  | INR v -> "inr(" ^ string_of_value v ^ ")"
  | CLOSURE (loc, c) -> "CLOSURE(" ^ string_of_closure (loc, c) ^ ")"

and string_of_closure (loc, env) =
  "(" ^ string_of_location loc ^ ", " ^ string_of_env env ^ ")"

and string_of_env env = string_of_list ",\n " string_of_binding env

and string_of_binding (x, v) = "(" ^ x ^ ", " ^ string_of_value v ^ ")"

and string_of_location = function
  | l, None -> l
  | l, Some i -> l ^ " = " ^ string_of_int i

and string_of_instruction = function
  | UNARY op -> "UNARY " ^ Oper.string_of_uop op
  | OPER op -> "OPER " ^ Oper.string_of_bop op
  | MK_PAIR -> "MK_PAIR"
  | FST -> "FST"
  | SND -> "SND"
  | MK_INL -> "MK_INL"
  | MK_INR -> "MK_INR"
  | MK_REF -> "MK_REF"
  | PUSH v -> "PUSH " ^ string_of_value v
  | LOOKUP x -> "LOOKUP " ^ x
  | TEST l -> "TEST " ^ string_of_location l
  | CASE l -> "CASE " ^ string_of_location l
  | GOTO l -> "GOTO " ^ string_of_location l
  | APPLY -> "APPLY"
  | RETURN -> "RETURN"
  | HALT -> "HALT"
  | BIND x -> "BIND " ^ x
  | LABEL l -> "LABEL " ^ l
  | SWAP -> "SWAP"
  | POP -> "POP"
  | DEREF -> "DEREF"
  | ASSIGN -> "ASSIGN"
  | MK_CLOSURE loc -> "MK_CLOSURE(" ^ string_of_location loc ^ ")"

and string_of_code c = string_of_list "\n " string_of_instruction c
