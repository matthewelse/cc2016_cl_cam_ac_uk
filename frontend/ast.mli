open Common

type var = string

type expr =
  | Unit
  | Var of var
  | Integer of int
  | Boolean of bool
  | UnaryOp of Oper.unary_oper * expr
  | Op of expr * Oper.oper * expr
  | If of expr * expr * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Inl of expr
  | Inr of expr
  | Case of expr * lambda * lambda
  | While of expr * expr
  | Seq of expr list
  | Ref of expr
  | Deref of expr
  | Assign of expr * expr
  | Lambda of lambda
  | App of expr * expr
  | LetFun of var * lambda * expr
  | LetRecFun of var * lambda * expr

and lambda = Past.var * expr

(* printing *)

val print_expr : expr -> unit

val eprint_expr : expr -> unit

val string_of_expr : expr -> string
