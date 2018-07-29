open Core

type offset = int [@@deriving sexp_of]

type label = string [@@deriving sexp_of]

type code_index = int [@@deriving sexp_of]

type location = label * code_index option [@@deriving sexp_of]

type stack_index = int [@@deriving sexp_of]

type heap_index = int [@@deriving sexp_of]

module Heap_item = struct
  type typ = Pair | Inl | Inr | Closure [@@deriving sexp_of]

  type t =
    | Int of int
    | Bool of bool
    | Unit
    | Heap_index of heap_index
    | Code_pointer of code_index
    | Header of int * typ
  [@@deriving sexp_of]

  let to_string t = Sexp.to_string (sexp_of_t t)
end

module Stack_item = struct
  type t =
    | Int of int
    | Bool of bool
    | Unit
    | Heap_index of heap_index
    | Return_address of code_index
    | Frame_pointer of stack_index
  [@@deriving sexp_of]

  let to_string t = Sexp.to_string (sexp_of_t t)
end

(* int is number of items to follow *)

type value_path = STACK_LOCATION of offset | HEAP_LOCATION of offset
[@@deriving sexp_of]

module Instruction = struct
  type t =
    | Push of Stack_item.t
    (* modified *)
    | Lookup of value_path
    (* modified *)
    | Unary of Oper.unary_oper
    | Oper of Oper.oper
    | Assign
    (* | SWAP *)
    | Pop
    | Fst
    | Snd
    | Deref
    | Apply
    | Return
    | Make_pair
    | Make_inl
    | Make_inr
    | Make_ref
    | Make_closure of location * int
    (* modified *)
    | Test of location
    | Case of location
    | Goto of location
    | Label of label
    | Halt
  [@@deriving sexp_of]

  let to_string t = Sexp.to_string (sexp_of_t t)
end

(********************** Printing ********************************)

let string_of_value_path = function
  | STACK_LOCATION offset -> "STACK_LOCATION " ^ string_of_int offset
  | HEAP_LOCATION offset -> "HEAP_LOCATION " ^ string_of_int offset

let string_of_location = function
  | l, None -> l
  | l, Some i -> sprintf "%s = %d" l i

let string_of_listing listing =
  List.map listing ~f:(function
    | Instruction.Label l -> sprintf "%s:" l
    | x -> "\t" ^ Instruction.to_string x )
  |> String.concat ~sep:"\n"

let string_of_installed_code (code, size) =
  let rec aux k =
    if size = k then ""
    else
      string_of_int k ^ ": "
      ^ Instruction.to_string code.(k)
      ^ "\n"
      ^ aux (k + 1)
  in
  aux 0

let string_of_stack (sp, stack) =
  Array.slice stack 0 sp
  |> Array.mapi ~f:(sprintf !"%d: %{sexp: Stack_item.t}")
  |> Array.to_list |> String.concat ~sep:"\n"

let string_of_heap heap limit =
  let inner =
    Array.slice heap 0 limit
    |> Array.mapi ~f:(sprintf !"%d -> %{sexp: Heap_item.t}")
    |> Array.to_list |> String.concat ~sep:"\n"
  in
  "\nHeap = \n" ^ inner
