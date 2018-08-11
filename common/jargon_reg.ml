(**
 * Define a register-based alternative to the Jargon VM. Has unlimited registers,
 * intended as an intermediate language for the ARM compiler.
 *)
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

(**
 * In the register-oriented machine, our registers can contain the same values as the
 * stack can in the Jargon VM. Note that we can still use the stack for things like
 * function calls - the stack will just contain these values.
 *)
module Register_item = struct
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

let reg_index = ref 0

module Register = struct
  module T = struct
    type t = Temporary of int | Return_value | Frame_pointer | Return_address
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let to_string = function
    | Temporary t -> sprintf "r%d" t
    | Return_value -> "r0"
    | Frame_pointer -> "fp"
    | Return_address -> "ra"

  let sexp_of_t t = Sexp.Atom (to_string t)

  let fresh () : t =
    reg_index := !reg_index + 1 ;
    Temporary !reg_index
end

(* int is number of items to follow *)

type value_path = STACK_LOCATION of offset | HEAP_LOCATION of offset
[@@deriving sexp_of]

module Instruction = struct
  type t =
    | Mov of Register.t * Register.t
    | Set of Register.t * Register_item.t
    | Push of Register.t
    | Lookup of Register.t * value_path
    | Unary of Oper.unary_oper * Register.t * Register.t
    | Oper of Oper.oper * Register.t * (Register.t * Register.t)
    | Assign of Register.t * Register.t
    | Fst of Register.t * Register.t
    | Snd of Register.t * Register.t
    | Deref of Register.t * Register.t
    | Apply of Register.t
    | Return of Register.t
    | Make_pair of Register.t * (Register.t * Register.t)
    | Make_inl of Register.t * Register.t
    | Make_inr of Register.t * Register.t
    | Make_ref of Register.t * Register.t
    | Make_closure of Register.t * location * int
    (* modified *)
    | Test of Register.t * location
    | Case of Register.t * Register.t * location
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

let string_of_register_file register_file =
  Map.to_alist register_file
  |> List.map ~f:(fun (key, data) ->
         sprintf !"%{Register}: %{sexp: Register_item.t}" key data )
  |> String.concat ~sep:"\n"

let string_of_stack stack sp =
  Array.slice stack 0 sp
  |> Array.mapi ~f:(sprintf !"%d: %{sexp: Register_item.t}")
  |> Array.to_list |> String.concat ~sep:"\n"

let string_of_heap heap limit =
  Array.slice heap 0 limit
  |> Array.mapi ~f:(sprintf !"%d: %{sexp: Heap_item.t}")
  |> Array.to_list |> String.concat ~sep:"\n"
