type offset = int

type label = string

type code_index = int

type location = label * code_index option

type stack_index = int

type heap_index = int

type heap_type = HT_PAIR | HT_INL | HT_INR | HT_CLOSURE

type heap_item =
  | HEAP_INT of int
  | HEAP_BOOL of bool
  | HEAP_UNIT
  | HEAP_HI of heap_index
  (* Pointer into Heap            *)
  | HEAP_CI of code_index
  (* Code pointer for closures    *)
  | HEAP_HEADER of int * heap_type

type stack_item =
  | STACK_INT of int
  | STACK_BOOL of bool
  | STACK_UNIT
  | STACK_HI of heap_index
  (* Pointer into Heap            *)
  | STACK_RA of code_index
  (* return address               *)
  | STACK_FP of stack_index

(* int is number of items to follow *)

type value_path = STACK_LOCATION of offset | HEAP_LOCATION of offset

type instruction =
  | PUSH of stack_item
  (* modified *)
  | LOOKUP of value_path
  (* modified *)
  | UNARY of Oper.unary_oper
  | OPER of Oper.oper
  | ASSIGN
  (* | SWAP *)
  | POP
  | FST
  | SND
  | DEREF
  | APPLY
  | RETURN
  | MK_PAIR
  | MK_INL
  | MK_INR
  | MK_REF
  | MK_CLOSURE of location * int
  (* modified *)
  | TEST of location
  | CASE of location
  | GOTO of location
  | LABEL of label
  | HALT


(********************** Printing ********************************)

let string_of_stack_item = function
  | STACK_INT i -> "STACK_INT " ^ string_of_int i
  | STACK_BOOL true -> "STACK_BOOL true"
  | STACK_BOOL false -> "STACK_BOOL false"
  | STACK_UNIT -> "STACK_UNIT"
  | STACK_HI i -> "STACK_HI " ^ string_of_int i
  | STACK_RA i -> "STACK_RA " ^ string_of_int i
  | STACK_FP i -> "STACK_FP " ^ string_of_int i

let string_of_heap_type = function
  | HT_PAIR -> "HT_PAIR"
  | HT_INL -> "HT_INL"
  | HT_INR -> "HT_INR"
  | HT_CLOSURE -> "HT_CLOSURE"

let string_of_heap_item = function
  | HEAP_INT i -> "HEAP_INT " ^ string_of_int i
  | HEAP_BOOL true -> "HEAP_BOOL true"
  | HEAP_BOOL false -> "HEAP_BOOL false"
  | HEAP_UNIT -> "HEAP_UNIT"
  | HEAP_HI i -> "HEAP_HI " ^ string_of_int i
  | HEAP_CI i -> "HEAP_CI " ^ string_of_int i
  | HEAP_HEADER (i, t) ->
      "HEAP_HEADER(" ^ string_of_int i ^ ", " ^ string_of_heap_type t ^ ")"

let string_of_value_path = function
  | STACK_LOCATION offset -> "STACK_LOCATION " ^ string_of_int offset
  | HEAP_LOCATION offset -> "HEAP_LOCATION " ^ string_of_int offset

let string_of_location = function
  | l, None -> l
  | l, Some i -> l ^ " = " ^ string_of_int i

let string_of_instruction = function
  | UNARY op -> "UNARY " ^ Oper.string_of_uop op
  | OPER op -> "OPER " ^ Oper.string_of_bop op
  | MK_PAIR -> "MK_PAIR"
  | FST -> "FST"
  | SND -> "SND"
  | MK_INL -> "MK_INL"
  | MK_INR -> "MK_INR"
  | MK_REF -> "MK_REF"
  | PUSH v -> "PUSH " ^ string_of_stack_item v
  | LOOKUP p -> "LOOKUP " ^ string_of_value_path p
  | TEST l -> "TEST " ^ string_of_location l
  | CASE l -> "CASE " ^ string_of_location l
  | GOTO l -> "GOTO " ^ string_of_location l
  | APPLY -> "APPLY"
  | RETURN -> "RETURN"
  | HALT -> "HALT"
  | LABEL l -> "LABEL " ^ l
  (* | SWAP -> "SWAP" *)
  | POP -> "POP"
  | DEREF -> "DEREF"
  | ASSIGN -> "ASSIGN"
  | MK_CLOSURE (loc, n) ->
      "MK_CLOSURE(" ^ string_of_location loc ^ ", " ^ string_of_int n ^ ")"

let rec string_of_listing = function
  | [] -> "\n"
  | LABEL l :: rest -> ("\n" ^ l ^ " :") ^ string_of_listing rest
  | i :: rest -> "\n\t" ^ string_of_instruction i ^ string_of_listing rest

let string_of_installed_code (code, size) =
  let rec aux k =
    if size = k then ""
    else
      string_of_int k ^ ": "
      ^ string_of_instruction code.(k)
      ^ "\n"
      ^ aux (k + 1)
  in
  aux 0

let string_of_stack (sp, stack) =
  let rec aux carry j =
    if j = sp then carry
    else
      aux
        (string_of_int j ^ ": " ^ string_of_stack_item stack.(j) ^ "\n" ^ carry)
        (j + 1)
  in
  aux "" 0

