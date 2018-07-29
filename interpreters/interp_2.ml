(**************************************
Compiler Construction 2016
Computer Laboratory
University of Cambridge
Timothy G. Griffin (tgg22@cam.ac.uk)
*****************************************)
(* Interpreter 2.

A high-level stack-oriented abstract machine with compiler.
What do I mean by "high-level"?
---Code is still tree-structured.
---Complex values are pushed onto value stack.
---Slang state (heap) used only for references.
---Code is maintained on a code stack.
---Program variables contained in code.
*)
open Core
open Common
open Common.Interp_2

let complain = Errors.complain

type env_or_value = EV of env | V of value

(* Printing *)

type t = {heap: value Array.t; options: Options.t; mutable next_address: int}

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
  | CLOSURE cl -> "CLOSURE(" ^ string_of_closure cl ^ ")"
  | REC_CLOSURE c -> "REC_CLOSURE(" ^ string_of_code c ^ ")"

and string_of_closure (c, env) =
  "(" ^ string_of_code c ^ ", " ^ string_of_env env ^ ")"

and string_of_env env = string_of_list ",\n " string_of_binding env

and string_of_binding (x, v) = "(" ^ x ^ ", " ^ string_of_value v ^ ")"

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
  | TEST (c1, c2) ->
      "TEST(" ^ string_of_code c1 ^ ", " ^ string_of_code c2 ^ ")"
  | CASE (c1, c2) ->
      "CASE(" ^ string_of_code c1 ^ ", " ^ string_of_code c2 ^ ")"
  | WHILE (c1, c2) ->
      "WHILE(" ^ string_of_code c1 ^ ", " ^ string_of_code c2 ^ ")"
  | APPLY -> "APPLY"
  | BIND x -> "BIND " ^ x
  | SWAP -> "SWAP"
  | POP -> "POP"
  | DEREF -> "DEREF"
  | ASSIGN -> "ASSIGN"
  | MK_CLOSURE c -> "MK_CLOSURE(" ^ string_of_code c ^ ")"
  | MK_REC (f, c) -> "MK_REC(" ^ f ^ ", " ^ string_of_code c ^ ")"

and string_of_code c = string_of_list ";\n " string_of_instruction c

let string_of_env_or_value = function
  | EV env -> "EV " ^ string_of_env env
  | V v -> "V " ^ string_of_value v

let string_of_env_value_stack = string_of_list ";\n " string_of_env_or_value

let string_of_state t =
  let rec aux k =
    if t.next_address < k then ""
    else
      string_of_int k ^ " -> "
      ^ string_of_value (t.heap).(k)
      ^ "\n"
      ^ aux (k + 1)
  in
  if t.next_address = 0 then "" else "\nHeap = \n" ^ aux 0

let string_of_interp_state t (c, evs) =
  "\nCode Stack = \n" ^ string_of_code c ^ "\nEnv/Value Stack = \n"
  ^ string_of_env_value_stack evs
  ^ string_of_state t

(* The "MACHINE" *)
(* allocate a new location in the heap
   and give it value v
*)
let allocate t v =
  let i = t.next_address in
  if i < t.options.heap_max then (
    (t.heap).(i) <- v ;
    t.next_address <- i + 1 ;
    i )
  else complain "runtime error: heap kaput"

let deref t a = (t.heap).(a)

let assign t a v = (t.heap).(a) <- v

(* update : (env * binding) -> env *)
(* let update (env, (x, v)) = (x, v) :: env *)

let mk_fun (c, env) = CLOSURE (c, env)

let mk_rec (f, c, env) = CLOSURE (c, (f, REC_CLOSURE c) :: env)

(*
   in interp_0:

   interpret(LetRecFun(f, (x, body), e), env) =
       let rec new_env g =
           if g = f then FUN (fun v -> interpret(body, update(new_env, (x, v)))) else env g
       in interpret(e, new_env, store)

      new_env x = env x
      new_env f = FUN (fun v -> interpret(body, update(new_env, (x, v))))

      lookup (env1 @ [(f, cl1)] @ evn2, f) =
        CLOSURE (false, (x, body, (f, cl2) :: env2))
*)
let lookup_opt (env, x) =
  let rec aux = function
    | [] -> None
    | (y, v) :: rest ->
        if x = y then
          Some
            (match v with REC_CLOSURE body -> mk_rec (x, body, rest) | _ -> v)
        else aux rest
  in
  aux env

let rec search (evs, x) =
  match evs with
  | [] -> complain (x ^ " is not defined!\n")
  | V _ :: rest -> search (rest, x)
  | EV env :: rest ->
    match lookup_opt (env, x) with None -> search (rest, x) | Some v -> v

let rec evs_to_env = function
  | [] -> []
  | V _ :: rest -> evs_to_env rest
  | EV env :: rest -> env @ evs_to_env rest

let do_unary = function
  | Oper.NOT, BOOL m -> BOOL (not m)
  | NEG, INT m -> INT (-m)
  | READ, UNIT -> INT (Utils.read_int ())
  | op, _ -> complain ("malformed unary operator: " ^ Oper.string_of_uop op)

let do_oper = function
  | Oper.AND, BOOL m, BOOL n -> BOOL (m && n)
  | OR, BOOL m, BOOL n -> BOOL (m || n)
  | EQB, BOOL m, BOOL n -> BOOL (m = n)
  | LT, INT m, INT n -> BOOL (m < n)
  | EQI, INT m, INT n -> BOOL (m = n)
  | ADD, INT m, INT n -> INT (m + n)
  | SUB, INT m, INT n -> INT (m - n)
  | MUL, INT m, INT n -> INT (m * n)
  | DIV, INT m, INT n -> INT (m / n)
  | op, _, _ -> complain ("malformed binary operator: " ^ Oper.string_of_bop op)

(*
    val step : interp_state -> interp_state
             = (code * env_value_stack * state) -> (code * env_value_stack * state)
*)
let step t = function
  (* (code stack,         value/env stack, state) -> (code stack,  value/env stack, state) *)
  | PUSH v :: ds, evs ->
      (ds, V v :: evs)
  | POP :: ds, _e :: evs -> (ds, evs)
  | SWAP :: ds, e1 :: e2 :: evs -> (ds, e2 :: e1 :: evs)
  | BIND x :: ds, V v :: evs -> (ds, EV [(x, v)] :: evs)
  | LOOKUP x :: ds, evs -> (ds, V (search (evs, x)) :: evs)
  | UNARY op :: ds, V v :: evs -> (ds, V (do_unary (op, v)) :: evs)
  | OPER op :: ds, V v2 :: V v1 :: evs -> (ds, V (do_oper (op, v1, v2)) :: evs)
  | MK_PAIR :: ds, V v2 :: V v1 :: evs -> (ds, V (PAIR (v1, v2)) :: evs)
  | FST :: ds, V (PAIR (v, _)) :: evs -> (ds, V v :: evs)
  | SND :: ds, V (PAIR (_, v)) :: evs -> (ds, V v :: evs)
  | MK_INL :: ds, V v :: evs -> (ds, V (INL v) :: evs)
  | MK_INR :: ds, V v :: evs -> (ds, V (INR v) :: evs)
  | CASE (c1, _) :: ds, V (INL v) :: evs -> (c1 @ ds, V v :: evs)
  | CASE (_, c2) :: ds, V (INR v) :: evs -> (c2 @ ds, V v :: evs)
  | TEST (c1, _c2) :: ds, V (BOOL true) :: evs -> (c1 @ ds, evs)
  | TEST (_c1, c2) :: ds, V (BOOL false) :: evs -> (c2 @ ds, evs)
  | ASSIGN :: ds, V v :: V (REF a) :: evs ->
      assign t a v ;
      (ds, V UNIT :: evs)
  | DEREF :: ds, V (REF a) :: evs -> (ds, V (deref t a) :: evs)
  | MK_REF :: ds, V v :: evs ->
      let a = allocate t v in
      (ds, V (REF a) :: evs)
  | WHILE (_c1, _c2) :: ds, V (BOOL false) :: evs -> (ds, V UNIT :: evs)
  | WHILE (c1, c2) :: ds, V (BOOL true) :: evs ->
      (c2 @ [POP] @ c1 @ [WHILE (c1, c2)] @ ds, evs)
  | MK_CLOSURE c :: ds, evs -> (ds, V (mk_fun (c, evs_to_env evs)) :: evs)
  | MK_REC (f, c) :: ds, evs -> (ds, V (mk_rec (f, c, evs_to_env evs)) :: evs)
  | APPLY :: ds, V (CLOSURE (c, env)) :: V v :: evs ->
      (c @ ds, V v :: EV env :: evs)
  | state ->
      complain ("step : bad state = " ^ string_of_interp_state t state ^ "\n")

let rec driver t n state =
  let _ =
    if t.options.verbose_back then
      print_string
        ( "\nState " ^ string_of_int n ^ " : "
        ^ string_of_interp_state t state
        ^ "\n" )
    else ()
  in
  match state with [], [V v] -> v | _ -> driver t (n + 1) (step t state)



(* The initial Slang state is the Slang state : all locations contain 0 *)

let initial_env = []

(* interpret : Options.t -> code -> value *)
let interpret (options: Options.t) c =
  let t =
    { heap= Array.init options.heap_max ~f:(fun _ -> INT 0)
    ; next_address= 0
    ; options }
  in
  if options.verbose_back then
    print_string ("Compile code =\n" ^ string_of_code c ^ "\n") ;
  driver t 1 (c, initial_env)
