(**************************************
Compiler Construction 2016
Computer Laboratory
University of Cambridge
Timothy G. Griffin (tgg22@cam.ac.uk)
*****************************************)
(*
   Interpreter 3.

   Derived from Interpreter 2 by
   --- Make instructions linear by introducing
       labels and goto.
   --- labels translated to numeric addresses.
   --- include "code pointer" in state
   --- compiler eliminates WHILE construct
*)
open Core
open Common
open Common.Interp_3

let complain = Errors.complain

type t =
  { heap: value Array.t
  ; mutable next_address: int
  ; mutable installed: instruction Array.t
  ; options: Options.t }

type env_or_value =
  | EV of env
  (* an environment on the run-time stack *)
  | V of value
  (* a value on the run-time stack *)
  | RA of address

let lookup env x =
  List.Assoc.find env ~equal:(=) x
  (* match env with
  | [] -> None
  | (y, v) :: rest -> if x = y then Some v else lookup (rest, x) *)

let rec search (evs, x) =
  match evs with
  | [] -> complain (x ^ " is not defined!\n")
  | V _ :: rest -> search (rest, x)
  | RA _ :: rest -> search (rest, x)
  | EV env :: rest ->
    match lookup env x with None -> search (rest, x) | Some v -> v

let rec evs_to_env = function
  | [] -> []
  | V _ :: rest -> evs_to_env rest
  | RA _ :: rest -> evs_to_env rest
  | EV env :: rest -> env @ evs_to_env rest


let string_of_env_or_value = function
  | EV env -> "EV " ^ string_of_env env
  | V v -> "V " ^ string_of_value v
  | RA i -> "RA " ^ string_of_int i

let string_of_env_value_stack = string_of_list ";\n " string_of_env_or_value

(* THE MACHINE *)

let string_of_installed_code t =
  let size = Array.length t.installed in
  let rec aux k =
    if size = k then ""
    else
      string_of_int k ^ ": "
      ^ string_of_instruction (t.installed).(k)
      ^ "\n"
      ^ aux (k + 1)
  in
  aux 0

let get_instruction t cp = (t.installed).(cp)

let new_address t =
  let a = t.next_address in
  t.next_address <- a + 1 ;
  a

let string_of_heap t =
  let rec aux k =
    if t.next_address < k then ""
    else
      string_of_int k ^ " -> "
      ^ string_of_value (t.heap).(k)
      ^ "\n"
      ^ aux (k + 1)
  in
  "\nHeap = \n" ^ aux 0

let string_of_state t (cp, evs) =
  "\nCode Pointer = " ^ string_of_int cp ^ " -> "
  ^ string_of_instruction (get_instruction t cp)
  ^ "\nStack = \n"
  ^ string_of_env_value_stack evs
  ^ if t.next_address = 0 then "" else string_of_heap t

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

let step t (cp, evs) =
  match (get_instruction t cp, evs) with
  | PUSH v, evs -> (cp + 1, V v :: evs)
  | POP, _s :: evs -> (cp + 1, evs)
  | SWAP, s1 :: s2 :: evs -> (cp + 1, s2 :: s1 :: evs)
  | BIND x, V v :: evs -> (cp + 1, EV [(x, v)] :: evs)
  | LOOKUP x, evs -> (cp + 1, V (search (evs, x)) :: evs)
  | UNARY op, V v :: evs -> (cp + 1, V (do_unary (op, v)) :: evs)
  | OPER op, V v2 :: V v1 :: evs -> (cp + 1, V (do_oper (op, v1, v2)) :: evs)
  | MK_PAIR, V v2 :: V v1 :: evs -> (cp + 1, V (PAIR (v1, v2)) :: evs)
  | FST, V (PAIR (v, _)) :: evs -> (cp + 1, V v :: evs)
  | SND, V (PAIR (_, v)) :: evs -> (cp + 1, V v :: evs)
  | MK_INL, V v :: evs -> (cp + 1, V (INL v) :: evs)
  | MK_INR, V v :: evs -> (cp + 1, V (INR v) :: evs)
  | CASE (_, Some _), V (INL v) :: evs -> (cp + 1, V v :: evs)
  | CASE (_, Some i), V (INR v) :: evs -> (i, V v :: evs)
  | TEST (_, Some _), V (BOOL true) :: evs -> (cp + 1, evs)
  | TEST (_, Some i), V (BOOL false) :: evs -> (i, evs)
  | ASSIGN, V v :: V (REF a) :: evs ->
      (t.heap).(a) <- v ;
      (cp + 1, V UNIT :: evs)
  | DEREF, V (REF a) :: evs -> (cp + 1, V (t.heap).(a) :: evs)
  | MK_REF, V v :: evs ->
      let a = new_address t in
      (t.heap).(a) <- v ;
      (cp + 1, V (REF a) :: evs)
  | MK_CLOSURE loc, evs -> (cp + 1, V (CLOSURE (loc, evs_to_env evs)) :: evs)
  | APPLY, V (CLOSURE ((_, Some i), env)) :: V v :: evs ->
      (i, V v :: EV env :: RA (cp + 1) :: evs)
  (* new intructions *)
  | RETURN, V v :: _ :: RA i :: evs -> (i, V v :: evs)
  | LABEL _l, evs -> (cp + 1, evs)
  | HALT, evs -> (cp, evs)
  | GOTO (_, Some i), evs -> (i, evs)
  | _ -> complain ("step : bad state = " ^ string_of_state t (cp, evs) ^ "\n")



let rec driver t n state =
  let _ =
    if t.options.verbose_back then
      print_string
        ("\nstate " ^ string_of_int n ^ ":" ^ string_of_state t state ^ "\n")
    else ()
  in
  match state with cp, evs ->
    if HALT = get_instruction t cp then
      match evs with
      | [V v] -> v
      | _ ->
          complain
            ("driver : bad halted state = " ^ string_of_state t state ^ "\n")
    else driver t (n + 1) (step t state)

(* put code listing into an array, associate an array index to each label *)
let load l =
  let rec find lab = function
    | [] -> complain ("find : " ^ lab ^ " is not found")
    | (x, v) :: rest -> if x = lab then v else find lab rest
    (* insert array index for each label *)
  in
  let apply_label_map_to_instruction m = function
    | GOTO (lab, _) -> GOTO (lab, Some (find lab m))
    | TEST (lab, _) -> TEST (lab, Some (find lab m))
    | CASE (lab, _) -> CASE (lab, Some (find lab m))
    | MK_CLOSURE (lab, _) -> MK_CLOSURE (lab, Some (find lab m))
    | (*
     | MK_CLOSURE ((lab, _), fvars) -> MK_CLOSURE((lab, Some(find lab m)), fvars)
*)
    inst ->
        inst
    (* find array index for each label *)
  in
  let listing_to_label_map l =
    let rec aux carry k = function
      | [] -> carry
      | LABEL lab :: rest -> aux ((lab, k) :: carry) (k + 1) rest
      | _ :: rest -> aux carry (k + 1) rest
    in
    aux [] 0 l
  in
  let l_map = listing_to_label_map l in
  Array.of_list (List.map l ~f:(apply_label_map_to_instruction l_map))

(* interpret : Options.t -> expr -> value *)
let interpret (options: Options.t) code =
  let t =
    { heap= Array.init options.heap_max ~f:(fun _ -> INT 0)
    ; next_address= 0
    ; installed= Array.of_list [HALT]
    ; options }
  in
  t.installed <- load code;
  let _ =
    if t.options.verbose_back then
      print_string ("\nInstalled Code = \n" ^ string_of_installed_code t)
    else ()
    (* set the code pointer to 0 *)
  in
  driver t 1 (0, [])
