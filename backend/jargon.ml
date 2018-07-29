open Core

open Frontend.Ast
open Common

(***************************** THE MACHINE ********************************)

(* COMPILE *)

let new_label =
  let i = ref 0 in
  let get () =
    let v = !i in
    i := !i + 1 ;
    "L" ^ string_of_int v
  in
  get

(*

Interp 2

 | (APPLY :: ds,  V(CLOSURE (_, (c, env))) :: (V v) :: evs)
    -> (c @ ds, (V v) :: (EV env) :: evs)

Interp 3

 | (APPLY,  V(CLOSURE ((_, Some i), env)) :: (V v) :: evs)
    -> (i, (V v) :: (EV env) :: (RA (cp + 1)) :: evs)


Jargon VM :

     [clsoure    ]
     [arg        ]
        ...

 == APPLY ==>

     [return address]
fp ->[old fp        ]
     [clsoure       ]
     [arg           ]
        ...

*)

let positions l =
  let rec aux k = function
    | [] -> []
    | a :: rest -> (a, k) :: aux (k + 1) rest
  in
  aux 1 l

let rec comp vmap = 
  let open Common.Jargon in
  function
  | Unit -> ([], [PUSH STACK_UNIT])
  | Boolean b -> ([], [PUSH (STACK_BOOL b)])
  | Integer n -> ([], [PUSH (STACK_INT n)])
  | UnaryOp (op, e) ->
      let defs, c = comp vmap e in
      (defs, c @ [UNARY op])
  | Op (e1, op, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c1 @ c2 @ [OPER op])
  | Pair (e1, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c1 @ c2 @ [MK_PAIR])
  | Fst e ->
      let defs, c = comp vmap e in
      (defs, c @ [FST])
  | Snd e ->
      let defs, c = comp vmap e in
      (defs, c @ [SND])
  | Inl e ->
      let defs, c = comp vmap e in
      (defs, c @ [MK_INL])
  | Inr e ->
      let defs, c = comp vmap e in
      (defs, c @ [MK_INR])
  | Case (e1, (x1, e2), (x2, e3)) ->
      let inr_label = new_label () in
      let after_inr_label = new_label () in
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap (Lambda (x1, e2)) in
      let defs3, c3 = comp vmap (Lambda (x2, e3)) in
      ( defs1 @ defs2 @ defs3
      , c1
        @ [CASE (inr_label, None)]
        @ c2
        @ [APPLY; GOTO (after_inr_label, None); LABEL inr_label]
        @ c3 @ [APPLY; LABEL after_inr_label] )
  | If (e1, e2, e3) ->
      let else_label = new_label () in
      let after_else_label = new_label () in
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      let defs3, c3 = comp vmap e3 in
      ( defs1 @ defs2 @ defs3
      , c1
        @ [TEST (else_label, None)]
        @ c2
        @ [GOTO (after_else_label, None); LABEL else_label]
        @ c3 @ [LABEL after_else_label] )
  | Seq [] -> ([], [])
  | Seq [e] -> comp vmap e
  | Seq (e :: rest) ->
      let defs1, c1 = comp vmap e in
      let defs2, c2 = comp vmap (Seq rest) in
      (defs1 @ defs2, c1 @ [POP] @ c2)
  | Ref e ->
      let defs, c = comp vmap e in
      (defs, c @ [MK_REF])
  | Deref e ->
      let defs, c = comp vmap e in
      (defs, c @ [DEREF])
  | While (e1, e2) ->
      let test_label = new_label () in
      let end_label = new_label () in
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      ( defs1 @ defs2
      , [LABEL test_label] @ c1
        @ [TEST (end_label, None)]
        @ c2
        @ [POP; GOTO (test_label, None); LABEL end_label; PUSH STACK_UNIT] )
  | Assign (e1, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c1 @ c2 @ [ASSIGN])
  | App (e1, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c2 @ c1 @ [APPLY])
  | Var x -> ([], [LOOKUP (List.Assoc.find_exn vmap ~equal:(=) x)])
  | LetFun (f, (x, e1), e2) -> comp vmap (App (Lambda (f, e2), Lambda (x, e1)))
  | Lambda (x, e) -> comp_lambda vmap (None, x, e)
  | LetRecFun (f, (x, e1), e2) ->
      let defs1, c1 = comp vmap (Lambda (f, e2)) in
      let defs2, c2 = comp_lambda vmap (Some f, x, e1) in
      (defs1 @ defs2, c2 @ c1 @ [APPLY])

and comp_lambda vmap (f_opt, x, e) =
  let open Common.Jargon in
  let bound_vars = match f_opt with None -> [x] | Some f -> [x; f] in
  let f = match f_opt with None -> new_label () | Some f -> f in
  let f_bind =
    match f_opt with None -> [] | Some f -> [(f, STACK_LOCATION (-1))]
  in
  let x_bind = (x, STACK_LOCATION (-2)) in
  let fvars = Free_vars.free_vars bound_vars e in
  let fetch_fvars = List.map fvars ~f:(fun y -> LOOKUP (List.Assoc.find_exn vmap  ~equal:(=) y)) in
  let fvar_bind (y, p) = (y, HEAP_LOCATION p) in
  let env_bind = List.map (positions fvars) ~f:fvar_bind in
  let new_vmap = x_bind :: (f_bind @ env_bind @ vmap) in
  let defs, c = comp new_vmap e in
  let def = [LABEL f] @ c @ [RETURN] in
  ( def @ defs
  , List.rev fetch_fvars @ [MK_CLOSURE ((f, None), List.length fvars)] )

let compile (options: Options.t) e =
  let open Common.Jargon in
  let defs, c = comp [] e in
  let result =
    c (* body of program *)
    @ [HALT] (* stop the interpreter *)
    @ defs
  in
  (* the function definitions *)
  if options.verbose_back then
    print_string ("\nCompiled Code = \n" ^ string_of_listing result);
  result
