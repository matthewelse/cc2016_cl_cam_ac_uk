open Core

open Frontend.Ast
open Common
open Common.Jargon

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

 | (Apply :: ds,  V(CLOSURE (_, (c, env))) :: (V v) :: evs)
    -> (c @ ds, (V v) :: (EV env) :: evs)

Interp 3

 | (Apply,  V(CLOSURE ((_, Some i), env)) :: (V v) :: evs)
    -> (i, (V v) :: (EV env) :: (RA (cp + 1)) :: evs)


Jargon VM :

     [clsoure    ]
     [arg        ]
        ...

 == Apply ==>

     [return address]
fp ->[old fp        ]
     [clsoure       ]
     [arg           ]
        ...

*)

let rec comp vmap = 
  let open Common.Jargon.Instruction in
  function
  | Unit -> ([], [Push Stack_item.Unit])
  | Boolean b -> ([], [Push (Stack_item.Bool b)])
  | Integer n -> ([], [Push (Stack_item.Int n)])
  | UnaryOp (op, e) ->
      let defs, c = comp vmap e in
      (defs, c @ [Unary op])
  | Op (e1, op, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c1 @ c2 @ [Oper op])
  | Pair (e1, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c1 @ c2 @ [Make_pair])
  | Fst e ->
      let defs, c = comp vmap e in
      (defs, c @ [Fst])
  | Snd e ->
      let defs, c = comp vmap e in
      (defs, c @ [Snd])
  | Inl e ->
      let defs, c = comp vmap e in
      (defs, c @ [Make_inl])
  | Inr e ->
      let defs, c = comp vmap e in
      (defs, c @ [Make_inr])
  | Case (e1, (x1, e2), (x2, e3)) ->
      let inr_label = new_label () in
      let after_inr_label = new_label () in
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap (Lambda (x1, e2)) in
      let defs3, c3 = comp vmap (Lambda (x2, e3)) in
      ( defs1 @ defs2 @ defs3
      , c1
        @ [Case (inr_label, None)]
        @ c2
        @ [Apply; Goto (after_inr_label, None); Label inr_label]
        @ c3 @ [Apply; Label after_inr_label] )
  | If (e1, e2, e3) ->
      let else_label = new_label () in
      let after_else_label = new_label () in
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      let defs3, c3 = comp vmap e3 in
      ( defs1 @ defs2 @ defs3
      , c1
        @ [Test (else_label, None)]
        @ c2
        @ [Goto (after_else_label, None); Label else_label]
        @ c3 @ [Label after_else_label] )
  | Seq [] -> ([], [])
  | Seq [e] -> comp vmap e
  | Seq (e :: rest) ->
      let defs1, c1 = comp vmap e in
      let defs2, c2 = comp vmap (Seq rest) in
      (defs1 @ defs2, c1 @ [Pop] @ c2)
  | Ref e ->
      let defs, c = comp vmap e in
      (defs, c @ [Make_ref])
  | Deref e ->
      let defs, c = comp vmap e in
      (defs, c @ [Deref])
  | While (e1, e2) ->
      let test_label = new_label () in
      let end_label = new_label () in
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      ( defs1 @ defs2
      , [Label test_label] @ c1
        @ [Test (end_label, None)]
        @ c2
        @ [Pop; Goto (test_label, None); Label end_label; Push Stack_item.Unit] )
  | Assign (e1, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c1 @ c2 @ [Assign])
  | App (e1, e2) ->
      let defs1, c1 = comp vmap e1 in
      let defs2, c2 = comp vmap e2 in
      (defs1 @ defs2, c2 @ c1 @ [Apply])
  | Var x -> ([], [Lookup (List.Assoc.find_exn vmap ~equal:(=) x)])
  | LetFun (f, (x, e1), e2) -> comp vmap (App (Lambda (f, e2), Lambda (x, e1)))
  | Lambda (x, e) -> comp_lambda vmap (None, x, e)
  | LetRecFun (f, (x, e1), e2) ->
      let defs1, c1 = comp vmap (Lambda (f, e2)) in
      let defs2, c2 = comp_lambda vmap (Some f, x, e1) in
      (defs1 @ defs2, c2 @ c1 @ [Apply])

and comp_lambda vmap (f_opt, x, e) =
  let open Common.Jargon in
  let open Common.Jargon.Instruction in
  let bound_vars = match f_opt with None -> [x] | Some f -> [x; f] in
  let f = match f_opt with None -> new_label () | Some f -> f in
  let f_bind =
    match f_opt with None -> [] | Some f -> [(f, STACK_LOCATION (-1))]
  in
  let x_bind = (x, STACK_LOCATION (-2)) in
  let fvars = Free_vars.free_vars bound_vars e in
  let fvar_bind p y = (y, HEAP_LOCATION (p + 1)) in
  let env_bind = List.mapi fvars ~f:fvar_bind in
  let fetch_fvars = List.map fvars ~f:(fun y -> Lookup (List.Assoc.find_exn vmap  ~equal:(=) y)) in
  let new_vmap = x_bind :: (f_bind @ env_bind @ vmap) in
  let defs, c = comp new_vmap e in
  let def = [Label f] @ c @ [Return] in
  ( def @ defs
  , List.rev fetch_fvars @ [Make_closure ((f, None), List.length fvars)] )

let compile (options: Options.t) e =
  let open Common.Jargon.Instruction in
  let defs, c = comp [] e in
  let result =
    c (* body of program *)
    @ [Halt] (* stop the interpreter *)
    @ defs
  in
  (* the function definitions *)
  if options.verbose_back then
    print_string ("\nCompiled Code = \n" ^ Common.Jargon.string_of_listing result);
  result
