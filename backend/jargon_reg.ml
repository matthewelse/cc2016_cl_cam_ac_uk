open Core
open Frontend.Ast
open Common
open Common.Jargon_reg

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

let rec comp vmap return_reg =
  let open Common.Jargon_reg.Instruction in
  function
    | Unit -> ([], [Set (return_reg, Register_item.Unit)])
    | Boolean b -> ([], [Set (return_reg, Register_item.Bool b)])
    | Integer n -> ([], [Set (return_reg, Register_item.Int n)])
    | UnaryOp (op, e) ->
        let value = Register.fresh () in
        let defs, c = comp vmap value e in
        (defs, c @ [Unary (op, return_reg, value)])
    | Op (e1, op, e2) ->
        let left = Register.fresh () in
        let right = Register.fresh () in
        let defs1, c1 = comp vmap left e1 in
        let defs2, c2 = comp vmap right e2 in
        (defs1 @ defs2, c1 @ c2 @ [Oper (op, return_reg, (left, right))])
    | Pair (e1, e2) ->
        let left = Register.fresh () in
        let right = Register.fresh () in
        let defs1, c1 = comp vmap left e1 in
        let defs2, c2 = comp vmap right e2 in
        (defs1 @ defs2, c1 @ c2 @ [Make_pair (return_reg, (left, right))])
    | Fst e ->
        let value = Register.fresh () in
        let defs, c = comp vmap value e in
        (defs, c @ [Fst (return_reg, value)])
    | Snd e ->
        let value = Register.fresh () in
        let defs, c = comp vmap value e in
        (defs, c @ [Snd (return_reg, value)])
    | Inl e ->
        let value = Register.fresh () in
        let defs, c = comp vmap value e in
        (defs, c @ [Make_inl (return_reg, value)])
    | Inr e ->
        let value = Register.fresh () in
        let defs, c = comp vmap value e in
        (defs, c @ [Make_inr (return_reg, value)])
    | If (e1, e2, e3) ->
        let else_label = new_label () in
        let after_else_label = new_label () in
        let condition = Register.fresh () in
        let defs1, c1 = comp vmap condition e1 in
        let defs2, c2 = comp vmap return_reg e2 in
        let defs3, c3 = comp vmap return_reg e3 in
        ( defs1 @ defs2 @ defs3
        , c1
          @ [Test (condition, (else_label, None))]
          @ c2
          @ [Goto (after_else_label, None); Label else_label]
          @ c3 @ [Label after_else_label] )
    | Seq [] -> ([], [])
    | Seq [e] -> comp vmap return_reg e
    | Seq (e :: rest) ->
        (* the start of the expression should not have a return value, so create a
        dummy variable to store nothing *)
        let ignored_result = Register.fresh () in
        let defs1, c1 = comp vmap ignored_result e in
        let defs2, c2 = comp vmap return_reg (Seq rest) in
        (defs1 @ defs2, c1 @ c2)
    | Ref e ->
        let value = Register.fresh () in
        let defs, c = comp vmap value e in
        (defs, c @ [Make_ref (return_reg, value)])
    | Deref e ->
        let value = Register.fresh () in
        let defs, c = comp vmap value e in
        (defs, c @ [Deref (return_reg, value)])
    | While (e1, e2) ->
        let condition = Register.fresh () in
        let test_label = new_label () in
        let end_label = new_label () in
        let defs1, c1 = comp vmap condition e1 in
        let defs2, c2 = comp vmap return_reg e2 in
        ( defs1 @ defs2
        , [Label test_label] @ c1
          @ [Test (condition, (end_label, None))]
          @ c2
          @ [ Goto (test_label, None)
            ; Label end_label
            ; Set (return_reg, Register_item.Unit) ] )
    | Assign (e1, e2) ->
        (* *e1 = e2 *)
        let r1 = Register.fresh () in
        let r2 = Register.fresh () in
        let defs1, c1 = comp vmap r1 e1 in
        let defs2, c2 = comp vmap r2 e2 in
        (* returns unit *)
        ( defs1 @ defs2
        , c1 @ c2 @ [Assign (r1, r2); Set (return_reg, Register_item.Unit)] )
    | Lambda (x, e) -> comp_lambda vmap return_reg (None, x, e)
    | Var x ->
        ([], [Lookup (return_reg, List.Assoc.find_exn vmap ~equal:( = ) x)])
    | LetFun (f, (x, lambda), expr) ->
        comp vmap return_reg (App (Lambda (f, expr), Lambda (x, lambda)))
    | LetRecFun (f, (x, lambda), expr) ->
        (* let x = lambda in expr *)
        let let_lambda = Register.fresh () in
        let inner_lambda = Register.fresh () in
        let defs1, c1 = comp vmap let_lambda (Lambda (f, expr)) in
        let defs2, c2 = comp_lambda vmap inner_lambda (Some f, x, lambda) in
        (* construct the argument to the inner body of the let (i.e. the function),
           then construct the closure for the body, then call the body with the function
           as an argument. Can we prove that c1 will never write to the
           Function_argument register, to avoid the move instruction? *)
        ( defs1 @ defs2
        , c2 @ c1
          @ [ Push Function_argument
            ; Mov (Function_argument, inner_lambda)
            ; Apply let_lambda
            ; Pop Function_argument ] )
    | App (e1, e2) ->

        let push_all_registers =
          List.range 0 !reg_index
          |> List.map ~f:(fun x -> Push (Temporary x))
        in
        let pop_all_registers =
          List.rev push_all_registers
        |> List.map ~f:(function Push x -> Pop x | y -> y) in
        let r1 = Register.fresh () in
        let r2 = Register.fresh () in
        let defs1, c1 = comp vmap r1 e1 in
        let defs2, c2 = comp vmap r2 e2 in
        (* compute the argument before the function consuming it *)
        ( defs1 @ defs2
        , c2 @ c1 @
        push_all_registers
          @ [ Push Function_argument
            ; Mov (Function_argument, r2)
            ; Apply r1
            ; Pop Function_argument ] @ pop_all_registers
            @ [Mov (return_reg, Return_value)])
    | Case _ -> failwith "unable to do cases"

and comp_lambda vmap dest (f_opt, x, e) =
  let open Common.Jargon_reg in
  let open Common.Jargon_reg.Instruction in
  (* the bound variables are either just {x} or {x, f} if f is recursive. *)
  let bound_vars = x :: Option.to_list f_opt in
  (* when computing a lambda, we need to first collect together all of the free
     variables into a closure: *)
  let free_vars = Free_vars.free_vars bound_vars e in
  (* either f is recursive (and has an existing label), or we should create one. *)
  (* where do we find the bound value of f? f is assigned to a register when we make
     closure, so make the caller decide this in advance. *)
  let f = match f_opt with None -> new_label () | Some f -> f in
  let f_bind =
    match f_opt with Some f -> [(f, REGISTER_LOCATION dest)] | None -> []
  in
  let x_bind = (x, REGISTER_LOCATION Register.Function_argument) in
  (* free variables exist in the closure *)
  (* make sure this is deterministic! *)
  let fv_bind p y = (y, HEAP_LOCATION (p + 1)) in
  let env_bind = List.mapi free_vars ~f:fv_bind in
  let fetch_fvars =
    List.map free_vars ~f:(fun y ->
        Lookup (Register.fresh (), List.Assoc.find_exn vmap ~equal:( = ) y) )
  in
  let new_vmap = x_bind :: (f_bind @ env_bind @ vmap) in
  let defs, c = comp new_vmap Return_value e in
  let def = [Label f] @ c @ [Return] in
  (* fetch fvars and put them into a closure, put the resulting pointer in dest *)
  ( def @ defs
  , List.rev fetch_fvars
    @ [Make_closure (dest, (f, None), List.length free_vars)] )

let compile (options: Options.t) e =
  let open Common.Jargon_reg.Instruction in
  let defs, c = comp [] Return_value e in
  let result =
    c (* body of program *)
    @ [Halt] (* stop the interpreter *)
    @ defs
  in
  (* the function definitions *)
  if options.verbose_back then
    printf "\nCompiled code =\n%s\n"
      (Common.Jargon_reg.string_of_listing result) ;
  result
