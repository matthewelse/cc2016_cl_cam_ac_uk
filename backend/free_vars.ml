open Core
open Frontend.Ast

(** Returns true if x is contained in the list l. *)
let inlist l x = List.exists l ~f:((=) x)

(** Return a list of the free variables in an expression that are not also
    contained in a list of bound variables.

    TODO me390: See if the use of List instead of Set here has any significant
    impact on performance. *)
let free_vars bvars exp =
  let rec aux bound free = function
    | Var x -> if inlist bound x || inlist free x then free else x :: free
    | UnaryOp (_, e) -> aux bound free e
    | Op (e1, _, e2) -> aux bound (aux bound free e1) e2
    | If (e1, e2, e3) -> aux bound (aux bound (aux bound free e1) e2) e3
    | Pair (e1, e2) -> aux bound (aux bound free e1) e2
    | App (e1, e2) -> aux bound (aux bound free e1) e2
    | Fst e -> aux bound free e
    | Snd e -> aux bound free e
    | Inl e -> aux bound free e
    | Inr e -> aux bound free e
    | Lambda l -> lambda bound free l
    | Case (e, l1, l2) -> lambda bound (lambda bound (aux bound free e) l1) l2
    | LetFun (f, l, e) -> aux (f :: bound) (lambda bound free l) e
    | LetRecFun (f, l, e) -> aux (f :: bound) (lambda (f :: bound) free l) e
    | Ref e -> aux bound free e
    | Deref e -> aux bound free e
    | Assign (e1, e2) -> aux bound (aux bound free e1) e2
    | While (e1, e2) -> aux bound (aux bound free e1) e2
    | Seq [] -> free
    | Seq (e :: rest) -> aux bound (aux bound free e) (Seq rest)
    | _ -> free
  and lambda bound free (x, e) = aux (x :: bound) free e in
  aux bvars [] exp
