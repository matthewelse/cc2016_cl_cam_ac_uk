open Frontend
open Common.Interp_2

(** After a BIND, an env is left on the stack. SWAP; POP removes this from the stack. *)
let leave_scope = [SWAP; POP]

let rec compile = function
  | Ast.Unit -> [PUSH UNIT]
  | Integer n -> [PUSH (INT n)]
  | Boolean b -> [PUSH (BOOL b)]
  | Var x -> [LOOKUP x]
  | UnaryOp (op, e) -> compile e @ [UNARY op]
  | Op (e1, op, e2) -> compile e1 @ compile e2 @ [OPER op]
  | Pair (e1, e2) -> compile e1 @ compile e2 @ [MK_PAIR]
  | Fst e -> compile e @ [FST]
  | Snd e -> compile e @ [SND]
  | Inl e -> compile e @ [MK_INL]
  | Inr e -> compile e @ [MK_INR]
  | Case (e, (x1, e1), (x2, e2)) ->
      compile e
      @ [ CASE
            ( (BIND x1 :: compile e1) @ leave_scope
            , (BIND x2 :: compile e2) @ leave_scope ) ]
  | If (e1, e2, e3) -> compile e1 @ [TEST (compile e2, compile e3)]
  | Seq [] -> []
  | Seq [e] -> compile e
  | Seq (e :: rest) -> compile e @ [POP] @ compile (Seq rest)
  | Ref e -> compile e @ [MK_REF]
  | Deref e -> compile e @ [DEREF]
  | While (e1, e2) ->
      let cl = compile e1 in
      cl @ [WHILE (cl, compile e2)]
  | Assign (e1, e2) -> compile e1 @ compile e2 @ [ASSIGN]
  | App (e1, e2) ->
      compile e2 (* I chose to evaluate arg first *)
      @ compile e1 @ [APPLY; SWAP; POP]
      (* get rid of env left on stack *)
  | Lambda (x, e) -> [MK_CLOSURE ((BIND x :: compile e) @ leave_scope)]
  | LetFun (f, (x, body), e) ->
      MK_CLOSURE ((BIND x :: compile body) @ leave_scope)
      :: BIND f :: compile e
      @ leave_scope
  | LetRecFun (f, (x, body), e) ->
      MK_REC (f, (BIND x :: compile body) @ leave_scope)
      :: BIND f :: compile e
      @ leave_scope
