open Frontend

(** This compiler is just the identity function, taking the abstract syntax
    tree and passing it on to the interpreter.
 *)
val compile : Ast.expr -> Ast.expr
