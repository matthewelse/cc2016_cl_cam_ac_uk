open Frontend

(** Return a list of the free variables in an expression that are not also
    contained in a list of bound variables *)
val free_vars : Past.var list -> Ast.expr -> Past.var list
