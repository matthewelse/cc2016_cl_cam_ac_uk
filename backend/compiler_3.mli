open Common
open Frontend

val compile : Options.t -> Ast.expr -> Interp_3.instruction list
