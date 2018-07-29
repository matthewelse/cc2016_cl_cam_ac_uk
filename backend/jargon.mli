open Frontend
open Common

val compile : Options.t -> Ast.expr -> Common.Jargon.Instruction.t list
