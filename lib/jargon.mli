type code_index = int

type stack_index = int

type vm_state

val interpret : Options.t -> Ast.expr -> vm_state

val string_of_value : vm_state -> string
