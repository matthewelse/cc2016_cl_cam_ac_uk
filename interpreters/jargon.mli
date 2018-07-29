open Common

type vm_state

val interpret : Options.t -> Common.Jargon.instruction list -> vm_state

val string_of_value : vm_state -> string
