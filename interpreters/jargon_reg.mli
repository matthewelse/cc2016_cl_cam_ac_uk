open Common

module Vm_state : sig
    type t

    val to_string : t -> string
    val string_of_value : t -> string
end

val interpret : Options.t -> Common.Jargon_reg.Instruction.t list -> Vm_state.t
