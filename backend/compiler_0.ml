(** This defines a program for interpreter zero. Since we directly interpret the
    abstract syntax tree, this compiler is the identity function. *)

let compile expr = expr
