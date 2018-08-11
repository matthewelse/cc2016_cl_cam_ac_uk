# Building an ARM backend for slang:

The jargon compiler goes a fair way towards the level of an architecture like ARM, it
has the notion of a program counter, representing instructions as a linear sequence,
rather than a tree like the earlier examples.

What do we need to do to make the jump from jargon to ARM assembly?

- Remove any remaining notion of types in the instruction set, this means we can't have
instructions like `Make_pair`. These should probably be replaced with (inlined) function
calls.
- Move from a stack-oriented machine to a register-oriented one (plus register allocation).
This shouldn't be too much of a problem, but depending on how this goes, I could just do 
the really inefficient example of doing all of the operations on the stack to start with.

Since we're aiming for a register-oriented solution, instead of pushing results onto the stack,
we should provide a temporary variable to put the result into.

## Representation of values in memory:

Once we move to the level of the ARM instruction set, we will no longer have the luxury of
the Ocaml type system to distinguish between the different kinds of `Stack_item`s or between
`Stack_item`s and `Heap_item`s. In essence, all of the operations will have to be performed
on integers.

We can use the later chapters from dev.realworldocaml.org to avoid thinking about too
many of these low-level details.

See here: https://dev.realworldocaml.org/runtime-memory-layout.html

Represent types as the following:

### int/bool/char/unit

Use the native integer width (64-bit or 32-bit), with the least-significant bit always set
to 1. (this indicates to the garbage collector that this is a value, not a pointer).

Likewise for true/false (slang int 1 and 0, respectively), characters and unit (slang int 0).

### everything else

These are all represented as `block`s on the heap - a block consists of a native-sized header,
followed by a series of values (which are defined recursively using these rules, as either
integers or a pointer to another block).

The header consists of either a 32-bit integer containing a 22-bit length, 2-bit 'colour' (used
by the GC), and 8 bit tag byte.

The 'tag' byte is used to indicate what kind of data is stored in the block (for example, a
tuple vs an array).


