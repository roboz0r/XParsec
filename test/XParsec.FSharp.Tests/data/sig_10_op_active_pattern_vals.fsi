module Operators

val (+): int -> int -> int
val (=?): obj -> obj -> bool
val inline (|||): int -> int -> int

val (|Even|_|): int -> int option
val inline (|Length|): string -> int
val (|Empty|NonEmpty|): 'T list -> Choice<unit, 'T * 'T list>
