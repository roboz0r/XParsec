module Test

// Recovery for malformed type annotations in let bindings.
// Each malformed binding should produce a local diagnostic but not cascade
// into subsequent bindings.

let a: int = 1

// Type.parse fails outright (numeric is not a valid type start).
// ReturnType.parse's recoverWith skips '999' until '='. Local recovery.
let bad1: 999 = 2

let b: string = "ok"

// Generic args list has no closing '>'. Previously: opt around the generic
// block silently fell back to NamedType Vector, leaving '<int' in the stream,
// pEquals saw '<' and the whole rest of the module cascaded. With the
// commit-on-'<' change in Type.parseAtomic: the missing '>' propagates as a
// Type.parse failure, ReturnType's recoverWith skips 'Vector < int' to '=',
// the binding's body and subsequent declarations parse cleanly.
let bad2: Vector<int = 3

let c: float = 4.0

// Empty type after ':'. Type.parse fails immediately; recovery returns
// Type.Missing without skipping.
let bad3: = 5

let d: int = 6
