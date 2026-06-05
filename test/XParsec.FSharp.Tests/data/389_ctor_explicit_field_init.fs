module Test

// Additional constructor body: explicit field-init form `new(...) = { f = e; ... }`.
// A `;`-separated multi-field init block used to fail — `pRecordInit` stopped at
// the first `;`, leaving it to fail `}` so the whole `new(...)` member leaked to
// module scope. Fields are now separated like a record literal.
type Pair =
    val mutable A: int
    val mutable B: int
    new(a: int, b: int) = { A = a; B = b }

// A `let`-preamble may precede the field-init block.
type LetPair =
    val mutable A: int
    val mutable B: int
    new(a: int) = let d = a + a in { A = a; B = d }
