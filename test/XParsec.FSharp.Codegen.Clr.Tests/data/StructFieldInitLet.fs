[<Struct>]
type LetPair =
    val mutable A: int
    val mutable B: int
    new(a: int) = let d = a + a in { A = a; B = d }
