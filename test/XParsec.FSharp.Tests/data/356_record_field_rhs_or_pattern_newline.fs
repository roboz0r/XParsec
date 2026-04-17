// Record pattern where a field's RHS is a multi-line Or-pattern whose
// '|' alternative sits at a column below the RHS's starting column but
// at/above the field's column. Exercises `pFieldPat`'s RHS context: a
// redundant `withContext SeqBlock` wrap around the RHS would block the
// '|' offside-wise, preventing the Or-chain from extending.
type MyDU =
    | A of int
    | B of int

type Record = { Value: MyDU; Count: int }

let test r =
    match r with
    | { Value = A x
              | B x
        Count = n } -> (x, n)
