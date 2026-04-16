// Record literal where one field value is a parenthesized multi-line `match`
// whose bars align with the opening `(`. Followed by more fields at the
// record's offside column.
// Affects: TypedTreeOps.fs
type Kind =
    | Delegate of int
    | Other

type Data =
    { Cases: int list
      Kind: Kind
      VSlots: int list
      RFields: int list }

let remap ctxt tmenv (x: Data) =
    {
        Cases = x.Cases
        Kind =
            (match x.Kind with
            | Delegate n -> Delegate (n + 1)
            | _ -> x.Kind)
        VSlots = x.VSlots |> List.map id
        RFields = x.RFields |> List.map id }
