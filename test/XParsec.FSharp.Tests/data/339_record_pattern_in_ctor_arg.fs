// Inline record pattern as argument to constructor/union case pattern.
// Affects: SynPat.fs, SignatureHash.fs, ServiceStructure.fs.
type Kind = Ctor | Getter | Setter
type Flags = { MemberKind: Kind }
type Data = { Flags: Flags option }

let test (d: Data) =
    match d with
    | { Flags = Some { MemberKind = Ctor } } -> 1
    | { Flags = Some { MemberKind = Getter | Setter } } -> 2
    | _ -> 0
