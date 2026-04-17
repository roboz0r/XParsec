// Nested named-field pattern where an outer constructor argument is itself a
// named-field destructure, and the inner named-field pattern uses `;` as the
// separator between fields.
// Affects: FileContentMapping.fs
type Repr = Repr of attributes: int * caseName: string * longId: int list
type Defn = Defn of exnRepr: Repr * members: int list
type Decl = Exn of exnDefn: Defn

let visit (d: Decl) =
    match d with
    | Exn(
        exnDefn = Defn(
            exnRepr = Repr(attributes = a; caseName = c; longId = lid); members = m)) ->
        (a, c, lid, m)
