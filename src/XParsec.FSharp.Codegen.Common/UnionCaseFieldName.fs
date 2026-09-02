namespace XParsec.FSharp.Codegen.Common

/// How one union case field is named, before any target spells it. `of radius: float` ⇒
/// `Declared "radius"`; a case's sole field ⇒ `Lone`; a positional field ⇒ its 1-based
/// position counted over every field, so `M of tag: string * float` ⇒ `Positional 2`.
[<RequireQualifiedAccess>]
type UnionCaseFieldName =
    | Declared of name: string
    | Lone
    | Positional of position: int

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UnionCaseFieldName =

    /// Each field's name verdict, in declaration order. `declared` is `ValueNone` where the
    /// source wrote the field positionally.
    let ofCase (declared: string voption list) : UnionCaseFieldName list =
        match declared with
        | [ ValueSome n ] -> [ UnionCaseFieldName.Declared n ]
        | [ ValueNone ] -> [ UnionCaseFieldName.Lone ]
        | many ->
            many
            |> List.mapi (fun i d ->
                match d with
                | ValueSome n -> UnionCaseFieldName.Declared n
                | ValueNone -> UnionCaseFieldName.Positional(i + 1)
            )

    /// Each field's name in F#'s own spelling, in declaration order: a declared name
    /// verbatim, `Item` for a lone positional field, `Item<n>` otherwise.
    let fsharpNames (declared: string voption list) : string list =
        ofCase declared
        |> List.map (fun n ->
            match n with
            | UnionCaseFieldName.Declared name -> name
            | UnionCaseFieldName.Lone -> "Item"
            | UnionCaseFieldName.Positional i -> "Item" + string i
        )

    /// Each field's name in FSC's backing-field spelling, in declaration order:
    /// `of radius: float` ⇒ `_radius`, a lone positional field `item`, `item<n>` otherwise.
    let fscFieldNames (declared: string voption list) : string list =
        ofCase declared
        |> List.map (fun n ->
            match n with
            | UnionCaseFieldName.Declared name -> "_" + name
            | UnionCaseFieldName.Lone -> "item"
            | UnionCaseFieldName.Positional i -> "item" + string i
        )
