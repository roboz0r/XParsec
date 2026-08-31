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
