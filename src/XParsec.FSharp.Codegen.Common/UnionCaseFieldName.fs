namespace XParsec.FSharp.Codegen.Common

/// How one union case field is named, before any target spells it. `of radius: float`
/// yields `Declared "radius"`; a positional field yields its 1-based position in the case,
/// or `Lone` where the case holds exactly one field. The position counts every field, not
/// only the positional ones, so `M of tag: string * float` yields `Declared "tag"` then
/// `Positional 2`.
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
