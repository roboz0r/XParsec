namespace XParsec.FSharp.Codegen.Common

/// Target-neutral `%A` *grammar* oracle — the record / union output forms
/// (`{ F = ·; G = · }`, `None`, `Some ·`, `Case (·, ·)`) as pure data. The JS
/// shape-keyed walker is checked against this by a cross-target differential test;
/// the CLR runtime encodes the same forms independently in `structural-printer.fs`,
/// so both targets are pinned to this grammar without either consuming it directly.
module StructuralFormatRecipe =

    /// One step of a record / union's layout. `FormatChild`/`FormatArg` name a field
    /// by declaration-order *index*. `FormatArg` is DU-argument position (parenthesises
    /// a payload-bearing union, `Some (Some 3)`); `FormatChild` is normal position.
    type SinkOp =
        | Text of string
        | Line
        | SoftBreak
        | BeginGroup
        | EndGroup
        | BeginNest of int
        | EndNest
        | BeginApplication
        | EndApplication
        | FormatChild of childIndex: int
        | FormatArg of argIndex: int

    /// The record form `{ F = ·; G = · }`. First label opens `{ `; each subsequent
    /// field prefixes `;` + soft `Line` + `label = `. Fields hang at +2 when the group
    /// breaks. F# records always have ≥1 field; `[]` stays total (`{ }`).
    let recordRecipe (fieldNames: string list) : SinkOp list =
        match fieldNames with
        | [] -> [ Text "{ }" ]
        | name0 :: rest ->
            [
                BeginGroup
                Text(sprintf "{ %s = " name0)
                BeginNest 2
                FormatChild 0
                for i, name in List.indexed rest do
                    Text ";"
                    Line
                    Text(sprintf "%s = " name)
                    FormatChild(i + 1)
                EndNest
                Text " }"
                EndGroup
            ]

    /// One union case: nullary is a bare identifier (`None`); a single payload renders
    /// via `FormatArg` inside an application (parenthesised in arg position,
    /// `Some (Some 3)`); a multi-field payload is a parenthesised tuple of
    /// `FormatChild`ren (`Case (a, b)` — the tuple's own parens disambiguate).
    let unionCaseRecipe (caseName: string) (arity: int) : SinkOp list =
        match arity with
        | 0 -> [ Text caseName ]
        | 1 -> [ BeginApplication; Text(caseName + " "); FormatArg 0; EndApplication ]
        | n ->
            [
                BeginApplication
                Text(caseName + " ")
                BeginGroup
                Text "("
                BeginNest 1
                for i in 0 .. n - 1 do
                    if i > 0 then
                        Text ","
                        Line

                    FormatChild i
                EndNest
                Text ")"
                EndGroup
                EndApplication
            ]
