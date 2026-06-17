namespace XParsec.FSharp.Codegen.Common

/// Target-neutral `%A` *grammar* recipe — the single source of truth for the
/// output forms a record / union renders (`{ F = ·; G = · }`, `None`, `Some ·`,
/// `Case (·, ·)`). Pure data: no IL, no JS. The CLR backend lowers a `SinkOp`
/// list to `IFormatSink` `callvirt`s (`EmitStructuralFormat`); the JS central
/// shape-keyed walker is *checked against* the same grammar by a cross-target
/// differential test rather than consuming it (the dispatch split is deliberate —
/// see printf-shared-core-plan.md). A future opt-in per-type JS `Format` emitter
/// would consume this recipe the same way the CLR emitter does.
module StructuralFormatRecipe =

    /// One step of the sink-op sequence the synthesised `Format` body replays.
    /// `FormatChild`/`FormatArg` reference a field by *index* into the type's
    /// declaration-order fields; the backend resolves the index to the concrete
    /// field handle + type. `FormatArg` is DU-argument position (parenthesises a
    /// payload-bearing union, `Some (Some 3)`); `FormatChild` is normal position.
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

    /// The record form `{ F = ·; G = · }`. The first label opens with `{ `; each
    /// subsequent field is preceded by `;` + a soft `Line` + its `label = `. The
    /// fields hang at +2 indent (`BeginNest 2`) when the group breaks; `EndNest`
    /// then ` }` close. F# records always have ≥1 field; `[]` stays total (`{ }`).
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

    /// One union case. A nullary case is a bare identifier (`None`); a single
    /// payload renders via `FormatArg` inside an application (so it parenthesises
    /// in argument position, `Some (Some 3)`); a multi-field payload renders as a
    /// parenthesised tuple of `FormatChild`ren (`Case (a, b)` — the tuple parens
    /// already disambiguate, so components are normal-position children).
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
