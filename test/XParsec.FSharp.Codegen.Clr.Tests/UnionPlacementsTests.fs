module XParsec.FSharp.Codegen.Clr.Tests.UnionPlacementsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `FlatUnionPlacements.ofCases` over the real contract stack: which slot each logical case
// field lands in, and how many slots the cases of the `StructUnion*` data programs need
// between them.

/// `<slot> : <stored type>` per physical slot, in field-row order.
let private slotLines (p: FlatUnionPlacements) : string list =
    [
        for s in p.Slots -> sprintf "%s: %s" s.MetaName (ConformanceTypars.describeType s.Ty)
    ]

/// `<Case>.<i> -> <slot>` per logical field, with the `castclass` target of an erased read,
/// or `-> _data.<CaseStruct>.<field>` for an overlaid one.
let private placementLines (p: FlatUnionPlacements) : string list =
    [
        for c in p.Cases do
            for f in c.Fields ->
                match f.Access with
                | UnionFieldAccess.Direct s -> sprintf "%s.%d -> %s" f.Case f.Index s.MetaName
                | UnionFieldAccess.Erased(s, declared) ->
                    sprintf "%s.%d -> %s as %s" f.Case f.Index s.MetaName (ConformanceTypars.describeType declared)
                | UnionFieldAccess.Overlaid o ->
                    sprintf "%s.%d -> _data.%s.%s" f.Case f.Index (UnionPayloadType.caseDataName o.Case) o.MetaName
    ]

[<Tests>]
let tests =
    testList
        "UnionPlacements"
        [
            // The placement table over the struct-union data corpus. Changing the
            // classifier, a slot's spelling or the assignment order changes this table.
            test "census over the struct-union data corpus" {
                let actual =
                    [
                        for entry in structUnionCorpus do
                            let symbols, u = analysedStructUnion entry
                            let p = flatPlacementsOf symbols u
                            yield sprintf "%s slots: %s" entry.Union (String.concat ", " (slotLines p))
                            yield! placementLines p
                    ]

                Expect.equal
                    actual
                    [
                        // Every field is an unmanaged `int`, so each case's fields sit on
                        // its own data struct and the overlay is the only slot.
                        "Shape slots: _data: Shape$Data"
                        "Point.0 -> _data.Data_Point._x"
                        "Pair.0 -> _data.Data_Pair._a"
                        "Pair.1 -> _data.Data_Pair._b"
                        // A typar is stored exactly; the `int` case lands in the overlay.
                        "GBox slots: _data: GBox$Data$1, _val0: '0"
                        "Val.0 -> _val0"
                        "Num.0 -> _data.Data_Num._n"
                        "Mixed slots: _data: Mixed$Data, _ref0: obj"
                        "I.0 -> _data.Data_I._x"
                        "S.0 -> _ref0 as string"
                        // A struct record of scalars is unmanaged and joins the overlay;
                        // `Guid` and `DateTime` are undetermined and take an exact slot each.
                        "External slots: _data: External$Data, _ref0: obj, _val0: Guid, _val1: DateTime"
                        "Scalars.0 -> _data.Data_Scalars._x"
                        "Scalars.1 -> _data.Data_Scalars._y"
                        "Nested.0 -> _data.Data_Nested._inner"
                        "Text.0 -> _ref0 as string"
                        "Id.0 -> _val0"
                        "Stamp.0 -> _val1"
                        // A record declared in this compilation is a settled reference,
                        // so it shares the `object` slot with `string`. No case is
                        // unmanaged, so there is no overlay.
                        "Holder slots: _ref0: obj, _val0: '0"
                        "Val.0 -> _val0"
                        "Text.0 -> _ref0 as string"
                        "Rec.0 -> _ref0 as Node"
                        // All four storage kinds in one union: `Tagged` holds a string, so
                        // it is a managed struct in an exact slot; `Both` splits its two
                        // fields between the overlay and the `object` slot.
                        "Storage slots: _data: Storage$Data, _ref0: obj, _val0: Tagged, _val1: Guid"
                        "Scalars.0 -> _data.Data_Scalars._x"
                        "Scalars.1 -> _data.Data_Scalars._y"
                        "Nested.0 -> _data.Data_Nested._inner"
                        "Text.0 -> _ref0 as string"
                        "Labelled.0 -> _val0"
                        "Id.0 -> _val1"
                        "Both.0 -> _data.Data_Both._k"
                        "Both.1 -> _ref0 as string"
                        "GShape slots: _data: GShape$Data$1, _val0: '0"
                        "Val.0 -> _val0"
                        "Pt.0 -> _data.Data_Pt._x"
                        "Pt.1 -> _data.Data_Pt._y"
                        // A case named `X_0` beside `X`: the case name reaches the reader
                        // spellings, never the placements.
                        "Readers slots: _data: Readers$Data, _ref0: obj"
                        "X.0 -> _data.Data_X._a"
                        "X.1 -> _data.Data_X._b"
                        "X_0.0 -> _ref0 as string"
                    ]
                    "the placement table"
            }

            // Slot assignment feeds the `.ctor` signature, the field rows and every read
            // path, so two computations over one shape must agree exactly.
            test "two computations of one shape agree" {
                for entry in structUnionCorpus do
                    let symbols, u = analysedStructUnion entry
                    Expect.equal (flatPlacementsOf symbols u) (flatPlacementsOf symbols u) entry.Union
            }

            // A case's factory writes all of its own fields into one `Payload`, so two of
            // its fields sharing a slot would lose one of them. An overlaid field has its
            // own field on the case's data struct by construction.
            test "a case's own fields take distinct slots" {
                for entry in structUnionCorpus do
                    let symbols, u = analysedStructUnion entry
                    let p = flatPlacementsOf symbols u
                    let unionName = entry.Union

                    for c in p.Cases do
                        let keys =
                            [
                                for f in c.Fields do
                                    match f.Access with
                                    | UnionFieldAccess.Direct s
                                    | UnionFieldAccess.Erased(s, _) -> s.Key
                                    | UnionFieldAccess.Overlaid _ -> ()
                            ]

                        Expect.equal
                            (List.length (List.distinct keys))
                            (List.length keys)
                            (sprintf "%s.%s" unionName c.Case.Name)
            }

            // The overlay lists exactly the cases with an unmanaged field, and `_data` is
            // declared exactly when the overlay is non-empty.
            test "the overlay holds exactly the cases with an unmanaged field" {
                for entry in structUnionCorpus do
                    let symbols, u = analysedStructUnion entry
                    let p = flatPlacementsOf symbols u
                    let unionName = entry.Union

                    let overlaidCases =
                        [
                            for c in p.Cases do
                                let overlaid =
                                    [
                                        for f in c.Fields do
                                            match f.Access with
                                            | UnionFieldAccess.Overlaid o -> o
                                            | UnionFieldAccess.Direct _
                                            | UnionFieldAccess.Erased _ -> ()
                                    ]

                                if not (List.isEmpty overlaid) then
                                    ({
                                        Case = c.Case.Name
                                        Fields = overlaid
                                    }
                                    : UnionCaseData)
                        ]

                    Expect.equal p.OverlaidCases overlaidCases unionName

                    Expect.equal
                        (p.Slots |> List.exists (fun s -> s.Key = UnionSlotKey.Data))
                        (not (List.isEmpty overlaidCases))
                        (unionName + " declares `_data`")
            }

            // Every payload-bearing case of a `Payload` home has a view, and the union's
            // field readers cover every placed field.
            test "the views and the field readers are projections of the placement table" {
                for entry in structUnionCorpus do
                    let symbols, u = analysedStructUnion entry
                    let p = flatPlacementsOf symbols u
                    let unionName = entry.Union
                    let cases = u.Cases

                    Expect.equal
                        [ for v in p.Views -> v.Case.Name ]
                        [
                            for c in cases do
                                if not c.Fields.IsEmpty then
                                    c.Name
                        ]
                        (unionName + " has a view per payload-bearing case")

                    Expect.equal
                        [ for g in p.Getters -> g.Case, g.Index ]
                        [
                            for c in cases do
                                for i in 0 .. c.Fields.Length - 1 -> c.Name, i
                        ]
                        (unionName + " has a field reader per logical field")

                    for v in p.Views do
                        let c = cases |> List.find (fun c -> c.Name = v.Case.Name)

                        Expect.equal
                            [ for f in v.Fields -> f.PropertyName ]
                            (UnionCaseFieldName.fsharpNames [ for (n, _) in c.Fields -> n ])
                            (sprintf "%s.%s spells its properties in F#'s own spelling" unionName v.Case.Name)
            }

            // A single case has nothing to overlap with, so its fields keep FSC's spelling
            // and their declared types.
            test "a single-case union keeps one slot per field at its declared type" {
                let symbols, decls =
                    analysedSymbols
                        defaultPackages
                        "SingleCasePlacement"
                        """
[<Struct>]
type Boxed = B of label: string * count: int

printfn "%d" (match B("a", 1) with | B(_, n) -> n)
"""

                let p = flatPlacementsOf symbols (unionOf decls "Boxed")

                Expect.equal (slotLines p) [ "_label: string"; "_count: int" ] "FSC-spelled, declared types"
                Expect.equal (placementLines p) [ "B.0 -> _label"; "B.1 -> _count" ] "no erasure, no sharing"
            }

            // A referencing compilation reads the overlay's `TypeDef` name back through
            // `typeKeyOfSegment`, so the spelling must parse to the key it was minted from.
            test "the overlay's metadata name reads back as its own key" {
                for entry in structUnionCorpus do
                    let _, u = analysedStructUnion entry
                    let overlayKey = UnionPayloadType.overlayKey u.Key

                    Expect.equal
                        (SymbolKeyOps.typeKeyOfSegment u.Key.Container (UnionPayloadType.overlayName u.Key))
                        overlayKey
                        entry.Union

                    Expect.equal overlayKey.TyparArity 0 (entry.Union + "'s overlay is non-generic")
            }
        ]
