module XParsec.FSharp.Codegen.Clr.Tests.UnionPlacementsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `FlatUnionPlacements.ofCases` over the real contract stack: which slot each logical case
// field lands in, and how many slots the cases of the `StructUnion*` data programs need
// between them.

/// The declaration of the union `unionName`, with its regime and cases.
type private AnalysedUnion =
    {
        Key: TypeKey
        Regime: UnionRegime
        Cases: Frozen.TUnionCase list
    }

/// Raises when the analysed program declares no such union.
let private unionOf (decls: TastAccessor.DeclId list) (unionName: string) : AnalysedUnion =
    let found =
        [
            for d in decls do
                match TastAccessor.declKind d with
                | DeclShape.Type ->
                    let td = TastAccessor.declType d

                    match td.Kind with
                    | TTypeKindG.Union u when td.Name = unionName ->
                        {
                            Key = td.TypeKey
                            Regime =
                                UnionRegime.classify
                                    u.ValueKind
                                    u.Cases.Length
                                    (u.Cases |> EqArray.exists (fun c -> not c.Fields.IsEmpty))
                            Cases = EqArray.toList u.Cases
                        }
                    | _ -> ()
                | _ -> ()
        ]

    match found with
    | [ one ] -> one
    | _ -> failwithf "no union '%s' among the analysed declarations" unionName

/// Raises when the union's regime is a hierarchy one.
let private ofAnalysed (symbols: ICodegenSymbols) (u: AnalysedUnion) : FlatUnionPlacements =
    match FlatUnionPlacements.ofCases symbols u.Key u.Regime u.Cases with
    | ValueSome p -> p
    | ValueNone -> failwithf "%A is a hierarchy regime" u.Regime

/// `<slot> : <stored type>` per physical slot, in field-row order.
let private slotLines (p: FlatUnionPlacements) : string list =
    [
        for s in p.Slots -> sprintf "%s: %s" s.MetaName (ConformanceTypars.describeType s.Ty)
    ]

/// `<Case>.<i> -> <slot>` per logical field, with the `castclass` target of an erased read,
/// or `-> _data.<CaseStruct>.<field>` for an overlaid one.
let private placementLines (p: FlatUnionPlacements) (cases: Frozen.TUnionCase list) : string list =
    [
        for c in cases do
            for (fi, access) in List.indexed (p.CaseAccess c) ->
                match access with
                | UnionFieldAccess.Direct s -> sprintf "%s.%d -> %s" c.Name fi s.MetaName
                | UnionFieldAccess.Erased(s, declared) ->
                    sprintf "%s.%d -> %s as %s" c.Name fi s.MetaName (ConformanceTypars.describeType declared)
                | UnionFieldAccess.Overlaid f ->
                    sprintf "%s.%d -> _data.%s.%s" c.Name fi (UnionPayloadType.caseDataName f.Case) f.MetaName
    ]

let private placementsOf
    (symbols: ICodegenSymbols)
    (decls: TastAccessor.DeclId list)
    (unionName: string)
    : FlatUnionPlacements * Frozen.TUnionCase list =
    let u = unionOf decls unionName
    ofAnalysed symbols u, u.Cases

/// The corpus the census and the determinism pin both run over.
let private corpus =
    [
        "StructUnionShape", "Shape"
        "StructUnionGenericShape", "GBox"
        "StructUnionSameNameFields", "Mixed"
        "StructUnionExternalPayload", "Payload"
        "StructUnionLocalRefPayload", "Holder"
        "StructUnionMixedStorage", "Storage"
        "StructUnionGenericOverlay", "GShape"
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
                        for (program, unionName) in corpus do
                            let symbols, decls = analysedSymbols defaultPackages program (dataSource program)
                            let p, cases = placementsOf symbols decls unionName
                            yield sprintf "%s slots: %s" unionName (String.concat ", " (slotLines p))
                            yield! placementLines p cases
                    ]

                Expect.equal
                    actual
                    [
                        // Every field is an unmanaged `int`, so each case's fields sit on
                        // its own data struct and the overlay is the only slot.
                        "Shape slots: _data: Data"
                        "Point.0 -> _data.Data_Point._x"
                        "Pair.0 -> _data.Data_Pair._a"
                        "Pair.1 -> _data.Data_Pair._b"
                        // A typar is stored exactly; the `int` case lands in the overlay.
                        "GBox slots: _data: Data, _val0: '0"
                        "Val.0 -> _val0"
                        "Num.0 -> _data.Data_Num._n"
                        "Mixed slots: _data: Data, _ref0: obj"
                        "I.0 -> _data.Data_I._x"
                        "S.0 -> _ref0 as string"
                        // A struct record of scalars is unmanaged and joins the overlay;
                        // `Guid` and `DateTime` are undetermined and take an exact slot each.
                        "Payload slots: _data: Data, _ref0: obj, _val0: Guid, _val1: DateTime"
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
                        "Storage slots: _data: Data, _ref0: obj, _val0: Tagged, _val1: Guid"
                        "Scalars.0 -> _data.Data_Scalars._x"
                        "Scalars.1 -> _data.Data_Scalars._y"
                        "Nested.0 -> _data.Data_Nested._inner"
                        "Text.0 -> _ref0 as string"
                        "Labelled.0 -> _val0"
                        "Id.0 -> _val1"
                        "Both.0 -> _data.Data_Both._k"
                        "Both.1 -> _ref0 as string"
                        "GShape slots: _data: Data, _val0: '0"
                        "Val.0 -> _val0"
                        "Pt.0 -> _data.Data_Pt._x"
                        "Pt.1 -> _data.Data_Pt._y"
                    ]
                    "the placement table"
            }

            // Slot assignment feeds the `.ctor` signature, the field rows and every read
            // path, so two computations over one shape must agree exactly.
            test "two computations of one shape agree" {
                for (program, unionName) in corpus do
                    let symbols, decls = analysedSymbols defaultPackages program (dataSource program)
                    let u = unionOf decls unionName
                    Expect.equal (ofAnalysed symbols u) (ofAnalysed symbols u) unionName
            }

            // A case's factory writes all of its own fields into one `Payload`, so two of
            // its fields sharing a slot would lose one of them. An overlaid field has its
            // own field on the case's data struct by construction.
            test "a case's own fields take distinct slots" {
                for (program, unionName) in corpus do
                    let symbols, decls = analysedSymbols defaultPackages program (dataSource program)
                    let p, cases = placementsOf symbols decls unionName

                    for c in cases do
                        let keys =
                            [
                                for a in p.CaseAccess c do
                                    match a with
                                    | UnionFieldAccess.Direct s
                                    | UnionFieldAccess.Erased(s, _) -> s.Key
                                    | UnionFieldAccess.Overlaid _ -> ()
                            ]

                        Expect.equal
                            (List.length (List.distinct keys))
                            (List.length keys)
                            (sprintf "%s.%s" unionName c.Name)
            }

            // The overlay lists exactly the cases with an unmanaged field, and `_data` is
            // declared exactly when the overlay is non-empty.
            test "the overlay holds exactly the cases with an unmanaged field" {
                for (program, unionName) in corpus do
                    let symbols, decls = analysedSymbols defaultPackages program (dataSource program)
                    let p, cases = placementsOf symbols decls unionName

                    let overlaidCases =
                        [
                            for c in cases do
                                let overlaid =
                                    [
                                        for a in p.CaseAccess c do
                                            match a with
                                            | UnionFieldAccess.Overlaid f -> f
                                            | UnionFieldAccess.Direct _
                                            | UnionFieldAccess.Erased _ -> ()
                                    ]

                                if not (List.isEmpty overlaid) then
                                    { Case = c.Name; Fields = overlaid }
                        ]

                    Expect.equal p.OverlaidCases overlaidCases unionName

                    Expect.equal
                        (p.Slots |> List.exists (fun s -> s.Key = UnionSlotKey.Data))
                        (not (List.isEmpty overlaidCases))
                        (unionName + " declares `_data`")
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

                let p, cases = placementsOf symbols decls "Boxed"

                Expect.equal (slotLines p) [ "_label: string"; "_count: int" ] "FSC-spelled, declared types"
                Expect.equal (placementLines p cases) [ "B.0 -> _label"; "B.1 -> _count" ] "no erasure, no sharing"
            }
        ]
