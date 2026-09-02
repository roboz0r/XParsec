module XParsec.FSharp.Codegen.Clr.Tests.UnionPlacementsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `FlatUnionPlacements.ofCases` over the real contract stack: which slot each logical case
// field lands in, and how many slots the cases of the `StructUnion*` data programs need
// between them.

/// The regime and cases of `unionName`. Raises when the analysed program declares no such
/// union.
let private unionOf (decls: TastAccessor.DeclId list) (unionName: string) : UnionRegime * Frozen.TUnionCase list =
    let found =
        [
            for d in decls do
                match TastAccessor.declKind d with
                | DeclShape.Type ->
                    let td = TastAccessor.declType d

                    match td.Kind with
                    | TTypeKindG.Union u when td.Name = unionName ->
                        UnionRegime.classify
                            u.ValueKind
                            u.Cases.Length
                            (u.Cases |> EqArray.exists (fun c -> not c.Fields.IsEmpty)),
                        EqArray.toList u.Cases
                    | _ -> ()
                | _ -> ()
        ]

    match found with
    | [ one ] -> one
    | _ -> failwithf "no union '%s' among the analysed declarations" unionName

/// `<slot> : <stored type>` per physical slot, in `.ctor` parameter order.
let private slotLines (p: FlatUnionPlacements) : string list =
    [
        for s in p.Slots -> sprintf "%s: %s" s.MetaName (ConformanceTypars.describeType s.Ty)
    ]

/// `<Case>.<i> -> <slot>` per logical field, with the `castclass` target of an erased read.
let private placementLines (p: FlatUnionPlacements) (cases: Frozen.TUnionCase list) : string list =
    [
        for c in cases do
            for (fi, access) in List.indexed (p.CaseAccess c) ->
                match access with
                | UnionFieldAccess.Direct s -> sprintf "%s.%d -> %s" c.Name fi s.MetaName
                | UnionFieldAccess.Erased(s, declared) ->
                    sprintf "%s.%d -> %s as %s" c.Name fi s.MetaName (ConformanceTypars.describeType declared)
    ]

let private placementsOf
    (symbols: ICodegenSymbols)
    (decls: TastAccessor.DeclId list)
    (unionName: string)
    : FlatUnionPlacements * Frozen.TUnionCase list =
    let regime, cases = unionOf decls unionName
    FlatUnionPlacements.ofCases symbols regime cases, cases

/// The corpus the census and the determinism pin both run over.
let private corpus =
    [
        "StructUnionShape", "Shape"
        "StructUnionGenericShape", "GBox"
        "StructUnionSameNameFields", "Mixed"
        "StructUnionExternalPayload", "Payload"
        "StructUnionLocalRefPayload", "Holder"
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
                        // `x`, `a` and `b` are all `int`, so `Point` and `Pair` overlap.
                        "Shape slots: _val0: int, _val1: int"
                        "Point.0 -> _val0"
                        "Pair.0 -> _val0"
                        "Pair.1 -> _val1"
                        // A typar is stored exactly, so the declaring typar and `int` take
                        // a slot each.
                        "GBox slots: _val0: '0, _val1: int"
                        "Val.0 -> _val0"
                        "Num.0 -> _val1"
                        "Mixed slots: _val0: int, _ref0: obj"
                        "I.0 -> _val0"
                        "S.0 -> _ref0 as string"
                        "Payload slots: _val0: int, _val1: bool, _val2: Inner, _ref0: obj, _val3: Guid, _val4: DateTime"
                        "Scalars.0 -> _val0"
                        "Scalars.1 -> _val1"
                        "Nested.0 -> _val2"
                        "Text.0 -> _ref0 as string"
                        "Id.0 -> _val3"
                        "Stamp.0 -> _val4"
                        // A record declared in this compilation is a settled reference,
                        // so it shares the `object` slot with `string`.
                        "Holder slots: _val0: '0, _ref0: obj"
                        "Val.0 -> _val0"
                        "Text.0 -> _ref0 as string"
                        "Rec.0 -> _ref0 as Node"
                    ]
                    "the placement table"
            }

            // Slot assignment feeds the `.ctor` signature, the field rows and every read
            // path, so two computations over one shape must agree exactly.
            test "two computations of one shape agree" {
                for (program, unionName) in corpus do
                    let symbols, decls = analysedSymbols defaultPackages program (dataSource program)
                    let regime, cases = unionOf decls unionName

                    Expect.equal
                        (FlatUnionPlacements.ofCases symbols regime cases)
                        (FlatUnionPlacements.ofCases symbols regime cases)
                        unionName
            }

            // A case writes all of its own fields through one `.ctor` call, so two of its
            // fields sharing a slot would lose one of them.
            test "a case's own fields take distinct slots" {
                for (program, unionName) in corpus do
                    let symbols, decls = analysedSymbols defaultPackages program (dataSource program)
                    let p, cases = placementsOf symbols decls unionName

                    for c in cases do
                        let keys = [ for a in p.CaseAccess c -> (UnionFieldAccess.slot a).Key ]

                        Expect.equal
                            (List.length (List.distinct keys))
                            (List.length keys)
                            (sprintf "%s.%s" unionName c.Name)
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
