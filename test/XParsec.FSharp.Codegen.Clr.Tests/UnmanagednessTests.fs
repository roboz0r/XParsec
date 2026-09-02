module XParsec.FSharp.Codegen.Clr.Tests.UnmanagednessTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `Unmanagedness.ofFrozen` over the real contract stack: primitives by key, this
// compilation's own struct records and unions through the analysed assembly's visibility,
// referenced-package and BCL value types, and a hand-built cyclic shape.

let private prim (key: TypeKey) : FrozenType = FTConst(key, EqArray.empty)

/// The union's case fields, `(case, fieldName, type)`, in declaration order.
let private caseFields (decls: TastAccessor.DeclId list) (unionName: string) : (string * string * FrozenType) list =
    [
        for d in decls do
            match TastAccessor.declKind d with
            | DeclShape.Type ->
                let td = TastAccessor.declType d

                match td.Kind with
                | TTypeKindG.Union u when td.Name = unionName ->
                    for c in u.Cases do
                        for (i, (name, ty)) in EqArray.toList c.Fields |> List.indexed ->
                            c.Name, (name |> ValueOption.defaultValue (sprintf "item%d" (i + 1))), ty
                | _ -> ()
            | _ -> ()
    ]

let private render (u: Unmanagedness) : string =
    match u with
    | Unmanagedness.Unmanaged -> "Unmanaged"
    | Unmanagedness.Managed -> "Managed"
    | Unmanagedness.Undetermined blocker -> sprintf "Undetermined(%s)" (ConformanceTypars.describeType blocker)

/// One census line per case field of `unionName`.
let private census (symbols: ICodegenSymbols) (decls: TastAccessor.DeclId list) (unionName: string) : string list =
    [
        for (case, field, ty) in caseFields decls unionName ->
            sprintf "%s.%s.%s: %s" unionName case field (render (Unmanagedness.ofFrozen symbols ty))
    ]

let private expectCensus
    (symbols: ICodegenSymbols)
    (decls: TastAccessor.DeclId list)
    (unionName: string)
    (expected: string list)
    =
    Expect.equal (census symbols decls unionName) expected unionName

/// A symbol view over one struct record `Cyc` whose sole field is `Cyc` itself.
let private cyclicSymbols: ICodegenSymbols * FrozenType =
    let key = SymbolKeyOps.typeKeyOf "Cyclic" "Cyc"
    let self = FTRecord(key, EqArray.empty)

    let shape =
        ExternalTypeShape.Record
            {
                Arity = 0
                Fields =
                    EqArray.singleton
                        { ExternalFieldShape.create ("Again", false) with
                            Frozen = self
                        }
                Origin = SymbolOrigin.Empty
                IsValueType = true
                RequiresQualifiedAccess = false
            }

    let symbols =
        { new ICodegenSymbols with
            member _.TryLookupType k =
                if k = key then ValueSome shape else ValueNone

            member _.TryLookupMemberByKey _ = ValueNone
            member _.TryLookupCtor(_, _, _) = ValueNone
            member _.TryRebaseCapabilityMember _ = ValueNone
            member _.TryLookupOpenSignature _ = ValueNone
            member _.ModuleClassNameOf _ = ModuleClassName.Undeclared
            member _.TryPlatformTypeId _ = ValueNone

            member _.IsValueType k =
                if k = key then ValueSome true else ValueNone

            member _.Platform = ValueNone
        }

    symbols, self

[<Tests>]
let tests =
    let core =
        lazy (CodegenSymbols.ofProvider (ClrSymbolProviders.buildContract defaultPackages))

    testList
        "Unmanagedness"
        [
            // Classified by the CLR type each `(# … #)` binding denotes, so `unit` is
            // `System.ValueTuple`, a zero-field struct, and `voidptr` is a pointer.
            test "intrinsics bound to primitive value types and pointers are Unmanaged" {
                for key in
                    RuntimeNames.boolKey
                    :: RuntimeNames.charKey
                    :: RuntimeNames.unitKey
                    :: RuntimeNames.voidptrKey
                    :: RuntimeNames.numericKeys do
                    Expect.equal (Unmanagedness.ofFrozen core.Value (prim key)) Unmanagedness.Unmanaged key.Name
            }

            test "intrinsics bound to reference types, arrays, functions and typars are Managed" {
                let managed =
                    [
                        prim RuntimeNames.stringKey
                        prim RuntimeNames.objKey
                        prim RuntimeNames.exnKey
                        FTConst(RuntimeNames.arrayKey 1, EqArray.singleton (prim RuntimeNames.intKey))
                        FTFun(prim RuntimeNames.intKey, prim RuntimeNames.intKey)
                        FTTypar(TyparAxis.Declaring, 0)
                    ]

                for t in managed do
                    Expect.equal
                        (Unmanagedness.ofFrozen core.Value t)
                        Unmanagedness.Managed
                        (ConformanceTypars.describeType t)
            }

            // The provider enumerates no fields for `System.Numerics.BigInteger`.
            test "an intrinsic bound to a BCL struct is Undetermined at itself" {
                let t = prim RuntimeNames.bigintKey
                Expect.equal (Unmanagedness.ofFrozen core.Value t) (Unmanagedness.Undetermined t) "bigint"
            }

            test "a tuple classifies as the combination of its items" {
                let ofItems (items: FrozenType list) =
                    Unmanagedness.ofFrozen core.Value (FTTuple(EqArray.ofList items))

                Expect.equal
                    (ofItems [ prim RuntimeNames.intKey; prim RuntimeNames.floatKey ])
                    Unmanagedness.Unmanaged
                    "int * float"

                Expect.equal
                    (ofItems [ prim RuntimeNames.intKey; prim RuntimeNames.stringKey ])
                    Unmanagedness.Managed
                    "int * string"
            }

            test "an unknown type is Undetermined at itself" {
                let t = FTUnknown UnknownReason.ArityMismatch
                Expect.equal (Unmanagedness.ofFrozen core.Value t) (Unmanagedness.Undetermined t) "FTUnknown"
            }

            test "Managed absorbs Undetermined, which absorbs Unmanaged" {
                let blocked = Unmanagedness.Undetermined(prim RuntimeNames.intKey)

                Expect.equal (Unmanagedness.combine Unmanagedness.Managed blocked) Unmanagedness.Managed "Managed wins"

                Expect.equal
                    (Unmanagedness.combine blocked Unmanagedness.Unmanaged)
                    blocked
                    "Undetermined over Unmanaged"

                Expect.equal (Unmanagedness.ofParts []) Unmanagedness.Unmanaged "no parts"
            }

            test "this compilation's struct records and unions recurse through their fields" {
                let symbols, decls =
                    analysedSymbols
                        defaultPackages
                        "LocalShapes"
                        """
[<Struct>]
type P = { X: int; Y: float }

type R = { A: int }

[<Struct>]
type Q = { P: P; S: string }

[<Struct>]
type G<'T> = { V: 'T; N: int }

[<Struct>]
type Inner =
    | Leaf of n: int64
    | Twig of p: P

type Tree =
    | Node of v: int

[<Struct>]
type U =
    | A of p: P
    | B of q: Q
    | C of gi: G<int>
    | D of gs: G<string>
    | E of t: int * bool
    | F of r: R
    | H of inner: Inner
    | I of tree: Tree
    | J of tup: (int * P)

printfn "%d" (match A { X = 1; Y = 2.0 } with | A p -> p.X | _ -> 0)
"""

                expectCensus
                    symbols
                    decls
                    "U"
                    [
                        "U.A.p: Unmanaged"
                        "U.B.q: Managed"
                        "U.C.gi: Unmanaged"
                        "U.D.gs: Managed"
                        "U.E.t: Unmanaged"
                        "U.E.item2: Unmanaged"
                        "U.F.r: Managed"
                        "U.H.inner: Unmanaged"
                        "U.I.tree: Managed"
                        "U.J.tup: Unmanaged"
                    ]
            }

            test "a generic union's typar-typed fields are Managed at the open definition" {
                let symbols, decls =
                    analysedSymbols
                        defaultPackages
                        "GenericShapes"
                        """
[<Struct>]
type G<'T> = { V: 'T; N: int }

[<Struct>]
type GU<'T> =
    | X of v: 'T
    | Y of g: G<'T>
    | Z of n: int

let z: GU<int> = Z 1
printfn "%d" (match z with | Z n -> n | _ -> 0)
"""

                expectCensus symbols decls "GU" [ "GU.X.v: Managed"; "GU.Y.g: Managed"; "GU.Z.n: Unmanaged" ]
            }

            // `Vesper.Option` declares `option` as `[<Struct>]`.
            test "a referenced package's struct union classifies at its instantiation" {
                let symbols, decls =
                    analysedSymbols
                        (defaultPackages @ [ srcPackage "Vesper.Option" ])
                        "ExternalShapes"
                        """
[<Struct>]
type U =
    | A of oi: int option
    | B of os: string option

printfn "%d" (match A (Some 1) with | A (Some n) -> n | _ -> 0)
"""

                expectCensus symbols decls "U" [ "U.A.oi: Unmanaged"; "U.B.os: Managed" ]
            }

            test "a BCL value type is Undetermined at itself and a BCL class is Managed" {
                let symbols, decls =
                    analysedSymbols
                        defaultPackages
                        "BclShapes"
                        """
[<Struct>]
type U =
    | A of g: System.Guid
    | B of sb: System.Text.StringBuilder

printfn "%d" (match B (System.Text.StringBuilder()) with | B _ -> 1 | _ -> 0)
"""

                expectCensus symbols decls "U" [ "U.A.g: Undetermined(Guid)"; "U.B.sb: Managed" ]
            }

            test "a cyclic struct shape terminates as Undetermined at the repeated type" {
                let symbols, self = cyclicSymbols
                Expect.equal (Unmanagedness.ofFrozen symbols self) (Unmanagedness.Undetermined self) "Cyc"
            }

            // The `Undetermined` worklist: every struct-union case field in the
            // `StructUnion*` data programs, classified. Widening the classifier or changing
            // a program changes this table.
            test "census over the struct-union data corpus" {
                let corpus =
                    [
                        "StructUnionShape", "Shape"
                        "StructUnionGenericShape", "GBox"
                        "StructUnionSameNameFields", "Mixed"
                        "StructUnionExternalPayload", "Payload"
                    ]

                let actual =
                    [
                        for (program, unionName) in corpus do
                            let symbols, decls = analysedSymbols defaultPackages program (dataSource program)
                            yield! census symbols decls unionName
                    ]

                Expect.equal
                    actual
                    [
                        "Shape.Point.x: Unmanaged"
                        "Shape.Pair.a: Unmanaged"
                        "Shape.Pair.b: Unmanaged"
                        "GBox.Val.v: Managed"
                        "GBox.Num.n: Unmanaged"
                        "Mixed.I.x: Unmanaged"
                        "Mixed.S.x: Managed"
                        "Payload.Scalars.x: Unmanaged"
                        "Payload.Scalars.y: Unmanaged"
                        "Payload.Nested.inner: Unmanaged"
                        "Payload.Text.s: Managed"
                        "Payload.Id.id: Undetermined(Guid)"
                        "Payload.Stamp.at: Undetermined(DateTime)"
                    ]
                    "the census"
            }
        ]
