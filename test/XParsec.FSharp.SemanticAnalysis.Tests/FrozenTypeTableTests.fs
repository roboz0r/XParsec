module XParsec.FSharp.SemanticAnalysis.Tests.FrozenTypeTableTests

open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis

// The hash-consing gate for the per-file type tables — what lets a `FrozenType[]` column become
// a `TypeId[]` one: `table.[intern t] = t`, and within one file `=` on two ids IS structural
// type equality. A coverage assertion below fails if a sample stops reaching a row case.

let private intKey = RuntimeNames.intKey
let private stringKey = RuntimeNames.stringKey
let private intTy = FTConst(intKey, Block.empty)
let private stringTy = FTConst(stringKey, Block.empty)

let private globalNs: NamespaceKey = NamespaceKey.Global

let private ns: NamespaceKey =
    {
        Path = Block.ofList [ "Test"; "Inner" ]
    }

let private outerModule: ModuleKey =
    {
        Container = ModuleContainer.InNamespace ns
        Name = "M"
    }

let private innerModule: ModuleKey =
    {
        Container = ModuleContainer.InModule outerModule
        Name = "N"
    }

let private boxKey: TypeKey =
    {
        Container = TypeContainer.InModule innerModule
        Name = "Box"
        TyparArity = KeyArity.Compiled 1<typeSlot>
    }

let private nestedKey: TypeKey =
    {
        Container = TypeContainer.InType boxKey
        Name = "Enumerator"
        TyparArity = KeyArity.Compiled 0<typeSlot>
    }

let private ifaceKey: TypeKey =
    {
        Container = TypeContainer.InNamespace globalNs
        Name = "IThing"
        TyparArity = KeyArity.Compiled 0<typeSlot>
    }

let private colourKey: TypeKey =
    {
        Container = TypeContainer.InNamespace ns
        Name = "Colour"
        TyparArity = KeyArity.Compiled 0<typeSlot>
    }

let private bindingKey: BindingKey =
    {
        Decl = ModuleContainer.InNamespace globalNs
        Name = "printfn"
    }

let private moduleBindingKey: BindingKey =
    {
        Decl = ModuleContainer.InModule outerModule
        Name = "f"
    }

/// A member key per `MemberKind` — the two interface kinds carry a second `TypeKey`.
let private memberKeyOf (kind: MemberKind) (argSig: FrozenType list) : MemberKey =
    {
        Decl = boxKey
        Name = "M"
        ArgSig = Block.ofList argSig
        MethodTyparArity = 1<typeSlot>
        Kind = kind
    }

/// Every `FrozenType` constructor at depth, plus the shapes that only appear inside a KEY (a
/// member's `ArgSig`, a nested containment chain).
let private samples: FrozenType list =
    [
        intTy
        stringTy
        FTConst(RuntimeNames.arrayKey 1, Block.singleton intTy)
        // An `FTConst` type constructor is a nominal TYPE; `keySamples` carries the binding
        // and member sorts, which reach the wire through a symbol reference instead.
        FTConst(nestedKey, Block.empty)
        FTFun(intTy, FTFun(stringTy, intTy))
        FTTuple(Block.ofList [ intTy; stringTy; FTTypar(TyparScope.Type boxKey, 0<typeSlot>) ])
        FTRecord(boxKey, Block.singleton (FTTypar(TyparScope.Type boxKey, 0<typeSlot>)))
        FTUnion(boxKey, Block.singleton stringTy)
        FTClass(nestedKey, Block.empty)
        FTEnum colourKey
        FrozenType.MkUnion [ intTy; stringTy ]
        FTLiteral(LiteralConst.String "GET")
        FTLiteral(LiteralConst.Int 42L)
        FrozenType.MkUnion [ FTLiteral(LiteralConst.String "ping"); FTLiteral(LiteralConst.String "pong") ]
        FTKeyOf(FTTypar(TyparScope.Type boxKey, 0<typeSlot>))
        FTIndexedAccess(FTTypar(TyparScope.Type boxKey, 0<typeSlot>), FTTypar(TyparScope.Member boxKey, 0<typeSlot>))
        FTConditional
            {
                Check = intTy
                Extends =
                    FTIndexedAccess(
                        FTTypar(TyparScope.Type boxKey, 0<typeSlot>),
                        FTTypar(TyparScope.Member boxKey, 0<typeSlot>)
                    )
                WhenTrue = FTTypar(TyparScope.Member boxKey, 0<typeSlot>)
                WhenFalse = FrozenType.MkUnion []
            }
        FTTypar(TyparScope.Type boxKey, 3<typeSlot>)
        FTTypar(TyparScope.Member boxKey, 2<typeSlot>)
        FTTypar(
            TyparScope.ModuleFunction(SymbolKeyOps.bindingKeyOf (SymbolKeyOps.inNamespace "Test") "map"),
            1<typeSlot>
        )
        FTTypar(TyparScope.LocalFunction(LocalBindingId 0), 0<typeSlot>)
        FTTypar(TyparScope.LocalFunction(LocalBindingId 1), 2<typeSlot>)
        FTUnknown(UnknownReason.UndefinedName "Unresolved.Head")
        // A measured nominal: the measure is a leaf in argument position, its atoms keyed.
        FTConst(
            FrozenTypeBridge.measuredClaimKey RuntimeNames.floatKey,
            Block.singleton (
                FTMeasure(
                    MeasureTerm.OfList
                        [
                            MeasureAtom.Named nestedKey, Rational.ofInt 1
                            MeasureAtom.Named colourKey, Rational.ofInt -2
                        ]
                )
            )
        )
    ]

/// The keys interned in their own right — an `FTConst` type constructor is not the only way
/// one reaches the wire.
let private keySamples: SymbolKey list =
    [
        SymbolKey.Type nestedKey
        SymbolKey.Type ifaceKey
        SymbolKey.Binding bindingKey
        SymbolKey.Binding moduleBindingKey
        SymbolKey.Member(memberKeyOf MemberKind.Method [ intTy; stringTy ])
        SymbolKey.Member(memberKeyOf MemberKind.Property [])
        SymbolKey.Member(memberKeyOf (MemberKind.ExplicitInterfaceImpl ifaceKey) [])
        SymbolKey.Member(memberKeyOf (MemberKind.InterfaceMethod ifaceKey) [ intTy ])
    ]

/// A builder with every sample interned, and the table its rows make.
let private internedSamples () =
    let builder = FrozenTypeTableBuilder()
    let ids = samples |> List.map builder.Intern
    ids, FrozenTypeTable.OfRows builder.Rows

/// The rows through the row codec and back, on EMPTY tables both sides — half the assertion:
/// the codec resolves no type reference (a row's children are already ids), so a lookup would
/// fault rather than resolve against a stand-in.
let private throughRowCodec (rows: FrozenTypeRows) : FrozenTypeRows =
    FrozenCodecPrimitives.toBytes (FrozenTypeTableBuilder()) FrozenCodecRows.writeTypeRows rows
    |> FrozenCodecPrimitives.ofBytes FrozenTypeTable.Empty FrozenCodecRows.readTypeRows

/// A `TypeRow`'s constructor name, for the coverage assertion.
let private rowTag (row: TypeRow) : string =
    match row with
    | TypeRow.Const _ -> "Const"
    | TypeRow.Fun _ -> "Fun"
    | TypeRow.Tuple _ -> "Tuple"
    | TypeRow.Record _ -> "Record"
    | TypeRow.Union _ -> "Union"
    | TypeRow.Class _ -> "Class"
    | TypeRow.Enum _ -> "Enum"
    | TypeRow.Or _ -> "Or"
    | TypeRow.Literal _ -> "Literal"
    | TypeRow.KeyOf _ -> "KeyOf"
    | TypeRow.IndexedAccess _ -> "IndexedAccess"
    | TypeRow.Conditional _ -> "Conditional"
    | TypeRow.Typar _ -> "Typar"
    | TypeRow.Unknown _ -> "Unknown"
    | TypeRow.Measure _ -> "Measure"

[<Tests>]
let tests =
    testList
        "FrozenTypeTable hash-conses a file's types and keys"
        [
            test "a row materialises back to the type it was interned from" {
                let ids, table = internedSamples ()

                for (ty, id) in List.zip samples ids do
                    Expect.equal table.[id] ty (sprintf "round-trips: %A" ty)
            }

            // Both directions, over every pair of samples.
            test "interning is injective on structural equality" {
                let ids, _ = internedSamples ()
                let pairs = List.zip samples ids

                for (a, ida) in pairs do
                    for (b, idb) in pairs do
                        Expect.equal
                            (ida = idb)
                            (a = b)
                            (sprintf "id equality tracks structural equality: %A vs %A" a b)
            }

            test "a repeated type mints no second row" {
                let builder = FrozenTypeTableBuilder()
                let first = samples |> List.map builder.Intern
                let rowsAfterFirst = builder.Rows.Types.Length
                let second = samples |> List.map builder.Intern

                Expect.equal second first "re-interning yields the same ids"
                Expect.equal builder.Rows.Types.Length rowsAfterFirst "re-interning appends no rows"
            }

            // `flatten` re-admits the freeze's own rows to intern the types a PAYLOAD embeds,
            // while the `ty` columns it writes hold ids from that first build. Move one id on
            // re-admission and every column entry identifies a different type.
            test "a re-admitted table keeps every id, and grows only at the end" {
                let frozen = FrozenTypeTableBuilder()
                let originals = samples |> List.map frozen.Intern
                let rows = frozen.Rows

                let reopened = FrozenTypeTableBuilder.OfRows rows
                Expect.equal (reopened.Rows.Types.Length) rows.Types.Length "re-admission mints nothing"
                Expect.equal (samples |> List.map reopened.Intern) originals "every id survives re-admission"

                // A payload type the columns never carried: it takes the NEXT row.
                let fresh =
                    FTKeyOf(FTFun(intTy, FTUnknown(UnknownReason.UndefinedName "payload-only")))

                let freshId = reopened.Intern fresh
                Expect.isFalse (List.contains freshId originals) "a new type takes a new row"

                let table = FrozenTypeTable.OfRows reopened.Rows
                Expect.equal table.[freshId] fresh "the appended row materialises"

                for (ty, id) in List.zip samples originals do
                    Expect.equal table.[id] ty (sprintf "still named by its original id: %A" ty)
            }

            // Re-admission reproduces the stored indices only because a stored array is DISTINCT
            // and in mint order, and the arrays reaching it come off the wire. A reader bug that
            // repeated a row would compact it, shifting later ids onto a different, valid type.
            test "a stored table whose rows repeat is rejected, not silently compacted" {
                let builder = FrozenTypeTableBuilder()
                samples |> List.iter (builder.Intern >> ignore)
                let rows = builder.Rows

                let repeated =
                    { rows with
                        Types = rows.Types.Insert(0, rows.Types.[0])
                    }

                Expect.throws
                    (fun () -> FrozenTypeTableBuilder.OfRows repeated |> ignore)
                    "a repeated row cannot be re-admitted"
            }

            test "a shared subtree is interned once" {
                let builder = FrozenTypeTableBuilder()
                builder.Intern(FTFun(intTy, intTy)) |> ignore
                let rowsBefore = builder.Rows.Types.Length
                builder.Intern(FTTuple(Block.ofList [ intTy; intTy ])) |> ignore

                // The tuple's own row, and nothing else: `int` was already interned.
                Expect.equal builder.Rows.Types.Length (rowsBefore + 1) "only the new node takes a row"
            }

            // `FTOr` is a SET: two spellings intern to one row, and the row keeps the order the
            // first declared them, since its `EqSet<TypeId>` preserves insertion order.
            test "a union interns order-insensitively and keeps the declared order" {
                let builder = FrozenTypeTableBuilder()
                let forward = builder.Intern(FrozenType.MkUnion [ intTy; stringTy ])
                let reverse = builder.Intern(FrozenType.MkUnion [ stringTy; intTy ])
                Expect.equal reverse forward "A|B and B|A are one row"

                let table = FrozenTypeTable.OfRows builder.Rows

                Expect.equal
                    table.[forward]
                    (FrozenType.MkUnion [ intTy; stringTy ])
                    "the stored disjunct order is the first spelling's"
            }

            test "a symbol key materialises back to the key it was interned from" {
                let builder = FrozenTypeTableBuilder()
                let ids = keySamples |> List.map builder.InternSymbol
                let table = FrozenTypeTable.OfRows builder.Rows

                for (k, id) in List.zip keySamples ids do
                    Expect.equal table.[id] k (sprintf "round-trips: %A" k)
            }

            // The writer matches the row DU, but the reader matches BYTES under a catch-all, so
            // a case the writer must emit is not one the reader must accept. Materialising
            // through the REREAD table is the only way to reach every key row's byte-tag reader.
            test "every row case survives the row codec at the id it was minted with" {
                let builder = FrozenTypeTableBuilder()
                let typeIds = samples |> List.map builder.Intern
                let symbolIds = keySamples |> List.map builder.InternSymbol
                let written = builder.Rows

                let reread = FrozenTypeTable.OfRows(throughRowCodec written)

                // A reader that dropped or duplicated a row would shift every id after it,
                // and the ids below were minted against the WRITTEN table.
                Expect.equal reread.Rows.Types.Length written.Types.Length "type rows"
                Expect.equal reread.Rows.Symbols.Length written.Symbols.Length "symbol rows"
                Expect.equal reread.Rows.Members.Length written.Members.Length "member rows"
                Expect.equal reread.Rows.TypeKeys.Length written.TypeKeys.Length "type-key rows"
                Expect.equal reread.Rows.Bindings.Length written.Bindings.Length "binding rows"
                Expect.equal reread.Rows.Modules.Length written.Modules.Length "module rows"
                Expect.equal reread.Rows.Namespaces.Length written.Namespaces.Length "namespace rows"
                Expect.equal reread.Rows.Strings.Length written.Strings.Length "strings"

                for (ty, id) in List.zip samples typeIds do
                    Expect.equal reread.[id] ty (sprintf "type survives the wire: %A" ty)

                for (k, id) in List.zip keySamples symbolIds do
                    Expect.equal reread.[id] k (sprintf "key survives the wire: %A" k)
            }

            // A `MemberKey`'s identity reaches into the type table, so its row does too.
            test "overloads differing only by argument type take distinct rows" {
                let builder = FrozenTypeTableBuilder()

                let a =
                    builder.InternSymbol(SymbolKey.Member(memberKeyOf MemberKind.Method [ intTy ]))

                let b =
                    builder.InternSymbol(SymbolKey.Member(memberKeyOf MemberKind.Method [ stringTy ]))

                Expect.notEqual a b "M(int) and M(string) are distinct symbols"
            }

            test "the samples reach every TypeRow case" {
                let _, table = internedSamples ()
                let produced = table.Rows.Types |> Seq.map rowTag |> Set.ofSeq

                let expected =
                    Set.ofList
                        [
                            "Const"
                            "Fun"
                            "Tuple"
                            "Record"
                            "Union"
                            "Class"
                            "Enum"
                            "Or"
                            "Literal"
                            "KeyOf"
                            "IndexedAccess"
                            "Conditional"
                            "Typar"
                            "Unknown"
                            "Measure"
                        ]

                Expect.equal (Set.difference expected produced) Set.empty "every row case is exercised"
            }

            // The samples reach the key cluster only through `FTConst` type constructors and
            // nominal keys, so a table that stopped interning one would pass everything above.
            // Both corpora, because a binding / member key reaches the tables only through
            // `InternSymbol`: an `FTConst` type constructor is a nominal TYPE.
            test "the samples populate every key table" {
                let builder = FrozenTypeTableBuilder()
                samples |> List.iter (builder.Intern >> ignore)
                keySamples |> List.iter (builder.InternSymbol >> ignore)
                let rows = builder.Rows
                Expect.isGreaterThan rows.Strings.Length 0 "strings"
                Expect.isGreaterThan rows.Namespaces.Length 0 "namespaces"
                Expect.isGreaterThan rows.Modules.Length 0 "modules"
                Expect.isGreaterThan rows.TypeKeys.Length 0 "type keys"
                Expect.isGreaterThan rows.Bindings.Length 0 "binding keys"
                Expect.isGreaterThan rows.Members.Length 0 "member keys"
                Expect.isGreaterThan rows.Symbols.Length 0 "symbol keys"
            }
        ]
