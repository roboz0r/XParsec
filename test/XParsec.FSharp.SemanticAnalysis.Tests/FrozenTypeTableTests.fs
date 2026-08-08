module XParsec.FSharp.SemanticAnalysis.Tests.FrozenTypeTableTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// The hash-consing gate for the per-file type tables. Four obligations, and they are the
// whole of what the rest of the frozen format rests on:
//
//   * `table.[intern t] = t` — a row materialises back to the type it was interned from, so
//     swapping a `FrozenType[]` column for a `TypeId[]` one loses nothing.
//   * intern is INJECTIVE on structural equality — two types intern to one id iff they are
//     equal, which is what makes a within-file id compare a type comparison.
//   * the tables are SHARED — a repeated type mints no second row.
//   * the rows survive `FrozenCodecRows` unchanged, so an id resolves to the same value
//     after a trip through a blob as before it.
//
// The samples reach every `TypeRow` and every key-cluster row, and a coverage assertion
// fails if one stops being produced, so a new constructor cannot ride the existing arms.
// The codec obligation is stated HERE, against those samples, for that reason: the corpus
// gate in `FrozenCodecRoundTripTests` exercises the row codec over realistic breadth but
// proves nothing about which row cases it reached.

let private intKey = RuntimeNames.intKey
let private stringKey = RuntimeNames.stringKey
let private intTy = FTConst(intKey, EqArray.empty)
let private stringTy = FTConst(stringKey, EqArray.empty)

/// The key cluster at full depth: a segmented namespace, nested modules, a type in a
/// module, a type nested in that type, a binding with no declaring module, and a member
/// whose `ArgSig` reaches back into the type domain.
let private globalNs: NamespaceKey = NamespaceKey.Global

let private ns: NamespaceKey =
    {
        Path = EqArray.ofList [ "Test"; "Inner" ]
    }

let private outerModule: ModuleKey =
    {
        Holder = ModuleHolder.InNamespace ns
        Name = "M"
    }

let private innerModule: ModuleKey =
    {
        Holder = ModuleHolder.InModule outerModule
        Name = "N"
    }

let private boxKey: TypeKey =
    {
        Holder = TypeHolder.InModule innerModule
        Name = "Box"
        TyparArity = 1
    }

let private nestedKey: TypeKey =
    {
        Holder = TypeHolder.InType boxKey
        Name = "Enumerator"
        TyparArity = 0
    }

let private ifaceKey: TypeKey =
    {
        Holder = TypeHolder.InNamespace globalNs
        Name = "IThing"
        TyparArity = 0
    }

let private colourKey: TypeKey =
    {
        Holder = TypeHolder.InNamespace ns
        Name = "Colour"
        TyparArity = 0
    }

let private bindingKey: BindingKey =
    {
        Decl = ModuleHolder.InNamespace globalNs
        Name = "printfn"
    }

let private moduleBindingKey: BindingKey =
    {
        Decl = ModuleHolder.InModule outerModule
        Name = "f"
    }

/// A member key per `MemberKind`, so the two interface cases (which carry a second
/// `TypeKey`) are interned as well as the two plain ones.
let private memberKeyOf (kind: MemberKind) (argSig: FrozenType list) : MemberKey =
    {
        Decl = boxKey
        Name = "M"
        ArgSig = EqArray.ofList argSig
        MethodTyparArity = 1
        Kind = kind
    }

/// Every `FrozenType` constructor, at depth, plus the shapes that only appear inside a KEY
/// (a member's `ArgSig`, a nested holder chain) — the type table and the key tables are one
/// interning problem, so the samples have to exercise the edge in both directions.
let private samples: FrozenType list =
    [
        intTy
        stringTy
        FTConst(RuntimeNames.arrayKey 1, EqArray.singleton intTy)
        // The key cluster's three sorts, each in `FTConst` type-constructor position.
        FTConst(SymbolKey.Type nestedKey, EqArray.empty)
        FTConst(SymbolKey.Binding bindingKey, EqArray.empty)
        FTConst(SymbolKey.Binding moduleBindingKey, EqArray.empty)
        FTConst(SymbolKey.Member(memberKeyOf MemberKind.Method [ intTy; stringTy ]), EqArray.empty)
        FTConst(SymbolKey.Member(memberKeyOf MemberKind.Property []), EqArray.empty)
        FTConst(SymbolKey.Member(memberKeyOf (MemberKind.InterfaceMethod ifaceKey) [ intTy ]), EqArray.empty)
        FTConst(SymbolKey.Member(memberKeyOf (MemberKind.ExplicitInterfaceImpl ifaceKey) []), EqArray.empty)
        FTFun(intTy, FTFun(stringTy, intTy))
        FTTuple(EqArray.ofList [ intTy; stringTy; FTTypar(TyparAxis.Declaring, 0) ])
        FTRecord(boxKey, EqArray.singleton (FTTypar(TyparAxis.Declaring, 0)))
        FTUnion(boxKey, EqArray.singleton stringTy)
        FTClass(nestedKey, EqArray.empty)
        FTEnum colourKey
        FrozenType.MkUnion [ intTy; stringTy ]
        FTLiteral(LiteralConst.String "GET")
        FTLiteral(LiteralConst.Int 42L)
        FrozenType.MkUnion [ FTLiteral(LiteralConst.String "ping"); FTLiteral(LiteralConst.String "pong") ]
        FTKeyOf(FTTypar(TyparAxis.Declaring, 0))
        FTIndexedAccess(FTTypar(TyparAxis.Declaring, 0), FTTypar(TyparAxis.Method, 0))
        FTConditional
            {
                Check = intTy
                Extends = FTIndexedAccess(FTTypar(TyparAxis.Declaring, 0), FTTypar(TyparAxis.Method, 0))
                WhenTrue = FTTypar(TyparAxis.Method, 0)
                WhenFalse = FrozenType.MkUnion []
            }
        FTTypar(TyparAxis.Declaring, 3)
        FTTypar(TyparAxis.Method, 2)
        FTLocalTypar(SchemeId 0, 0)
        FTLocalTypar(SchemeId 1, 2)
        FTUnknown "Unresolved.Head"
    ]

/// The key-cluster values interned in their own right, as `FrozenCodecTypes`' nominal
/// reference codecs write them — an `FTConst` type constructor is not the only way one reaches the wire.
let private keySamples: SymbolKey list =
    [
        SymbolKey.Type nestedKey
        SymbolKey.Type ifaceKey
        SymbolKey.Binding bindingKey
        SymbolKey.Binding moduleBindingKey
        SymbolKey.Member(memberKeyOf MemberKind.Method [ intTy; stringTy ])
        SymbolKey.Member(memberKeyOf (MemberKind.InterfaceMethod ifaceKey) [ intTy ])
    ]

/// A builder with every sample interned, and the table its rows make — the fixture the
/// obligations below are all stated against.
let private internedSamples () =
    let builder = FrozenTypeTableBuilder()
    let ids = samples |> List.map builder.Intern
    ids, FrozenTypeTable.OfRows builder.Rows

/// The rows through `FrozenCodecRows` and back. EMPTY tables on both sides of the seam, and
/// that is half the assertion: the row codec resolves no type reference — its children are
/// already ids — so anything here reaching for `w.Types` would fault rather than quietly
/// resolve against a stand-in.
let private throughRowCodec (rows: FrozenTypeRows) : FrozenTypeRows =
    FrozenCodecPrimitives.toBytes (FrozenTypeTableBuilder()) FrozenCodecRows.writeTypeRows rows
    |> FrozenCodecPrimitives.ofBytes FrozenTypeTable.Empty FrozenCodecRows.readTypeRows

/// A `TypeRow`'s constructor name, for the coverage assertion. Exhaustive with no
/// catch-all, so a new row case fails to compile here rather than going uncounted.
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
    | TypeRow.LocalTypar _ -> "LocalTypar"
    | TypeRow.Unknown _ -> "Unknown"

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

            // The whole point of the id column: within one file, `=` on two `TypeId`s IS
            // structural type equality. Both directions, over every pair of samples.
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

                Expect.equal second first "re-interning answers with the same ids"
                Expect.equal builder.Rows.Types.Length rowsAfterFirst "re-interning appends no rows"
            }

            // What `FrozenCodec.flatten` rests on: it must intern the types a PAYLOAD embeds
            // into the tables the freeze already built, and the `ty` columns it is about to
            // write hold ids from that first build. If re-admitting the stored rows moved a
            // single one, every column entry in the file would silently name a different
            // type.
            test "a re-admitted table keeps every id, and grows only at the end" {
                let frozen = FrozenTypeTableBuilder()
                let originals = samples |> List.map frozen.Intern
                let rows = frozen.Rows

                let reopened = FrozenTypeTableBuilder.OfRows rows
                Expect.equal (reopened.Rows.Types.Length) rows.Types.Length "re-admission mints nothing"
                Expect.equal (samples |> List.map reopened.Intern) originals "every id survives re-admission"

                // The payload type the columns never carried: it takes the next row, and the
                // ids already handed out still name what they named.
                let fresh = FTKeyOf(FTFun(intTy, FTUnknown "payload-only"))
                let freshId = reopened.Intern fresh
                Expect.isFalse (List.contains freshId originals) "a new type takes a new row"

                let table = FrozenTypeTable.OfRows reopened.Rows
                Expect.equal table.[freshId] fresh "the appended row materialises"

                for (ty, id) in List.zip samples originals do
                    Expect.equal table.[id] ty (sprintf "still named by its original id: %A" ty)
            }

            // The other side of that invariant. Re-admission reproduces the stored indices
            // only because a stored array is DISTINCT and in mint order, and the arrays
            // reaching it come off the wire — so a reader bug that repeated a row would
            // compact it here, shift every id after it, and leave every `ty` column entry in
            // the file naming a different, valid type. Nothing types that away; it faults.
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

            // A sub-type shared between two samples occupies ONE row, which is what makes
            // the table smaller than the occurrences it stands for.
            test "a shared subtree is interned once" {
                let builder = FrozenTypeTableBuilder()
                builder.Intern(FTFun(intTy, intTy)) |> ignore
                let rowsBefore = builder.Rows.Types.Length
                builder.Intern(FTTuple(EqArray.ofList [ intTy; intTy ])) |> ignore

                // The tuple's own row, and nothing else: `int` was already interned.
                Expect.equal builder.Rows.Types.Length (rowsBefore + 1) "only the new node takes a row"
            }

            // `FTOr` is a SET. Two spellings of one union intern to one row, and the row
            // keeps the order the first spelling declared — `SemanticInfo`'s `FTOr`
            // insertion-order rule, now carried by the row's `EqSet<TypeId>`.
            test "a union interns order-insensitively and keeps the declared order" {
                let builder = FrozenTypeTableBuilder()
                let forward = builder.Intern(FrozenType.MkUnion [ intTy; stringTy ])
                let reverse = builder.Intern(FrozenType.MkUnion [ stringTy; intTy ])
                Expect.equal reverse forward "A|B and B|A are one row"

                let table = FrozenTypeTable.OfRows builder.Rows

                Expect.equal
                    table.[forward]
                    (FrozenType.MkUnion [ intTy; stringTy ])
                    "the stored member order is the first spelling's"
            }

            test "a symbol key materialises back to the key it was interned from" {
                let builder = FrozenTypeTableBuilder()
                let ids = keySamples |> List.map builder.InternSymbol
                let table = FrozenTypeTable.OfRows builder.Rows

                for (k, id) in List.zip keySamples ids do
                    Expect.equal table.[id] k (sprintf "round-trips: %A" k)
            }

            // The row CODEC, gated on the very samples the coverage assertions below prove
            // reach every `TypeRow` case and populate every key table. It needs its own gate
            // because the two halves of the pair are not held to each other by the compiler:
            // `writeTypeRow` matches on the DU exhaustively, so a new row case fails to
            // compile there, but `readTypeRow` matches on BYTES with a catch-all and is under
            // no such obligation — a case the writer is forced to emit is not a case the
            // reader is forced to accept.
            //
            // Materialising through the REREAD table is what makes this reach further than
            // the type rows: every key row (`SymbolRow`, `MemberKindRow`, the two holder
            // rows, `LiteralRow`) has a byte-tag reader with the same catch-all, and the only
            // way to resolve a sample's id is through all of them.
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

            // Two overloads differing ONLY in their argument types must not collapse: the
            // `MemberKey` identity reaches into the type table, so its row does too.
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
                            "LocalTypar"
                            "Unknown"
                        ]

                Expect.equal (Set.difference expected produced) Set.empty "every row case is exercised"
            }

            // Every KEY row array is populated too — the samples reach the key cluster only
            // through `FTConst` type constructors and nominal keys, so a table that stopped interning
            // one of them would otherwise pass every assertion above.
            test "the samples populate every key table" {
                let _, table = internedSamples ()
                let rows = table.Rows
                Expect.isGreaterThan rows.Strings.Length 0 "strings"
                Expect.isGreaterThan rows.Namespaces.Length 0 "namespaces"
                Expect.isGreaterThan rows.Modules.Length 0 "modules"
                Expect.isGreaterThan rows.TypeKeys.Length 0 "type keys"
                Expect.isGreaterThan rows.Bindings.Length 0 "binding keys"
                Expect.isGreaterThan rows.Members.Length 0 "member keys"
                Expect.isGreaterThan rows.Symbols.Length 0 "symbol keys"
            }
        ]
