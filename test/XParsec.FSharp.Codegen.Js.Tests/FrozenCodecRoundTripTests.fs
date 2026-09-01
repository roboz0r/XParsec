module XParsec.FSharp.Codegen.Js.Tests.FrozenCodecRoundTripTests

open System.Collections.Generic
open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The value-codec gate: `read (write x) = x` for a diagnostic, its `Kind`/`Site` and a node's
// anchor; and for a type, `materialise (read (write (intern x))) = x`, since a type reaches
// the blob only as a row id, so the whole path through the file's tables has to survive.

// Data: the frozen conformance corpus for breadth, plus hand-built edge cases pinning EVERY
// case shape it may not exercise (an `FTOr` of several disjuncts, each `MemberKind`, …).

/// Filtered, so `frozenOfJs` never trips on a `Diagnose` program's error diagnostics.
let private gated = compiledBy "js"

/// The deduped values collected from the corpus plus the hand-built edge cases.
type private Collected =
    {
        FrozenTypes: FrozenType list
        SymbolKeys: SymbolKey list
        TypeKeys: TypeKey list
        Sites: Site list
        Anchors: Anchor list
    }

let private collect () : Collected =
    let fts = HashSet<FrozenType>(HashIdentity.Structural)
    let sks = HashSet<SymbolKey>(HashIdentity.Structural)
    let tks = HashSet<TypeKey>(HashIdentity.Structural)
    let sites = HashSet<Site>(HashIdentity.Structural)
    let toks = HashSet<Anchor>(HashIdentity.Structural)

    // FrozenType child-walk: collect the node and every key / nested type it reaches, so
    // the recursive codec is exercised over whole subtrees.
    let rec visitFt (ft: FrozenType) =
        if fts.Add ft then
            match ft with
            | FTConst(key, args) ->
                visitTypeKey key
                EqArray.iter visitFt args
            | FTFun(a, b) ->
                visitFt a
                visitFt b
            | FTTuple items -> EqArray.iter visitFt items
            | FTRecord(key, args)
            | FTUnion(key, args)
            | FTClass(key, args) ->
                tks.Add key |> ignore
                EqArray.iter visitFt args
            | FTEnum key -> tks.Add key |> ignore
            | FTOr disjuncts -> EqSet.iter visitFt disjuncts.Disjuncts
            | FTLiteral _ -> ()
            | FTKeyOf ty -> visitFt ty
            | FTIndexedAccess(o, i) ->
                visitFt o
                visitFt i
            | FTConditional p ->
                visitFt p.Check
                visitFt p.Extends
                visitFt p.WhenTrue
                visitFt p.WhenFalse
            | FTTypar _ -> ()
            // Its scheme id addresses nothing outside the body that carried it, so there is
            // nothing here to collect into any of the key corpora.
            | FTLocalTypar _ -> ()
            | FTUnknown _ -> ()

    and visitTypeKey (tk: TypeKey) =
        sks.Add(SymbolKey.Type tk) |> ignore
        tks.Add tk |> ignore

    and visitSym (sk: SymbolKey) =
        sks.Add sk |> ignore

        match sk with
        | SymbolKey.Type tk -> tks.Add tk |> ignore
        | SymbolKey.Binding _ -> ()
        | SymbolKey.Member mk ->
            tks.Add mk.Decl |> ignore
            EqArray.iter visitFt mk.ArgSig

            match mk.Kind with
            | MemberKind.InterfaceMethod iface
            | MemberKind.ExplicitInterfaceImpl iface -> tks.Add iface |> ignore
            | MemberKind.Method
            | MemberKind.Property -> ()

    let collectFile (file: Pooled.TastFile) =
        // A diagnostic's position, which can point to a node the emittable tree does not
        // contain and so takes no pool id.
        for d in file.Diagnostics do
            sites.Add d.Site |> ignore

        for k in file.IntrinsicBindings.Keys do
            visitTypeKey k

        for k in file.Accessibility.Keys do
            visitSym k

        for KeyValue(_, constraints) in file.GenericFnSchemes do
            for c in constraints do
                match c with
                | FrozenConstraint.Coercion(_, target) -> visitFt target

        // No `BindingValReprs` pass: a binding's source arity is a PROJECTION of its lambda
        // chain derived off the columns, so every type and slot it references is already reached
        // by the walks above.

        // Real frozen anchors, shallowly: the source anchor of each top-level decl body.
        for decl in file.Decls do
            match decl with
            | TDeclG.Let(_, value, _, _) -> toks.Add(TastWalk.exprTok value) |> ignore
            | TDeclG.Expression(expr, _) -> toks.Add(TastWalk.exprTok expr) |> ignore
            | TDeclG.Type _ -> ()

    for p in gated do
        collectFile (TastUnpool.ofPools (frozenOfJs p.Source))

    // ── hand-built edge cases: pin every case shape regardless of the corpus ──
    let nsGlobal = NamespaceKey.Global

    let nsSystem =
        {
            Path = EqArray.ofList [ "System"; "Collections"; "Generic" ]
        }

    let tkInt =
        {
            Container = TypeContainer.InNamespace nsGlobal
            Name = "int"
            TyparArity = 0
        }

    let tkString =
        {
            Container = TypeContainer.InNamespace nsGlobal
            Name = "string"
            TyparArity = 0
        }

    let tkArray =
        {
            Container = TypeContainer.InNamespace nsGlobal
            Name = "[]"
            TyparArity = 1
        }

    let tkList =
        {
            Container = TypeContainer.InNamespace nsSystem
            Name = "List`1"
            TyparArity = 1
        }

    let modKey =
        {
            Container = ModuleContainer.InNamespace nsSystem
            Name = "ListModule"
        }

    let tkInMod =
        {
            Container = TypeContainer.InModule modKey
            Name = "Inner"
            TyparArity = 0
        }

    let tkNested =
        {
            Container = TypeContainer.InType tkList
            Name = "Enumerator"
            TyparArity = 0
        }

    let ftInt = FTConst(tkInt, EqArray.empty)
    let ftString = FTConst(tkString, EqArray.empty)
    let ftArray = FTConst(tkArray, EqArray.singleton ftInt)
    let ftLitStr = FTLiteral(LiteralConst.String "GET")
    let ftLitInt = FTLiteral(LiteralConst.Int 42L)
    let ftRecord = FTRecord(tkList, EqArray.singleton ftInt)

    let ftCond =
        FTConditional
            {
                Check = ftInt
                Extends = ftString
                WhenTrue = ftLitStr
                WhenFalse = ftLitInt
            }

    let memberKey =
        {
            Decl = tkList
            Name = "Add"
            ArgSig = EqArray.ofList [ ftInt; ftString ]
            MethodTyparArity = 1
            Kind = MemberKind.Method
        }

    let edgeSymbols =
        [
            SymbolKey.Type tkNested
            SymbolKey.Binding
                {
                    Decl = ModuleContainer.InNamespace nsSystem
                    Name = "printfn"
                }
            SymbolKey.Binding
                {
                    Decl = ModuleContainer.InModule modKey
                    Name = "map"
                }
            SymbolKey.Member memberKey
            SymbolKey.Member
                { memberKey with
                    Kind = MemberKind.Property
                    Name = "Count"
                    ArgSig = EqArray.empty
                }
            SymbolKey.Member
                { memberKey with
                    Kind = MemberKind.InterfaceMethod tkList
                    Name = "GetEnumerator"
                    ArgSig = EqArray.empty
                }
            SymbolKey.Member
                { memberKey with
                    Kind = MemberKind.ExplicitInterfaceImpl tkNested
                    Name = "System.IDisposable.Dispose"
                    ArgSig = EqArray.empty
                }
        ]

    let edgeTypes = [ tkInMod; tkNested ]

    let edgeFrozen =
        [
            ftInt
            ftString
            ftArray
            FTFun(ftInt, FTFun(ftString, ftInt))
            FTTuple(EqArray.ofList [ ftInt; ftString; ftArray ])
            ftRecord
            FTUnion(tkList, EqArray.singleton ftString)
            FTClass(tkList, EqArray.empty)
            FTEnum tkInt
            // FTOr of several disjuncts — must survive as the exact stored set, uncollapsed.
            FTOr(FTDisjuncts.OfSeq [ ftInt; ftString; ftLitStr; ftLitInt ])
            ftLitStr
            ftLitInt
            FTKeyOf ftRecord
            FTIndexedAccess(ftRecord, ftLitStr)
            ftCond
            FTTypar(TyparAxis.Declaring, 0)
            FTTypar(TyparAxis.Method, 3)
            FTLocalTypar(SchemeId 7, 2)
            // One per `UnknownReason` case: the row shape differs per case, so a missing
            // sample is a codec arm nothing round-trips.
            FTUnknown(UnknownReason.UndefinedName "Missing.Thing")
            FTUnknown(UnknownReason.UnfreezableExternal "'Widget' is a delegate type")
            FTUnknown UnknownReason.UnresolvedTypar
            FTUnknown UnknownReason.Deferred
            FTUnknown UnknownReason.ArityMismatch
            FTUnknown UnknownReason.NoValueType
            // Deeply nested: functions, tuples, sets and computations composed together.
            FTFun(
                FTTuple(EqArray.ofList [ ftCond; ftArray ]),
                FTOr(FTDisjuncts.OfSeq [ FTKeyOf ftRecord; ftCond; ftInt ])
            )
        ]

    // Both ends of the anchor's value range plus its absence, which the column stores as
    // the negative space of the index rather than as a case of its own.
    let edgeAnchors =
        [
            Anchor.ofStored 0
            Anchor.ofStored 1
            Anchor.ofStored 1_000_000
            Anchor.nowhere
        ]

    for ft in edgeFrozen do
        visitFt ft

    for sk in edgeSymbols do
        visitSym sk

    for tk in edgeTypes do
        tks.Add tk |> ignore

    // Every case of the position DU, plus both ends of a token index's range.
    for s in
        [
            Site.Nowhere
            Site.At 0<token>
            Site.At 1_000_000<token>
            Site.between 3<token> 9<token>
            Site.After 7<token>
        ] do
        sites.Add s |> ignore

    for a in edgeAnchors do
        toks.Add a |> ignore

    {
        FrozenTypes = List.ofSeq fts
        SymbolKeys = List.ofSeq sks
        TypeKeys = List.ofSeq tks
        Sites = List.ofSeq sites
        Anchors = List.ofSeq toks
    }

/// Round-trip a codec that resolves no type reference: a `Site`, an anchor, a diagnostic, or
/// the type ROWS themselves. The tables are empty on both sides, and that is the assertion;
/// none of these may resolve a type id against a stand-in table.
let private roundTrips (write: FrozenWriter -> 'a -> unit) (read: FrozenReader -> 'a) (x: 'a) =
    FrozenCodecPrimitives.ofBytes
        FrozenTypeTable.Empty
        read
        (FrozenCodecPrimitives.toBytes (FrozenTypeTableBuilder()) write x)

/// The collected values interned into ONE file's tables, and the table those rows make after
/// a trip through the row codec, with the ids so each source value can be asked for back.
type private Interned =
    {
        TypeIds: TypeId list
        SymbolIds: SymbolId list
        TypeKeyIds: TypeKeyId list
        Table: FrozenTypeTable
    }

/// The whole path a type takes to a blob and back: intern it into the file's tables, write
/// the ROWS, read them, materialise the id. One builder for every collected value, as the
/// freeze does, so this exercises rows that reference rows rather than isolated values.
let private intern (h: Collected) : Interned =
    let builder = FrozenTypeTableBuilder()
    let typeIds = h.FrozenTypes |> List.map builder.Intern
    let symbolIds = h.SymbolKeys |> List.map builder.InternSymbol
    let typeKeyIds = h.TypeKeys |> List.map builder.InternTypeKey

    let rows =
        roundTrips FrozenCodecRows.writeTypeRows FrozenCodecRows.readTypeRows builder.Rows

    {
        TypeIds = typeIds
        SymbolIds = symbolIds
        TypeKeyIds = typeKeyIds
        Table = FrozenTypeTable.OfRows rows
    }

[<Tests>]
let tests =
    let h = collect ()
    let interned = intern h

    testList
        "FrozenCodec value round-trip"
        [
            test "FrozenType survives interning, the row codec and materialisation" {
                for (ft, id) in List.zip h.FrozenTypes interned.TypeIds do
                    Expect.equal interned.Table.[id] ft "FrozenType"
            }

            test "SymbolKey survives interning, the row codec and materialisation" {
                for (sk, id) in List.zip h.SymbolKeys interned.SymbolIds do
                    Expect.equal interned.Table.[id] sk "SymbolKey"
            }

            test "TypeKey survives interning, the row codec and materialisation" {
                for (tk, id) in List.zip h.TypeKeys interned.TypeKeyIds do
                    Expect.equal interned.Table.[id] tk "TypeKey"
            }

            test "a diagnostic's Site round-trips, every case" {
                for s in h.Sites do
                    Expect.equal (roundTrips FrozenCodecPrimitives.writeSite FrozenCodecPrimitives.readSite s) s "Site"
            }

            // The bare `Between` constructor can spell a run the type says does not exist:
            // one token wide, or ends reversed. Writer and reader must agree on which form
            // those take, or a blob decodes to a value that never went in.
            test "a Between the smart constructor would not build canonicalises the same way on both sides" {
                for raw in [ Site.Between(5<token>, 5<token>); Site.Between(9<token>, 3<token>) ] do
                    Expect.equal
                        (roundTrips FrozenCodecPrimitives.writeSite FrozenCodecPrimitives.readSite raw)
                        (Site.normalise raw)
                        "Site (non-canonical Between)"
            }

            // `Related` is populated only at the PARSE seam, whose diagnostics do not reach a
            // frozen file's `Diagnostics`, so no corpus program reaches the label leg of the
            // codec: a labelled diagnostic would decode stripped and every test still pass.
            test "a diagnostic round-trips, its labels included" {
                let opener: Label =
                    {
                        Site = Site.At 3<token>
                        Message = DiagnosticCode.openedHereLabel
                    }

                let placeless: Label =
                    {
                        Site = Site.Nowhere
                        Message = "a label with no place"
                    }

                let labelled =
                    Diagnostic.create
                        (Kind.NoMember("Widget", MemberNoun.Field, "nope"))
                        (Site.After 7<token>)
                        [ opener; placeless ]

                for d in [ labelled; { labelled with Related = [] } ] do
                    Expect.equal
                        (roundTrips FrozenCodecDiagnostics.writeDiagnostic FrozenCodecDiagnostics.readDiagnostic d)
                        d
                        "Diagnostic"
            }

            // ONE value per `Kind` case: the reader is a byte match, so only a value that makes
            // the whole round trip proves it agrees with the writer. Severity and code are
            // carried on the kind, so the whole value is compared, not just the tag.
            test "every Kind case round-trips" {
                let verdicts =
                    [
                        ConformanceVerdict.Unimplemented("a.fsi", "M is missing")
                        ConformanceVerdict.ModulePairingMismatch("a.fsi", "a.fs", "M", "N")
                        ConformanceVerdict.SignatureNotPublished "a delegate"
                        ConformanceVerdict.SignatureRejected "a non-inline member on an extern type"
                        ConformanceVerdict.AttributeArgumentsDiffer(
                            "a.fsi",
                            {
                                Declaration = "V.foo"
                                Attribute = "V.ExperimentalAttribute"
                            }
                        )
                    ]

                // One value per `ConformanceError` case, because the inner reader is its own
                // byte match.
                let findings =
                    [
                        Conformance.ConformanceError.MissingInImpl "V.T"
                        Conformance.ConformanceError.ExternWithoutIntrinsic "V.T"
                        Conformance.ConformanceError.IntrinsicWithoutExtern "V.T"
                        Conformance.ConformanceError.HeritabilityMismatch "V.T"
                        Conformance.ConformanceError.TypeKindMismatch(
                            "V.T",
                            Conformance.TypeKindFamily.Class,
                            Conformance.TypeKindFamily.Record
                        )
                        Conformance.ConformanceError.TypeKindMismatch(
                            "V.T",
                            Conformance.TypeKindFamily.Interface,
                            Conformance.TypeKindFamily.Union
                        )
                        Conformance.ConformanceError.TypeKindMismatch(
                            "V.T",
                            Conformance.TypeKindFamily.Enum,
                            Conformance.TypeKindFamily.Class
                        )
                        Conformance.ConformanceError.ValueMissingInImpl "v"
                        Conformance.ConformanceError.ImportBodyNotNativeOnly "served"
                        Conformance.ConformanceError.NativeOnlyWithoutImport "served"
                        Conformance.ConformanceError.ImportSelectorMismatch("served", "other")
                        Conformance.ConformanceError.ImportMalformed "served"
                        Conformance.ConformanceError.ImportPathMalformed("served", "Asset.mjs")
                        Conformance.ConformanceError.ImportAssetNotListed("served", "./Other.mjs")
                        Conformance.ConformanceError.ImportUnsupportedTarget "served"
                        Conformance.ConformanceError.ImportMissingExport("served", "served", "Asset.mjs")
                    ]

                let kinds =
                    [
                        Kind.UndefinedType "Nope"
                        Kind.Internal(InternalBreak.UnresolvedTyVars 3)
                        Kind.Internal(InternalBreak.MemberNotResolvable("mkMethodCall", "Widget", "M"))
                        Kind.Internal(InternalBreak.UnflattenedModule "Validation")
                        Kind.Internal(InternalBreak.UnstampedStaticDeclArgs "Widget")
                        Kind.UnsupportedOnTarget("nativeint", "js")
                        Kind.UnsupportedOnTarget("voidptr", "js")
                        Kind.NoMember("Widget", MemberNoun.Field, "nope")
                        Kind.NoMember("Widget", MemberNoun.InstanceMember, "nope")
                        Kind.NoMember("Widget", MemberNoun.StaticMember, "nope")
                        Kind.NoMember("Widget", MemberNoun.BuiltInStaticMember, "nope")
                        Kind.NoMember("Widget", MemberNoun.AccessibleMember, "nope")
                        Kind.NoMember("Widget", MemberNoun.ValueOrMember, "nope")
                        Kind.NoMember("Widget", MemberNoun.FieldOrMember, "nope")
                        Kind.NoMember("Widget", MemberNoun.Member, "nope")
                        Kind.NoCase(CaseOwner.Enum, "Colour", "Mauve")
                        Kind.NoCase(CaseOwner.Union, "Shape", "Blob")
                        Kind.UnknownNominalType(NominalKind.Record, "R")
                        Kind.UnknownNominalType(NominalKind.Class, "C")
                        Kind.UnknownNominalType(NominalKind.Union, "U")
                        Kind.TypeArgArity("Map", 2, 1)
                        Kind.UnresolvedQualifiedName "A.B.c"
                        Kind.AbbreviatedNamespace "System.Collections"
                        Kind.RequireQualifiedAccessModule "Test.A.Rqa"
                        Kind.DuplicateModule "Test.A.Dup"
                        Kind.OperatorFormQualifiedName "A"
                        Kind.ConstraintNotSupported("int", "Equality")
                        Kind.TraitNotSupported(EqArray.singleton "Widget", MemberNoun.Operator, "+")
                        Kind.TraitNotSupported(EqArray.ofList [ "Widget"; "Gadget" ], MemberNoun.Operator, "+")
                        Kind.TraitAmbiguous(EqArray.ofList [ "Widget"; "Gadget" ], MemberNoun.Operator, "+")
                        Kind.UpcastUnrelated("int", "string")
                        Kind.DowncastUnrelated("int", "string")
                        Kind.MeasureMismatch("m", "s")
                        Kind.DimensionlessMeasureMismatch "kg"
                        Kind.NullaryConstructorPattern("Some", 1)
                        Kind.AmbiguousConstructor("Ok", 2)
                        Kind.ConstructorArity("Some", 1, 2)
                        Kind.NewRequiresClassType
                        Kind.ImmutableFieldAssignment "X"
                        Kind.EnumCaseNotConstant
                        Kind.RangeNotFirstClassValue
                        Kind.CustomEqualityOnRecordOrUnion
                        Kind.StructuralEqualityAttributeOnWrongKind
                        Kind.CustomEqualityAttributeOnInterface
                        Kind.InvalidEqualityAttributeMix
                        Kind.CapabilityNotImplemented("[<CustomEquality>]", "System.IEquatable`1")
                        Kind.CapabilityNotDeclared("[<CustomEquality>]", "equatable")
                        Kind.MissingGetHashCodeOverride
                        Kind.CustomComparisonNeedsEquality
                        Kind.MemberAndLocalBindingClash "x"
                        Kind.DuplicateMember "M"
                        Kind.CyclicType("A", TypeCycle.Inheritance)
                        Kind.CyclicType("A", TypeCycle.StructField)
                        Kind.CyclicType("A", TypeCycle.Abbreviation)
                        Kind.CyclicInline("f", [])
                        Kind.CyclicInline("f", [ "g"; "h" ])
                        Kind.NotYetSupported "inheritance"
                        Kind.IntrinsicNotInScope Intrinsic.ConsList
                        Kind.IntrinsicNotInScope Intrinsic.DynamicGet
                        Kind.IntrinsicNotInScope Intrinsic.DynamicSet
                        Kind.IntrinsicNotInScope Intrinsic.GetIndex
                        Kind.DynamicEscape "int"
                        Kind.HeterogeneousEnum "E"
                        Kind.IncompleteAnonUnionMatch [ "a"; "b"; "c" ]
                        Kind.IncompleteAnonUnionMatch []
                        Kind.UnrelatedTypeTest("int", "string")
                        Kind.RedundantDowncast "int"
                        Kind.ParseFailure "unexpected token"
                        Kind.Driver "no target framework"
                        Kind.Message "an un-migrated sentence"
                    ]
                    @ (verdicts |> List.map (fun v -> Kind.Conformance("Vesper.Core", v)))
                    @ (findings |> List.map Kind.ConformanceFinding)

                for k in kinds do
                    let d = Diagnostic.create k Site.Nowhere []

                    Expect.equal
                        (roundTrips FrozenCodecDiagnostics.writeDiagnostic FrozenCodecDiagnostics.readDiagnostic d)
                        d
                        "Diagnostic (Kind)"
            }

            // `Kind.Parse` forwards the PARSER's vocabulary whole, so the codec carries it too.
            // A delimiter code is three fields (two `Token`s that must not swap, and a `Site`),
            // and `Token` is a flag-bearing enum whose bits have to survive.
            test "every parse DiagnosticCode round-trips inside a Kind.Parse" {
                let codes =
                    [
                        DiagnosticCode.Other "a message the parser built"
                        DiagnosticCode.TyparInConstant
                        DiagnosticCode.MissingExpression
                        DiagnosticCode.MissingPattern
                        DiagnosticCode.MissingType
                        DiagnosticCode.MissingRule
                        DiagnosticCode.MissingTypeDefn
                        DiagnosticCode.MissingModuleElem
                        DiagnosticCode.UnexpectedTopLevel
                        DiagnosticCode.ExpectedEnd
                        DiagnosticCode.ExpectedRParen
                        DiagnosticCode.ExpectedRBracket
                        DiagnosticCode.ExpectedRArrayBracket
                        DiagnosticCode.ExpectedRBraceBar
                        DiagnosticCode.ExpectedQuotationTypedRight
                        DiagnosticCode.ExpectedQuotationUntypedRight
                        DiagnosticCode.UnclosedDelimiter(Token.KWLParen, Site.At 4<token>, Token.KWRParen)
                        DiagnosticCode.MismatchedDelimiter(Token.KWLBraceBar, Site.At 2<token>, Token.KWRBraceBar)
                        // A delimiter the parser itself inserted has no place of its own.
                        DiagnosticCode.UnclosedDelimiter(Token.KWLBracket, Site.Nowhere, Token.KWRBracket)
                    ]

                for c in codes do
                    let d = Diagnostic.create (Kind.Parse c) (Site.After 7<token>) []

                    Expect.equal
                        (roundTrips FrozenCodecDiagnostics.writeDiagnostic FrozenCodecDiagnostics.readDiagnostic d)
                        d
                        "Diagnostic (Kind.Parse)"
            }

            test "an anchor round-trips, its absence included" {
                for a in h.Anchors do
                    Expect.equal
                        (roundTrips FrozenCodecPrimitives.writeAnchor FrozenCodecPrimitives.readAnchor a)
                        a
                        "anchor"
            }

            // The collection must actually reach the corpus, not just the edge cases,
            // or the gate would silently pass on an empty frozen file.
            test "collection exercises a non-trivial value set" {
                Expect.isGreaterThan (List.length gated) 0 "gated programs"
                Expect.isGreaterThan (List.length h.FrozenTypes) 20 "FrozenTypes"
                Expect.isGreaterThan (List.length h.SymbolKeys) 5 "SymbolKeys"
                Expect.isGreaterThan (List.length h.TypeKeys) 2 "TypeKeys"
                // The corpus contributes NO site: it compiles clean, so it bears no
                // diagnostic, and a diagnostic's position is the only one a frozen file
                // carries. The bound below pins the hand-built edge cases alone.
                Expect.isGreaterThan (List.length h.Sites) 4 "Sites"
                Expect.isGreaterThan (List.length h.Anchors) 4 "Anchors"
            }
        ]
