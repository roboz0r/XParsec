module XParsec.FSharp.Codegen.Js.Tests.FrozenCodecRoundTripTests

open System.Collections.Generic
open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The leaf-codec gate: `read (write x) = x` STRUCTURALLY for every value in the leaf
// domains — `FrozenType`, the `SymbolKey`/`TypeKey` key cluster (`FrozenCodecTypes`),
// `NodeKey`, and a node's anchor (`FrozenCodecPrimitives`). Data comes from two sources:
// the frozen conformance corpus (realistic breadth), harvested from the leaf-bearing side
// tables + FrozenType child-walk of each `Frozen.TastFile` (no full expr/decl tree walk —
// that arrives with the tree codec), and hand-built edge cases that pin EVERY case shape
// the corpus may not exercise (an `FTOr` of several members, a deeply nested type, each
// `MemberKind`, an absent anchor, …).

/// Corpus programs the JS backend actually compiles — the same gate the byte-identity
/// test uses, so `frozenOfJs` never trips on a `Diagnose` program's error diagnostics.
let private gated =
    programs
    |> List.filter (fun p ->
        match Map.tryFind "js" p.Obligations with
        | Some Obligation.Run
        | Some(Obligation.Fault _) -> true
        | _ -> false
    )

/// The deduped leaf values harvested from the corpus plus the hand-built edge cases.
type private Harvest =
    {
        FrozenTypes: FrozenType list
        SymbolKeys: SymbolKey list
        TypeKeys: TypeKey list
        Sites: Site list
        Anchors: Anchor list
    }

let private collect () : Harvest =
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
                visitSym key
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
            | FTOr members -> EqSet.iter visitFt members
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
            // no leaf here to harvest into any of the key corpora.
            | FTLocalTypar _ -> ()
            | FTUnknown _ -> ()

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

    let harvestFile (file: Pooled.TastFile) =
        // A diagnostic's position, which can name a node the emittable tree does not
        // contain and so takes no pool id.
        for d in file.Diagnostics do
            sites.Add d.Site |> ignore

        for k in file.IntrinsicReprKeys.Keys do
            visitSym k

        for k in file.Accessibility.Keys do
            visitSym k

        for KeyValue(_, constraints) in file.GenericFnSchemes do
            for c in constraints do
                match c with
                | FrozenConstraint.Coercion(_, target) -> visitFt target

        // No `BindingValReprs` harvest: the DU carries none — a binding's source arity is
        // a PROJECTION of its lambda spine that `TastPools.toPools` derives off the
        // columns, so every type and slot it names is already reached by the spine walk
        // this harvest runs.

        // Real frozen anchors, shallowly: the source anchor of each top-level decl body.
        for decl in file.Decls do
            match decl with
            | TDeclG.Let(_, value, _, _) -> toks.Add(TastWalk.exprTok value) |> ignore
            | TDeclG.Expression(expr, _) -> toks.Add(TastWalk.exprTok expr) |> ignore
            | TDeclG.Type _ -> ()

    for p in gated do
        harvestFile (TastUnpool.ofPools (frozenOfJs p.Source))

    // ── hand-built edge cases: pin every case shape regardless of the corpus ──
    let nsGlobal = NamespaceKey.Global

    let nsSystem =
        {
            Path = EqArray.ofList [ "System"; "Collections"; "Generic" ]
        }

    let tkInt =
        {
            Holder = TypeHolder.InNamespace nsGlobal
            Name = "int"
            TyparArity = 0
        }

    let tkString =
        {
            Holder = TypeHolder.InNamespace nsGlobal
            Name = "string"
            TyparArity = 0
        }

    let tkArray =
        {
            Holder = TypeHolder.InNamespace nsGlobal
            Name = "[]"
            TyparArity = 1
        }

    let tkList =
        {
            Holder = TypeHolder.InNamespace nsSystem
            Name = "List`1"
            TyparArity = 1
        }

    let modKey =
        {
            Holder = ModuleHolder.InNamespace nsSystem
            Name = "ListModule"
        }

    let tkInMod =
        {
            Holder = TypeHolder.InModule modKey
            Name = "Inner"
            TyparArity = 0
        }

    let tkNested =
        {
            Holder = TypeHolder.InType tkList
            Name = "Enumerator"
            TyparArity = 0
        }

    let ftInt = FTConst(SymbolKey.Type tkInt, EqArray.empty)
    let ftString = FTConst(SymbolKey.Type tkString, EqArray.empty)
    let ftArray = FTConst(SymbolKey.Type tkArray, EqArray.singleton ftInt)
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
                    Decl = ModuleHolder.InNamespace nsSystem
                    Name = "printfn"
                }
            SymbolKey.Binding
                {
                    Decl = ModuleHolder.InModule modKey
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
            // FTOr of several members — must survive as the exact stored set (no MkUnion
            // flatten/collapse on read).
            FTOr(EqSet.ofSeq [ ftInt; ftString; ftLitStr; ftLitInt ])
            ftLitStr
            ftLitInt
            FTKeyOf ftRecord
            FTIndexedAccess(ftRecord, ftLitStr)
            ftCond
            FTTypar(TyparAxis.Declaring, 0)
            FTTypar(TyparAxis.Method, 3)
            FTLocalTypar(SchemeId 7, 2)
            FTUnknown "?free-typar"
            // Deeply nested: functions, tuples, sets and computations composed together.
            FTFun(FTTuple(EqArray.ofList [ ftCond; ftArray ]), FTOr(EqSet.ofSeq [ FTKeyOf ftRecord; ftCond; ftInt ]))
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

let private roundTrips (write: System.IO.BinaryWriter -> 'a -> unit) (read: System.IO.BinaryReader -> 'a) (x: 'a) =
    FrozenCodecPrimitives.ofBytes read (FrozenCodecPrimitives.toBytes write x)

[<Tests>]
let tests =
    let h = collect ()

    testList
        "FrozenCodec leaf round-trip"
        [
            test "FrozenType round-trips structurally" {
                for ft in h.FrozenTypes do
                    Expect.equal
                        (roundTrips FrozenCodecTypes.writeFrozenType FrozenCodecTypes.readFrozenType ft)
                        ft
                        "FrozenType"
            }

            test "SymbolKey round-trips structurally" {
                for sk in h.SymbolKeys do
                    Expect.equal
                        (roundTrips FrozenCodecTypes.writeSymbolKey FrozenCodecTypes.readSymbolKey sk)
                        sk
                        "SymbolKey"
            }

            test "TypeKey round-trips structurally" {
                for tk in h.TypeKeys do
                    Expect.equal (roundTrips FrozenCodecTypes.writeTypeKey FrozenCodecTypes.readTypeKey tk) tk "TypeKey"
            }

            test "a diagnostic's Site round-trips, every case" {
                for s in h.Sites do
                    Expect.equal (roundTrips FrozenCodecPrimitives.writeSite FrozenCodecPrimitives.readSite s) s "Site"
            }

            // The bare `Between` constructor can spell a run the type says does not exist —
            // one token wide, or ends reversed. Writer and reader must agree on which form
            // those take, or a blob would decode to a value that never went in.
            test "a Between the smart constructor would not build canonicalises the same way on both sides" {
                for raw in [ Site.Between(5<token>, 5<token>); Site.Between(9<token>, 3<token>) ] do
                    Expect.equal
                        (roundTrips FrozenCodecPrimitives.writeSite FrozenCodecPrimitives.readSite raw)
                        (Site.normalise raw)
                        "Site (non-canonical Between)"
            }

            // `Related` is populated only at the PARSE seam, whose diagnostics do not reach
            // a frozen file's `Diagnostics`, so no corpus program can reach the label leg of
            // the codec. Without this, a labelled diagnostic would decode to a stripped one
            // and every test would still pass.
            test "a diagnostic round-trips, its labels included" {
                let opener: Label =
                    {
                        Site = Site.At 3<token>
                        Message = DiagnosticCode.openedHereLabel
                    }

                let placeless: Label =
                    {
                        Site = Site.Nowhere
                        Message = "a label that names no place"
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

            // ONE value per `Kind` case. The WRITER is exhaustive — the compiler refuses a
            // case with no tag — but the reader is a byte match, so only a value that makes
            // the whole round trip proves the two agree. The list payloads are where a
            // writer/reader pair most easily disagrees on framing, and severity and code ride
            // the kind, so a value that decoded to the wrong case would also report at the
            // wrong severity; hence the whole value is compared, not just the tag.
            test "every Kind case round-trips" {
                let verdicts =
                    [
                        ConformanceVerdict.Unimplemented("a.fsi", "M is missing")
                        ConformanceVerdict.SigWithoutImpl "a.fsi"
                        ConformanceVerdict.ModulePairingMismatch("a.fsi", "a.fs", "M", "N")
                        ConformanceVerdict.ImplWithoutContract "a.fs"
                        ConformanceVerdict.StaleSigOnly "list.fsi"
                        ConformanceVerdict.UnknownSigOnly "gone.fsi"
                        ConformanceVerdict.PairParseFailure("a.fsi", "unexpected token")
                    ]

                let kinds =
                    [
                        Kind.UndefinedType "Nope"
                        Kind.Internal(InternalBreak.UnresolvedTyVars 3)
                        Kind.Internal(InternalBreak.MemberNotResolvable("mkMethodCall", "Widget", "M"))
                        Kind.Internal(InternalBreak.UnflattenedModule "Validation")
                        Kind.UnrepresentableTypes [ "System.Guid"; "System.DateTime" ]
                        Kind.UnrepresentableTypes []
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
                        Kind.OperatorFormQualifiedName "A"
                        Kind.ConstraintNotSupported("int", "Equality")
                        Kind.TraitNotSupported("Widget", MemberNoun.Operator, "+")
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
                        Kind.CapabilityNotNamed("[<CustomEquality>]", "equatable")
                        Kind.MissingGetHashCodeOverride
                        Kind.CustomComparisonNeedsEquality
                        Kind.MemberAndLocalBindingClash "x"
                        Kind.DuplicateMember "M"
                        Kind.CyclicType("A", TypeCycle.Inheritance)
                        Kind.CyclicType("A", TypeCycle.Immediate)
                        Kind.NotYetSupported "inheritance"
                        Kind.IntrinsicNotInScope "Array indexing intrinsic 'GetArray'"
                        Kind.DynamicEscape "int"
                        Kind.HeterogeneousEnum "E"
                        Kind.IncompleteAnonUnionMatch [ "a"; "b"; "c" ]
                        Kind.IncompleteAnonUnionMatch []
                        Kind.UnrelatedTypeTest("int", "string")
                        Kind.RedundantDowncast "int"
                        Kind.LexFailure "unexpected character"
                        Kind.ParseFailure "unexpected token"
                        Kind.Driver "no target framework"
                        Kind.Message "an un-migrated sentence"
                    ]
                    @ (verdicts |> List.map (fun v -> Kind.Conformance("Vesper.Core", v)))

                for k in kinds do
                    let d = Diagnostic.create k Site.Nowhere []

                    Expect.equal
                        (roundTrips FrozenCodecDiagnostics.writeDiagnostic FrozenCodecDiagnostics.readDiagnostic d)
                        d
                        "Diagnostic (Kind)"
            }

            // `Kind.Parse` forwards the PARSER's vocabulary whole, so the codec has to carry
            // that vocabulary too. Every payload-carrying case is covered: a delimiter code
            // is three fields (two `Token`s that must not swap, and a `Site`), and `Token` is
            // a flag-bearing enum whose bits have to survive.
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
                        // A delimiter the parser itself inserted names no place of its own.
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

            // The harvest must actually reach the corpus, not just the edge cases —
            // otherwise the gate would silently pass on an empty frozen file.
            test "harvest exercises a non-trivial value set" {
                Expect.isGreaterThan (List.length gated) 0 "gated programs"
                Expect.isGreaterThan (List.length h.FrozenTypes) 20 "FrozenTypes"
                Expect.isGreaterThan (List.length h.SymbolKeys) 5 "SymbolKeys"
                Expect.isGreaterThan (List.length h.TypeKeys) 2 "TypeKeys"
                // The corpus contributes NO site: it compiles clean, so it bears no
                // diagnostic, and a diagnostic's position is the only one a frozen file
                // carries. The hand-built edge cases are the whole of this corpus, and
                // their count is what the bound pins.
                Expect.isGreaterThan (List.length h.Sites) 4 "Sites"
                Expect.isGreaterThan (List.length h.Anchors) 4 "Anchors"
            }
        ]
