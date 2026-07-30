namespace XParsec.FSharp.SemanticAnalysis

open System.IO

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

open XParsec.FSharp.SemanticAnalysis.FrozenCodecPrimitives

/// The FROZEN diagnostic domain: a `Diagnostic`, the `Kind` that is its verdict, and the
/// small closed vocabularies a kind's facts are drawn from. Split from `FrozenCodecTypes`
/// because it shares nothing with the frozen TYPE domain but the codec sink — a `Kind`
/// carries strings, ints and its own enums, never a `FrozenType` or a key.
///
/// `Site` itself is a primitive (`FrozenCodecPrimitives.writeSite`): it is the position
/// every diagnostic and every future span-bearing payload speaks, not a diagnostic's own
/// business.
module FrozenCodecDiagnostics =

    // `Diagnostic` is qualified below rather than aliased; see the type's declaration for
    // why the bare name would otherwise be the parser's.
    let private writeLabel (w: FrozenWriter) (l: Label) =
        writeSite w l.Site
        w.Write l.Message

    let private readLabel (r: FrozenReader) : Label =
        let site = readSite r
        let message = r.ReadString()
        { Site = site; Message = message }

    let private writeMemberNoun (w: FrozenWriter) (n: MemberNoun) =
        match n with
        | MemberNoun.Field -> w.Write 0uy
        | MemberNoun.InstanceMember -> w.Write 1uy
        | MemberNoun.StaticMember -> w.Write 2uy
        | MemberNoun.BuiltInStaticMember -> w.Write 3uy
        | MemberNoun.AccessibleMember -> w.Write 4uy
        | MemberNoun.ValueOrMember -> w.Write 5uy
        | MemberNoun.FieldOrMember -> w.Write 6uy
        | MemberNoun.Operator -> w.Write 7uy
        | MemberNoun.Member -> w.Write 8uy

    let private readMemberNoun (r: FrozenReader) : MemberNoun =
        match r.ReadByte() with
        | 0uy -> MemberNoun.Field
        | 1uy -> MemberNoun.InstanceMember
        | 2uy -> MemberNoun.StaticMember
        | 3uy -> MemberNoun.BuiltInStaticMember
        | 4uy -> MemberNoun.AccessibleMember
        | 5uy -> MemberNoun.ValueOrMember
        | 6uy -> MemberNoun.FieldOrMember
        | 7uy -> MemberNoun.Operator
        | 8uy -> MemberNoun.Member
        | b -> failwithf "FrozenCodec: unknown MemberNoun tag %d" b

    let private writeNominalKind (w: FrozenWriter) (k: NominalKind) =
        match k with
        | NominalKind.Record -> w.Write 0uy
        | NominalKind.Class -> w.Write 1uy
        | NominalKind.Union -> w.Write 2uy

    let private readNominalKind (r: FrozenReader) : NominalKind =
        match r.ReadByte() with
        | 0uy -> NominalKind.Record
        | 1uy -> NominalKind.Class
        | 2uy -> NominalKind.Union
        | b -> failwithf "FrozenCodec: unknown NominalKind tag %d" b

    let private writeConformanceVerdict (w: FrozenWriter) (v: ConformanceVerdict) =
        match v with
        | ConformanceVerdict.Unimplemented(sigFile, detail) ->
            w.Write 0uy
            w.Write sigFile
            w.Write detail
        | ConformanceVerdict.SigWithoutImpl sigFile ->
            w.Write 1uy
            w.Write sigFile
        | ConformanceVerdict.ModulePairingMismatch(sigFile, implFile, sigDecl, implDecl) ->
            w.Write 2uy
            w.Write sigFile
            w.Write implFile
            w.Write sigDecl
            w.Write implDecl
        | ConformanceVerdict.ImplWithoutContract implFile ->
            w.Write 3uy
            w.Write implFile
        | ConformanceVerdict.StaleSigOnly name ->
            w.Write 4uy
            w.Write name
        | ConformanceVerdict.UnknownSigOnly name ->
            w.Write 5uy
            w.Write name
        | ConformanceVerdict.PairParseFailure(sigFile, detail) ->
            w.Write 6uy
            w.Write sigFile
            w.Write detail

    let private readConformanceVerdict (r: FrozenReader) : ConformanceVerdict =
        match r.ReadByte() with
        | 0uy ->
            let sigFile = r.ReadString()
            ConformanceVerdict.Unimplemented(sigFile, r.ReadString())
        | 1uy -> ConformanceVerdict.SigWithoutImpl(r.ReadString())
        | 2uy ->
            let sigFile = r.ReadString()
            let implFile = r.ReadString()
            let sigDecl = r.ReadString()
            ConformanceVerdict.ModulePairingMismatch(sigFile, implFile, sigDecl, r.ReadString())
        | 3uy -> ConformanceVerdict.ImplWithoutContract(r.ReadString())
        | 4uy -> ConformanceVerdict.StaleSigOnly(r.ReadString())
        | 5uy -> ConformanceVerdict.UnknownSigOnly(r.ReadString())
        | 6uy ->
            let sigFile = r.ReadString()
            ConformanceVerdict.PairParseFailure(sigFile, r.ReadString())
        | b -> failwithf "FrozenCodec: unknown ConformanceVerdict tag %d" b

    /// `Token` is a `uint16`-backed enum, so it rides as its own representation — including
    /// the flag bits, which a diagnostic's spelling helper masks off on READ
    /// (`TokenInfo.withoutFlags`) rather than at rest.
    let private writeToken (w: FrozenWriter) (t: Token) = w.Write(uint16 t)

    let private readToken (r: FrozenReader) : Token =
        LanguagePrimitives.EnumOfValue(r.ReadUInt16())

    /// The PARSER's verdict, forwarded whole by `Kind.Parse`. Codeable at all because every
    /// `DiagnosticCode` payload is a `Token`, a `Site` or a string — see that type for why
    /// it holds no CST node.
    let private writeDiagnosticCode (w: FrozenWriter) (c: DiagnosticCode) =
        match c with
        | DiagnosticCode.Other msg ->
            w.Write 0uy
            w.Write msg
        | DiagnosticCode.TyparInConstant -> w.Write 1uy
        | DiagnosticCode.MissingExpression -> w.Write 2uy
        | DiagnosticCode.MissingPattern -> w.Write 3uy
        | DiagnosticCode.MissingType -> w.Write 4uy
        | DiagnosticCode.MissingRule -> w.Write 5uy
        | DiagnosticCode.MissingTypeDefn -> w.Write 6uy
        | DiagnosticCode.MissingModuleElem -> w.Write 7uy
        | DiagnosticCode.UnexpectedTopLevel -> w.Write 8uy
        | DiagnosticCode.ExpectedEnd -> w.Write 9uy
        | DiagnosticCode.ExpectedRParen -> w.Write 10uy
        | DiagnosticCode.ExpectedRBracket -> w.Write 11uy
        | DiagnosticCode.ExpectedRArrayBracket -> w.Write 12uy
        | DiagnosticCode.ExpectedRBraceBar -> w.Write 13uy
        | DiagnosticCode.ExpectedQuotationTypedRight -> w.Write 14uy
        | DiagnosticCode.ExpectedQuotationUntypedRight -> w.Write 15uy
        | DiagnosticCode.UnclosedDelimiter(opened, openedAt, expected) ->
            w.Write 16uy
            writeToken w opened
            writeSite w openedAt
            writeToken w expected
        | DiagnosticCode.MismatchedDelimiter(opened, openedAt, expected) ->
            w.Write 17uy
            writeToken w opened
            writeSite w openedAt
            writeToken w expected

    let private readDiagnosticCode (r: FrozenReader) : DiagnosticCode =
        match r.ReadByte() with
        | 0uy -> DiagnosticCode.Other(r.ReadString())
        | 1uy -> DiagnosticCode.TyparInConstant
        | 2uy -> DiagnosticCode.MissingExpression
        | 3uy -> DiagnosticCode.MissingPattern
        | 4uy -> DiagnosticCode.MissingType
        | 5uy -> DiagnosticCode.MissingRule
        | 6uy -> DiagnosticCode.MissingTypeDefn
        | 7uy -> DiagnosticCode.MissingModuleElem
        | 8uy -> DiagnosticCode.UnexpectedTopLevel
        | 9uy -> DiagnosticCode.ExpectedEnd
        | 10uy -> DiagnosticCode.ExpectedRParen
        | 11uy -> DiagnosticCode.ExpectedRBracket
        | 12uy -> DiagnosticCode.ExpectedRArrayBracket
        | 13uy -> DiagnosticCode.ExpectedRBraceBar
        | 14uy -> DiagnosticCode.ExpectedQuotationTypedRight
        | 15uy -> DiagnosticCode.ExpectedQuotationUntypedRight
        | 16uy ->
            let opened = readToken r
            let openedAt = readSite r
            DiagnosticCode.UnclosedDelimiter(opened, openedAt, readToken r)
        | 17uy ->
            let opened = readToken r
            let openedAt = readSite r
            DiagnosticCode.MismatchedDelimiter(opened, openedAt, readToken r)
        | b -> failwithf "FrozenCodec: unknown DiagnosticCode tag %d" b

    let private writeInternalBreak (w: FrozenWriter) (b: InternalBreak) =
        match b with
        | InternalBreak.UnresolvedTyVars count ->
            w.Write 0uy
            w.Write count
        | InternalBreak.MemberNotResolvable(resolver, declaringType, memberName) ->
            w.Write 1uy
            w.Write resolver
            w.Write declaringType
            w.Write memberName
        | InternalBreak.UnflattenedModule pass ->
            w.Write 2uy
            w.Write pass

    let private readInternalBreak (r: FrozenReader) : InternalBreak =
        match r.ReadByte() with
        | 0uy -> InternalBreak.UnresolvedTyVars(r.ReadInt32())
        | 1uy ->
            let resolver = r.ReadString()
            let declaringType = r.ReadString()
            InternalBreak.MemberNotResolvable(resolver, declaringType, r.ReadString())
        | 2uy -> InternalBreak.UnflattenedModule(r.ReadString())
        | b -> failwithf "FrozenCodec: unknown InternalBreak tag %d" b

    /// A diagnostic's VERDICT: a case tag plus that case's facts. The WRITER is exhaustive,
    /// so a new `Kind` case cannot land without being given a tag; the reader is a byte
    /// match and can only fault on a tag nothing wrote, which is why the round-trip test
    /// covers one value per case rather than trusting the two to agree.
    let private writeKind (w: FrozenWriter) (k: Kind) =
        match k with
        | Kind.UndefinedType name ->
            w.Write 0uy
            w.Write name
        | Kind.Internal b ->
            w.Write 1uy
            writeInternalBreak w b
        | Kind.UnrepresentableTypes names ->
            w.Write 2uy
            writeStringList w names
        | Kind.NoMember(typeName, noun, memberName) ->
            w.Write 3uy
            w.Write typeName
            writeMemberNoun w noun
            w.Write memberName
        | Kind.NoCase(owner, typeName, caseName) ->
            w.Write 4uy

            w.Write(
                match owner with
                | CaseOwner.Enum -> 0uy
                | CaseOwner.Union -> 1uy
            )

            w.Write typeName
            w.Write caseName
        | Kind.UnknownNominalType(kind, name) ->
            w.Write 5uy
            writeNominalKind w kind
            w.Write name
        | Kind.TypeArgArity(name, expected, got) ->
            w.Write 6uy
            w.Write name
            w.Write expected
            w.Write got
        | Kind.UnresolvedQualifiedName name ->
            w.Write 7uy
            w.Write name
        | Kind.OperatorFormQualifiedName firstSegment ->
            w.Write 8uy
            w.Write firstSegment
        | Kind.ConstraintNotSupported(ty, constraintName) ->
            w.Write 10uy
            w.Write ty
            w.Write constraintName
        | Kind.TraitNotSupported(receiver, noun, name) ->
            w.Write 11uy
            w.Write receiver
            writeMemberNoun w noun
            w.Write name
        | Kind.UpcastUnrelated(source, target) ->
            w.Write 12uy
            w.Write source
            w.Write target
        | Kind.DowncastUnrelated(source, target) ->
            w.Write 13uy
            w.Write source
            w.Write target
        | Kind.MeasureMismatch(left, right) ->
            w.Write 14uy
            w.Write left
            w.Write right
        | Kind.DimensionlessMeasureMismatch measure ->
            w.Write 15uy
            w.Write measure
        | Kind.NullaryConstructorPattern(name, arity) ->
            w.Write 16uy
            w.Write name
            w.Write arity
        | Kind.AmbiguousConstructor(name, candidates) ->
            w.Write 17uy
            w.Write name
            w.Write candidates
        | Kind.ConstructorArity(name, expected, got) ->
            w.Write 18uy
            w.Write name
            w.Write expected
            w.Write got
        | Kind.NewRequiresClassType -> w.Write 19uy
        | Kind.ImmutableFieldAssignment field ->
            w.Write 20uy
            w.Write field
        | Kind.EnumCaseNotConstant -> w.Write 21uy
        | Kind.RangeNotFirstClassValue -> w.Write 22uy
        | Kind.CustomEqualityOnRecordOrUnion -> w.Write 23uy
        | Kind.StructuralEqualityAttributeOnWrongKind -> w.Write 24uy
        | Kind.CustomEqualityAttributeOnInterface -> w.Write 25uy
        | Kind.InvalidEqualityAttributeMix -> w.Write 26uy
        | Kind.CapabilityNotImplemented(attribute, capability) ->
            w.Write 27uy
            w.Write attribute
            w.Write capability
        | Kind.CapabilityNotNamed(attribute, capabilityWord) ->
            w.Write 28uy
            w.Write attribute
            w.Write capabilityWord
        | Kind.MissingGetHashCodeOverride -> w.Write 29uy
        | Kind.CustomComparisonNeedsEquality -> w.Write 30uy
        | Kind.MemberAndLocalBindingClash name ->
            w.Write 31uy
            w.Write name
        | Kind.DuplicateMember name ->
            w.Write 32uy
            w.Write name
        | Kind.CyclicType(name, via) ->
            w.Write 33uy
            w.Write name

            w.Write(
                match via with
                | TypeCycle.Inheritance -> 0uy
                | TypeCycle.Immediate -> 1uy
            )
        | Kind.NotYetSupported feature ->
            w.Write 34uy
            w.Write feature
        | Kind.IntrinsicNotInScope intrinsic ->
            w.Write 35uy
            w.Write intrinsic
        | Kind.DynamicEscape pinnedType ->
            w.Write 36uy
            w.Write pinnedType
        | Kind.HeterogeneousEnum name ->
            w.Write 37uy
            w.Write name
        | Kind.IncompleteAnonUnionMatch unhandled ->
            w.Write 38uy
            writeStringList w unhandled
        | Kind.UnrelatedTypeTest(source, target) ->
            w.Write 39uy
            w.Write source
            w.Write target
        | Kind.RedundantDowncast ty ->
            w.Write 40uy
            w.Write ty
        | Kind.Conformance(package, verdict) ->
            w.Write 41uy
            w.Write package
            writeConformanceVerdict w verdict
        | Kind.LexFailure detail ->
            w.Write 42uy
            w.Write detail
        | Kind.ParseFailure detail ->
            w.Write 43uy
            w.Write detail
        | Kind.Driver message ->
            w.Write 44uy
            w.Write message
        | Kind.Message text ->
            w.Write 45uy
            w.Write text
        | Kind.Parse c ->
            w.Write 46uy
            writeDiagnosticCode w c
        | Kind.CyclicInline(binding, via) ->
            w.Write 47uy
            w.Write binding
            writeStringList w via

    let private readKind (r: FrozenReader) : Kind =
        match r.ReadByte() with
        | 0uy -> Kind.UndefinedType(r.ReadString())
        | 1uy -> Kind.Internal(readInternalBreak r)
        | 2uy -> Kind.UnrepresentableTypes(readStringList r)
        | 3uy ->
            let typeName = r.ReadString()
            let noun = readMemberNoun r
            Kind.NoMember(typeName, noun, r.ReadString())
        | 4uy ->
            let owner =
                match r.ReadByte() with
                | 0uy -> CaseOwner.Enum
                | 1uy -> CaseOwner.Union
                | b -> failwithf "FrozenCodec: unknown CaseOwner tag %d" b

            let typeName = r.ReadString()
            Kind.NoCase(owner, typeName, r.ReadString())
        | 5uy ->
            let kind = readNominalKind r
            Kind.UnknownNominalType(kind, r.ReadString())
        | 6uy ->
            let name = r.ReadString()
            let expected = r.ReadInt32()
            Kind.TypeArgArity(name, expected, r.ReadInt32())
        | 7uy -> Kind.UnresolvedQualifiedName(r.ReadString())
        | 8uy -> Kind.OperatorFormQualifiedName(r.ReadString())
        // Tag 9 is RETIRED: `MemberNotResolvable` became an `InternalBreak` under tag 1. Blobs
        // that carried it are unreachable (`Cache.CodeVersion` moved), so the tag is free —
        // named here only so it is reused deliberately rather than by accident.
        | 10uy ->
            let ty = r.ReadString()
            Kind.ConstraintNotSupported(ty, r.ReadString())
        | 11uy ->
            let receiver = r.ReadString()
            let noun = readMemberNoun r
            Kind.TraitNotSupported(receiver, noun, r.ReadString())
        | 12uy ->
            let source = r.ReadString()
            Kind.UpcastUnrelated(source, r.ReadString())
        | 13uy ->
            let source = r.ReadString()
            Kind.DowncastUnrelated(source, r.ReadString())
        | 14uy ->
            let left = r.ReadString()
            Kind.MeasureMismatch(left, r.ReadString())
        | 15uy -> Kind.DimensionlessMeasureMismatch(r.ReadString())
        | 16uy ->
            let name = r.ReadString()
            Kind.NullaryConstructorPattern(name, r.ReadInt32())
        | 17uy ->
            let name = r.ReadString()
            Kind.AmbiguousConstructor(name, r.ReadInt32())
        | 18uy ->
            let name = r.ReadString()
            let expected = r.ReadInt32()
            Kind.ConstructorArity(name, expected, r.ReadInt32())
        | 19uy -> Kind.NewRequiresClassType
        | 20uy -> Kind.ImmutableFieldAssignment(r.ReadString())
        | 21uy -> Kind.EnumCaseNotConstant
        | 22uy -> Kind.RangeNotFirstClassValue
        | 23uy -> Kind.CustomEqualityOnRecordOrUnion
        | 24uy -> Kind.StructuralEqualityAttributeOnWrongKind
        | 25uy -> Kind.CustomEqualityAttributeOnInterface
        | 26uy -> Kind.InvalidEqualityAttributeMix
        | 27uy ->
            let attribute = r.ReadString()
            Kind.CapabilityNotImplemented(attribute, r.ReadString())
        | 28uy ->
            let attribute = r.ReadString()
            Kind.CapabilityNotNamed(attribute, r.ReadString())
        | 29uy -> Kind.MissingGetHashCodeOverride
        | 30uy -> Kind.CustomComparisonNeedsEquality
        | 31uy -> Kind.MemberAndLocalBindingClash(r.ReadString())
        | 32uy -> Kind.DuplicateMember(r.ReadString())
        | 33uy ->
            let name = r.ReadString()

            let via =
                match r.ReadByte() with
                | 0uy -> TypeCycle.Inheritance
                | 1uy -> TypeCycle.Immediate
                | b -> failwithf "FrozenCodec: unknown TypeCycle tag %d" b

            Kind.CyclicType(name, via)
        | 34uy -> Kind.NotYetSupported(r.ReadString())
        | 35uy -> Kind.IntrinsicNotInScope(r.ReadString())
        | 36uy -> Kind.DynamicEscape(r.ReadString())
        | 37uy -> Kind.HeterogeneousEnum(r.ReadString())
        | 38uy -> Kind.IncompleteAnonUnionMatch(readStringList r)
        | 39uy ->
            let source = r.ReadString()
            Kind.UnrelatedTypeTest(source, r.ReadString())
        | 40uy -> Kind.RedundantDowncast(r.ReadString())
        | 41uy ->
            let package = r.ReadString()
            Kind.Conformance(package, readConformanceVerdict r)
        | 42uy -> Kind.LexFailure(r.ReadString())
        | 43uy -> Kind.ParseFailure(r.ReadString())
        | 44uy -> Kind.Driver(r.ReadString())
        | 45uy -> Kind.Message(r.ReadString())
        | 46uy -> Kind.Parse(readDiagnosticCode r)
        | 47uy ->
            let binding = r.ReadString()
            Kind.CyclicInline(binding, readStringList r)
        | b -> failwithf "FrozenCodec: unknown Kind tag %d" b

    let writeDiagnostic (w: FrozenWriter) (d: XParsec.FSharp.SemanticAnalysis.Diagnostic) =
        writeSite w d.Site
        writeKind w d.Kind
        writeListWith w writeLabel d.Related

    let readDiagnostic (r: FrozenReader) : XParsec.FSharp.SemanticAnalysis.Diagnostic =
        let site = readSite r
        let kind = readKind r
        Diagnostic.create kind site (readListWith r readLabel)
