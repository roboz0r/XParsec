namespace XParsec.FSharp.SemanticAnalysis

open System.IO

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

open XParsec.FSharp.SemanticAnalysis.FrozenCodecPrimitives

/// The FROZEN diagnostic domain: a `Diagnostic`, the `Kind` that is its verdict, and the
/// small closed vocabularies a kind's facts are drawn from. A `Kind` carries strings, ints
/// and its own enums, never a `FrozenType` or a key, so nothing here references the tables.
module FrozenCodecDiagnostics =

    // `XParsec.FSharp.Parser` declares its own `Diagnostic`, so the bare name here would be
    // the parser's, hence the fully qualified signatures at the bottom of the file.
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
        | ConformanceVerdict.ModulePairingMismatch(sigFile, implFile, sigDecl, implDecl) ->
            w.Write 1uy
            w.Write sigFile
            w.Write implFile
            w.Write sigDecl
            w.Write implDecl
        | ConformanceVerdict.SignatureNotPublished detail ->
            w.Write 2uy
            w.Write detail
        | ConformanceVerdict.SignatureRejected detail ->
            w.Write 3uy
            w.Write detail
        | ConformanceVerdict.AttributeArgumentsDiffer(sigFile, divergence) ->
            w.Write 4uy
            w.Write sigFile
            w.Write divergence.Declaration
            w.Write divergence.Attribute

    let private readConformanceVerdict (r: FrozenReader) : ConformanceVerdict =
        match r.ReadByte() with
        | 0uy ->
            let sigFile = r.ReadString()
            ConformanceVerdict.Unimplemented(sigFile, r.ReadString())
        | 1uy ->
            let sigFile = r.ReadString()
            let implFile = r.ReadString()
            let sigDecl = r.ReadString()
            ConformanceVerdict.ModulePairingMismatch(sigFile, implFile, sigDecl, r.ReadString())
        | 2uy -> ConformanceVerdict.SignatureNotPublished(r.ReadString())
        | 3uy -> ConformanceVerdict.SignatureRejected(r.ReadString())
        | 4uy ->
            let sigFile = r.ReadString()
            let declaration = r.ReadString()

            ConformanceVerdict.AttributeArgumentsDiffer(
                sigFile,
                {
                    Declaration = declaration
                    Attribute = r.ReadString()
                }
            )
        | b -> failwithf "FrozenCodec: unknown ConformanceVerdict tag %d" b

    let private writeTypeKindFamily (w: FrozenWriter) (f: Conformance.TypeKindFamily) =
        match f with
        | Conformance.TypeKindFamily.Class -> w.Write 0uy
        | Conformance.TypeKindFamily.Interface -> w.Write 1uy
        | Conformance.TypeKindFamily.Record -> w.Write 2uy
        | Conformance.TypeKindFamily.Union -> w.Write 3uy
        | Conformance.TypeKindFamily.Enum -> w.Write 4uy

    let private readTypeKindFamily (r: FrozenReader) : Conformance.TypeKindFamily =
        match r.ReadByte() with
        | 0uy -> Conformance.TypeKindFamily.Class
        | 1uy -> Conformance.TypeKindFamily.Interface
        | 2uy -> Conformance.TypeKindFamily.Record
        | 3uy -> Conformance.TypeKindFamily.Union
        | 4uy -> Conformance.TypeKindFamily.Enum
        | b -> failwithf "FrozenCodec: unknown TypeKindFamily tag %d" b

    let private writeConformanceError (w: FrozenWriter) (e: Conformance.ConformanceError) =
        match e with
        | Conformance.ConformanceError.MissingInImpl name ->
            w.Write 0uy
            w.Write name
        | Conformance.ConformanceError.ExternWithoutIntrinsic name ->
            w.Write 1uy
            w.Write name
        | Conformance.ConformanceError.IntrinsicWithoutExtern name ->
            w.Write 2uy
            w.Write name
        | Conformance.ConformanceError.HeritabilityMismatch name ->
            w.Write 3uy
            w.Write name
        | Conformance.ConformanceError.TypeKindMismatch(name, declared, defined) ->
            w.Write 4uy
            w.Write name
            writeTypeKindFamily w declared
            writeTypeKindFamily w defined
        | Conformance.ConformanceError.ValueMissingInImpl name ->
            w.Write 5uy
            w.Write name
        | Conformance.ConformanceError.ImportBodyNotNativeOnly name ->
            w.Write 6uy
            w.Write name
        | Conformance.ConformanceError.NativeOnlyWithoutImport name ->
            w.Write 7uy
            w.Write name
        | Conformance.ConformanceError.ImportSelectorMismatch(name, selector) ->
            w.Write 8uy
            w.Write name
            w.Write selector
        | Conformance.ConformanceError.ImportMalformed name ->
            w.Write 9uy
            w.Write name
        | Conformance.ConformanceError.ImportPathMalformed(name, path) ->
            w.Write 10uy
            w.Write name
            w.Write path
        | Conformance.ConformanceError.ImportAssetNotListed(name, path) ->
            w.Write 11uy
            w.Write name
            w.Write path
        | Conformance.ConformanceError.ImportUnsupportedTarget name ->
            w.Write 12uy
            w.Write name
        | Conformance.ConformanceError.ImportMissingExport(name, selector, asset) ->
            w.Write 13uy
            w.Write name
            w.Write selector
            w.Write asset
        | Conformance.ConformanceError.CompiledNameDiffers(name, declared, defined) ->
            w.Write 14uy
            w.Write name
            w.Write declared
            w.Write defined

    let private readConformanceError (r: FrozenReader) : Conformance.ConformanceError =
        match r.ReadByte() with
        | 0uy -> Conformance.ConformanceError.MissingInImpl(r.ReadString())
        | 1uy -> Conformance.ConformanceError.ExternWithoutIntrinsic(r.ReadString())
        | 2uy -> Conformance.ConformanceError.IntrinsicWithoutExtern(r.ReadString())
        | 3uy -> Conformance.ConformanceError.HeritabilityMismatch(r.ReadString())
        | 4uy ->
            let name = r.ReadString()
            let declared = readTypeKindFamily r
            Conformance.ConformanceError.TypeKindMismatch(name, declared, readTypeKindFamily r)
        | 5uy -> Conformance.ConformanceError.ValueMissingInImpl(r.ReadString())
        | 6uy -> Conformance.ConformanceError.ImportBodyNotNativeOnly(r.ReadString())
        | 7uy -> Conformance.ConformanceError.NativeOnlyWithoutImport(r.ReadString())
        | 8uy ->
            let name = r.ReadString()
            Conformance.ConformanceError.ImportSelectorMismatch(name, r.ReadString())
        | 9uy -> Conformance.ConformanceError.ImportMalformed(r.ReadString())
        | 10uy ->
            let name = r.ReadString()
            Conformance.ConformanceError.ImportPathMalformed(name, r.ReadString())
        | 11uy ->
            let name = r.ReadString()
            Conformance.ConformanceError.ImportAssetNotListed(name, r.ReadString())
        | 12uy -> Conformance.ConformanceError.ImportUnsupportedTarget(r.ReadString())
        | 13uy ->
            let name = r.ReadString()
            let selector = r.ReadString()
            Conformance.ConformanceError.ImportMissingExport(name, selector, r.ReadString())
        | 14uy ->
            let name = r.ReadString()
            let declared = r.ReadString()
            Conformance.ConformanceError.CompiledNameDiffers(name, declared, r.ReadString())
        | b -> failwithf "FrozenCodec: unknown ConformanceError tag %d" b

    /// A `uint16`-backed enum, written as its own representation INCLUDING the flag bits; a
    /// diagnostic's spelling helper masks those off on read rather than at rest.
    let private writeToken (w: FrozenWriter) (t: Token) = w.Write(uint16 t)

    let private readToken (r: FrozenReader) : Token =
        LanguagePrimitives.EnumOfValue(r.ReadUInt16())

    /// The PARSER's verdict, forwarded whole. Codeable at all because every `DiagnosticCode`
    /// payload is a `Token`, a `Site` or a string, never a CST node.
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
        | InternalBreak.NonAccessorInWithClause(propertyName, bindingName) ->
            w.Write 3uy
            w.Write propertyName
            w.Write bindingName
        | InternalBreak.UnstampedStaticDeclArgs declaringType ->
            w.Write 4uy
            w.Write declaringType

    let private readInternalBreak (r: FrozenReader) : InternalBreak =
        match r.ReadByte() with
        | 0uy -> InternalBreak.UnresolvedTyVars(r.ReadInt32())
        | 1uy ->
            let resolver = r.ReadString()
            let declaringType = r.ReadString()
            InternalBreak.MemberNotResolvable(resolver, declaringType, r.ReadString())
        | 2uy -> InternalBreak.UnflattenedModule(r.ReadString())
        | 3uy ->
            let propertyName = r.ReadString()
            InternalBreak.NonAccessorInWithClause(propertyName, r.ReadString())
        | 4uy -> InternalBreak.UnstampedStaticDeclArgs(r.ReadString())
        | b -> failwithf "FrozenCodec: unknown InternalBreak tag %d" b

    let private writePackageSetFault (w: FrozenWriter) (f: PackageSetFault) =
        match f with
        | PackageSetFault.FileMissing(package, relative) ->
            w.Write 0uy
            w.Write package
            w.Write relative
        | PackageSetFault.MalformedManifest(path, detail) ->
            w.Write 1uy
            w.Write path
            w.Write detail
        | PackageSetFault.NoManifestForTarget(packageDir, target) ->
            w.Write 2uy
            w.Write packageDir
            w.Write target
        | PackageSetFault.UnresolvedDependency detail ->
            w.Write 3uy
            w.Write detail
        | PackageSetFault.DuplicateType(typeName, first, second) ->
            w.Write 4uy
            w.Write typeName
            w.Write first
            w.Write second

    let private readPackageSetFault (r: FrozenReader) : PackageSetFault =
        match r.ReadByte() with
        | 0uy ->
            let package = r.ReadString()
            PackageSetFault.FileMissing(package, r.ReadString())
        | 1uy ->
            let path = r.ReadString()
            PackageSetFault.MalformedManifest(path, r.ReadString())
        | 2uy ->
            let packageDir = r.ReadString()
            PackageSetFault.NoManifestForTarget(packageDir, r.ReadString())
        | 3uy -> PackageSetFault.UnresolvedDependency(r.ReadString())
        | 4uy ->
            let typeName = r.ReadString()
            let first = r.ReadString()
            PackageSetFault.DuplicateType(typeName, first, r.ReadString())
        | b -> failwithf "FrozenCodec: unknown PackageSetFault tag %d" b

    let private writeKind (w: FrozenWriter) (k: Kind) =
        match k with
        | Kind.UndefinedType name ->
            w.Write 0uy
            w.Write name
        | Kind.Internal b ->
            w.Write 1uy
            writeInternalBreak w b
        | Kind.UnsupportedOnTarget(typeName, target) ->
            w.Write 2uy
            w.Write typeName
            w.Write target
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
        | Kind.AmbiguousTypeArity(name, arities) ->
            w.Write 61uy
            w.Write name
            writeEqArrayWith w (fun w (n: int) -> w.Write n) arities
        | Kind.UnresolvedQualifiedName name ->
            w.Write 7uy
            w.Write name
        | Kind.AbbreviatedNamespace path ->
            w.Write 58uy
            w.Write path
        | Kind.RequireQualifiedAccessModule path ->
            w.Write 59uy
            w.Write path
        | Kind.DuplicateModule path ->
            w.Write 60uy
            w.Write path
        | Kind.OperatorFormQualifiedName firstSegment ->
            w.Write 8uy
            w.Write firstSegment
        | Kind.UndefinedPatternDiscriminator name ->
            w.Write 9uy
            w.Write name
        | Kind.ConstraintNotSupported(ty, constraintName) ->
            w.Write 10uy
            w.Write ty
            w.Write constraintName
        | Kind.TraitNotSupported(supportTys, noun, name) ->
            w.Write 11uy
            writeStringArray w supportTys
            writeMemberNoun w noun
            w.Write name
        | Kind.TraitAmbiguous(supportTys, noun, name) ->
            w.Write 56uy
            writeStringArray w supportTys
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
        | Kind.CapabilityNotDeclared(attribute, capabilityWord) ->
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
                | TypeCycle.StructField -> 1uy
                | TypeCycle.Abbreviation -> 2uy
            )
        | Kind.NotYetSupported feature ->
            w.Write 34uy
            w.Write feature
        | Kind.IntrinsicNotInScope intrinsic ->
            w.Write 35uy

            w.Write(
                match intrinsic with
                | Intrinsic.ConsList -> 0uy
                | Intrinsic.DynamicGet -> 1uy
                | Intrinsic.DynamicSet -> 2uy
                | Intrinsic.GetIndex -> 3uy
            )
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
        | Kind.ConformanceFinding finding ->
            w.Write 57uy
            writeConformanceError w finding
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
        | Kind.AllowNullLiteralOnWrongKind -> w.Write 48uy
        | Kind.RequireQualifiedAccessCase(unionName, caseName) ->
            w.Write 50uy
            w.Write unionName
            w.Write caseName
        | Kind.PackageSet fault ->
            w.Write 49uy
            writePackageSetFault w fault
        | Kind.EscapeTrigraphOutOfRange raw ->
            w.Write 51uy
            w.Write raw
        | Kind.EscapeNotUnicodeScalar raw ->
            w.Write 52uy
            w.Write raw
        | Kind.NotConstantExpression -> w.Write 53uy
        | Kind.AttributeTargetInvalid(element, validOn) ->
            w.Write 54uy
            w.Write element
            w.Write validOn
        | Kind.ReferenceEqualityOnStruct -> w.Write 55uy

    let private readKind (r: FrozenReader) : Kind =
        match r.ReadByte() with
        | 0uy -> Kind.UndefinedType(r.ReadString())
        | 1uy -> Kind.Internal(readInternalBreak r)
        | 2uy ->
            let typeName = r.ReadString()
            Kind.UnsupportedOnTarget(typeName, r.ReadString())
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
        | 61uy ->
            let name = r.ReadString()
            Kind.AmbiguousTypeArity(name, EqArray.ofArray (readArrayWith r (fun r -> r.ReadInt32())))
        | 7uy -> Kind.UnresolvedQualifiedName(r.ReadString())
        | 58uy -> Kind.AbbreviatedNamespace(r.ReadString())
        | 59uy -> Kind.RequireQualifiedAccessModule(r.ReadString())
        | 60uy -> Kind.DuplicateModule(r.ReadString())
        | 8uy -> Kind.OperatorFormQualifiedName(r.ReadString())
        | 9uy -> Kind.UndefinedPatternDiscriminator(r.ReadString())
        | 10uy ->
            let ty = r.ReadString()
            Kind.ConstraintNotSupported(ty, r.ReadString())
        | 11uy ->
            let supportTys = readStringArray r
            let noun = readMemberNoun r
            Kind.TraitNotSupported(supportTys, noun, r.ReadString())
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
            Kind.CapabilityNotDeclared(attribute, r.ReadString())
        | 29uy -> Kind.MissingGetHashCodeOverride
        | 30uy -> Kind.CustomComparisonNeedsEquality
        | 31uy -> Kind.MemberAndLocalBindingClash(r.ReadString())
        | 32uy -> Kind.DuplicateMember(r.ReadString())
        | 33uy ->
            let name = r.ReadString()

            let via =
                match r.ReadByte() with
                | 0uy -> TypeCycle.Inheritance
                | 1uy -> TypeCycle.StructField
                | 2uy -> TypeCycle.Abbreviation
                | b -> failwithf "FrozenCodec: unknown TypeCycle tag %d" b

            Kind.CyclicType(name, via)
        | 34uy -> Kind.NotYetSupported(r.ReadString())
        | 35uy ->
            Kind.IntrinsicNotInScope(
                match r.ReadByte() with
                | 0uy -> Intrinsic.ConsList
                | 1uy -> Intrinsic.DynamicGet
                | 2uy -> Intrinsic.DynamicSet
                | 3uy -> Intrinsic.GetIndex
                | b -> failwithf "FrozenCodec: unknown Intrinsic tag %d" b
            )
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
        | 43uy -> Kind.ParseFailure(r.ReadString())
        | 44uy -> Kind.Driver(r.ReadString())
        | 45uy -> Kind.Message(r.ReadString())
        | 46uy -> Kind.Parse(readDiagnosticCode r)
        | 47uy ->
            let binding = r.ReadString()
            Kind.CyclicInline(binding, readStringList r)
        | 48uy -> Kind.AllowNullLiteralOnWrongKind
        | 49uy -> Kind.PackageSet(readPackageSetFault r)
        | 50uy ->
            let unionName = r.ReadString()
            Kind.RequireQualifiedAccessCase(unionName, r.ReadString())
        | 51uy -> Kind.EscapeTrigraphOutOfRange(r.ReadString())
        | 52uy -> Kind.EscapeNotUnicodeScalar(r.ReadString())
        | 53uy -> Kind.NotConstantExpression
        | 54uy ->
            let element = r.ReadInt32()
            Kind.AttributeTargetInvalid(element, r.ReadInt32())
        | 55uy -> Kind.ReferenceEqualityOnStruct
        | 56uy ->
            let supportTys = readStringArray r
            let noun = readMemberNoun r
            Kind.TraitAmbiguous(supportTys, noun, r.ReadString())
        | 57uy -> Kind.ConformanceFinding(readConformanceError r)
        | b -> failwithf "FrozenCodec: unknown Kind tag %d" b

    let writeDiagnostic (w: FrozenWriter) (d: XParsec.FSharp.SemanticAnalysis.Diagnostic) =
        writeSite w d.Site
        writeKind w d.Kind
        writeListWith w writeLabel d.Related

    let readDiagnostic (r: FrozenReader) : XParsec.FSharp.SemanticAnalysis.Diagnostic =
        let site = readSite r
        let kind = readKind r
        Diagnostic.create kind site (readListWith r readLabel)
