namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser
open Vesper

/// WHAT a diagnostic says. A case carries the facts its sentence is built from, never the
/// sentence, so a consumer selects on the verdict instead of parsing English.
[<RequireQualifiedAccess>]
type Kind =
    // ── Types and members that do not resolve ──────────────────────────────────
    /// `name` does not resolve to a type: no scope of this file claims it and the target's external
    /// universe does not hold it.
    | UndefinedType of name: string
    /// An intrinsic type the compiling target binds no representation for. Naming it is
    /// the error, whether or not the mention would ever demand the representation.
    | UnsupportedOnTarget of typeName: string * target: string
    /// The type resolves; the name on it does not. `noun` is what was looked for.
    | NoMember of typeName: string * noun: MemberNoun * memberName: string
    | NoCase of owner: CaseOwner * typeName: string * caseName: string
    /// A nominal shape resolved to a key unknown to every registry and provider.
    | UnknownNominalType of kind: NominalKind * name: string
    | TypeArgArity of name: string * expected: int * got: int
    /// Types called `name` reach the use site at several arities. The bare name requires a
    /// written instantiation. `arities` is ascending.
    | AmbiguousTypeArity of name: string * arities: Block<int>
    | UnresolvedQualifiedName of name: string
    /// A `module R = N` whose target is a namespace. An abbreviation binds a module.
    | AbbreviatedNamespace of path: string
    /// An `open` of a `[<RequireQualifiedAccess>]` module. `path` is the TARGET's full path,
    /// so an `open` written through a module abbreviation names what it reached.
    | RequireQualifiedAccessModule of path: string
    /// A module path declared by two files of one assembly. `path` is the module's full path.
    | DuplicateModule of path: string
    | OperatorFormQualifiedName of firstSegment: string
    | ConstraintNotSupported of ty: string * constraintName: string
    /// A `new` clause whose constructed type is other than the constrained typar.
    | NewConstraintResultType
    /// A trait call in an inline body that no type in the support set satisfies.
    | TraitNotSupported of supportTys: Block<string> * noun: MemberNoun * name: string
    /// A trait call in an inline body that more than one type in the support set satisfies.
    | TraitAmbiguous of supportTys: Block<string> * noun: MemberNoun * name: string

    // ── Casts and type tests ───────────────────────────────────────────────────
    | UpcastUnrelated of source: string * target: string
    | DowncastUnrelated of source: string * target: string

    // ── Units of measure ───────────────────────────────────────────────────────
    /// Each side is a `MeasureTerm` rendered by declared path (`M.kg`), so two same-named
    /// measures read apart.
    | MeasureMismatch of left: string * right: string
    | DimensionlessMeasureMismatch of measure: string
    /// A TYPE written where a measure is expected: an argument on a measure-kinded parameter
    /// (`float<int>`), or a measure atom that resolves to a type.
    | MeasureExpected
    /// A MEASURE written where a type is expected: a measure claim in type position (`x: m`),
    /// or a measure argument on a type-kinded parameter (`Box<m>`).
    | TypeExpectedNotMeasure
    /// A measure-kinded parameter named where a type-kinded one is required: the target of a
    /// `when` clause, or a member of an SRTP support set.
    | TypeParameterExpectedNotMeasure

    // ── Constructors and patterns ──────────────────────────────────────────────
    /// The head of an applied or dotted pattern (`Foo x`, `Bar.Baz`) is neither a union case
    /// nor an enum case.
    | UndefinedPatternDiscriminator of name: string
    | NullaryConstructorPattern of name: string * arity: int
    | AmbiguousConstructor of name: string * candidates: int
    /// A case of a `[<RequireQualifiedAccess>]` union written without its union's name.
    | RequireQualifiedAccessCase of unionName: string * caseName: string
    | ConstructorArity of name: string * expected: int * got: int
    | NewRequiresClassType
    | ImmutableFieldAssignment of field: string
    | EnumCaseNotConstant
    | RangeNotFirstClassValue

    // ── String literals ────────────────────────────────────────────────────────
    /// A `\DDD` escape above 255 (`"\256"`). fsc wraps the value to a byte under warning
    /// FS1252 and deprecates the wrap; this compiler refuses it outright.
    | EscapeTrigraphOutOfRange of raw: string
    /// A `\UXXXXXXXX` escape above 0x10FFFF, which denotes no Unicode scalar value.
    | EscapeNotUnicodeScalar of raw: string

    // ── Equality / comparison attribute legality ───────────────────────────────
    | CustomEqualityOnRecordOrUnion
    | StructuralEqualityAttributeOnWrongKind
    | CustomEqualityAttributeOnInterface
    | InvalidEqualityAttributeMix
    /// `[<AllowNullLiteral>]` on a kind with no reference slot for `null` to occupy.
    | AllowNullLiteralOnWrongKind
    /// `[<ReferenceEquality>]` on a value type, which has no reference identity to compare.
    | ReferenceEqualityOnStruct
    /// An expression outside the constant domain, in attribute-argument (`[<Foo(1 + x)>]`)
    /// or `[<Literal>]`-RHS position.
    | NotConstantExpression
    /// `[<Literal>] val X: T` in a signature, with no `= e` declaring the value.
    | SignatureLiteralWithoutValue
    /// `val X: T = e` in a signature, with no `[<Literal>]` licensing the value.
    | SignatureValueWithoutLiteral
    /// A constant of another type than its position declares (FS0001): a `[<Literal>]`
    /// signature value against its annotation, or an attribute argument against the
    /// constructor parameter or member it fills. Both are rendered types.
    | ConstantTypeMismatch of expected: string * actual: string
    /// `null` in a constant position whose declared type has no null value (FS0043).
    | NullNotProperValue of ty: string
    /// No constructor of the attribute class takes the written positional arguments:
    /// `given` written, `declared` the parameter count of each constructor.
    | AttributeCtorArgCount of className: string * given: int * declared: Block<int>
    /// Every arity-matching constructor refuses an argument; `candidates` are the rendered
    /// constructor signatures.
    | AttributeCtorNoOverload of className: string * candidates: Block<string>
    /// More than one constructor admits every argument and none is preferred.
    | AttributeCtorAmbiguous of className: string * candidates: Block<string>
    /// A named argument matching neither a parameter of the chosen constructor nor a
    /// property or field of the class (FS0495).
    | AttributeNamedArgUnknown of className: string * name: string
    /// An argument fills a parameter of a local attribute class written without a type
    /// annotation; an attribute argument checks against a declared type.
    | AttributeCtorParamUnannotated of className: string * param: string
    /// The compiling target's attribute metadata cannot encode an argument of this type.
    | UnencodableConstant of typeName: string * target: string
    /// The attribute's declared `[<AttributeUsage>]` mask admits none of the flags the
    /// written-on element occupies. Both are `AttributeTargetFlags` sets.
    | AttributeTargetInvalid of element: int * validOn: int
    /// `attribute` is the posture attribute (`[<CustomEquality>]`); `capability` the
    /// resolved interface it demands, as this compilation's provider spells it.
    | CapabilityNotImplemented of attribute: string * capability: string
    /// The same demand, where the provider does not resolve that capability at all.
    | CapabilityNotDeclared of attribute: string * capabilityWord: string
    | MissingGetHashCodeOverride
    | CustomComparisonNeedsEquality

    // ── Declaration-level clashes and cycles ───────────────────────────────────
    | MemberAndLocalBindingClash of name: string
    | DuplicateMember of name: string
    /// Two fields of one union case resolve to the logical name `name`.
    | UnionCaseFieldNameClash of name: string * clash: UnionFieldNameClash
    | CyclicType of name: string * via: TypeCycle
    /// An `inline` binding whose expansion reaches itself. `via` is the bindings between
    /// `binding` and itself in call order, EMPTY for a direct self-reference.
    | CyclicInline of binding: string * via: string list
    /// `inline` on a member of a `let rec … and …` group.
    | InlineInRecGroup of name: string

    // ── Written, understood, not implemented ───────────────────────────────────
    /// The program is not WRONG, but this compiler does not do that yet.
    | NotYetSupported of feature: string
    /// A lowering needs an intrinsic the compilation cannot see (its package absent from the
    /// reference set), so the fault is the reference set's, not the source's.
    | IntrinsicNotInScope of intrinsic: Intrinsic

    // ── Not the program's fault at all ─────────────────────────────────────────
    | Internal of InternalBreak

    // ── Warnings ───────────────────────────────────────────────────────────────
    /// A `d?foo` whose result type was pinned to something concrete by context: the
    /// `dynamic` default never fired, so the member access is an unchecked assertion.
    | DynamicEscape of pinnedType: string
    | HeterogeneousEnum of name: string
    | IncompleteAnonUnionMatch of unhandled: string list
    | UnrelatedTypeTest of source: string * target: string
    | RedundantDowncast of ty: string
    | OverstatedRecursion of shape: RecursionOverstatement

    // ── Whole-file and whole-package verdicts ──────────────────────────────────
    /// `assembly` is what the checked pair belongs to, which a package build spells with
    /// its manifest name and an assembly build with the name it compiles into.
    | Conformance of assembly: string * verdict: ConformanceVerdict
    /// A `.fsi`↔`.fs` conformance finding positioned at the declaring binding, unlike the
    /// whole-pair `Conformance` verdict above.
    | ConformanceFinding of finding: Conformance.ConformanceError
    /// A fault in the package SET, which has no place in any file being compiled to point at.
    | PackageSet of fault: PackageSetFault
    | ParseFailure of detail: string
    /// A refusal by the DRIVER rather than a verdict about the code: a missing target
    /// framework, an unreadable project.
    | Driver of message: string

    /// A diagnostic the PARSER raised, forwarded whole in the parser's own vocabulary.
    | Parse of DiagnosticCode

    /// The un-migrated remainder: a message built at the call site rather than named as a verdict.
    | Message of text: string

[<RequireQualifiedAccess>]
module Kind =

    /// `'a', 'b'`
    let private quotedNames (names: string list) : string =
        names |> List.map (sprintf "'%s'") |> String.concat ", "

    /// Every `DiagCode.FSharp` number is one fsc ITSELF files the same verdict under, read
    /// out of the F# compiler sources; the exception or resource it came from is named
    /// beside it.
    let code (k: Kind) : DiagCode =
        match k with
        // ── Names and members that do not resolve. fsc files the whole family under one
        // number (`UndefinedName`), and so do we: the noun differs, the verdict does not.
        | Kind.UndefinedType _
        | Kind.NoMember _
        | Kind.NoCase _
        | Kind.UnknownNominalType _
        | Kind.UndefinedPatternDiscriminator _
        | Kind.UnresolvedQualifiedName _ -> DiagCode.FSharp 39 // UndefinedName
        | Kind.AbbreviatedNamespace _ -> DiagCode.FSharp 965 // tcModuleAbbreviationForNamespace
        | Kind.RequireQualifiedAccessModule _ -> DiagCode.FSharp 892 // tcModuleRequiresQualifiedAccess
        | Kind.DuplicateModule _ -> DiagCode.FSharp 248 // DuplicateModuleSpecification
        | Kind.TypeArgArity _ -> DiagCode.FSharp 33 // TyconBadArgs
        | Kind.AmbiguousTypeArity _ -> DiagCode.FSharp 1124 // MultipleGenericTypesOfSameName
        | Kind.RequireQualifiedAccessCase _ -> DiagCode.FSharp 35 // Deprecated
        // ── Constructors: fsc's "union case expects N arguments" covers both the wrong
        // count and the nullary-in-pattern-position case.
        | Kind.ConstructorArity _
        | Kind.NullaryConstructorPattern _ -> DiagCode.FSharp 19 // UnionCaseWrongArguments
        | Kind.ImmutableFieldAssignment _ -> DiagCode.FSharp 5 // FieldNotMutable
        | Kind.EnumCaseNotConstant -> DiagCode.FSharp 886 // tcInvalidEnumerationLiteral
        | Kind.InlineInRecGroup _ -> DiagCode.FSharp 1114 // optValueMarkedInlineButNotBoundInTheOptEnv
        // ── String-literal escapes: fsc's numbers, though fsc files 1252 as a warning
        // (wrapping the value) where this compiler refuses.
        | Kind.EscapeTrigraphOutOfRange _ -> DiagCode.FSharp 1252
        | Kind.EscapeNotUnicodeScalar _ -> DiagCode.FSharp 1245
        // ── Measures reconcile through the type equation, which is where fsc reports them.
        | Kind.MeasureMismatch _
        | Kind.DimensionlessMeasureMismatch _ -> DiagCode.FSharp 1 // ErrorFromAddingTypeEquation
        | Kind.TypeExpectedNotMeasure -> DiagCode.FSharp 704 // ExpectedTypeNotUnitOfMeasure
        | Kind.TypeParameterExpectedNotMeasure -> DiagCode.FSharp 703 // ExpectedTypeParameterNotUnitOfMeasureParameter
        | Kind.MeasureExpected -> DiagCode.FSharp 705 // ExpectedUnitOfMeasureNotType
        | Kind.DowncastUnrelated _ -> DiagCode.FSharp 7 // InvalidRuntimeCoercion
        // ── The two "this coercion tells you nothing" warnings are one number in fsc.
        | Kind.RedundantDowncast _
        | Kind.UnrelatedTypeTest _ -> DiagCode.FSharp 67 // TypeTestUnnecessary
        | Kind.IncompleteAnonUnionMatch _ -> DiagCode.FSharp 25 // MatchIncomplete
        // ── Equality / comparison attribute legality.
        | Kind.CustomEqualityOnRecordOrUnion
        | Kind.CapabilityNotImplemented _
        | Kind.CapabilityNotDeclared _ -> DiagCode.FSharp 378
        | Kind.MissingGetHashCodeOverride -> DiagCode.FSharp 344
        | Kind.CustomComparisonNeedsEquality -> DiagCode.FSharp 379
        | Kind.StructuralEqualityAttributeOnWrongKind
        | Kind.CustomEqualityAttributeOnInterface -> DiagCode.FSharp 382
        | Kind.InvalidEqualityAttributeMix -> DiagCode.FSharp 377
        | Kind.AllowNullLiteralOnWrongKind -> DiagCode.FSharp 934
        | Kind.ReferenceEqualityOnStruct -> DiagCode.FSharp 376
        | Kind.NotConstantExpression -> DiagCode.FSharp 267 // tcInvalidConstantExpression
        | Kind.SignatureLiteralWithoutValue -> DiagCode.Vesper "V261"
        | Kind.SignatureValueWithoutLiteral -> DiagCode.Vesper "V262"
        | Kind.ConstantTypeMismatch _ -> DiagCode.FSharp 1 // ErrorFromAddingTypeEquation
        | Kind.NullNotProperValue _ -> DiagCode.FSharp 43 // TypeDoesNotHaveNull
        | Kind.AttributeCtorArgCount(given = given; declared = declared) ->
            match declared.Length with
            | 1 when given > declared.[0] -> DiagCode.FSharp 501 // one constructor, too many given
            | 1 -> DiagCode.FSharp 496 // one constructor, too few given
            | _ -> DiagCode.FSharp 505 // several constructors, none of the written count
        | Kind.AttributeCtorNoOverload _
        | Kind.AttributeCtorAmbiguous _ -> DiagCode.FSharp 41 // NoOverloadsFound / UnresolvedOverloading
        | Kind.AttributeNamedArgUnknown _ -> DiagCode.FSharp 495
        | Kind.AttributeCtorParamUnannotated _ -> DiagCode.Vesper "V263"
        // fsc refuses the same values in its front end, under codes that describe the written
        // form rather than the encoding: FS0073 for `decimal`, FS0267 for `nativeint`.
        | Kind.UnencodableConstant _ -> DiagCode.Vesper "V264"
        | Kind.NewConstraintResultType -> DiagCode.FSharp 700 // tcNewConstraintMustTakeOneArg
        // fsc files this as a WARNING; here the mismatch is an error.
        | Kind.AttributeTargetInvalid _ -> DiagCode.FSharp 842 // tcAttributeIsNotValidForLanguageElement
        | Kind.MemberAndLocalBindingClash _ -> DiagCode.FSharp 905
        | Kind.DuplicateMember _ -> DiagCode.FSharp 438
        | Kind.UnionCaseFieldNameClash _ -> DiagCode.FSharp 3176
        | Kind.CyclicType(via = TypeCycle.Abbreviation) -> DiagCode.FSharp 953 // tcTypeDefinitionIsCyclic
        // tcTypeDefinitionIsCyclicThroughInheritance, which fsc files BOTH remaining relations
        // under: a self- or mutual-`inherit` cycle and a struct-field cycle all report 954.
        | Kind.CyclicType _ -> DiagCode.FSharp 954
        // ── This compiler's own published families.
        | Kind.Conformance(verdict = v) -> ConformanceVerdict.code v
        | Kind.ConformanceFinding _ -> DiagCode.Vesper "V240"
        | Kind.OverstatedRecursion _ -> DiagCode.Vesper "V260"
        | Kind.PackageSet fault -> PackageSetFault.code fault
        | Kind.ParseFailure _ -> DiagCode.Vesper "PARSE"
        | Kind.Driver _ -> DiagCode.Vesper "DRV"
        | Kind.Parse c -> DiagCode.Parse(DiagnosticCode.code c)
        // ── No published number: fsc has no analogue at all, or its counterpart is a
        // catch-all rather than a classification.
        // fsc has no analogue: it declines to inline a recursive binding and emits the
        // ordinary function instead, where a cross-file `val inline` here has no such
        // function to fall back to.
        | Kind.CyclicInline _
        | Kind.UnsupportedOnTarget _
        | Kind.OperatorFormQualifiedName _
        | Kind.Internal _
        | Kind.ConstraintNotSupported _
        | Kind.TraitNotSupported _
        | Kind.TraitAmbiguous _
        | Kind.UpcastUnrelated _
        | Kind.AmbiguousConstructor _
        | Kind.NewRequiresClassType
        | Kind.RangeNotFirstClassValue
        | Kind.NotYetSupported _
        | Kind.IntrinsicNotInScope _
        | Kind.DynamicEscape _
        | Kind.HeterogeneousEnum _
        | Kind.Message _ -> DiagCode.Unpublished

    let message (k: Kind) : string =
        match k with
        | Kind.UndefinedType name -> sprintf "The type '%s' is not defined" name
        | Kind.UnsupportedOnTarget(typeName, target) -> sprintf "%s is not supported on the %s target" typeName target
        | Kind.NoMember(typeName, noun, memberName) ->
            sprintf "Type '%s' has no %s '%s'" typeName (MemberNoun.word noun) memberName
        | Kind.NoCase(owner, typeName, caseName) ->
            let ownerWord =
                match owner with
                | CaseOwner.Enum -> "Enum"
                | CaseOwner.Union -> "Union"

            sprintf "%s '%s' has no case '%s'" ownerWord typeName caseName
        | Kind.UnknownNominalType(kind, name) ->
            let kindWord =
                match kind with
                | NominalKind.Record -> "record"
                | NominalKind.Class -> "class"
                | NominalKind.Union -> "union"

            sprintf "Unknown %s type '%s'" kindWord name
        | Kind.TypeArgArity(name, expected, got) ->
            sprintf "Type '%s' expects %d type argument(s) but got %d" name expected got
        | Kind.AmbiguousTypeArity(name, arities) ->
            let counts = arities |> Seq.map string |> String.concat ", "

            sprintf
                "Multiple types exist called '%s', taking different numbers of generic parameters (%s). Provide a type instantiation to disambiguate, e.g. '%s<_>'."
                name
                counts
                name
        | Kind.UnresolvedQualifiedName name -> sprintf "Unresolved qualified name: %s" name
        | Kind.AbbreviatedNamespace path ->
            sprintf "The path '%s' is a namespace. A module abbreviation may not abbreviate a namespace." path
        | Kind.RequireQualifiedAccessModule path ->
            sprintf
                "This declaration opens the module '%s', which is marked as 'RequireQualifiedAccess'. Adjust your code to use qualified references to the elements of the module instead, e.g. 'List.map' instead of 'map'. This change will ensure that your code is robust as new constructs are added to libraries."
                path
        | Kind.DuplicateModule path -> sprintf "Two modules named '%s' occur in two parts of this assembly" path
        | Kind.OperatorFormQualifiedName firstSegment ->
            sprintf "Operator-form qualified names not yet resolved (starting at '%s')" firstSegment
        | Kind.ConstraintNotSupported(ty, constraintName) ->
            sprintf "The type '%s' does not support the '%s' constraint" ty constraintName
        | Kind.NewConstraintResultType ->
            "'new' constraints must take one argument of type 'unit' and return the constructed type"
        | Kind.TraitNotSupported(supportTys, noun, name) ->
            match supportTys.Length with
            | 1 -> sprintf "The type '%s' does not support the %s '%s'" supportTys.[0] (MemberNoun.word noun) name
            | _ ->
                sprintf
                    "Neither type '%s' supports the %s '%s'"
                    (String.concat "' nor '" (Block.toArray supportTys))
                    (MemberNoun.word noun)
                    name
        | Kind.TraitAmbiguous(supportTys, noun, name) ->
            sprintf
                "The types '%s' each support the %s '%s', so the call is ambiguous"
                (String.concat "' and '" (Block.toArray supportTys))
                (MemberNoun.word noun)
                name
        | Kind.UpcastUnrelated(source, target) ->
            sprintf "Cannot upcast type '%s' to '%s', because neither inherits the other" source target
        | Kind.DowncastUnrelated(source, target) ->
            sprintf "Cannot downcast type '%s' to unrelated type '%s'" source target
        | Kind.MeasureMismatch(left, right) -> sprintf "Measure mismatch: <%s> vs <%s>" left right
        | Kind.DimensionlessMeasureMismatch measure -> sprintf "Measure mismatch: dimensionless vs <%s>" measure
        | Kind.TypeExpectedNotMeasure -> "Expected type, not unit-of-measure"
        | Kind.TypeParameterExpectedNotMeasure -> "Expected type parameter, not unit-of-measure parameter"
        | Kind.MeasureExpected -> "Expected unit-of-measure, not type"
        | Kind.UndefinedPatternDiscriminator name -> sprintf "The pattern discriminator '%s' is not defined" name
        | Kind.NullaryConstructorPattern(name, arity) ->
            sprintf "Constructor '%s' takes %d argument(s) but is used nullary in pattern position" name arity
        | Kind.AmbiguousConstructor(name, candidates) ->
            sprintf
                "Ambiguous constructor '%s': declared in %d union types, so add a qualifier or annotation"
                name
                candidates
        | Kind.RequireQualifiedAccessCase(unionName, caseName) ->
            sprintf "The union case '%s' requires qualified access: write '%s.%s'" caseName unionName caseName
        | Kind.ConstructorArity(name, expected, got) ->
            sprintf "Constructor '%s' expects %d argument(s) but got %d" name expected got
        | Kind.NewRequiresClassType -> "'new' requires a class type"
        | Kind.ImmutableFieldAssignment field -> sprintf "Cannot assign to immutable field '%s'" field
        | Kind.EnumCaseNotConstant ->
            "An enum case value must be a literal integer or string constant, not an expression"
        | Kind.RangeNotFirstClassValue ->
            "a range expression is only supported as the source of a 'for i in a..b do' counted loop; it has no first-class value"
        | Kind.EscapeTrigraphOutOfRange raw ->
            sprintf "'%s' is not a valid character literal; a decimal escape must be in the range \\000–\\255" raw
        | Kind.EscapeNotUnicodeScalar raw -> sprintf "%s is not a valid Unicode character escape sequence" raw
        | Kind.CustomEqualityOnRecordOrUnion ->
            "[<CustomEquality>]/[<CustomComparison>] on a record or union is not supported in this compiler, so wrap the type in a class that implements IEquatable<_>/IComparable<_>."
        | Kind.StructuralEqualityAttributeOnWrongKind ->
            "Only record, union, exception and struct types may be augmented with the 'ReferenceEquality', 'StructuralEquality' and 'StructuralComparison' attributes."
        | Kind.CustomEqualityAttributeOnInterface ->
            "The 'CustomEquality' and 'CustomComparison' attributes are not valid on an interface type."
        | Kind.InvalidEqualityAttributeMix ->
            "This type uses an invalid mix of the attributes 'NoEquality', 'ReferenceEquality', 'StructuralEquality', 'NoComparison' and 'StructuralComparison'."
        | Kind.AllowNullLiteralOnWrongKind ->
            "Records, union, abbreviations and struct types cannot have the 'AllowNullLiteral' attribute"
        | Kind.ReferenceEqualityOnStruct ->
            "The 'ReferenceEquality' attribute cannot be used on structs. Consider using the 'StructuralEquality' attribute instead, or implement an override for 'System.Object.Equals(obj)'."
        | Kind.NotConstantExpression -> "This is not a valid constant expression or custom attribute value"
        | Kind.SignatureLiteralWithoutValue ->
            "A [<Literal>] value in a signature must declare its value: 'val X: int = 3'"
        | Kind.SignatureValueWithoutLiteral -> "Only a value marked [<Literal>] may declare its value in a signature"
        | Kind.ConstantTypeMismatch(expected, actual) ->
            sprintf "This expression was expected to have type '%s' but here has type '%s'" expected actual
        | Kind.NullNotProperValue ty -> sprintf "The type '%s' does not have 'null' as a proper value" ty
        | Kind.AttributeCtorArgCount(className, given, declared) ->
            match Block.toList declared with
            | [] -> sprintf "The attribute class '%s' declares no constructor" className
            | [ n ] when given > n ->
                sprintf "The object constructor '%s' takes %d argument(s) but is here given %d" className n given
            | [ n ] -> sprintf "The object constructor '%s' requires %d argument(s), given %d" className n given
            | ns ->
                sprintf
                    "The object constructor '%s' does not take %d argument(s); overloads were found taking %s"
                    className
                    given
                    (ns |> List.map string |> String.concat ", ")
        | Kind.AttributeCtorNoOverload(className, candidates) ->
            sprintf
                "No overloads match for the object constructor '%s'. Available overloads: %s"
                className
                (String.concat "; " candidates)
        | Kind.AttributeCtorAmbiguous(className, candidates) ->
            sprintf
                "A unique overload for the object constructor '%s' could not be determined. Candidates: %s"
                className
                (String.concat "; " candidates)
        | Kind.AttributeNamedArgUnknown(className, name) ->
            sprintf "The object constructor '%s' has no argument or settable return property '%s'" className name
        | Kind.AttributeCtorParamUnannotated(className, param) ->
            sprintf
                "The constructor parameter '%s' of the attribute class '%s' has no type annotation; an attribute argument checks against a declared type"
                param
                className
        | Kind.UnencodableConstant(typeName, target) ->
            sprintf "An attribute argument of type '%s' cannot be encoded on the %s target" typeName target
        | Kind.AttributeTargetInvalid(element, validOn) ->
            sprintf
                "This attribute cannot be applied to %s. Valid targets are: %s"
                (AttributeTargetFlags.words element)
                (AttributeTargetFlags.words validOn)
        | Kind.CapabilityNotImplemented(attribute, capability) ->
            sprintf "A type with %s must implement '%s'." attribute capability
        | Kind.CapabilityNotDeclared(attribute, capabilityWord) ->
            sprintf
                "A type with %s requires the '%s' capability, which this compilation's provider does not declare."
                attribute
                capabilityWord
        | Kind.MissingGetHashCodeOverride -> "A type with [<CustomEquality>] must override 'Object.GetHashCode()'."
        | Kind.CustomComparisonNeedsEquality -> "A type with [<CustomComparison>] must also have [<CustomEquality>]."
        | Kind.MemberAndLocalBindingClash name ->
            sprintf "A member and a local class binding both have the name '%s'" name
        | Kind.DuplicateMember name ->
            sprintf "Duplicate definition of member '%s': the same name and signature as an earlier member" name
        | Kind.UnionCaseFieldNameClash(name, UnionFieldNameClash.Declared) ->
            sprintf "Named field '%s' is used more than once." name
        | Kind.UnionCaseFieldNameClash(name, UnionFieldNameClash.AnonymousSpelling) ->
            sprintf "Named field '%s' conflicts with autogenerated name for anonymous field." name
        | Kind.CyclicType(name, TypeCycle.Inheritance) -> sprintf "Type '%s' has a cyclic inheritance hierarchy" name
        | Kind.CyclicType(name, TypeCycle.StructField) ->
            sprintf "Type '%s' involves an immediate cyclic reference through a struct field" name
        | Kind.CyclicType(name, TypeCycle.Abbreviation) ->
            sprintf "Type abbreviation '%s' involves an immediate cyclic reference" name
        | Kind.CyclicInline(binding, via) ->
            sprintf
                "The inline binding '%s' expands into itself (%s). An inline body is spliced at its call site, so a binding that reaches itself has no expansion"
                binding
                (String.concat " → " (binding :: via @ [ binding ]))
        | Kind.InlineInRecGroup name ->
            sprintf "The value '%s' was marked inline but was not bound in the optimization environment" name
        | Kind.NotYetSupported feature -> sprintf "not yet supported: %s" feature
        | Kind.IntrinsicNotInScope intrinsic ->
            sprintf
                "%s is not in scope; add depends-on \"%s\" to this package's manifest"
                (Intrinsic.describe intrinsic)
                (Intrinsic.package intrinsic)
        | Kind.Internal b -> sprintf "internal compiler error: %s" (InternalBreak.describe b)
        | Kind.DynamicEscape pinnedType ->
            sprintf
                "implicit escape from 'dynamic' to '%s': the compiler cannot verify this member access. Annotate the '?' expression as '(expr : %s)' to assert the type explicitly."
                pinnedType
                pinnedType
        | Kind.HeterogeneousEnum name ->
            sprintf "Enum '%s' mixes integer and string case values; heterogeneous enums are legal but discouraged" name
        | Kind.IncompleteAnonUnionMatch unhandled ->
            sprintf
                "Incomplete pattern match on anonymous union: member(s) '%s' not handled"
                (String.concat " | " unhandled)
        | Kind.UnrelatedTypeTest(source, target) ->
            sprintf "Type test of '%s' against unrelated type '%s' is always false" source target
        | Kind.RedundantDowncast ty -> sprintf "Downcast is redundant, because the static type '%s' already matches" ty
        | Kind.OverstatedRecursion(RecursionOverstatement.RedundantRec names) ->
            sprintf
                "The 'rec' keyword is redundant on %s, whose value refers to names outside itself only"
                (quotedNames names)
        | Kind.OverstatedRecursion(RecursionOverstatement.SplittableGroup groups) ->
            sprintf
                "This 'let rec' group covers %d independent recursion groups. Declare them apart, in the order: %s"
                groups.Length
                (groups |> List.map quotedNames |> String.concat "; then ")
        | Kind.Conformance(assembly, verdict) -> sprintf "%s: %s" assembly (ConformanceVerdict.describe verdict)
        | Kind.PackageSet fault -> PackageSetFault.describe fault
        | Kind.ParseFailure detail -> sprintf "parse error: %s" detail
        | Kind.Driver message -> message
        | Kind.Parse c -> DiagnosticCode.message c
        | Kind.ConformanceFinding e -> Conformance.describe e
        | Kind.Message text -> text

    let severity (k: Kind) : Severity =
        match k with
        // A gap in what this compiler MODELS is a warning, as is an attribute divergence,
        // which fsc also compiles (FS1200); every other verdict is a fault in the program
        // being compiled.
        | Kind.Conformance(verdict = ConformanceVerdict.SignatureNotPublished _)
        | Kind.Conformance(verdict = ConformanceVerdict.AttributeArgumentsDiffer _) -> Severity.Warning
        | Kind.DynamicEscape _
        | Kind.HeterogeneousEnum _
        | Kind.IncompleteAnonUnionMatch _
        | Kind.UnrelatedTypeTest _
        | Kind.RedundantDowncast _
        | Kind.OverstatedRecursion _ -> Severity.Warning
        | Kind.UndefinedType _
        | Kind.UnsupportedOnTarget _
        | Kind.NoMember _
        | Kind.NoCase _
        | Kind.UnknownNominalType _
        | Kind.TypeArgArity _
        | Kind.AmbiguousTypeArity _
        | Kind.UnresolvedQualifiedName _
        | Kind.AbbreviatedNamespace _
        | Kind.RequireQualifiedAccessModule _
        | Kind.DuplicateModule _
        | Kind.OperatorFormQualifiedName _
        | Kind.Internal _
        | Kind.ConstraintNotSupported _
        | Kind.NewConstraintResultType
        | Kind.TraitNotSupported _
        | Kind.TraitAmbiguous _
        | Kind.UpcastUnrelated _
        | Kind.DowncastUnrelated _
        | Kind.MeasureMismatch _
        | Kind.DimensionlessMeasureMismatch _
        | Kind.MeasureExpected
        | Kind.TypeExpectedNotMeasure
        | Kind.TypeParameterExpectedNotMeasure
        | Kind.UndefinedPatternDiscriminator _
        | Kind.NullaryConstructorPattern _
        | Kind.AmbiguousConstructor _
        | Kind.RequireQualifiedAccessCase _
        | Kind.ConstructorArity _
        | Kind.NewRequiresClassType
        | Kind.ImmutableFieldAssignment _
        | Kind.EnumCaseNotConstant
        | Kind.RangeNotFirstClassValue
        | Kind.EscapeTrigraphOutOfRange _
        | Kind.EscapeNotUnicodeScalar _
        | Kind.CustomEqualityOnRecordOrUnion
        | Kind.StructuralEqualityAttributeOnWrongKind
        | Kind.CustomEqualityAttributeOnInterface
        | Kind.InvalidEqualityAttributeMix
        | Kind.AllowNullLiteralOnWrongKind
        | Kind.ReferenceEqualityOnStruct
        | Kind.NotConstantExpression
        | Kind.SignatureLiteralWithoutValue
        | Kind.SignatureValueWithoutLiteral
        | Kind.ConstantTypeMismatch _
        | Kind.NullNotProperValue _
        | Kind.AttributeCtorArgCount _
        | Kind.AttributeCtorNoOverload _
        | Kind.AttributeCtorAmbiguous _
        | Kind.AttributeNamedArgUnknown _
        | Kind.AttributeCtorParamUnannotated _
        | Kind.UnencodableConstant _
        | Kind.AttributeTargetInvalid _
        | Kind.CapabilityNotImplemented _
        | Kind.CapabilityNotDeclared _
        | Kind.MissingGetHashCodeOverride
        | Kind.CustomComparisonNeedsEquality
        | Kind.MemberAndLocalBindingClash _
        | Kind.DuplicateMember _
        | Kind.UnionCaseFieldNameClash _
        | Kind.CyclicType _
        | Kind.CyclicInline _
        | Kind.InlineInRecGroup _
        | Kind.NotYetSupported _
        | Kind.IntrinsicNotInScope _
        | Kind.Conformance _
        | Kind.ConformanceFinding _
        | Kind.PackageSet _
        | Kind.ParseFailure _
        | Kind.Driver _
        | Kind.Parse _
        | Kind.Message _ -> Severity.Error

/// A SECONDARY place a diagnostic points at, and what it means there. The unclosed-delimiter
/// diagnostic is the shape: primary at the hole where the delimiter belonged, one label back
/// on the delimiter left open.
type Label = { Site: Site; Message: string }

/// NAME COLLISION: `XParsec.FSharp.Parser` declares its own `Diagnostic`, so a file that
/// `open`s the parser must alias or fully qualify this one.
type Diagnostic =
    {
        Kind: Kind
        /// The primary position: what a one-line renderer points at.
        Site: Site
        /// Secondary positions, in the order a renderer should show them.
        Related: Label list
    }

    member this.Code: DiagCode = Kind.code this.Kind
    member this.Message: string = Kind.message this.Kind
    member this.Severity: Severity = Kind.severity this.Kind

[<RequireQualifiedAccess>]
module Diagnostic =

    let create (kind: Kind) (site: Site) (related: Label list) : Diagnostic =
        {
            Kind = kind
            Site = site
            Related = related
        }

    /// A verdict about a whole file or package rather than about a place in one: a lex or
    /// parse failure, a driver refusal, a conformance finding about a signature.
    let nowhere (kind: Kind) : Diagnostic = create kind Site.Nowhere []

    let isError (d: Diagnostic) : bool = d.Severity = Severity.Error

    let errors (ds: Diagnostic seq) : Diagnostic list = ds |> Seq.filter isError |> List.ofSeq
