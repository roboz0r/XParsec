namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// `RequireQualifiedAccess` because an unqualified `Error` case would shadow the `Result`
// constructor.
[<RequireQualifiedAccess; Struct>]
type Severity =
    | Error
    | Warning

/// The PUBLISHED number a diagnostic is filed under: what a consumer suppresses, filters
/// or asserts on across compiler versions.
[<RequireQualifiedAccess>]
type DiagCode =
    /// An fsc diagnostic number this compiler deliberately reproduces, so a program moved
    /// between the two compilers is refused under the same number.
    | FSharp of number: int
    /// This compiler's OWN published families: the `V24x` package-conformance codes, and the
    /// front-end/driver refusals that are about a file rather than about a program.
    | Vesper of code: string
    /// No published number, which is the case for most verdicts.
    | Unpublished

[<RequireQualifiedAccess>]
module DiagCode =

    /// How a code prints; `Unpublished` renders empty.
    let render (c: DiagCode) : string =
        match c with
        | DiagCode.FSharp n -> sprintf "FS%04d" n
        | DiagCode.Vesper code -> code
        | DiagCode.Unpublished -> ""

/// Which NOMINAL shape a type is: the axis a "no such type" verdict differs on.
[<RequireQualifiedAccess>]
type NominalKind =
    | Record
    | Class
    | Union

/// What a member-lookup verdict was LOOKING for.
[<RequireQualifiedAccess>]
type MemberNoun =
    | Field
    | InstanceMember
    | StaticMember
    | BuiltInStaticMember
    | AccessibleMember
    | ValueOrMember
    | FieldOrMember
    | Operator
    /// No narrower claim than "this name is not on that type".
    | Member

[<RequireQualifiedAccess>]
module MemberNoun =

    let word (n: MemberNoun) : string =
        match n with
        | MemberNoun.Field -> "field"
        | MemberNoun.InstanceMember -> "instance member"
        | MemberNoun.StaticMember -> "static member"
        | MemberNoun.BuiltInStaticMember -> "built-in static member"
        | MemberNoun.AccessibleMember -> "accessible member"
        | MemberNoun.ValueOrMember -> "value or member"
        | MemberNoun.FieldOrMember -> "field or member"
        | MemberNoun.Operator -> "operator"
        | MemberNoun.Member -> "member"

[<RequireQualifiedAccess>]
module IntrinsicHost =

    /// A construct that needs a real type in the output to live on, so no spelling of it
    /// is admissible on an intrinsic host.
    [<RequireQualifiedAccess>]
    type Construct =
        /// The grammar gives `override`/`default` no `inline` slot, and an intrinsic
        /// publishes no method table to hold the overridden one.
        | Override
        /// A secondary constructor carries a body that must be emitted as a real `.ctor`. A
        /// `.fsi` `new: … -> T` on a heritable primitive merely NAMES a target-provided one,
        /// and does not hit this case.
        | Constructor

    let memberNeedsInline (hostName: string) : string =
        sprintf
            "A member of intrinsic type '%s' must be declared 'inline': the type carries no method in the output, so a member body is spliced at the use site, never called"
            hostName

    /// The same rule where `inline` is not even a remedy.
    let cannotDeclare (hostName: string) (construct: Construct) : string =
        let what =
            match construct with
            | Construct.Override -> "an 'override' or 'default' member"
            | Construct.Constructor -> "a constructor with a body"

        sprintf
            "Intrinsic type '%s' cannot declare %s: the type carries no representation in the output, so only a spliced 'member inline' is admissible on it"
            hostName
            what

/// Which sort of type declares the cases a `NoCase` verdict is about.
[<RequireQualifiedAccess>]
type CaseOwner =
    | Enum
    | Union

/// How a type's definition reaches itself. `Immediate` is fsc's FS0954: a struct field or
/// inheritance relation that makes the type contain itself with no indirection.
[<RequireQualifiedAccess>]
type TypeCycle =
    | Inheritance
    | Immediate

/// One package-conformance verdict about one contract `.fsi` (and its companion `.fs`).
[<RequireQualifiedAccess>]
type ConformanceVerdict =
    /// A binding the contract declares that the implementation does not satisfy.
    | Unimplemented of sigFile: string * detail: string
    /// A contract with no companion implementation, not declared `sig-only`.
    | SigWithoutImpl of sigFile: string
    /// The paired files' leading module / namespace declarations disagree.
    | ModulePairingMismatch of sigFile: string * implFile: string * sigDecl: string * implDecl: string
    /// A compiled implementation with no `.fsi` contract.
    | ImplWithoutContract of implFile: string
    /// Declared `sig-only`, but a companion implementation exists.
    | StaleSigOnly of name: string
    /// Declared `sig-only`, but no contract `.fsi` in the package has that name at all.
    | UnknownSigOnly of name: string
    /// Declared `impl-only`, but the target compiles no such contract-less body, so either
    /// the name is a typo or the `.fsi` it disclaims has since appeared.
    | UnknownImplOnly of name: string
    /// The contract or its companion failed to parse, so that pair could not be conformed.
    | PairParseFailure of sigFile: string * detail: string

[<RequireQualifiedAccess>]
module ConformanceVerdict =

    /// The `V24x` family code. Findings share one where the verdict is the same: `V240` is
    /// "the implementation does not answer the contract", however that came about.
    let code (v: ConformanceVerdict) : DiagCode =
        match v with
        | ConformanceVerdict.Unimplemented _
        | ConformanceVerdict.SigWithoutImpl _ -> DiagCode.Vesper "V240"
        | ConformanceVerdict.ModulePairingMismatch _ -> DiagCode.Vesper "V241"
        | ConformanceVerdict.ImplWithoutContract _ -> DiagCode.Vesper "V242"
        | ConformanceVerdict.StaleSigOnly _
        | ConformanceVerdict.UnknownSigOnly _
        | ConformanceVerdict.UnknownImplOnly _ -> DiagCode.Vesper "V243"
        | ConformanceVerdict.PairParseFailure _ -> DiagCode.Vesper "V244"

    let describe (v: ConformanceVerdict) : string =
        match v with
        | ConformanceVerdict.Unimplemented(sigFile, detail) -> sprintf "%s: %s" sigFile detail
        | ConformanceVerdict.SigWithoutImpl sigFile ->
            sprintf
                "the signature file '%s' has no corresponding implementation file and is not declared `sig-only` in the manifest"
                sigFile
        | ConformanceVerdict.ModulePairingMismatch(sigFile, implFile, sigDecl, implDecl) ->
            sprintf
                "%s ↔ %s: the paired files' leading module/namespace declarations disagree ('%s' vs '%s')"
                sigFile
                implFile
                sigDecl
                implDecl
        | ConformanceVerdict.ImplWithoutContract implFile ->
            sprintf "the implementation file '%s' has no '.fsi' contract" implFile
        | ConformanceVerdict.StaleSigOnly name ->
            sprintf
                "'%s' is declared `sig-only` but a companion implementation exists — remove the stale exemption"
                name
        | ConformanceVerdict.UnknownSigOnly name ->
            sprintf "`sig-only` names '%s', which is not a contract `.fsi` in this package" name
        | ConformanceVerdict.UnknownImplOnly name ->
            sprintf
                "`impl-only` names '%s', which this target does not compile as a contract-less body — remove it, or implement the `.fsi` it now has"
                name
        | ConformanceVerdict.PairParseFailure(sigFile, detail) ->
            sprintf "the contract '%s' or its implementation failed to parse: %s" sigFile detail

/// A broken invariant INSIDE this compiler, never a verdict about the program: the source
/// that provoked one may be perfectly correct. A diagnostic rather than a crash, so a break
/// in one declaration surfaces alongside the rest of the file's findings.
[<RequireQualifiedAccess>]
type InternalBreak =
    /// The TAST reaching the freeze still holds inference metavariables.
    | UnresolvedTyVars of count: int
    /// Inference committed to a member that resolves in neither the local registry nor the
    /// provider.
    | MemberNotResolvable of resolver: string * declaringType: string * memberName: string
    /// A nested `module` reached a pass that runs on the FLATTENED element list.
    | UnflattenedModule of pass: string
    /// A binding under `member x.P with …` named something other than `get` / `set`. A
    /// property with no `with` clause at all is an implicit get, so the clause being
    /// PRESENT means its halves are named.
    | NonAccessorInWithClause of propertyName: string * bindingName: string

[<RequireQualifiedAccess>]
module InternalBreak =

    /// The rendered English. The "internal compiler error:" prefix is added by the caller,
    /// so no case here writes it.
    let describe (b: InternalBreak) : string =
        match b with
        | InternalBreak.UnresolvedTyVars count -> sprintf "the frozen TAST holds %d unresolved TyVar(s)" count
        | InternalBreak.MemberNotResolvable(resolver, declaringType, memberName) ->
            sprintf
                "Elaborate.%s: member '%s' on %s was committed by inference but resolves in neither the local registry nor the provider"
                resolver
                memberName
                declaringType
        | InternalBreak.UnflattenedModule pass ->
            sprintf "a nested `module` reached %s; the `implFileElems` flattening invariant has drifted" pass
        | InternalBreak.NonAccessorInWithClause(propertyName, bindingName) ->
            sprintf
                "the `with` clause of property '%s' binds '%s'; the grammar admits only `get` and `set` there"
                propertyName
                bindingName

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
    /// A nominal shape resolved to a key no registry and no provider answers for.
    | UnknownNominalType of kind: NominalKind * name: string
    | TypeArgArity of name: string * expected: int * got: int
    | UnresolvedQualifiedName of name: string
    | OperatorFormQualifiedName of firstSegment: string
    | ConstraintNotSupported of ty: string * constraintName: string
    /// An inline body's trait call the support type cannot answer.
    | TraitNotSupported of supportTy: string * noun: MemberNoun * name: string

    // ── Casts and type tests ───────────────────────────────────────────────────
    | UpcastUnrelated of source: string * target: string
    | DowncastUnrelated of source: string * target: string

    // ── Units of measure ───────────────────────────────────────────────────────
    | MeasureMismatch of left: string * right: string
    | DimensionlessMeasureMismatch of measure: string

    // ── Constructors and patterns ──────────────────────────────────────────────
    /// The head of an applied or dotted pattern (`Foo x`, `Bar.Baz`) is neither a union case
    /// nor an enum case.
    | UndefinedPatternDiscriminator of name: string
    | NullaryConstructorPattern of name: string * arity: int
    | AmbiguousConstructor of name: string * candidates: int
    | ConstructorArity of name: string * expected: int * got: int
    | NewRequiresClassType
    | ImmutableFieldAssignment of field: string
    | EnumCaseNotConstant
    | RangeNotFirstClassValue

    // ── Equality / comparison attribute legality ───────────────────────────────
    | CustomEqualityOnRecordOrUnion
    | StructuralEqualityAttributeOnWrongKind
    | CustomEqualityAttributeOnInterface
    | InvalidEqualityAttributeMix
    /// `[<AllowNullLiteral>]` on a kind with no reference slot for `null` to occupy.
    | AllowNullLiteralOnWrongKind
    /// `attribute` is the posture attribute (`[<CustomEquality>]`); `capability` the
    /// resolved interface it demands, as this compilation's provider names it.
    | CapabilityNotImplemented of attribute: string * capability: string
    /// The same demand, where the provider does not resolve that capability at all.
    | CapabilityNotNamed of attribute: string * capabilityWord: string
    | MissingGetHashCodeOverride
    | CustomComparisonNeedsEquality

    // ── Declaration-level clashes and cycles ───────────────────────────────────
    | MemberAndLocalBindingClash of name: string
    | DuplicateMember of name: string
    | CyclicType of name: string * via: TypeCycle
    /// An `inline` binding whose expansion reaches itself. `via` is the bindings between
    /// `binding` and itself in call order, EMPTY for a direct self-reference.
    | CyclicInline of binding: string * via: string list

    // ── Written, understood, not implemented ───────────────────────────────────
    /// The program is not WRONG, but this compiler does not do that yet.
    | NotYetSupported of feature: string
    /// A lowering needs an intrinsic the compilation cannot see (`Vesper.Core` absent from
    /// the reference set), so the fault is the reference set's, not the source's.
    | IntrinsicNotInScope of intrinsic: string

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

    // ── Whole-file and whole-package verdicts ──────────────────────────────────
    | Conformance of package: string * verdict: ConformanceVerdict
    | LexFailure of detail: string
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
        | Kind.TypeArgArity _ -> DiagCode.FSharp 33 // TyconBadArgs
        // ── Constructors: fsc's "union case expects N arguments" covers both the wrong
        // count and the nullary-in-pattern-position case.
        | Kind.ConstructorArity _
        | Kind.NullaryConstructorPattern _ -> DiagCode.FSharp 19 // UnionCaseWrongArguments
        | Kind.ImmutableFieldAssignment _ -> DiagCode.FSharp 5 // FieldNotMutable
        | Kind.EnumCaseNotConstant -> DiagCode.FSharp 886 // tcInvalidEnumerationLiteral
        // ── Measures reconcile through the type equation, which is where fsc reports them.
        | Kind.MeasureMismatch _
        | Kind.DimensionlessMeasureMismatch _ -> DiagCode.FSharp 1 // ErrorFromAddingTypeEquation
        | Kind.DowncastUnrelated _ -> DiagCode.FSharp 7 // InvalidRuntimeCoercion
        // ── The two "this coercion tells you nothing" warnings are one number in fsc.
        | Kind.RedundantDowncast _
        | Kind.UnrelatedTypeTest _ -> DiagCode.FSharp 67 // TypeTestUnnecessary
        | Kind.IncompleteAnonUnionMatch _ -> DiagCode.FSharp 25 // MatchIncomplete
        // ── Equality / comparison attribute legality.
        | Kind.CustomEqualityOnRecordOrUnion
        | Kind.CapabilityNotImplemented _
        | Kind.CapabilityNotNamed _ -> DiagCode.FSharp 378
        | Kind.MissingGetHashCodeOverride -> DiagCode.FSharp 344
        | Kind.CustomComparisonNeedsEquality -> DiagCode.FSharp 379
        | Kind.StructuralEqualityAttributeOnWrongKind
        | Kind.CustomEqualityAttributeOnInterface -> DiagCode.FSharp 382
        | Kind.InvalidEqualityAttributeMix -> DiagCode.FSharp 377
        | Kind.AllowNullLiteralOnWrongKind -> DiagCode.FSharp 934
        | Kind.MemberAndLocalBindingClash _ -> DiagCode.FSharp 905
        | Kind.DuplicateMember _ -> DiagCode.FSharp 438
        | Kind.CyclicType(via = TypeCycle.Immediate) -> DiagCode.FSharp 954 // tcTypeDefinitionIsCyclicThroughInheritance
        // ── This compiler's own published families.
        | Kind.Conformance(verdict = v) -> ConformanceVerdict.code v
        | Kind.LexFailure _ -> DiagCode.Vesper "LEX"
        | Kind.ParseFailure _ -> DiagCode.Vesper "PARSE"
        | Kind.Driver _ -> DiagCode.Vesper "DRV"
        | Kind.Parse c -> DiagCode.Vesper(DiagnosticCode.code c)
        // ── No published number: fsc has no analogue at all, or its counterpart is a
        // catch-all rather than a classification.
        | Kind.CyclicType(via = TypeCycle.Inheritance)
        // fsc has no analogue: it declines to inline a recursive binding and emits the
        // ordinary function instead, where a cross-file `val inline` here has no such
        // function to fall back to.
        | Kind.CyclicInline _
        | Kind.UnsupportedOnTarget _
        | Kind.OperatorFormQualifiedName _
        | Kind.Internal _
        | Kind.ConstraintNotSupported _
        | Kind.TraitNotSupported _
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
        | Kind.UnresolvedQualifiedName name -> sprintf "Unresolved qualified name: %s" name
        | Kind.OperatorFormQualifiedName firstSegment ->
            sprintf "Operator-form qualified names not yet resolved (starting at '%s')" firstSegment
        | Kind.ConstraintNotSupported(ty, constraintName) ->
            sprintf "The type '%s' does not support the '%s' constraint" ty constraintName
        | Kind.TraitNotSupported(supportTy, noun, name) ->
            sprintf "The type '%s' does not support the %s '%s'" supportTy (MemberNoun.word noun) name
        | Kind.UpcastUnrelated(source, target) ->
            sprintf "Cannot upcast type '%s' to '%s' — no inheritance relationship" source target
        | Kind.DowncastUnrelated(source, target) ->
            sprintf "Cannot downcast type '%s' to unrelated type '%s'" source target
        | Kind.MeasureMismatch(left, right) -> sprintf "Measure mismatch: <%s> vs <%s>" left right
        | Kind.DimensionlessMeasureMismatch measure -> sprintf "Measure mismatch: dimensionless vs <%s>" measure
        | Kind.UndefinedPatternDiscriminator name -> sprintf "The pattern discriminator '%s' is not defined" name
        | Kind.NullaryConstructorPattern(name, arity) ->
            sprintf "Constructor '%s' takes %d argument(s) but is used nullary in pattern position" name arity
        | Kind.AmbiguousConstructor(name, candidates) ->
            sprintf
                "Ambiguous constructor '%s'; declared in %d union types — add a qualifier or annotation"
                name
                candidates
        | Kind.ConstructorArity(name, expected, got) ->
            sprintf "Constructor '%s' expects %d argument(s) but got %d" name expected got
        | Kind.NewRequiresClassType -> "'new' requires a class type"
        | Kind.ImmutableFieldAssignment field -> sprintf "Cannot assign to immutable field '%s'" field
        | Kind.EnumCaseNotConstant ->
            "An enum case value must be a literal integer or string constant, not an expression"
        | Kind.RangeNotFirstClassValue ->
            "a range expression is only supported as the source of a 'for i in a..b do' counted loop; it has no first-class value"
        | Kind.CustomEqualityOnRecordOrUnion ->
            "[<CustomEquality>]/[<CustomComparison>] on a record or union is not supported in this compiler — wrap the type in a class that implements IEquatable<_>/IComparable<_>."
        | Kind.StructuralEqualityAttributeOnWrongKind ->
            "Only record, union, exception and struct types may be augmented with the 'ReferenceEquality', 'StructuralEquality' and 'StructuralComparison' attributes."
        | Kind.CustomEqualityAttributeOnInterface ->
            "The 'CustomEquality' and 'CustomComparison' attributes are not valid on an interface type."
        | Kind.InvalidEqualityAttributeMix ->
            "This type uses an invalid mix of the attributes 'NoEquality', 'ReferenceEquality', 'StructuralEquality', 'NoComparison' and 'StructuralComparison'."
        | Kind.AllowNullLiteralOnWrongKind ->
            "Records, union, abbreviations and struct types cannot have the 'AllowNullLiteral' attribute"
        | Kind.CapabilityNotImplemented(attribute, capability) ->
            sprintf "A type with %s must implement '%s'." attribute capability
        | Kind.CapabilityNotNamed(attribute, capabilityWord) ->
            sprintf
                "A type with %s requires the '%s' capability, which this compilation's provider does not name."
                attribute
                capabilityWord
        | Kind.MissingGetHashCodeOverride -> "A type with [<CustomEquality>] must override 'Object.GetHashCode()'."
        | Kind.CustomComparisonNeedsEquality -> "A type with [<CustomComparison>] must also have [<CustomEquality>]."
        | Kind.MemberAndLocalBindingClash name ->
            sprintf "A member and a local class binding both have the name '%s'" name
        | Kind.DuplicateMember name ->
            sprintf "Duplicate definition of member '%s' — same name and signature as an earlier member" name
        | Kind.CyclicType(name, TypeCycle.Inheritance) -> sprintf "Type '%s' has a cyclic inheritance hierarchy" name
        | Kind.CyclicType(name, TypeCycle.Immediate) ->
            sprintf
                "Type '%s' involves an immediate cyclic reference through a struct field or inheritance relation"
                name
        | Kind.CyclicInline(binding, via) ->
            sprintf
                "The inline binding '%s' expands into itself (%s) — an inline body is spliced at its call site, so a binding that reaches itself has no expansion"
                binding
                (String.concat " → " (binding :: via @ [ binding ]))
        | Kind.NotYetSupported feature -> sprintf "not yet supported: %s" feature
        | Kind.IntrinsicNotInScope intrinsic -> sprintf "%s is not in scope (Vesper.Core missing?)" intrinsic
        | Kind.Internal b -> sprintf "internal compiler error: %s" (InternalBreak.describe b)
        | Kind.DynamicEscape pinnedType ->
            sprintf
                "implicit escape from 'dynamic' to '%s': the compiler cannot verify this member access. Annotate the '?' expression — '(expr : %s)' — to assert the type explicitly."
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
        | Kind.RedundantDowncast ty -> sprintf "Downcast is redundant — the static type '%s' already matches" ty
        | Kind.Conformance(package, verdict) -> sprintf "%s: %s" package (ConformanceVerdict.describe verdict)
        | Kind.LexFailure detail -> sprintf "lex error: %s" detail
        | Kind.ParseFailure detail -> sprintf "parse error: %s" detail
        | Kind.Driver message -> message
        | Kind.Parse c -> DiagnosticCode.message c
        | Kind.Message text -> text

    let severity (k: Kind) : Severity =
        match k with
        | Kind.DynamicEscape _
        | Kind.HeterogeneousEnum _
        | Kind.IncompleteAnonUnionMatch _
        | Kind.UnrelatedTypeTest _
        | Kind.RedundantDowncast _ -> Severity.Warning
        | Kind.UndefinedType _
        | Kind.UnsupportedOnTarget _
        | Kind.NoMember _
        | Kind.NoCase _
        | Kind.UnknownNominalType _
        | Kind.TypeArgArity _
        | Kind.UnresolvedQualifiedName _
        | Kind.OperatorFormQualifiedName _
        | Kind.Internal _
        | Kind.ConstraintNotSupported _
        | Kind.TraitNotSupported _
        | Kind.UpcastUnrelated _
        | Kind.DowncastUnrelated _
        | Kind.MeasureMismatch _
        | Kind.DimensionlessMeasureMismatch _
        | Kind.UndefinedPatternDiscriminator _
        | Kind.NullaryConstructorPattern _
        | Kind.AmbiguousConstructor _
        | Kind.ConstructorArity _
        | Kind.NewRequiresClassType
        | Kind.ImmutableFieldAssignment _
        | Kind.EnumCaseNotConstant
        | Kind.RangeNotFirstClassValue
        | Kind.CustomEqualityOnRecordOrUnion
        | Kind.StructuralEqualityAttributeOnWrongKind
        | Kind.CustomEqualityAttributeOnInterface
        | Kind.InvalidEqualityAttributeMix
        | Kind.AllowNullLiteralOnWrongKind
        | Kind.CapabilityNotImplemented _
        | Kind.CapabilityNotNamed _
        | Kind.MissingGetHashCodeOverride
        | Kind.CustomComparisonNeedsEquality
        | Kind.MemberAndLocalBindingClash _
        | Kind.DuplicateMember _
        | Kind.CyclicType _
        | Kind.CyclicInline _
        | Kind.NotYetSupported _
        | Kind.IntrinsicNotInScope _
        | Kind.Conformance _
        | Kind.LexFailure _
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
