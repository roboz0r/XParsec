namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// WHAT a diagnostic says, next to `Site.fs`'s WHERE it points. The verdict is a `Kind`
// carrying the facts it is about; the code, the English and the severity are read back OFF
// it. Nothing here is stored on the record, so a producer cannot pair a message with the
// wrong severity, and a consumer can filter on the verdict itself instead of matching
// substrings of a rendered sentence.

// `RequireQualifiedAccess` because this compiles ahead of the VesperLib /
// ReferencedProject extractors, whose `Result` plumbing uses a bare `Error`
// constructor — an unqualified `Severity.Error` case would shadow it. Every use
// site already writes `Severity.Error` / `.Warning` (or is qualified here).
//
// Two cases, because two is what `Kind.severity` can return. An `Info` case was declared
// and never constructed; a severity nothing can mint is not a severity.
[<RequireQualifiedAccess; Struct>]
type Severity =
    | Error
    | Warning

/// The PUBLISHED number a diagnostic is filed under — the identity a consumer suppresses,
/// filters or asserts on across compiler versions, as distinct from the `Kind`, which is the
/// classification itself and always exists.
///
/// A closed set with an explicit `Unpublished`, and NOT a `string`: "this verdict has no
/// published number" is a fact worth stating, where the `""` that used to stand for it was a
/// sentinel a consumer could compare equal to a real code by accident.
[<RequireQualifiedAccess>]
type DiagCode =
    /// An fsc diagnostic number this compiler deliberately reproduces, so a program moved
    /// between the two compilers is refused under the same number. Sourced from the F#
    /// compiler itself (`FSComp.txt`'s numbered entries and
    /// `CompilerDiagnostics.DiagnosticNumber` for the sub-200 exceptions), never invented.
    | FSharp of number: int
    /// This compiler's OWN published families: the `V24x` package-conformance codes, and the
    /// front-end/driver refusals that are about a file rather than about a program.
    | Vesper of code: string
    /// No published number. Not a failure to assign one — most verdicts genuinely have none,
    /// and the `Kind` is the classification a consumer should be selecting on.
    | Unpublished

[<RequireQualifiedAccess>]
module DiagCode =

    /// How a code prints. `Unpublished` renders empty, which is what a diagnostic with no
    /// number has always shown — a DISPLAY choice, made here, rather than a value producers
    /// and consumers pass around.
    let render (c: DiagCode) : string =
        match c with
        | DiagCode.FSharp n -> sprintf "FS%04d" n
        | DiagCode.Vesper code -> code
        | DiagCode.Unpublished -> ""

/// Which NOMINAL shape a type is — the axis a "this names no such type" verdict differs on,
/// and the axis `tryResolveNominal` reports. Declared here rather than in the unification
/// core because a diagnostic names it and the diagnostic types compile first.
[<RequireQualifiedAccess>]
type NominalKind =
    | Record
    | Class
    | Union

/// What a member-lookup verdict was LOOKING for. A discriminator, not a noun a producer
/// spells: the same verdict reached from the field path and from the instance-member path
/// differs only here, and a `string` in this position is a discriminator that has stopped
/// being checked.
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

    /// The noun as a sentence spells it. THE one spelling, so the field path and the
    /// member path cannot drift into two phrasings of one verdict.
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

/// Which sort of type declares the cases a `NoCase` verdict is about — the only axis its
/// producers differ on.
[<RequireQualifiedAccess>]
type CaseOwner =
    | Enum
    | Union

/// How a type's definition reaches itself. `Immediate` is fsc's FS0954 — a struct field or
/// inheritance relation that makes the type contain itself with no indirection; the
/// inheritance walk's own finding has no published code, which is exactly why the code is a
/// function of the verdict and not a field on it.
[<RequireQualifiedAccess>]
type TypeCycle =
    | Inheritance
    | Immediate

/// One package-conformance verdict about one contract `.fsi` (and its companion `.fs`).
/// Its own DU because every such verdict names the package it is about and the seven
/// findings differ only after that: the package rides ONCE on `Kind.Conformance` rather
/// than being repeated as a field on seven near-identical cases.
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
    /// Declared `sig-only`, but it names no contract `.fsi` in the package at all.
    | UnknownSigOnly of name: string
    /// The contract or its companion failed to parse, so that pair could not be conformed.
    | PairParseFailure of sigFile: string * detail: string

[<RequireQualifiedAccess>]
module ConformanceVerdict =

    /// The `V24x` family code. Two findings can share one — `V240` is "the implementation
    /// does not answer the contract", however that came about — which is exactly why the
    /// code is a function of the verdict rather than a field on it.
    let code (v: ConformanceVerdict) : DiagCode =
        match v with
        | ConformanceVerdict.Unimplemented _
        | ConformanceVerdict.SigWithoutImpl _ -> DiagCode.Vesper "V240"
        | ConformanceVerdict.ModulePairingMismatch _ -> DiagCode.Vesper "V241"
        | ConformanceVerdict.ImplWithoutContract _ -> DiagCode.Vesper "V242"
        | ConformanceVerdict.StaleSigOnly _
        | ConformanceVerdict.UnknownSigOnly _ -> DiagCode.Vesper "V243"
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
        | ConformanceVerdict.PairParseFailure(sigFile, detail) ->
            sprintf "the contract '%s' or its implementation failed to parse: %s" sigFile detail

/// A broken invariant INSIDE this compiler. Never a verdict about the program: the source
/// that provoked one may be perfectly correct, and telling its author to fix it is the wrong
/// answer said confidently.
///
/// Its own vocabulary because "your code is wrong", "this compiler does not do that yet"
/// (`Kind.NotYetSupported`) and "this is a compiler bug" are three different answers, and a
/// consumer that cannot tell the third from the first reports a bug as a user error.
///
/// A DIAGNOSTIC rather than a crash, deliberately: a broken invariant reached in one
/// declaration should surface ALONGSIDE the rest of the file's findings instead of replacing
/// them with a stack trace. The invariants that cannot be carried on — where continuing
/// would produce nonsense rather than a partial answer — still `failwith` at their site.
[<RequireQualifiedAccess>]
type InternalBreak =
    /// The TAST reaching the freeze still holds inference metavariables, surfaced per
    /// declaration rather than as a hard failure inside `toFrozen`.
    | UnresolvedTyVars of count: int
    /// Inference committed to a member that resolves in neither the local registry nor the
    /// provider — an `Elaborate` break, named by the resolver that hit it.
    | MemberNotResolvable of resolver: string * declaringType: string * memberName: string
    /// A nested `module` reached a pass that runs on the FLATTENED element list, so
    /// `CstWalk.implFileElems` no longer reaches every module-level construct — typically a
    /// newly added one that slipped past the flattening.
    | UnflattenedModule of pass: string

[<RequireQualifiedAccess>]
module InternalBreak =

    /// The rendered English, prefixed by its reader at `Kind.message` so that every internal
    /// break announces itself as one without each case having to remember to.
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

/// WHAT a diagnostic says. A case carries the facts its sentence is built from, never the
/// sentence — so a consumer can ask "is this an undefined type, and which name?" without
/// parsing English, and a renderer can be replaced without touching a producer.
[<RequireQualifiedAccess>]
type Kind =
    // ── Types and members that do not resolve ──────────────────────────────────
    /// `name` names no type: no scope of this unit claims it and the target's external
    /// universe does not hold it.
    | UndefinedType of name: string
    /// Types with no representation on the compiling target: they exist only as a
    /// .NET/BCL runtime type, so this back end cannot lower them.
    | UnrepresentableTypes of names: string list
    /// The type resolves; the name on it does not. `noun` is what was looked for.
    | NoMember of typeName: string * noun: MemberNoun * memberName: string
    | NoCase of owner: CaseOwner * typeName: string * caseName: string
    /// A nominal shape resolved to a key no registry and no provider answers for.
    | UnknownNominalType of kind: NominalKind * name: string
    | TypeArgArity of name: string * expected: int * got: int
    | UnresolvedQualifiedName of name: string
    | OperatorFormQualifiedName of firstSegment: string
    | ConstraintNotSupported of ty: string * constraintName: string
    /// An inline body's trait call the receiver cannot answer.
    | TraitNotSupported of receiver: string * noun: MemberNoun * name: string

    // ── Casts and type tests. Four verdicts about one pair of types, so they are four
    // cases of one shape rather than four sentences. ───────────────────────────────
    | UpcastUnrelated of source: string * target: string
    | DowncastUnrelated of source: string * target: string

    // ── Units of measure ───────────────────────────────────────────────────────
    | MeasureMismatch of left: string * right: string
    | DimensionlessMeasureMismatch of measure: string

    // ── Constructors and patterns ──────────────────────────────────────────────
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
    /// `attribute` is the posture attribute (`[<CustomEquality>]`); `capability` the
    /// resolved interface it demands, as this compilation's provider names it.
    | CapabilityNotImplemented of attribute: string * capability: string
    /// The same demand, where the provider names no such capability at all — reported
    /// honestly rather than skipped or mis-blamed.
    | CapabilityNotNamed of attribute: string * capabilityWord: string
    | MissingGetHashCodeOverride
    | CustomComparisonNeedsEquality

    // ── Declaration-level clashes and cycles ───────────────────────────────────
    | MemberAndLocalBindingClash of name: string
    | DuplicateMember of name: string
    | CyclicType of name: string * via: TypeCycle
    /// An `inline` binding whose expansion reaches itself. `binding` is the binding the cycle
    /// closes on; `via` the bindings between it and itself in call order, EMPTY for a direct
    /// self-reference — so the pair is non-empty by construction and a renderer never has to
    /// decide what an empty chain would mean.
    ///
    /// The bindings as DATA rather than an arrow-joined sentence, like every other case: a
    /// consumer asking "which binding is recursive?" must not have to parse English.
    | CyclicInline of binding: string * via: string list

    // ── Written, understood, not implemented ───────────────────────────────────
    /// The program is not WRONG — this compiler does not do that yet. Its own verdict
    /// because "fix your code" and "wait for the compiler" are different answers, and a
    /// consumer that cannot tell them apart reports the second as the first.
    | NotYetSupported of feature: string
    /// A lowering needs an intrinsic the compilation cannot see (`Vesper.Core` absent from
    /// the reference set), so the fault is the reference set's, not the source's.
    | IntrinsicNotInScope of intrinsic: string

    // ── Not the program's fault at all ─────────────────────────────────────────
    /// A broken invariant inside this compiler. Beside `NotYetSupported` because they are
    /// the two verdicts that are not about the source; see `InternalBreak` for why it is a
    /// diagnostic rather than a crash.
    | Internal of InternalBreak

    // ── Warnings ───────────────────────────────────────────────────────────────
    /// A `d?foo` whose result type was pinned to something concrete by context: the
    /// `dynamic` default never fired, so the member access is an unchecked assertion.
    | DynamicEscape of pinnedType: string
    | HeterogeneousEnum of name: string
    | IncompleteAnonUnionMatch of unhandled: string list
    | UnrelatedTypeTest of source: string * target: string
    | RedundantDowncast of ty: string

    // ── Whole-unit and whole-package verdicts ──────────────────────────────────
    | Conformance of package: string * verdict: ConformanceVerdict
    | LexFailure of detail: string
    | ParseFailure of detail: string
    /// A refusal by the DRIVER rather than a verdict about the code: a missing target
    /// framework, an unreadable project. Its own case because the driver is its own
    /// producer layer, with its own code, and says things no pass can say.
    | Driver of message: string

    /// A diagnostic the PARSER raised, forwarded whole. The parser owns its own error
    /// vocabulary; this case is the seam, not a copy of it. Forwardable BECAUSE a
    /// `DiagnosticCode` holds no CST node — only tokens, `Site`s and strings — so it crosses
    /// into the frozen format like any other verdict.
    | Parse of DiagnosticCode

    /// The un-migrated tail: a message built at the call site. Its call COUNT is the
    /// migration's progress bar — it shrinks as verdicts are named.
    | Message of text: string

[<RequireQualifiedAccess>]
module Kind =

    /// The published code a consumer filters on. A SEPARATE function from `message`, so
    /// minting a case does not force a code and renaming a case does not change one.
    ///
    /// Every `DiagCode.FSharp` number here is one fsc ITSELF files the same verdict under,
    /// read out of the F# compiler sources rather than guessed: `FSComp.txt`'s numbered
    /// entries, and `CompilerDiagnostics.fs`'s `DiagnosticNumber` for the sub-200 exceptions
    /// (whose numbers live on the exception, not the message). The named exception or
    /// resource each one comes from is in the comment beside it, so the claim is checkable
    /// against that repo instead of taken on trust.
    ///
    /// EXHAUSTIVE rather than defaulted: `Unpublished` is a decision each case states, not
    /// one a new case falls into — and it is the RIGHT answer for most of them. A code is an
    /// extra, published commitment on top of the `Kind`; the `Kind` is the classification,
    /// and a consumer that wants to select on a verdict this compiler owns should select on
    /// that rather than wait for a number to be minted for it.
    let code (k: Kind) : DiagCode =
        match k with
        // ── Names and members that do not resolve. fsc files the whole family under one
        // number (`UndefinedName`), and so do we: the noun differs, the verdict does not.
        | Kind.UndefinedType _
        | Kind.NoMember _
        | Kind.NoCase _
        | Kind.UnknownNominalType _
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
        | Kind.MemberAndLocalBindingClash _ -> DiagCode.FSharp 905
        | Kind.DuplicateMember _ -> DiagCode.FSharp 438
        // fsc's message for 954 literally names "a struct field or inheritance relation",
        // which is this case and not the inheritance WALK's own finding below.
        | Kind.CyclicType(via = TypeCycle.Immediate) -> DiagCode.FSharp 954 // tcTypeDefinitionIsCyclicThroughInheritance
        // ── This compiler's own published families.
        | Kind.Conformance(verdict = v) -> ConformanceVerdict.code v
        | Kind.LexFailure _ -> DiagCode.Vesper "LEX"
        | Kind.ParseFailure _ -> DiagCode.Vesper "PARSE"
        | Kind.Driver _ -> DiagCode.Vesper "DRV"
        // The parser publishes its own vocabulary; forward it rather than renumber it.
        | Kind.Parse c -> DiagCode.Vesper(DiagnosticCode.code c)
        // ── No published number. Several of these are verdicts fsc has no analogue for at
        // all (they are about THIS back end, about a feature it has not grown yet, or about
        // a bug in it); the rest are ones whose fsc counterpart is a catch-all rather than a
        // classification, which is not worth reproducing.
        | Kind.CyclicType(via = TypeCycle.Inheritance)
        // fsc has no analogue: it declines to inline a recursive binding and emits the
        // ordinary function instead, where a cross-unit `val inline` here has no such
        // function to fall back to. A refusal fsc never makes cannot borrow its number.
        | Kind.CyclicInline _
        | Kind.UnrepresentableTypes _
        | Kind.OperatorFormQualifiedName _
        // An internal break is not a verdict about the program, so there is nothing for a
        // user to look up and nothing for fsc to have numbered.
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

    /// The rendered English. Separate from `code` for the same reason, and so a future
    /// localisation or structured renderer replaces ONE function.
    let message (k: Kind) : string =
        match k with
        | Kind.UndefinedType name -> sprintf "The type '%s' is not defined" name
        | Kind.UnrepresentableTypes names ->
            sprintf
                "PlatformTypes: type(s) with no representation on the target platform: %s — they exist only as a .NET/BCL runtime type"
                (String.concat ", " names)
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
        | Kind.TraitNotSupported(receiver, noun, name) ->
            sprintf "The type '%s' does not support the %s '%s'" receiver (MemberNoun.word noun) name
        | Kind.UpcastUnrelated(source, target) ->
            sprintf "Cannot upcast type '%s' to '%s' — no inheritance relationship" source target
        | Kind.DowncastUnrelated(source, target) ->
            sprintf "Cannot downcast type '%s' to unrelated type '%s'" source target
        | Kind.MeasureMismatch(left, right) -> sprintf "Measure mismatch: <%s> vs <%s>" left right
        | Kind.DimensionlessMeasureMismatch measure -> sprintf "Measure mismatch: dimensionless vs <%s>" measure
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
        // The prefix is applied HERE rather than written into each `InternalBreak` case, so
        // no internal break can be phrased as if it were the programmer's mistake.
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

    /// Severity is a FUNCTION of the kind, never a field: the verdict decides, so no
    /// producer is left able to report a warning as an error or the reverse.
    ///
    /// EXHAUSTIVE, with no default. A wildcard here would silently promote a new warning to
    /// an error — and unlike a message, which every case is forced to write, severity is an
    /// independent axis that nothing else would make anyone think about.
    let severity (k: Kind) : Severity =
        match k with
        | Kind.DynamicEscape _
        | Kind.HeterogeneousEnum _
        | Kind.IncompleteAnonUnionMatch _
        | Kind.UnrelatedTypeTest _
        | Kind.RedundantDowncast _ -> Severity.Warning
        | Kind.UndefinedType _
        | Kind.UnrepresentableTypes _
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

/// A SECONDARY place a diagnostic points at, and what it means there. Labelled, because
/// an unlabelled list of extra positions leaves a consumer guessing from list order what
/// each one meant. The unclosed-delimiter diagnostic is the shape: primary at the hole
/// where the delimiter belonged, one label back on the delimiter left open.
type Label = { Site: Site; Message: string }

/// A REFERENCE type carrying a DU, a list and three derived members, and that is the right
/// shape: a diagnostic is allocated only on an error path, so a compilation that SUCCEEDS
/// allocates none at all and a failing one allocates a handful next to millions of nodes.
/// Nothing here is worth packing — not this record, and not the `Site` it carries — and a
/// packed layout would cost the property that makes both readable: that every case says
/// what it is.
///
/// `Code`, `Message` and `Severity` are members, not fields: each is a function of `Kind`,
/// so a producer answers ONE question and the three derived facts cannot disagree with it.
///
/// NAME COLLISION: `XParsec.FSharp.Parser` declares its own `Diagnostic` (the parser's
/// recovery record), so in any file that `open`s the parser the bare name binds to THAT
/// one. Such files alias or fully qualify this type; the alias is what the `type
/// Diagnostic = …` lines elsewhere in this assembly are for.
type Diagnostic =
    {
        /// The verdict, and the facts it is about.
        Kind: Kind
        /// The primary position — what a one-line renderer points at.
        Site: Site
        /// Secondary positions, in the order a renderer should show them.
        Related: Label list
    }

    member this.Code: DiagCode = Kind.code this.Kind
    member this.Message: string = Kind.message this.Kind
    member this.Severity: Severity = Kind.severity this.Kind

[<RequireQualifiedAccess>]
module Diagnostic =

    /// THE one `Diagnostic` literal. Every producer reaches the record through here (or
    /// through a `PassContext` member that does), so a field added to it is answered once.
    let create (kind: Kind) (site: Site) (related: Label list) : Diagnostic =
        {
            Kind = kind
            Site = site
            Related = related
        }

    /// A verdict about a whole unit or package rather than about a place in one: a lex or
    /// parse failure, a driver refusal, a conformance finding about a signature.
    let nowhere (kind: Kind) : Diagnostic = create kind Site.Nowhere []

    /// Does this diagnostic BLOCK? THE one spelling of the question every emission gate
    /// asks, so no two gates can admit different severities.
    let isError (d: Diagnostic) : bool = d.Severity = Severity.Error

    /// The blocking subset, in order.
    let errors (ds: Diagnostic seq) : Diagnostic list = ds |> Seq.filter isError |> List.ofSeq
