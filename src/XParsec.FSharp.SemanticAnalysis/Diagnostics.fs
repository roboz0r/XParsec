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
    /// A PARSER-owned code (`DiagnosticCode`), forwarded verbatim by `Kind.Parse`.
    | Parse of code: string
    /// No published number, which is the case for most verdicts.
    | Unpublished

[<RequireQualifiedAccess>]
module DiagCode =

    /// How a code prints; `Unpublished` renders empty.
    let render (c: DiagCode) : string =
        match c with
        | DiagCode.FSharp n -> sprintf "FS%04d" n
        | DiagCode.Vesper code -> code
        | DiagCode.Parse code -> code
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
        /// An `interface … with` implementation needs a real type to carry the interface
        /// slots, which an intrinsic host does not have.
        | InterfaceImpl

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
            | Construct.InterfaceImpl -> "an 'interface … with' implementation"

        sprintf
            "Intrinsic type '%s' cannot declare %s: the type carries no representation in the output, so only a spliced 'member inline' is admissible on it"
            hostName
            what

/// Which sort of type declares the cases a `NoCase` verdict is about.
[<RequireQualifiedAccess>]
type CaseOwner =
    | Enum
    | Union

/// Which relation carries a type's definition back to itself with no indirection. fsc splits
/// the three across two numbers: `Abbreviation` is FS0953, the other two share FS0954.
[<RequireQualifiedAccess>]
type TypeCycle =
    /// A self- or mutual-`inherit` chain.
    | Inheritance
    /// A struct storing a field of a type that stores one of it. The same pair declared as
    /// reference types has a finite layout and compiles.
    | StructField
    /// An abbreviation whose right-hand side expands back to it.
    | Abbreviation

/// One conformance verdict about one `.fsi` and its companion `.fs`, the two of which sit
/// beside each other in one assembly. A verdict requires both halves paired and parsed: the
/// manifest read refuses an unpaired `.fsi`, and a parse failure reports `Kind.ParseFailure`.
[<RequireQualifiedAccess>]
type ConformanceVerdict =
    /// A binding the contract declares that the implementation does not satisfy.
    | Unimplemented of sigFile: string * detail: string
    /// The paired files' leading module / namespace declarations disagree.
    | ModulePairingMismatch of sigFile: string * implFile: string * sigDecl: string * implDecl: string
    /// A declaration the signature makes that the front end could not MODEL, so the signature
    /// publishes LESS than it says: the declaration is absent for everything that reads it.
    /// Anchored at the signature that made the claim.
    | SignatureNotPublished of detail: string
    /// A declaration the signature is not ALLOWED to make (a non-`inline` member on an
    /// `extern` type). A rule violation rather than a gap in what this compiler models.
    | SignatureRejected of detail: string
    /// Both halves write one attribute with differing arguments. The signature's copy is what
    /// ships, so the implementation's arguments are discarded.
    | AttributeArgumentsDiffer of sigFile: string * divergence: Conformance.AttributeDivergence

[<RequireQualifiedAccess>]
module ConformanceVerdict =

    /// The `V24x` family code. `V240` is "the implementation does not satisfy the contract",
    /// however that came about.
    let code (v: ConformanceVerdict) : DiagCode =
        match v with
        | ConformanceVerdict.Unimplemented _ -> DiagCode.Vesper "V240"
        | ConformanceVerdict.ModulePairingMismatch _ -> DiagCode.Vesper "V241"
        | ConformanceVerdict.SignatureNotPublished _ -> DiagCode.Vesper "V245"
        | ConformanceVerdict.SignatureRejected _ -> DiagCode.Vesper "V246"
        | ConformanceVerdict.AttributeArgumentsDiffer _ -> DiagCode.Vesper "V247"

    let describe (v: ConformanceVerdict) : string =
        match v with
        | ConformanceVerdict.Unimplemented(sigFile, detail) -> sprintf "%s: %s" sigFile detail
        | ConformanceVerdict.ModulePairingMismatch(sigFile, implFile, sigDecl, implDecl) ->
            sprintf
                "%s ↔ %s: the paired files' leading module/namespace declarations disagree ('%s' vs '%s')"
                sigFile
                implFile
                sigDecl
                implDecl
        | ConformanceVerdict.SignatureNotPublished detail ->
            sprintf "the signature declares something this compiler cannot publish, so it is hidden: %s" detail
        | ConformanceVerdict.SignatureRejected detail -> sprintf "the signature declares %s" detail
        | ConformanceVerdict.AttributeArgumentsDiffer(sigFile, divergence) ->
            sprintf "%s: %s" sigFile (Conformance.describeDivergence divergence)

/// A fault in the PACKAGE SET a compilation was handed, rather than in any one file's text:
/// a manifest listing a path it has not got, a `depends-on` that does not resolve, a type two
/// referenced packages both declare.
[<RequireQualifiedAccess>]
type PackageSetFault =
    /// A `[core]` list references a path that is not on disk.
    | FileMissing of package: string * relative: string
    /// A `manifest.<target>.toml` that does not read as a manifest: TOML that does not parse,
    /// a missing or unknown `[core]` key, a `name` diverging from the directory name.
    | MalformedManifest of path: string * detail: string
    /// A package directory has no `manifest.<target>.toml` for the target compiled.
    | NoManifestForTarget of packageDir: string * target: string
    /// A `depends-on` entry that resolves to nothing, or a cycle in the closure.
    | UnresolvedDependency of detail: string
    /// One qualified type name declared by two packages of the referenced set: the CS0433
    /// equivalent.
    | DuplicateType of typeName: string * first: string * second: string

[<RequireQualifiedAccess>]
module PackageSetFault =

    let code (f: PackageSetFault) : DiagCode =
        match f with
        | PackageSetFault.FileMissing _ -> DiagCode.Vesper "V250"
        | PackageSetFault.MalformedManifest _
        | PackageSetFault.NoManifestForTarget _
        | PackageSetFault.UnresolvedDependency _ -> DiagCode.Vesper "V251"
        | PackageSetFault.DuplicateType _ -> DiagCode.Vesper "V252"

    let describe (f: PackageSetFault) : string =
        match f with
        | PackageSetFault.FileMissing(package, relative) ->
            sprintf "package '%s' names '%s', which is not on disk" package relative
        | PackageSetFault.MalformedManifest(path, detail) -> sprintf "%s: %s" path detail
        | PackageSetFault.NoManifestForTarget(packageDir, target) ->
            sprintf "package '%s' does not build for target `%s`: no manifest.%s.toml" packageDir target target
        | PackageSetFault.UnresolvedDependency detail ->
            sprintf "the referenced package set does not resolve: %s" detail
        | PackageSetFault.DuplicateType(typeName, first, second) when first = second ->
            sprintf
                "the type '%s' is declared twice by package '%s', because the referenced set contains two copies (or versions) of it. Reference the package once."
                typeName
                first
        | PackageSetFault.DuplicateType(typeName, first, second) ->
            sprintf
                "the type '%s' exists in both '%s' and '%s'. A referenced package set must declare each type once; reference only one of the two packages."
                typeName
                first
                second

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
    /// A static access on a GENERIC declaring type reached Elaborate with no
    /// `StaticDeclaringArgs` stamp.
    | UnstampedStaticDeclArgs of declaringType: string

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
        | InternalBreak.UnstampedStaticDeclArgs declaringType ->
            sprintf
                "a static access on generic type '%s' carries no declaring instantiation; inference did not stamp this site"
                declaringType

/// A construct with no syntax of its own that a lowering resolves out of a referenced
/// package: `[…]` needs a cons-list type, `x?n` needs an `op_Dynamic` to call.
[<RequireQualifiedAccess>]
type Intrinsic =
    /// The cons-list type `[…]`, `h :: t` and `for … in` build.
    | ConsList
    /// `x?name`.
    | DynamicGet
    /// `x?name <- value`.
    | DynamicSet
    /// `x.[i]` against an index signature.
    | GetIndex

[<RequireQualifiedAccess>]
module Intrinsic =

    /// The package supplying it, spelled as a manifest's `depends-on` spells it, because the
    /// diagnostic tells the author to add exactly that line.
    let package (i: Intrinsic) : string =
        match i with
        | Intrinsic.ConsList -> RuntimeNames.listPackageName
        | Intrinsic.DynamicGet
        | Intrinsic.DynamicSet
        | Intrinsic.GetIndex -> RuntimeNames.corePackageName

    /// The noun phrase referring to it in a sentence, subject-position.
    let describe (i: Intrinsic) : string =
        match i with
        | Intrinsic.ConsList -> "the cons-list type a '[…]' literal builds"
        | Intrinsic.DynamicGet -> "the dynamic-access operator '?' (op_Dynamic)"
        | Intrinsic.DynamicSet -> "the dynamic-set operator '?<-' (op_DynamicAssignment)"
        | Intrinsic.GetIndex -> "the index-signature intrinsic 'GetIndex'"

/// `System.AttributeTargets` flag values (ECMA-335 §II.23.1.1; `Vesper.AttributeTargets`
/// declares the same numbers), plus fsc's wording for a flag set.
[<RequireQualifiedAccess>]
module AttributeTargetFlags =

    [<Literal>]
    let Assembly = 1

    [<Literal>]
    let Module = 2

    [<Literal>]
    let Class = 4

    [<Literal>]
    let Struct = 8

    [<Literal>]
    let Enum = 16

    [<Literal>]
    let Constructor = 32

    [<Literal>]
    let Method = 64

    [<Literal>]
    let Property = 128

    [<Literal>]
    let Field = 256

    [<Literal>]
    let Event = 512

    [<Literal>]
    let Interface = 1024

    [<Literal>]
    let Parameter = 2048

    [<Literal>]
    let Delegate = 4096

    [<Literal>]
    let ReturnValue = 8192

    [<Literal>]
    let GenericParameter = 16384

    [<Literal>]
    let All = 32767

    /// fsc's rendering of a flag set (FS0842): lowercase words in ascending flag order,
    /// comma-separated — `"property, field, return value"`.
    let words (mask: int) : string =
        [
            Assembly, "assembly"
            Module, "module"
            Class, "class"
            Struct, "struct"
            Enum, "enum"
            Constructor, "constructor"
            Method, "method"
            Property, "property"
            Field, "field"
            Event, "event"
            Interface, "interface"
            Parameter, "parameter"
            Delegate, "delegate"
            ReturnValue, "return value"
            GenericParameter, "generic parameter"
        ]
        |> List.filter (fun (flag, _) -> mask &&& flag <> 0)
        |> List.map snd
        |> String.concat ", "

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
    /// A trait call in an inline body that no type in the support set satisfies.
    | TraitNotSupported of supportTys: EqArray<string> * noun: MemberNoun * name: string
    /// A trait call in an inline body that more than one type in the support set satisfies.
    | TraitAmbiguous of supportTys: EqArray<string> * noun: MemberNoun * name: string

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
    | CyclicType of name: string * via: TypeCycle
    /// An `inline` binding whose expansion reaches itself. `via` is the bindings between
    /// `binding` and itself in call order, EMPTY for a direct self-reference.
    | CyclicInline of binding: string * via: string list

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
        | Kind.RequireQualifiedAccessCase _ -> DiagCode.FSharp 35 // Deprecated
        // ── Constructors: fsc's "union case expects N arguments" covers both the wrong
        // count and the nullary-in-pattern-position case.
        | Kind.ConstructorArity _
        | Kind.NullaryConstructorPattern _ -> DiagCode.FSharp 19 // UnionCaseWrongArguments
        | Kind.ImmutableFieldAssignment _ -> DiagCode.FSharp 5 // FieldNotMutable
        | Kind.EnumCaseNotConstant -> DiagCode.FSharp 886 // tcInvalidEnumerationLiteral
        // ── String-literal escapes: fsc's numbers, though fsc files 1252 as a warning
        // (wrapping the value) where this compiler refuses.
        | Kind.EscapeTrigraphOutOfRange _ -> DiagCode.FSharp 1252
        | Kind.EscapeNotUnicodeScalar _ -> DiagCode.FSharp 1245
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
        | Kind.CapabilityNotDeclared _ -> DiagCode.FSharp 378
        | Kind.MissingGetHashCodeOverride -> DiagCode.FSharp 344
        | Kind.CustomComparisonNeedsEquality -> DiagCode.FSharp 379
        | Kind.StructuralEqualityAttributeOnWrongKind
        | Kind.CustomEqualityAttributeOnInterface -> DiagCode.FSharp 382
        | Kind.InvalidEqualityAttributeMix -> DiagCode.FSharp 377
        | Kind.AllowNullLiteralOnWrongKind -> DiagCode.FSharp 934
        | Kind.ReferenceEqualityOnStruct -> DiagCode.FSharp 376
        | Kind.NotConstantExpression -> DiagCode.FSharp 267 // tcInvalidConstantExpression
        // fsc files this as a WARNING; here the mismatch is an error.
        | Kind.AttributeTargetInvalid _ -> DiagCode.FSharp 842 // tcAttributeIsNotValidForLanguageElement
        | Kind.MemberAndLocalBindingClash _ -> DiagCode.FSharp 905
        | Kind.DuplicateMember _ -> DiagCode.FSharp 438
        | Kind.CyclicType(via = TypeCycle.Abbreviation) -> DiagCode.FSharp 953 // tcTypeDefinitionIsCyclic
        // tcTypeDefinitionIsCyclicThroughInheritance, which fsc files BOTH remaining relations
        // under: a self- or mutual-`inherit` cycle and a struct-field cycle all report 954.
        | Kind.CyclicType _ -> DiagCode.FSharp 954
        // ── This compiler's own published families.
        | Kind.Conformance(verdict = v) -> ConformanceVerdict.code v
        | Kind.ConformanceFinding _ -> DiagCode.Vesper "V240"
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
        | Kind.TraitNotSupported(supportTys, noun, name) ->
            match supportTys.Length with
            | 1 -> sprintf "The type '%s' does not support the %s '%s'" supportTys.[0] (MemberNoun.word noun) name
            | _ ->
                sprintf
                    "Neither type '%s' supports the %s '%s'"
                    (String.concat "' nor '" (EqArray.toArray supportTys))
                    (MemberNoun.word noun)
                    name
        | Kind.TraitAmbiguous(supportTys, noun, name) ->
            sprintf
                "The types '%s' each support the %s '%s', so the call is ambiguous"
                (String.concat "' and '" (EqArray.toArray supportTys))
                (MemberNoun.word noun)
                name
        | Kind.UpcastUnrelated(source, target) ->
            sprintf "Cannot upcast type '%s' to '%s', because neither inherits the other" source target
        | Kind.DowncastUnrelated(source, target) ->
            sprintf "Cannot downcast type '%s' to unrelated type '%s'" source target
        | Kind.MeasureMismatch(left, right) -> sprintf "Measure mismatch: <%s> vs <%s>" left right
        | Kind.DimensionlessMeasureMismatch measure -> sprintf "Measure mismatch: dimensionless vs <%s>" measure
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
        | Kind.RedundantDowncast _ -> Severity.Warning
        | Kind.UndefinedType _
        | Kind.UnsupportedOnTarget _
        | Kind.NoMember _
        | Kind.NoCase _
        | Kind.UnknownNominalType _
        | Kind.TypeArgArity _
        | Kind.UnresolvedQualifiedName _
        | Kind.AbbreviatedNamespace _
        | Kind.RequireQualifiedAccessModule _
        | Kind.DuplicateModule _
        | Kind.OperatorFormQualifiedName _
        | Kind.Internal _
        | Kind.ConstraintNotSupported _
        | Kind.TraitNotSupported _
        | Kind.TraitAmbiguous _
        | Kind.UpcastUnrelated _
        | Kind.DowncastUnrelated _
        | Kind.MeasureMismatch _
        | Kind.DimensionlessMeasureMismatch _
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
        | Kind.AttributeTargetInvalid _
        | Kind.CapabilityNotImplemented _
        | Kind.CapabilityNotDeclared _
        | Kind.MissingGetHashCodeOverride
        | Kind.CustomComparisonNeedsEquality
        | Kind.MemberAndLocalBindingClash _
        | Kind.DuplicateMember _
        | Kind.CyclicType _
        | Kind.CyclicInline _
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
