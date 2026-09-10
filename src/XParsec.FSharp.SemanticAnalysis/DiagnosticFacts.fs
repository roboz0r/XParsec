namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser
open Vesper

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

/// Which pair of one union case's fields claims a single logical name. fsc files both under
/// FS3176 with distinct sentences.
[<RequireQualifiedAccess>]
type UnionFieldNameClash =
    /// Two declared names of one case agree: `of a: int * a: float`.
    | Declared
    /// A declared name is the positional spelling of an anonymous field of the same case,
    /// as `Item2` is in `M of Item2: int * float`.
    | AnonymousSpelling

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

/// A `rec` keyword claiming wider recursion than the bindings under it exhibit.
[<RequireQualifiedAccess>]
type RecursionOverstatement =
    /// A `let rec … and …` group whose members split into independent recursion groups.
    /// Each inner list is the names bound by one group, in source order; the outer list is a
    /// valid declaration order for the groups.
    | SplittableGroup of groups: string list list
    /// A `let rec` binding whose value refers to names outside itself only. `names` are those
    /// bound by the binding's pattern.
    | RedundantRec of names: string list
