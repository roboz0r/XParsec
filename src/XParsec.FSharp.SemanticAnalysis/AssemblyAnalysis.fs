namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles

// An assembly is a LINEAR composition of per-file provider views, ahead of the external
// (package/BCL) provider. A `.fsi` replaces its file's signatures and leaves the bodies
// alone: a declaration it withholds still splices as a `member inline` into later files.

module AssemblyAnalysis =

    type Diagnostic = XParsec.FSharp.SemanticAnalysis.Diagnostic

    /// The namespaces a file DECLARES. F# implicitly opens a file's own `namespace N` over its
    /// body, which is how a PRIOR file's namespace-direct declarations resolve, including
    /// through provider-layer probes whose visibility stops at the external scope.
    let private declaredNamespaces (lexed: Lexed) (file: ImplementationFile<SyntaxToken>) : string list =
        match file with
        | ImplementationFile.Namespaces groups ->
            [
                for g in groups do
                    match g with
                    | NamespaceDeclGroup.Named(longIdent = li) ->
                        let path = li.Idents |> Seq.map (SyntaxToken.nameIn lexed) |> String.concat "."

                        if path.Length > 0 then
                            yield path
                    | NamespaceDeclGroup.Global _ -> ()
            ]
            |> List.distinct
        | _ -> []


    /// The attribute sets on top-level `do` and `let` elements of an implementation file, the
    /// only elements F# accepts an assembly-targeted attribute on.
    let private topLevelAttributes (file: ImplementationFile<SyntaxToken>) : Attributes<SyntaxToken> list =
        let elems (es: ModuleElems<SyntaxToken>) =
            [
                for e in es do
                    match e with
                    | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Do(attributes = ValueSome attrs))
                    | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(attributes = ValueSome attrs)) -> attrs
                    | _ -> ()
            ]

        match file with
        | ImplementationFile.Namespaces groups ->
            [
                for g in groups do
                    match g with
                    | NamespaceDeclGroup.Named(elements = es)
                    | NamespaceDeclGroup.Global(elements = es) -> yield! elems es
            ]
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = es)) -> elems es
        | ImplementationFile.AnonymousModule es -> elems es

    /// The namespaces `[<assembly: AutoOpen("…")>]` opens over every file compiled against this
    /// assembly, its own included. Recognition is on the WRITTEN attribute name under F#'s
    /// `Attribute`-suffix rule, and the path is taken as written.
    let assemblyAutoOpens (lexed: Lexed) (file: ImplementationFile<SyntaxToken>) : ImplicitOpen list =
        let nameOf = SyntaxToken.nameIn lexed

        [
            for sets in topLevelAttributes file do
                for AttributeSet(attributes = entries) in sets do
                    for (Attribute(target = target; construction = construction), _) in entries do
                        match target with
                        | ValueSome(AttributeTarget.Assembly _, _) ->
                            match AttributeDecode.writtenTypeRef construction with
                            | ValueSome typeRef when AttributeDecode.isWrittenAutoOpen nameOf typeRef.LongIdent ->
                                match AttributeDecode.tryStringArgument nameOf construction with
                                | ValueSome path when path.Length > 0 -> SymbolKeyOps.assemblyAutoOpen path
                                | _ -> ()
                            | _ -> ()
                        | _ -> ()
        ]
        |> List.distinct

    /// Which side of the assembly boundary the analysis publishes for.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type Publication =
        /// Compiling these units: each file homes in itself, the frozen `.fs` is kept for
        /// codegen, and a `.fsi` half is held to the conformance rules.
        | InAssembly
        /// Reading them as a reference: the published surfaces alone cross, and the caller
        /// stamps every symbol's home with the assembly. `bodyExternal` maps the intrinsic
        /// axis published so far to the platform metadata a BODY analyses over.
        | AcrossAssemblies of bodyExternal: (IntrinsicTypeMap -> IExternalSymbolProvider)

    /// The assembly's OWN `[<assembly: AutoOpen("…")>]` namespaces as a source, carrying no
    /// symbols. The FLOOR of the visibility stack: a file resolves its own assembly's prelude
    /// whether or not the declaring file has been analysed yet.
    let private ownAutoOpens (autoOpens: ImplicitOpen list) : IExternalSymbolProvider =
        ExternalSymbolProviders.stack ValueNone autoOpens []

    /// What a unit of an assembly resolves through: the views `published` so far, NEAREST
    /// first, above the reference floor `external` and the assembly's own prelude.
    let visibility
        (autoOpens: ImplicitOpen list)
        (external: IExternalSymbolProvider)
        (published: IExternalSymbolProvider list)
        : IExternalSymbolProvider =
        ExternalSymbolProviders.composite (published @ [ external; ownAutoOpens autoOpens ])

    /// Short name ⇒ platform type id, read off the unit's own implementation tree, so the
    /// `.fsi`'s `type t = extern` picks `Bound` over `Unsupported`.
    let private implementationBindings
        (lexed: Lexed)
        (file: ImplementationFile<SyntaxToken>)
        : Dictionary<string, PlatformTypeId> =
        let bindings = Dictionary<string, PlatformTypeId>(System.StringComparer.Ordinal)
        IntrinsicBindings.ofImplementationInto bindings (SyntaxToken.nameIn lexed) file
        bindings

    /// One analysed unit of an assembly: the frozen file, the surface it publishes across the
    /// assembly boundary — its `.fsi`'s when it has one, else the one its implementation
    /// infers — and its splice templates.
    [<NoEquality; NoComparison>]
    type AnalysedUnit =
        {
            File: FrozenFile
            Surface: PublishedSurface
            Bodies: InlineBodies.FileInlineBodies
            /// The provider this unit pushed onto the visibility stack: later units resolve
            /// through it, and a package build composes its outward provider from it.
            Published: IExternalSymbolProvider
            /// The diagnostics this unit surfaces to the caller, anchored in their
            /// own files. When compiling, everything both halves reported; when referencing,
            /// the `.fsi`'s findings, or for a `.fs` without one its analysis ERRORS.
            Surfaced: AnchoredDiagnostic list
        }

    /// A signature file resolved against the units before it, and its surface published.
    [<NoEquality; NoComparison>]
    type ResolvedSignature =
        {
            /// The parsed half the surface was resolved from; its `Parsed.Diagnostics` are
            /// recovery's findings.
            Signature: ParsedFile<ParseChain.ParsedSignature>
            Retained: LexedFile
            Surface: PublishedSurface
            /// What resolving the signature found.
            Diagnostics: Diagnostic list
            /// `Surface` wrapped as a provider.
            Published: IExternalSymbolProvider
        }

    /// What analysing one unit produced.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type UnitOutcome =
        | Analysed of AnalysedUnit
        /// Every half of the unit that yielded no tree, the implementation's leading.
        | Failed of leading: UnparsedFile * rest: UnparsedFile list

    [<RequireQualifiedAccess>]
    module UnitOutcome =

        /// The diagnostics a unit surfaces to its consumer, anchored in their own files.
        let surfaced (unit: UnitOutcome) : AnchoredDiagnostic list =
            match unit with
            | UnitOutcome.Analysed u -> u.Surfaced
            | UnitOutcome.Failed(leading, rest) -> List.collect failureDiagnostics (leading :: rest)

    /// A unit list analysed in manifest order.
    [<NoEquality; NoComparison>]
    type AnalysedUnits =
        {
            Units: UnitOutcome list
            /// Each analysed unit's published view, NEAREST first: the last unit's at the
            /// head. Composing the whole list gives the assembly-wide domain; a unit itself
            /// resolved only the views following its own.
            Published: IExternalSymbolProvider list
            /// The namespaces this assembly's `[<assembly: AutoOpen("…")>]` attributes list:
            /// its files were analysed under them and a consumer inherits them. Every entry is
            /// an `ImplicitOpen.AssemblyAutoOpen`.
            AutoOpens: ImplicitOpen list
        }

    /// One implementation file analysed and frozen against `composed`, with its splice
    /// templates collected.
    [<NoEquality; NoComparison>]
    type private ImplAnalysis =
        {
            Retained: LexedFile
            Frozen: FrozenPools
            Imports: ImportObligation list
            Scoped: IExternalSymbolProvider
            Bodies: InlineBodies.FileInlineBodies
        }

    let private analyseImplementation
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (composed: IExternalSymbolProvider)
        (implementation: ParsedFile<ParseChain.ParsedImplementation>)
        : ImplAnalysis =
        let parsed = implementation.Parsed

        // This file's own `namespace N` ahead of whatever prelude the external surface
        // carries, so a bare `bool` finds the `N.bool` an earlier file of the SAME package
        // declared.
        let scoped =
            match declaredNamespaces parsed.Lexed parsed.Tree with
            | [] -> composed
            | ns ->
                ExternalSymbolProviders.stack
                    ValueNone
                    ([ for n in ns -> SymbolKeyOps.currentFileScope n ] @ composed.ImplicitOpens
                     |> List.distinct)
                    [ composed ]

        let retained = LexedFile.inAssembly assembly.Name implementation.Id parsed.Lexed

        let frozen, imports = analyse assembly scoped retained parsed.Tree

        {
            Retained = retained
            Frozen = frozen
            Imports = imports
            Scoped = scoped
            // The templates key off `SymbolKey` alone, so a `.fsi` replacing the file's
            // signatures leaves every one of them reachable.
            Bodies = InlineBodies.collect retained frozen
        }

    let private resolveSignatureFile
        (assembly: CompilingAssembly)
        (composed: IExternalSymbolProvider)
        (bindings: Dictionary<string, PlatformTypeId>)
        (signature: ParsedFile<ParseChain.ParsedSignature>)
        : LexedFile * PublishedSurface * Diagnostic list =
        // Anchored to the signature's OWN token stream: its diagnostics index that text.
        let retained =
            LexedFile.inAssembly assembly.Name signature.Id signature.Parsed.Lexed

        let surface, diagnostics =
            SignatureResolution.resolveFile
                composed
                retained
                {
                    Assembly = assembly.Name
                    Target = assembly.Target
                    Bindings = bindings
                }
                signature.Parsed.Tree

        retained, surface, diagnostics

    /// A signature's provider as the LATER files of its assembly resolve it, and the verdict on
    /// the pair that produced it. An empty `Conformance` means the two halves conform.
    [<NoEquality; NoComparison>]
    type private ConformedSignature =
        {
            /// The signature's surface, homed in the implementation file.
            Published: IExternalSymbolProvider
            Conformance: Diagnostic list
        }

    /// Home a resolved signature in its implementation and check the pair over the two ANALYSED
    /// halves: type and value presence, the `extern` ↔ repr pairing, and typar ORDER, each by
    /// resolved identity. The module-decl pairing alone stays SYNTACTIC.
    ///
    /// This is the only site that homes a signature for an assembly it is compiled in, so the
    /// verdict is taken on every pair that reaches a later file's scope.
    let private conformSignature
        (assembly: AssemblyName)
        (implementation: ParsedFile<ParseChain.ParsedImplementation>)
        (impl: ImplAnalysis)
        (r: ResolvedSignature)
        : ConformedSignature =
        // Homed in the IMPLEMENTATION file, so a later file of the same assembly resolves the
        // signature's symbols as locals.
        let published =
            ExternalSymbolProviders.stack (ValueSome(SymbolHome.InFile impl.Retained.Path)) [] [ r.Published ]

        let verdict (v: ConformanceVerdict) =
            Diagnostic.nowhere (Kind.Conformance(assembly.Name, v))

        let unimplemented (detail: string) =
            verdict (ConformanceVerdict.Unimplemented(r.Signature.Id.Name, detail))

        let sigPath =
            Conformance.sigDeclPath r.Signature.Parsed.Lexed r.Signature.Parsed.Tree

        let implPath =
            Conformance.implDeclPath implementation.Parsed.Lexed implementation.Parsed.Tree

        {
            Published = published
            Conformance =
                [
                    if sigPath <> implPath then
                        yield
                            verdict (
                                ConformanceVerdict.ModulePairingMismatch(
                                    r.Signature.Id.Name,
                                    implementation.Id.Name,
                                    sigPath,
                                    implPath
                                )
                            )

                    for e in ConformanceSurface.checkTypes r.Surface impl.Frozen do
                        yield unimplemented (Conformance.describe e)

                    let values = ConformanceSurface.checkValues r.Surface impl.Frozen

                    for e in values.Errors do
                        yield unimplemented (Conformance.describe e)

                    for d in values.Divergent do
                        yield verdict (ConformanceVerdict.AttributeArgumentsDiffer(r.Signature.Id.Name, d))

                    for m in ConformanceTypars.checkFile published impl.Frozen do
                        yield unimplemented (ConformanceTypars.describe m)

                    for m in ConformanceTypars.checkMembers published impl.Frozen do
                        yield unimplemented (ConformanceTypars.describeMember m)
                ]
        }

    /// Analyse a unit list in manifest order over `external`, with the language prelude at the
    /// visibility floor. Every implementation is analysed ONCE, one with no `.fsi` publishes
    /// the surface it infers, and each unit resolves only the units BEFORE it, nearest first.
    let analyseUnits
        (analyse: AnalyseFile)
        (assembly: CompilingAssembly)
        (external: IExternalSymbolProvider)
        (publication: Publication)
        (units: AssemblyUnit list)
        : AnalysedUnits =
        // Read off every unit ahead of the fold: an `[<assembly: AutoOpen>]` is a whole-
        // assembly fact, so it holds over the files written before the one declaring it.
        let autoOpens =
            [
                for unit in units do
                    match unit with
                    | AssemblyUnit.Faulted _ -> ()
                    | AssemblyUnit.Analysable parsedUnit ->
                        let parsed = parsedUnit.Implementation.Parsed
                        yield! assemblyAutoOpens parsed.Lexed parsed.Tree
            ]
            |> List.distinct

        // The units' published views so far, NEAREST first.
        let mutable own: IExternalSymbolProvider list = []

        let signatureFloor () = visibility autoOpens external own

        let resolveSignature
            (bindings: Dictionary<string, PlatformTypeId>)
            (signature: ParsedFile<ParseChain.ParsedSignature>)
            =
            let retained, surface, diagnostics =
                resolveSignatureFile assembly (signatureFloor ()) bindings signature

            {
                Signature = signature
                Retained = retained
                Surface = surface
                Diagnostics = diagnostics
                Published = PublishedSurface.toProvider surface
            }

        // What a BODY analyses over: the signature floor when compiling; when referencing,
        // platform metadata re-seeded with the intrinsics published so far PLUS the unit's
        // own, which BCL member canonicalisation reads.
        let bodyProvider (sigPublished: IExternalSymbolProvider voption) : IExternalSymbolProvider =
            match publication with
            | Publication.InAssembly -> signatureFloor ()
            | Publication.AcrossAssemblies bodyExternal ->
                let axis =
                    ExternalSymbolProviders.mergeIntrinsics (
                        match sigPublished with
                        | ValueSome p -> p :: own
                        | ValueNone -> own
                    )

                visibility autoOpens (bodyExternal axis) own

        let outcomes =
            [
                for unit in units ->
                    match unit with
                    | AssemblyUnit.Faulted(leading, rest) -> UnitOutcome.Failed(leading, rest)
                    | AssemblyUnit.Analysable parsedUnit ->
                        // The unit's `.fsi` resolves against the units BEFORE it, and is not
                        // pushed until the body has analysed: neither half sees the other's
                        // names.
                        let resolvedSignature =
                            parsedUnit.Signature
                            |> ValueOption.map (
                                resolveSignature (
                                    implementationBindings
                                        parsedUnit.Implementation.Parsed.Lexed
                                        parsedUnit.Implementation.Parsed.Tree
                                )
                            )

                        let impl =
                            analyseImplementation
                                analyse
                                assembly
                                (bodyProvider (resolvedSignature |> ValueOption.map (fun r -> r.Published)))
                                parsedUnit.Implementation

                        let surface, published, signatureFile =
                            match resolvedSignature with
                            | ValueSome r ->
                                let published, conformance =
                                    match publication with
                                    | Publication.InAssembly ->
                                        let conformed = conformSignature assembly.Name parsedUnit.Implementation impl r

                                        conformed.Published, conformed.Conformance
                                    // Published bare, the caller stamping the assembly home
                                    // once. A reference was conformed when its own assembly was
                                    // compiled.
                                    | Publication.AcrossAssemblies _ -> r.Published, []

                                r.Surface,
                                published,
                                ValueSome
                                    {
                                        Retained = r.Retained
                                        ParseDiagnostics = r.Signature.Parsed.Diagnostics
                                        Diagnostics = r.Diagnostics @ conformance
                                    }
                            | ValueNone ->
                                let surface = FrozenSignature.toSurface impl.Retained impl.Frozen
                                surface, PublishedSurface.toProvider surface, ValueNone

                        let view =
                            ExternalSymbolProviders.withInlineBodies (InlineBodies.index impl.Bodies) published

                        // Pushed on top of the units it may shadow; later units resolve through
                        // it. The full view crosses only inside a compiling assembly: across the
                        // boundary the surface alone does, with the splice templates beside it.
                        let pushed =
                            match publication with
                            | Publication.InAssembly -> view
                            | Publication.AcrossAssemblies _ -> published

                        own <- pushed :: own

                        let file =
                            {
                                Retained = impl.Retained
                                ParseDiagnostics = parsedUnit.Implementation.Parsed.Diagnostics
                                Frozen = impl.Frozen
                                Imports = impl.Imports
                                Scoped = impl.Scoped
                                View = view
                                Signature = signatureFile
                            }

                        let surfaced =
                            match publication with
                            | Publication.InAssembly -> fileDiagnostics file
                            | Publication.AcrossAssemblies _ ->
                                match signatureFile with
                                | ValueSome s -> signatureFileDiagnostics s
                                | ValueNone ->
                                    // The inferred surface IS what the unit publishes, so its
                                    // analysis errors are findings about it. A unit with a
                                    // `.fsi` keeps its tolerance: analysis feeds templates.
                                    implementationFileDiagnostics file |> AnchoredDiagnostic.errors

                        UnitOutcome.Analysed
                            {
                                File = file
                                Surface = surface
                                Bodies = impl.Bodies
                                Published = pushed
                                Surfaced = surfaced
                            }
            ]

        {
            Units = outcomes
            Published = own
            AutoOpens = autoOpens
        }
