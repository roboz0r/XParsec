module XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

open System.IO
open Vesper
open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common.Tests

/// `src/<pkg>`, relative to this test file.
let srcPackage (pkg: string) : string =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", pkg)

/// `src/<pkg>`'s manifest for `target`. A package that does not build for it fails the test.
let srcManifest (target: string) (pkg: string) : ReferencedProject.ManifestPath =
    ReferencedProject.resolveManifest target (srcPackage pkg)
    |> PackageFaults.okOrFail "srcManifest"

/// The default contract stack for the SA front-end tests: real SRTP operators (`(+) : ^T
/// -> ^T -> ^T`), the ordering operators, `hash`/`failwith`, the cons-list and the printf
/// family, with the primitive reprs canonicalised to `int` / `string`. Splice templates are
/// SERVED, as a compilation resolves them, so an operator use elaborates to an `InlineCall`
/// edge (`spec#N` in pretty output), never a bare `External` App.
let realProvider: Lazy<IExternalSymbolProvider> =
    lazy
        [ "Vesper.Core"; "Vesper.List"; "Vesper.Comparison"; "Vesper.Printf" ]
        |> List.map (srcManifest "clr")
        |> PackageProviders.composeContract PackageProviders.noPlatformMetadata
        |> fun composed -> composed.Provider

// Shadow the nominal `SemType` constructors so a test writes `TyUnion("X", args)` rather
// than minting a `SymbolKey`; the active patterns below project a key back to a name.
// Live only in a file that `open`s this module AFTER `open …SemanticAnalysis`.
let TyUnion (name: string, args: Block<SemType>) =
    SemType.TyUnion(SymbolKeyOps.qualifiedTypeKeyOf name args.Length, args)

let TyRecord (name: string, args: Block<SemType>) =
    SemType.TyRecord(SymbolKeyOps.qualifiedTypeKeyOf name args.Length, args)

let TyClass (name: string, args: Block<SemType>) =
    SemType.TyClass(SymbolKeyOps.qualifiedTypeKeyOf name args.Length, args)

// The arity-QUALIFIED metadata name: a `Choice` key at arity 2 projects back as
// `Choice`2`, not as the bare source name the constructors above take.
let private nominalDisplayName (k: TypeKey) : string = SymbolKeyOps.typeMetaName k

let (|TyUnion|_|) (t: SemType) =
    match t with
    | SemType.TyUnion(k, args) -> Some(nominalDisplayName k, args)
    | _ -> None

let (|TyRecord|_|) (t: SemType) =
    match t with
    | SemType.TyRecord(k, args) -> Some(nominalDisplayName k, args)
    | _ -> None

let (|TyClass|_|) (t: SemType) =
    match t with
    | SemType.TyClass(k, args) -> Some(nominalDisplayName k, args)
    | _ -> None

/// A ONE-argument-group `ExternalSignature` whose method typars are all unconstrained —
/// reflection, `.fsi` and JS-native producers carry no keyof constraint. Call it qualified from a
/// file that must not take this module's shadow `TyUnion`/`TyRecord`/`TyClass` constructors.
let mkSignature
    (declaringTyparArity: int)
    (methodTyparArity: int)
    (parameters: FrozenType)
    (ret: FrozenType)
    : ExternalSignature =
    ExternalSignature.make (declaringTyparArity, methodTyparArity, parameters, ret)

/// A skeleton `ExternalMember` named `name`: a static `unit -> unit` method on a stub
/// declaring type `C`.
let mkMember (name: string) : ExternalMember =
    { ExternalMember.OfKey(
          SymbolKeyOps.memberKeyOf (SymbolKeyOps.qualifiedTypeKeyOf "C" 0) name Block.empty 0 MemberKind.Method
      ) with
        IsStatic = true
        Signature =
            mkSignature 0 0 (FTConst(RuntimeNames.unitKey, Block.empty)) (FTConst(RuntimeNames.unitKey, Block.empty))
    }

/// A throwaway source token: `TExprG` pins `'tok = SyntaxToken`, so every hand-assembled
/// `TExpr.*` node needs one, and a test that asserts nothing about positions passes this.
let dummyTok: SyntaxToken =
    SyntaxToken.virtualToken (PositionedToken.Create(Token.EOF, 0))

/// Match a `Block<'T>` with list-literal arms: `| EqList [ _; d ] -> d`. Test-only —
/// production code iterates the struct enumerator or goes through `Block.*`.
let inline (|EqList|) (xs: Block<'T>) : 'T list = Block.toList xs

/// Lex + parse a source string (a script fragment comes back as an AnonymousModule).
/// Raises on failure, and also on a parse that only succeeded because recovery patched a
/// hole — `parseRecoveredFile` is the one that accepts a patched tree.
let parseFile (input: string) : Lexed * ImplementationFile<SyntaxToken> =
    match ParseChain.parseUnrecovered Set.empty input with
    | Result.Error ds -> failwithf "parse failed: %A" (ds |> List.map (fun d -> d.Message))
    | Result.Ok parsed -> parsed.Lexed, parsed.Tree

/// `parseFile` for a source whose parse is EXPECTED to need recovery: the tree comes out
/// patched, and what analysis makes of it is the point of the test. Fails if the source
/// stops needing recovery.
let parseRecoveredFile (input: string) : Lexed * ImplementationFile<SyntaxToken> =
    match ParseChain.parse Set.empty input with
    | Result.Error f -> failwithf "parse failed: %A" (f.Diagnostics |> List.map (fun d -> d.Message))
    | Result.Ok parsed ->
        match parsed.Diagnostics with
        | [] -> failwith "expected a parse that needed recovery; nothing was reported"
        | _ -> parsed.Lexed, parsed.Tree

/// Thaw a WIRE inline body against the file it was published from: the body carries the
/// declaring file's own token indices, and only that file can resolve them.
let thawPublished (store: TypeStore) (source: LexedFile) (published: Wire.UnpooledDecl) : InlineThaw.ThawedTemplate =
    InlineThaw.bodyAtPath (MeasuredThaw.noneOver store) (LexedFiles.ofSeq [ source ]) source.Path published

let thawPublishedDecl (store: TypeStore) (source: LexedFile) (published: Wire.UnpooledDecl) : TDecl =
    (thawPublished store source published).Decl

/// Lex + parse a signature (`.fsi`) source string and return Lexed + a
/// SignatureFile. Raises on failure.
let parseSigFile (input: string) : Lexed * SignatureFile<SyntaxToken> =
    let lexed = Lexing.lexString input
    let reader = Reader.ofParseInput (lexed.WithDefines Set.empty)

    match FSharpAst.parseSignature reader with
    | Result.Error e -> failwithf "parse failed: %A" e
    | Result.Ok(FSharpAst.SignatureFile f) -> lexed, f
    | Result.Ok ast -> failwithf "unexpected AST: %A" ast

/// The assembly name every freeze in these suites is taken under.
let testAsm = AssemblyName "TestAsm"

/// `testAsm` as a compiling identity: `realProvider` is the clr contract stack.
let testCompiling: CompilingAssembly = { Name = testAsm; Target = "clr" }

/// The error-severity messages of `ds`, in report order. The one reader of severity for a
/// suite that asserts on wording — a frozen `FrozenPools.blockingErrors` list and a
/// `TastFile.Diagnostics` list both come through here.
let errorMessages (ds: Diagnostic seq) : string list =
    Diagnostic.errors ds |> List.map (fun d -> d.Message)

/// Analyse `src` through NameResolution, Unification and Elaborate, stopping short of the
/// freeze. `freezeFor` runs the whole front end.
let analyseSem (src: string) : TastFile =
    let lexed, file = parseFile src
    Pipeline.analyseSemFor testCompiling realProvider.Value (LexedFile.ofText lexed) file

let semErrors (src: string) : string list =
    errorMessages (analyseSem src).Diagnostics

let expectCleanTast (tast: TastFile) : unit =
    let es = errorMessages tast.Diagnostics
    Expect.isEmpty es (sprintf "expected no errors; diagnostics were %A" es)

let expectClean (src: string) : unit = expectCleanTast (analyseSem src)

/// Some message in `es` contains `needle`.
let expectErrorIn (es: string list) (needle: string) : unit =
    Expect.isTrue
        (es |> List.exists (fun m -> m.Contains needle))
        (sprintf "expected an error containing '%s'; diagnostics were %A" needle es)

let expectError (needle: string) (src: string) : unit = expectErrorIn (semErrors src) needle

/// `expectErrorIn`, and no `internal compiler error` stands beside the match: a name that
/// fails to resolve reaches the freeze as a stray `TyVar` and trips the internal backstop.
let expectUserErrorIn (es: string list) (needle: string) : unit =
    expectErrorIn es needle
    Expect.isFalse (es |> List.exists (fun m -> m.Contains "internal compiler error")) "no internal error"

let expectUserError (needle: string) (src: string) : unit =
    expectUserErrorIn (semErrors src) needle

/// `expectUserError`, and `needle` matches exactly one diagnostic. Errors it does not match
/// may stand beside it — `expectUserErrorReportedAlone` is the one that forbids them.
let expectUserErrorReportedOnce (needle: string) (src: string) : unit =
    let es = semErrors src
    expectUserErrorIn es needle

    Expect.equal
        (es |> List.filter (fun m -> m.Contains needle) |> List.length)
        1
        (sprintf "'%s' reported once; diagnostics were %A" needle es)

/// `expectUserError`, and the analysis reported exactly one error in total. Use it where F#
/// reports one diagnostic and the noise beside ours is the finding.
let expectUserErrorReportedAlone (needle: string) (src: string) : unit =
    let es = semErrors src
    expectUserErrorIn es needle
    Expect.equal es.Length 1 (sprintf "'%s' reported alone; diagnostics were %A" needle es)

/// Freeze `src` through the whole front end, returning the origin it was analysed FROM —
/// what a consumer needs to read the frozen templates' positions. The origin is bucketed
/// under `testAsm`, which the signature projection reads back as the home assembly.
let freezeWithOrigin (src: string) : LexedFile * FrozenPools =
    let lexed, file = parseFile src

    let origin = LexedFile.inAssembly testAsm (AssemblyFileId.ofText src) lexed

    origin, Pipeline.analyseFor testCompiling realProvider.Value origin file

/// Freeze `src` through the whole front end: the pooled output the cache stores, the codec
/// flattens, and the signature projection reads. Raises on lex/parse failure.
let freezeFor (src: string) : FrozenPools = snd (freezeWithOrigin src)

/// `AnalysedAssembly.analyse` over units held as TEXT, under no compilation defines, against
/// `external`.
let analyseUnitsOf
    (assembly: CompilingAssembly)
    (external: IExternalSymbolProvider)
    (units: AssemblyFiles.SourceUnit list)
    : AnalysedAssembly =
    AnalysedAssembly.analyse
        Pipeline.analyseFileFor
        external
        {
            Assembly = assembly
            Units = List.map (AssemblyFiles.AssemblyUnit.parse Set.empty) units
        }

/// `analyseUnitsOf` against the real contract stack, over implementation-only files given as
/// `fileName, text`, on a `none`-target assembly named `name`.
let analyseFiles (name: string) (files: (string * string) list) : AnalysedAssembly =
    analyseUnitsOf
        {
            Name = AssemblyName name
            Target = "none"
        }
        realProvider.Value
        [
            for (fileName, text) in files ->
                AssemblyFiles.SourceUnit.ofImplementation (AssemblyFiles.SourceFile.ofText fileName text)
        ]

/// Each unit's analysed file in order, or a test failure naming the first that did not parse.
let analysedFiles (analysed: AnalysedAssembly) : AssemblyFiles.FrozenFile list =
    analysed.Units
    |> List.map (
        function
        | AssemblyAnalysis.UnitOutcome.Analysed u -> u.File
        | AssemblyAnalysis.UnitOutcome.Failed(leading, rest) ->
            failtestf
                "unit failed to parse: %A"
                [ for e in leading :: rest -> e.Id.Name, FileFault.diagnostics e.Fault ]
    )

/// The pools under test, paired with the tree they encode. The freeze's pools are unpooled
/// and then re-pooled from THAT tree, so a gate over the pair judges a genuine round trip
/// rather than the freeze's own pools against themselves.
let poolsFor (src: string) : FrozenPools * Pooled.TastFile =
    let frozen = freezeFor src
    let unpooled = TastUnpool.ofPools frozen
    TastPools.rePool frozen unpooled, unpooled

/// The re-pool `poolsFor` runs, for a test that MODIFIES the unpooled tree first. A
/// `BoundVarId`-named tree has no naming column of its own, so this closes over the
/// freeze's.
let rePoolFor (src: string) : Pooled.TastFile -> FrozenPools = TastPools.rePool (freezeFor src)

/// A stand-in dependency over the surface `fill` accumulates. Every table a real reference
/// publishes is filled, scope contents included.
let providerOfSurface (fill: PublishedSurfaceBuilder -> unit) : IExternalSymbolProvider =
    PublishedSurface.build fill |> PublishedSurface.toProvider

/// A stand-in dependency serving only the member channels, addressed by the declaring
/// type's compiled RENDERING. Both member channels read `lookup`, so the by-key selection
/// runs over the same overload set the by-name scan sees.
let membersProvider (lookup: string -> string -> Block<ExternalMember>) : IExternalSymbolProvider =
    { new ExternalSymbolProviders.ProviderDecorator(ExternalSymbolProviders.nullProvider) with
        override _.TryLookupMembers(key, memberName) =
            lookup (SymbolKeyOps.typeMetaName key) memberName

        override _.TryLookupMemberByKey key =
            lookup (SymbolKeyOps.typeMetaName key.Decl) key.Name
            |> ExternalSymbols.memberByKey key
    }

/// A stand-in dependency publishing `symbols` as a referenced package publishes its values.
let providerOfValues (symbols: ExternalSymbol list) : IExternalSymbolProvider =
    providerOfSurface (fun b ->
        for sym in symbols do
            PublishedSurfaceBuilder.addValue b sym
    )

/// `providerOfValues` for the type shapes a referenced package publishes.
let providerOfTypes (types: (TypeKey * ExternalTypeShape) list) : IExternalSymbolProvider =
    providerOfSurface (fun b ->
        for (key, shape) in types do
            PublishedSurfaceBuilder.addType b key shape
    )

/// A static get-only property `decl.name : ret`. Copy it with `{ … with … }` for an instance
/// member, a method or an overload.
let mkStaticProperty (decl: TypeKey) (name: string) (ret: FrozenType) : ExternalMember =
    { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf decl name Block.empty 0 MemberKind.Property) with
        IsStatic = true
        Storage = MemberStorage.Property
        Signature = ExternalSignature.value (decl.TyparArity, 0, ret)
    }

let private unionShape (key: TypeKey) (cases: ExternalCaseShape list) (rqa: bool) : ExternalTypeShape =
    ExternalTypeShape.Union
        {
            Typars = TyparList.positional key.TyparArity
            Cases = Block.ofList cases
            Interfaces = Block.empty
            Origin = SymbolOrigin.Empty
            IsValueType = false
            RequiresQualifiedAccess = rqa
        }

let publishUnion (b: PublishedSurfaceBuilder) (key: TypeKey) (cases: ExternalCaseShape list) : unit =
    PublishedSurfaceBuilder.addType b key (unionShape key cases false)

/// `publishUnion` for a `[<RequireQualifiedAccess>]` union: a consumer must write `Color.Red`.
let publishRqaUnion (b: PublishedSurfaceBuilder) (key: TypeKey) (cases: ExternalCaseShape list) : unit =
    PublishedSurfaceBuilder.addType b key (unionShape key cases true)

let publishRecord (b: PublishedSurfaceBuilder) (key: TypeKey) (fields: ExternalFieldShape list) : unit =
    PublishedSurfaceBuilder.addType
        b
        key
        (ExternalTypeShape.Record
            {
                Typars = TyparList.positional key.TyparArity
                Fields = Block.ofList fields
                Origin = SymbolOrigin.Empty
                IsValueType = false
                RequiresQualifiedAccess = false
            })

/// Publish the class `key` with `members`. Build each member over this same `key`, which its
/// `MemberKey` names as the declaring type.
let publishClass (b: PublishedSurfaceBuilder) (key: TypeKey) (members: ExternalMember list) : unit =
    PublishedSurfaceBuilder.addTypeWith
        b
        key
        (ExternalTypeShape.Class
            { ExternalClassShape.basic (TyparList.positional key.TyparArity, ClassCommitment.Class, SymbolOrigin.Empty) with
                Members = Block.ofList members
            })
        members

/// Parse `input` and run NameResolution against `provider`. Run
/// `Passes.Unification.run` on the returned pair to continue into inference.
let analyseNameRes (provider: IExternalSymbolProvider) (input: string) : PassContext * ImplementationFile<SyntaxToken> =
    let lexed, file = parseFile input

    let ctx = PassContext(provider, LexedFile.ofText lexed, testCompiling)

    Passes.NameResolution.run ctx file
    ctx, file

/// The file's first `let` binding.
let firstBinding (file: ImplementationFile<SyntaxToken>) : Binding<SyntaxToken> =
    CstModuleTree.implFileElems file
    |> Seq.pick (fun m ->
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) when bindings.Length > 0 ->
            Some bindings.[0]
        | _ -> None
    )

let firstBindingExpr (file: ImplementationFile<SyntaxToken>) : Expr<SyntaxToken> = (firstBinding file).expr

/// The zonked `SemType` inference settled on at `key`. Worth asserting on: an unresolved
/// name still yields a well-formed type (`TyConst(RuntimeNames.opaqueKey name)`), so a
/// diagnostics-only test cannot tell it from a resolved one.
let typeOf (ctx: PassContext) (key: NodeKey) : SemType =
    match ctx.Bindings.TypeVar.TryGetValue key with
    | ValueSome tv -> Passes.Unification.zonk ctx.Store (TyVar tv)
    | ValueNone -> failwithf "no TypeVar entry for %O" key

/// The registered record / union / class named `name`, resolved from the whole-file view
/// (`UseSite.unbounded`), or a test failure.
let expectRecord (ctx: PassContext) (name: string) =
    match TypeRegistry.tryRecord ctx.Types UseSite.unbounded name with
    | ValueSome info -> info
    | ValueNone -> failtestf "record type %s not registered" name

let expectUnion (ctx: PassContext) (name: string) =
    match TypeRegistry.tryUnionBare ctx.Types UseSite.unbounded name with
    | ValueSome info -> info
    | ValueNone -> failtestf "union type %s not registered" name

let expectClass (ctx: PassContext) (name: string) =
    match TypeRegistry.tryClass ctx.Types UseSite.unbounded name with
    | ValueSome info -> info
    | ValueNone -> failtestf "class type %s not registered" name

/// Assert a pooled expression row's payload agrees with the walk's view of an already-anchored
/// node: `Complete` compares equal outright, `Binding` on `isRec`, and `BindingGroup` on the
/// member anchors and the components. The walk's `Recursion` / `AppKind` verdict is unchecked.
let expectPayloadOf (actual: ExprPayload) (expected: PayloadOfNode<Anchor, 'id>) : unit =
    match expected, actual with
    | PayloadOfNode.Complete p, actual -> Expect.equal actual p "expr payload"
    | PayloadOfNode.Application, ExprPayload.App _ -> ()
    | PayloadOfNode.Binding(isRec = isRec), ExprPayload.Let(isRec = isRec') -> Expect.equal isRec' isRec "Let isRec"
    | PayloadOfNode.BindingGroup(members = members; components = components), ExprPayload.LetGroup g ->
        let memberToks = members |> Block.toArray |> Array.map (fun m -> m.Tok)
        Expect.equal (g.Members |> Array.map (fun m -> m.Tok)) memberToks "LetGroup member anchors"
        Expect.equal g.Components components "LetGroup components"
    | expected, actual -> failtestf "payload %A is not the walk's view %A" actual expected
