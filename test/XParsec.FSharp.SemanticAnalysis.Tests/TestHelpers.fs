module XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

open System.IO
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
let TyUnion (name: string, args: EqArray<SemType>) =
    SemType.TyUnion(SymbolKeyOps.qualifiedTypeKeyOf name args.Length, args)

let TyRecord (name: string, args: EqArray<SemType>) =
    SemType.TyRecord(SymbolKeyOps.qualifiedTypeKeyOf name args.Length, args)

let TyClass (name: string, args: EqArray<SemType>) =
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

/// A ONE-argument-group `ExternalSignature` with `MethodTyparBounds` defaulted to empty —
/// reflection, `.fsi` and JS-native producers carry no keyof bound. Call it qualified from a
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
          SymbolKeyOps.memberKeyOf (SymbolKeyOps.qualifiedTypeKeyOf "C" 0) name EqArray.empty 0 MemberKind.Method
      ) with
        IsStatic = true
        Signature =
            mkSignature
                0
                0
                (FTConst(RuntimeNames.unitKey, EqArray.empty))
                (FTConst(RuntimeNames.unitKey, EqArray.empty))
    }

/// A throwaway source token: `TExprG` pins `'tok = SyntaxToken`, so every hand-assembled
/// `TExpr.*` node needs one, and a test that asserts nothing about positions passes this.
let dummyTok: SyntaxToken =
    SyntaxToken.virtualToken (PositionedToken.Create(Token.EOF, 0))

/// Match an `EqArray<'T>` with list-literal arms: `| EqList [ _; d ] -> d`. Test-only —
/// production code iterates the struct enumerator or goes through `EqArray.*`.
let inline (|EqList|) (xs: EqArray<'T>) : 'T list = EqArray.toList xs

/// Lex + parse a source string (a script fragment comes back as an AnonymousModule).
/// Raises on failure, and also on a parse that only succeeded because recovery patched a
/// hole — `parseRecoveredFile` is the one that accepts a patched tree.
let parseFile (input: string) : Lexed * ImplementationFile<SyntaxToken> =
    match ParseChain.parseUnrecovered Set.empty input with
    | Result.Error ds -> failwithf "parse failed: %A" (ds |> List.map (fun d -> d.Message))
    | Result.Ok parsed -> parsed.Lexed, parsed.File

/// `parseFile` for a source whose parse is EXPECTED to need recovery: the tree comes out
/// patched, and what analysis makes of it is the point of the test. Fails if the source
/// stops needing recovery.
let parseRecoveredFile (input: string) : Lexed * ImplementationFile<SyntaxToken> =
    match ParseChain.parse Set.empty input with
    | Result.Error f -> failwithf "parse failed: %A" (f.Diagnostics |> List.map (fun d -> d.Message))
    | Result.Ok parsed ->
        match parsed.Diagnostics with
        | [] -> failwith "expected a parse that needed recovery; nothing was reported"
        | _ -> parsed.Lexed, parsed.File

/// Realise a WIRE inline body against the file it was published from: the body carries the
/// declaring file's own token indices, and only that file can resolve them.
let thawPublished (store: TypeStore) (source: LexedFile) (decl: Wire.TDecl) : TDecl =
    InlineThaw.bodyAtPath store (LexedFiles.ofSeq [ source ]) source.Path decl

/// Lex + parse a signature (`.fsi`) source string and return Lexed + a
/// SignatureFile. Raises on failure.
let parseSigFile (input: string) : Lexed * SignatureFile<SyntaxToken> =
    match Lexing.lexString input with
    | Result.Error e -> failwithf "lex failed: %A" e
    | Result.Ok lexed ->
        let reader = Reader.ofParseInput (lexed.WithDefines Set.empty)

        match FSharpAst.parseSignature reader with
        | Result.Error e -> failwithf "parse failed: %A" e
        | Result.Ok(FSharpAst.SignatureFile f) -> lexed, f
        | Result.Ok ast -> failwithf "unexpected AST: %A" ast

/// The assembly name every freeze in these suites is taken under.
let testAsm = "TestAsm"

/// `testAsm` as a compiling identity: `realProvider` is the clr contract stack.
let testCompiling: CompilingAssembly = { Name = testAsm; Target = "clr" }

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

/// Parse `input` and run Desugar + NameResolution against `provider`. Run
/// `Passes.Unification.run` on the returned pair to continue into inference.
let analyseNameRes (provider: IExternalSymbolProvider) (input: string) : PassContext * ImplementationFile<SyntaxToken> =
    let lexed, file = parseFile input

    let ctx = PassContext(provider, LexedFile.ofText lexed, CompilingAssembly.none)

    Passes.Desugar.run ctx file
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
