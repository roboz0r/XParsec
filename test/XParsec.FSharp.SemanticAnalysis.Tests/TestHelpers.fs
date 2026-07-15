module XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

open System.IO
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// `src/<pkg>/manifest.toml`, relative to this test file.
let srcManifest (pkg: string) : string =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", pkg, "manifest.toml")

/// The real default contract stack that replaced the value-only `MockBuiltins`
/// fixture across the SA front-end tests: real SRTP operators (`(+) : ^T -> ^T ->
/// ^T`), the ordering operators, `hash`/`failwith`, the cons-list, the printf
/// family, and the primitive reprs canonicalised to `int` / `string` (matching
/// `BuiltinTypes`) — resolved the way the production front end resolves them, not
/// a hand-curated monomorphic stand-in.
///
/// These are the `Vesper.*` self-host packages (NOT the FSharp.Core port
/// `XParsec.FSharp.Lib`): their primitive canonicalisation agrees with the
/// front-end's `BuiltinTypes`, whereas the FSharp.Core port canonicalises
/// `int`→`int32` and leaves `string` an unfreezable template.
///
/// Composed through `ReferencedProject.composeContract` — the SAME dependency-order
/// wiring the codegen `SymbolProviders` stack uses — with `noMetaTail` (no BCL/native
/// leaf; the front end resolves primitives from the `.fsi` reprs alone). The manifest
/// set is unordered: `composeContract` derives the topological order from each
/// manifest's `depends-on`, so a dependent's members freeze against real dependency
/// types rather than opaque templates. Forced lazily so a run that never analyses
/// pays nothing.
let realProvider: Lazy<IExternalSymbolProvider> =
    lazy
        [ "Vesper.Core"; "Vesper.List"; "Vesper.Comparison"; "Vesper.Printf" ]
        |> List.map srcManifest
        |> ReferencedProject.composeContract ReferencedProject.noMetaTail None

// The nominal `SemType` cases now carry a
// `SymbolKey`, but tests construct and assert them by *string* name. These shadow
// the three constructors (minting the key the production pipeline mints for an
// `ns`-less test type — `qualifiedTypeKey` splits a qualified name and arity-
// qualifies it, matching `LocalSymbolKey.ofType` / `externalTypeKey`) and expose
// name-projecting active patterns (the arity-stripped qualified name — the legacy
// string form every assertion was written against). Existing `TyUnion("X", args)`
// construction *and* `| TyUnion("X", args)` match sites compile unchanged. A file
// gets these only when it `open`s `TestHelpers` after `open …SemanticAnalysis`.
let TyUnion (name: string, args: EqArray<SemType>) =
    SemType.TyUnion(SymbolKeyOps.qualifiedTypeKeyOfT name args.Length, args)

let TyRecord (name: string, args: EqArray<SemType>) =
    SemType.TyRecord(SymbolKeyOps.qualifiedTypeKeyOfT name args.Length, args)

let TyClass (name: string, args: EqArray<SemType>) =
    SemType.TyClass(SymbolKeyOps.qualifiedTypeKeyOfT name args.Length, args)

// The arity-qualified qualified name (`Microsoft.FSharp.Core.Result`2`,
// `Choice`2`) — the new canonical convention. Assertions that pinned the old
// non-suffixed / bare form were updated to match (the doc's "convention ripple").
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

/// An `ExternalSignature` with `MethodTyparBounds` DEFAULTED to empty (the churn-free
/// default for every non-TS producer — reflection/`.fsi`/JS-native carry no keyof bound).
/// Route hand-built signatures through this so the next `ExternalSignature` field
/// addition is a ONE-site change here, not a mechanical edit at every construction site
/// (as the `MethodTyparBounds` slot addition was). Reference qualified
/// (`TestHelpers.mkSignature`) from files that must not `open` this module's shadow
/// `TyUnion`/`TyRecord`/`TyClass` constructors.
let mkSignature
    (declaringTyparArity: int)
    (methodTyparArity: int)
    (parameters: FrozenType)
    (ret: FrozenType)
    : ExternalSignature =
    {
        DeclaringTyparArity = declaringTyparArity
        MethodTyparArity = methodTyparArity
        Parameters = parameters
        Return = ret
        MethodTyparBounds = [||]
    }

/// A skeleton `ExternalMember` named `name` for the signature-INSTANTIATION oracle tests —
/// a static `unit -> unit` method on a stub declaring type `C`. The oracle under test reads
/// only `Signature`, but the entry is KEYED as the member it is: a member's identity is a
/// `MemberKey`, so `Name` is derived from the key and the two cannot disagree.
let mkMember (name: string) : ExternalMember =
    { ExternalMember.OfKey(
          SymbolKeyOps.memberKeyOf (SymbolKeyOps.qualifiedTypeKeyOfT "C" 0) name EqArray.empty MemberKind.Method
      ) with
        IsStatic = true
        Signature =
            mkSignature
                0
                0
                (FTConst(RuntimeNames.unitKey, EqArray.empty))
                (FTConst(RuntimeNames.unitKey, EqArray.empty))
    }

/// A throwaway source token for hand-built TAST construction in tests. The
/// frozen `TExprG` spine pins `'tok = SyntaxToken`, so every hand-assembled
/// `TExpr.*` node needs a token; tests that don't exercise source-map positions
/// pass this virtual EOF token.
let dummyTok: SyntaxToken =
    SyntaxToken.virtualToken (PositionedToken.Create(Token.EOF, 0))

/// Project an `EqArray<'T>` as a plain `'T list` inside a pattern match — lets
/// tests written against the pre-EqArray TAST keep their list-literal arms
/// (`| [ TDecl.Let _ ] -> …`, `| [ x; y ] -> …`) verbatim across the flip
/// (docs/tast-eqarray-list.md Stage 2). Use sparingly — production code should
/// iterate via the struct enumerator or `EqArray.*` helpers.
let inline (|EqList|) (xs: EqArray<'T>) : 'T list = EqArray.toList xs

/// Lex + parse a source string and return Lexed + an ImplementationFile
/// (script fragments are wrapped as AnonymousModule). Raises on failure.
let parseFile (input: string) : Lexed * ImplementationFile<SyntaxToken> =
    match Lexing.lexString input with
    | Result.Error e -> failwithf "lex failed: %A" e
    | Result.Ok lexed ->
        let reader = Reader.ofLexed lexed input Set.empty

        match FSharpAst.parse reader with
        | Result.Error e -> failwithf "parse failed: %A" e
        | Result.Ok(FSharpAst.ImplementationFile f) -> lexed, f
        | Result.Ok(FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment elems)) ->
            lexed, ImplementationFile.AnonymousModule elems
        | Result.Ok ast -> failwithf "unexpected AST: %A" ast

/// Lex + parse a signature (`.fsi`) source string and return Lexed + a
/// SignatureFile. Raises on failure.
let parseSigFile (input: string) : Lexed * SignatureFile<SyntaxToken> =
    match Lexing.lexString input with
    | Result.Error e -> failwithf "lex failed: %A" e
    | Result.Ok lexed ->
        let reader = Reader.ofLexed lexed input Set.empty

        match FSharpAst.parseSignature reader with
        | Result.Error e -> failwithf "parse failed: %A" e
        | Result.Ok(FSharpAst.SignatureFile f) -> lexed, f
        | Result.Ok ast -> failwithf "unexpected AST: %A" ast

/// Parse `input` and run the front-end passes up to NameResolution against
/// `provider` — the shared harness of the stamp suites, which assert on the side
/// tables NameResolution writes. (Run `Passes.Unification.run` on the returned
/// pair to take a test through inference as well.)
let analyseNameRes (provider: IExternalSymbolProvider) (input: string) : PassContext * ImplementationFile<SyntaxToken> =
    let lexed, file = parseFile input
    let ctx = PassContext(provider, input, lexed)
    Passes.Desugar.run ctx file
    Passes.NameResolution.run ctx file
    ctx, file

/// The file's first `let` binding.
let firstBinding (file: ImplementationFile<SyntaxToken>) : Binding<SyntaxToken> =
    CstWalk.implFileElems file
    |> Seq.pick (fun m ->
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) when bindings.Length > 0 ->
            Some bindings.[0]
        | _ -> None
    )

/// The RHS expression of the file's first `let` binding.
let firstBindingExpr (file: ImplementationFile<SyntaxToken>) : Expr<SyntaxToken> = (firstBinding file).expr

/// The zonked `SemType` inference settled on at `key` — the node's TypeVar
/// resolved through the substitution. Asserting on this pins the type's IDENTITY,
/// which a diagnostics-only assertion cannot: an unresolved head still yields a
/// well-formed type (`TyConst(RuntimeNames.opaqueKey name)`), so it unifies and
/// clashes exactly like a resolved one.
let typeOf (ctx: PassContext) (key: NodeKey) : SemType =
    match ctx.Bindings.TypeVar.TryGetValue key with
    | ValueSome tv -> Passes.Unification.zonk (TyVar tv)
    | ValueNone -> failwithf "no TypeVar entry for %O" key
