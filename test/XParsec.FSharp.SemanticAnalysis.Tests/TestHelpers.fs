module XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

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
    SemType.TyUnion(SymbolKeyOps.qualifiedTypeKey name args.Length, args)

let TyRecord (name: string, args: EqArray<SemType>) =
    SemType.TyRecord(SymbolKeyOps.qualifiedTypeKey name args.Length, args)

let TyClass (name: string, args: EqArray<SemType>) =
    SemType.TyClass(SymbolKeyOps.qualifiedTypeKey name args.Length, args)

// The arity-qualified qualified name (`Microsoft.FSharp.Core.Result`2`,
// `Choice`2`) — the new canonical convention. Assertions that pinned the old
// non-suffixed / bare form were updated to match (the doc's "convention ripple").
let private nominalDisplayName (k: SymbolKey) : string = SymbolKeyOps.qualifiedName k

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
