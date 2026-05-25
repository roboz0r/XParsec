namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Generic
open System.IO
open XParsec.Toml
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Loads `XParsec.FSharp.Lib` (the signature-only port of FSharp.Core) and
/// answers `IExternalSymbolProvider` lookups from the parsed signatures.
///
/// See `src/XParsec.FSharp.Lib/compiler-clr-project.md` for the manifest
/// schema and bucket layout, and `docs/extract-symbols-plan.md` for the
/// extraction design.
module FSharpLib =

    type BucketEntry =
        {
            Name: string
            /// Directory name relative to the lib root.
            Path: string
            Description: string
            /// Direct dependencies (other bucket names). Transitive closure
            /// is computed by the consumer.
            DependsOn: string list
        }

    type RootManifest =
        {
            UpstreamCommit: string
            UpstreamTag: string
            UpstreamPath: string
            Buckets: BucketEntry list
        }

    /// One file resolved through the manifests. Order in a `LoadedLib`
    /// reflects the topological bucket order followed by each bucket's
    /// manifest's `files` array.
    type LibFile =
        {
            BucketName: string
            /// Relative path from the bucket directory (e.g. `"math/z.fsi"`).
            Relative: string
            Absolute: string
        }

    type LoadedLib =
        {
            Root: RootManifest
            Files: LibFile list
        }

    let private asString (v: TomlValue) : string option =
        match v with
        | TomlValue.String s -> Some s
        | _ -> None

    let private asTable (v: TomlValue) : TomlTable option =
        match v with
        | TomlValue.Table t
        | TomlValue.InlineTable t -> Some t
        | _ -> None

    let private asArray (v: TomlValue) : TomlValue list option =
        match v with
        | TomlValue.Array xs -> Some xs
        | _ -> None

    let private findString (t: TomlTable) (key: string) : string option =
        Map.tryFind key t |> Option.bind asString

    let private findStringList (t: TomlTable) (key: string) : string list option =
        match Map.tryFind key t with
        | Some(TomlValue.Array xs) -> xs |> List.choose asString |> Some
        | _ -> None

    let private parseRootManifest (doc: TomlDocument) : Result<RootManifest, string> =
        let upstream = Map.tryFind "upstream" doc |> Option.bind asTable
        let buckets = Map.tryFind "bucket" doc |> Option.bind asArray

        match upstream, buckets with
        | None, _ -> Error "manifest.toml: missing [upstream] table"
        | _, None -> Error "manifest.toml: missing [[bucket]] array"
        | Some up, Some bs ->
            let bucketEntries =
                bs
                |> List.choose (fun b ->
                    match asTable b with
                    | None -> None
                    | Some t ->
                        match findString t "name", findString t "path" with
                        | Some n, Some p ->
                            Some
                                {
                                    Name = n
                                    Path = p
                                    Description = findString t "description" |> Option.defaultValue ""
                                    DependsOn = findStringList t "depends-on" |> Option.defaultValue []
                                }
                        | _ -> None
                )

            Ok
                {
                    UpstreamCommit = findString up "commit" |> Option.defaultValue ""
                    UpstreamTag = findString up "tag" |> Option.defaultValue ""
                    UpstreamPath = findString up "path" |> Option.defaultValue ""
                    Buckets = bucketEntries
                }

    let private parseBucketManifest (doc: TomlDocument) : Result<string list, string> =
        match Map.tryFind "files" doc with
        | Some(TomlValue.Array xs) -> xs |> List.choose asString |> Ok
        | _ -> Error "bucket manifest.toml: missing `files = [...]`"

    /// Topologically sorts buckets by their `DependsOn` graph. Within a
    /// depth tier, original input order is preserved. Errors on cycles and
    /// references to unknown buckets.
    let private topoSort (buckets: BucketEntry list) : Result<BucketEntry list, string> =
        let byName = buckets |> List.map (fun b -> b.Name, b) |> Map.ofList
        let mutable sorted = []
        let mutable visiting = Set.empty
        let mutable visited = Set.empty
        let mutable err = None

        let rec visit name =
            if err.IsSome then
                ()
            elif Set.contains name visited then
                ()
            elif Set.contains name visiting then
                err <- Some(sprintf "Bucket dependency cycle through '%s'" name)
            else
                match Map.tryFind name byName with
                | None -> err <- Some(sprintf "Unknown bucket '%s' in depends-on" name)
                | Some b ->
                    visiting <- Set.add name visiting

                    for d in b.DependsOn do
                        visit d

                    visiting <- Set.remove name visiting
                    visited <- Set.add name visited
                    sorted <- b :: sorted

        for b in buckets do
            visit b.Name

        match err with
        | Some e -> Error e
        | None -> Ok(List.rev sorted)

    /// Reads `<libRoot>/manifest.toml` plus each per-bucket manifest;
    /// returns the topologically-sorted flat list of files to feed the
    /// type-checker.
    let loadAll (libRoot: string) : Result<LoadedLib, string> =
        let rootPath = Path.Combine(libRoot, "manifest.toml")

        if not (File.Exists rootPath) then
            Error(sprintf "Root manifest not found: %s" rootPath)
        else
            match Toml.parse (File.ReadAllText rootPath) with
            | Error e -> Error(sprintf "Root manifest parse error: %s" e)
            | Ok doc ->
                match parseRootManifest doc with
                | Error e -> Error e
                | Ok root ->
                    match topoSort root.Buckets with
                    | Error e -> Error e
                    | Ok ordered ->
                        let mutable err = None
                        let files = ResizeArray()

                        for bucket in ordered do
                            if err.IsNone then
                                let bp = Path.Combine(libRoot, bucket.Path, "manifest.toml")

                                if not (File.Exists bp) then
                                    err <- Some(sprintf "Bucket manifest not found: %s" bp)
                                else
                                    match Toml.parse (File.ReadAllText bp) with
                                    | Error e ->
                                        err <- Some(sprintf "Bucket manifest parse error (%s): %s" bucket.Name e)
                                    | Ok bdoc ->
                                        match parseBucketManifest bdoc with
                                        | Error e -> err <- Some(sprintf "%s bucket: %s" bucket.Name e)
                                        | Ok fileList ->
                                            for rel in fileList do
                                                files.Add
                                                    {
                                                        BucketName = bucket.Name
                                                        Relative = rel
                                                        Absolute = Path.Combine(libRoot, bucket.Path, rel)
                                                    }

                        match err with
                        | Some e -> Error e
                        | None ->
                            Ok
                                {
                                    Root = root
                                    Files = List.ofSeq files
                                }

    /// The lexer's token table and source text are retained so subsequent
    /// passes can extract identifier text off a `SyntaxToken`.
    type ParsedFile =
        {
            File: LibFile
            Input: string
            Lexed: Lexed
            Ast: FSharpAst<SyntaxToken>
        }

    /// Force-load the parser's `ObjectConstruction` ref so attribute
    /// parsing succeeds even when the only entry points hit are
    /// signature-file parsers. The init lives behind a `do` at the head
    /// of `ImplementationFile.pNamedModule`, which a pure-signature path
    /// may never touch.
    do ObjectConstruction.init ()

    /// Parse one `.fsi` file via XParsec.FSharp's signature-file parser.
    /// `.fs` files (rare — only when no signature exists, e.g. `SI.fs`)
    /// are routed through the implementation parser.
    let parseFileFull (file: LibFile) : Result<ParsedFile, string> =
        let raw = File.ReadAllText file.Absolute
        let input = raw.Replace("\r\n", "\n")

        match Lexing.lexString input with
        | Error _ -> Error(sprintf "Lex error in %s" file.Relative)
        | Ok lexed ->
            let reader = Reader.ofLexed lexed input Set.empty

            let result =
                if file.Relative.EndsWith ".fsi" then
                    FSharpAst.parseSignature reader
                else
                    FSharpAst.parse reader

            match result with
            | Ok ast ->
                Ok
                    {
                        File = file
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }
            | Error e -> Error(ErrorFormatting.splitAndFormatTokenErrors e)

    let parseFile (file: LibFile) : Result<FSharpAst<SyntaxToken>, string> =
        parseFileFull file |> Result.map (fun p -> p.Ast)

    /// Mutable accumulator threaded through `extractSymbols` across every
    /// file in `LoadedLib.Files`. Cross-bucket references resolve through
    /// the accumulated tables.
    [<Sealed>]
    type ExtractCtx() =
        member val Symbols = Dictionary<string, ExternalSymbol>(StringComparer.Ordinal) with get
        /// File-level diagnostics — parse failures, AST-shape rejections.
        /// Kept distinct from `Skipped`; tests filter against them to detect
        /// parsing regressions.
        member val Diagnostics = ResizeArray<LibFile * string>() with get
        /// Per-val extraction failures. Captured separately from
        /// `Diagnostics` so file-level health stays visible: an .fsi can
        /// extract 90% of its vals fine and still parse cleanly.
        member val Skipped = ResizeArray<LibFile * string>() with get
        /// Type-name index: short name -> (arity, compiledName). Multiple
        /// entries with the same short name are allowed; first declaration
        /// wins (warn-and-take-first per the plan).
        member val Types = Dictionary<string, int * string>(StringComparer.Ordinal) with get
        /// Every registered qualified compiled name. Lets the resolver accept
        /// fully-qualified references that disagree with the short-name index
        /// (e.g. cross-bucket name clashes the first-wins rule otherwise hides).
        member val QualifiedTypes = HashSet<string>(StringComparer.Ordinal) with get
        /// Type-shape index: qualified compiled name -> body shape. Only
        /// records, unions, and abbreviations are populated in v1; classes
        /// and other shapes land later.
        member val TypeShapes = Dictionary<string, ExternalTypeShape>(StringComparer.Ordinal) with get
        /// Qualified names of `[<AutoOpen>]` modules encountered during
        /// extraction, in source order (`"Vesper.ArithmeticOperators"`). A
        /// referenced contract surfaces these as its ambient open-prefix set so a
        /// consumer resolves `op_Addition` / `hash` with no explicit `open`
        /// (symbol-resolution-handoff.md, open-resolution). The hardcoded FSharp.Core prelude list
        /// stays separate (compiler-magic opens not expressible as `[<AutoOpen>]`).
        member val AutoOpenPrefixes = ResizeArray<string>() with get

    /// Auto-open prefixes that F# applies implicitly to FSharp.Core symbols.
    /// Surfaced as the provider's `IAmbientOpenScope` (no longer a
    /// provider-internal retry): the pipeline seeds them into the open scope and
    /// probes them BEHIND explicit `open`s — `1 + 2`'s desugared `op_Addition`
    /// lives in `Microsoft.FSharp.Core.Operators`, not at the root, and resolves
    /// through the ambient (symbol-resolution-handoff.md, open-resolution). Order matches F#'s prelude
    /// open order; earlier entries win on collision.
    let private autoOpenPrefixes =
        [
            "Microsoft.FSharp.Core.Operators"
            "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators"
            "Microsoft.FSharp.Core.ExtraTopLevelOperators"
            "Microsoft.FSharp.Core"
            "Microsoft.FSharp.Collections"
            "Microsoft.FSharp.Control"
        ]

    module ExtractCtx =
        let empty () = ExtractCtx()

        let toProvider (ctx: ExtractCtx) : IExternalSymbolProvider =
            // Direct hits only: the implicit prelude is the `IAmbientOpenScope`
            // below, applied by the pipeline behind explicit opens — not a
            // provider-internal retry that would shadow an explicit open by
            // resolving the bare name to a prelude symbol first (O3).
            { new IExternalSymbolProvider with
                member _.TryLookup(name) =
                    match ctx.Symbols.TryGetValue name with
                    | true, sym -> ValueSome sym
                    | _ -> ValueNone

                member _.TryLookupType(name) =
                    match ctx.TypeShapes.TryGetValue name with
                    | true, shape -> ValueSome shape
                    | _ -> ValueNone

                member _.TryLookupMember(_, _) = ValueNone

              interface IAmbientOpenScope with
                  member _.AmbientOpenPrefixes = autoOpenPrefixes
            }

    let private nameOfTok (lexed: Lexed) (input: string) (tok: SyntaxToken) : string =
        match tok.Index with
        | TokenIndex.Regular iT -> lexed.GetTokenString(iT, input)
        | TokenIndex.Virtual -> ""

    let private longIdentName (lexed: Lexed) (input: string) (li: LongIdent<SyntaxToken>) : string =
        let parts =
            [ for i in 0 .. li.Idents.Length - 1 -> nameOfTok lexed input li.Idents.[i] ]

        String.concat "." parts

    let private longIdentShortName (lexed: Lexed) (input: string) (li: LongIdent<SyntaxToken>) : string =
        if li.Idents.Length = 0 then
            ""
        else
            nameOfTok lexed input li.Idents.[li.Idents.Length - 1]

    /// Map an operator token (`+`, `<|`, etc.) to its compiled name
    /// (`op_Addition`, `op_PipeLeft`, etc.). The Token-enum match covers the
    /// well-known operators with dedicated enum values; the text fallback
    /// covers generic operators the lexer collapses to `OpGeneric`.
    let private opTokenToCompiled (tok: SyntaxToken) (text: string) : string voption =
        match tok.Token with
        | Token.OpAddition -> ValueSome "op_Addition"
        | Token.OpSubtraction -> ValueSome "op_Subtraction"
        | Token.OpMultiply -> ValueSome "op_Multiply"
        | Token.OpDivision -> ValueSome "op_Division"
        | Token.OpModulus -> ValueSome "op_Modulus"
        | Token.OpLessThan -> ValueSome "op_LessThan"
        | Token.OpGreaterThan -> ValueSome "op_GreaterThan"
        | Token.OpLessThanOrEqual -> ValueSome "op_LessThanOrEqual"
        | Token.OpGreaterThanOrEqual -> ValueSome "op_GreaterThanOrEqual"
        | Token.OpEquality -> ValueSome "op_Equality"
        | Token.OpInequality -> ValueSome "op_Inequality"
        | Token.OpAmpAmp -> ValueSome "op_BooleanAnd"
        | Token.OpBarBar -> ValueSome "op_BooleanOr"
        | Token.OpPipeRight -> ValueSome "op_PipeRight"
        | Token.OpPipeLeft -> ValueSome "op_PipeLeft"
        | Token.OpComposeRight -> ValueSome "op_ComposeRight"
        | Token.OpComposeLeft -> ValueSome "op_ComposeLeft"
        | _ ->
            // Only operators that appear inside `.fsi` val sigs need to land
            // here; the type checker picks them up by compiled name.
            match text with
            | "|>" -> ValueSome "op_PipeRight"
            | "<|" -> ValueSome "op_PipeLeft"
            | ">>" -> ValueSome "op_ComposeRight"
            | "<<" -> ValueSome "op_ComposeLeft"
            | "||>" -> ValueSome "op_PipeRight2"
            | "<||" -> ValueSome "op_PipeLeft2"
            | "|||>" -> ValueSome "op_PipeRight3"
            | "<|||" -> ValueSome "op_PipeLeft3"
            | "@" -> ValueSome "op_Append"
            | "^" -> ValueSome "op_Concatenate"
            | "?" -> ValueSome "op_Dynamic"
            | "?<-" -> ValueSome "op_DynamicAssignment"
            | ".." -> ValueSome "op_Range"
            | ".. .." -> ValueSome "op_RangeStep"
            | _ -> ValueNone

    let private identOrOpName (lexed: Lexed) (input: string) (io: IdentOrOp<SyntaxToken>) : string voption =
        match io with
        | IdentOrOp.Ident tok -> ValueSome(nameOfTok lexed input tok)
        | IdentOrOp.ParenOp(_, OpName.SymbolicOp opTok, _) -> opTokenToCompiled opTok (nameOfTok lexed input opTok)
        | IdentOrOp.ParenOp(_, OpName.RangeOp(RangeOpName.DotDot _), _) -> ValueSome "op_Range"
        | IdentOrOp.ParenOp(_, OpName.RangeOp(RangeOpName.DotDotDotDot _), _) -> ValueSome "op_RangeStep"
        | IdentOrOp.StarOp _ -> ValueSome "op_Multiply"
        | IdentOrOp.ParenOp(_, OpName.NilOp _, _) -> ValueSome "op_Nil"
        | IdentOrOp.ParenOp(_, OpName.ActivePatternOp _, _) ->
            // Active-pattern compiled names are non-trivial — defer.
            ValueNone

    /// First attribute whose short name (last segment) matches a candidate,
    /// ignoring the optional `Attribute` suffix.
    let private findAttribute
        (lexed: Lexed)
        (input: string)
        (attrs: Attributes<SyntaxToken> voption)
        (candidates: string list)
        : ObjectConstruction<SyntaxToken> voption =
        let candidateSet = Set.ofList candidates

        let matchName (name: string) =
            let trimmed =
                if name.EndsWith "Attribute" then
                    name.Substring(0, name.Length - "Attribute".Length)
                else
                    name

            Set.contains trimmed candidateSet

        let mutable found = ValueNone

        match attrs with
        | ValueNone -> ()
        | ValueSome sets ->
            for i in 0 .. sets.Length - 1 do
                if found.IsNone then
                    let (AttributeSet(_, items, _)) = sets.[i]

                    for j in 0 .. items.Length - 1 do
                        if found.IsNone then
                            let (Attribute(_, construction), _) = items.[j]

                            let typ =
                                match construction with
                                | ObjectConstruction(typ = t)
                                | InterfaceConstruction(typ = t) -> t

                            let attrName =
                                match typ with
                                | Type.NamedType li -> longIdentShortName lexed input li
                                | Type.GenericType(li, _, _, _, _) -> longIdentShortName lexed input li
                                | _ -> ""

                            if matchName attrName then
                                found <- ValueSome construction

        found

    let private constructionExpr (oc: ObjectConstruction<SyntaxToken>) : Expr<SyntaxToken> voption =
        match oc with
        | ObjectConstruction(_, e) -> ValueSome e
        | InterfaceConstruction _ -> ValueNone

    /// Text of a parsed string-literal expression. Ignores expression holes
    /// and other interpolation artefacts (compiled-name args are non-
    /// interpolated strings in practice).
    let private stringExprText
        (lexed: Lexed)
        (input: string)
        (parts: System.Collections.Immutable.ImmutableArray<StringPart<SyntaxToken>>)
        : string =
        let sb = System.Text.StringBuilder()

        for i in 0 .. parts.Length - 1 do
            match parts.[i] with
            | StringPart.Text tok -> sb.Append(nameOfTok lexed input tok) |> ignore
            | StringPart.EscapeSequence tok ->
                let raw = nameOfTok lexed input tok
                // Preserve source-level text; full escape decoding is the
                // lexer's job and unneeded for attribute args.
                sb.Append raw |> ignore
            | _ -> ()

        sb.ToString()

    let private tryCompiledName
        (lexed: Lexed)
        (input: string)
        (attrs: Attributes<SyntaxToken> voption)
        : string voption =
        match findAttribute lexed input attrs [ "CompiledName" ] with
        | ValueNone -> ValueNone
        | ValueSome oc ->
            match constructionExpr oc with
            | ValueNone -> ValueNone
            | ValueSome argExpr ->
                // Argument is `("Foo")`; accept either `Expr.String` (typical)
                // or an older `Expr.Const(Constant.Literal _)` fallback.
                let rec stripParens (e: Expr<SyntaxToken>) =
                    match e with
                    | Expr.EnclosedBlock(_, inner, _) -> stripParens inner
                    | _ -> e

                match stripParens argExpr with
                | Expr.String(_, parts, _) ->
                    let s = stringExprText lexed input parts
                    if s.Length > 0 then ValueSome s else ValueNone
                | Expr.Const(Constant.Literal tok) ->
                    let raw = nameOfTok lexed input tok
                    let trimmed = raw.Trim([| '"' |])
                    if trimmed.Length > 0 then ValueSome trimmed else ValueNone
                | _ -> ValueNone

    /// True iff the module-level attributes carry `[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]`.
    let private hasModuleSuffix (lexed: Lexed) (input: string) (attrs: Attributes<SyntaxToken> voption) : bool =
        match findAttribute lexed input attrs [ "CompilationRepresentation" ] with
        | ValueNone -> false
        | ValueSome oc ->
            match constructionExpr oc with
            | ValueNone -> false
            | ValueSome argExpr ->
                // Heuristic: look for the token text "ModuleSuffix" anywhere
                // in the expression. Good enough for the v1 walker.
                let rec containsModuleSuffix (e: Expr<SyntaxToken>) =
                    match e with
                    | Expr.EnclosedBlock(_, inner, _) -> containsModuleSuffix inner
                    | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) ->
                        let name = longIdentShortName lexed input li
                        name = "ModuleSuffix"
                    | Expr.DotLookup(_, _, LongIdentOrOp.LongIdent li) ->
                        let name = longIdentShortName lexed input li
                        name = "ModuleSuffix"
                    | _ -> false

                containsModuleSuffix argExpr

    /// True iff the module-level attributes carry `[<AutoOpen>]` — the module's
    /// members are in scope unqualified for a consumer of the package. Recorded
    /// as an ambient open prefix (symbol-resolution-handoff.md, open-resolution).
    let private isAutoOpen (lexed: Lexed) (input: string) (attrs: Attributes<SyntaxToken> voption) : bool =
        findAttribute lexed input attrs [ "AutoOpen" ] |> ValueOption.isSome

    /// SemType template parameterised over a fresh-TyVar array (one per
    /// declared typar in the val's merged typar list). Independent
    /// invocations of `Instantiate level` allocate a fresh array, so two
    /// call sites of the same val never share TyVars.
    type private SemBuilder = SemType[] -> SemType

    [<RequireQualifiedAccess>]
    type private TyparKind =
        | Regular
        | Static

    /// Order-preserving typar collector. The first time a name is seen it
    /// gets the next index; subsequent occurrences re-use it. Explicit
    /// `<'T>` typars are added first by the caller before the body walk,
    /// so they keep their declared positions; implicit (body-only) typars
    /// pick up indices in source order.
    [<Sealed>]
    type private TyparCollector() =
        let dict = Dictionary<string, int>(StringComparer.Ordinal)
        let order = ResizeArray<string * TyparKind>()

        member _.IndexOf(name: string, kind: TyparKind) : int =
            match dict.TryGetValue name with
            | true, idx -> idx
            | _ ->
                let idx = order.Count
                dict.[name] <- idx
                order.Add((name, kind))
                idx

        /// Index for `name` if already registered, `ValueNone` otherwise.
        /// Used by the constraint resolver to map captured typar names to
        /// their typar-list positions *without* introducing extra typars.
        member _.TryIndexOf(name: string) : int voption =
            match dict.TryGetValue name with
            | true, idx -> ValueSome idx
            | _ -> ValueNone

        member _.Count = order.Count
        member _.Entries = order.ToArray()

    /// Raw `when`-clause capture, before typar names are mapped to indices.
    [<RequireQualifiedAccess>]
    type private RawConstraint =
        | Trait of typarName: string * kind: SemanticConstraintKind
        /// Captured `when (^T or ^U) : (static member (+) : ^T * ^U -> ^V)`.
        /// `memberName` is the compiled name. `argTypes` / `returnType` are
        /// raw CST types so translation runs later — they may reference
        /// typars that the body walk hasn't registered yet.
        | MemberTrait of
            typarNames: string list *
            memberName: string *
            argTypes: Type<SyntaxToken> list *
            returnType: Type<SyntaxToken>
        /// `target` is the unresolved RHS of `default ^T : <type>`. Stored
        /// as a `Type<SyntaxToken>` so translation can run later, after the
        /// body walk has registered every typar the target might reference.
        | Default of typarName: string * target: Type<SyntaxToken>

    [<Sealed>]
    type private ConstraintCollector() =
        let items = ResizeArray<RawConstraint>()
        member _.Add(c: RawConstraint) = items.Add c
        member _.Snapshot() = List.ofSeq items
        member _.Count = items.Count

    /// Resolve a long-identifier type name against `ctx.Types`, using the
    /// per-file open prefixes (newest first) as candidate qualifiers when
    /// the short-name lookup misses. Returns the canonical compiled name
    /// on success; `Error` with a brief reason on failure so the caller
    /// can attach a per-file diagnostic and skip the val.
    ///
    /// Resolution order:
    ///   1. Direct hit on the qualified name as written.
    ///   2. Short-name lookup in `ctx.Types`. Arity mismatch still resolves
    ///      (cross-file disagreements shouldn't block extraction) but uses
    ///      the recorded compiled name.
    ///   3. For each open prefix in newest-first order, try
    ///      `prefix + "." + name` against the qualified-name set, then
    ///      strip that to a short name and re-check `ctx.Types`.
    let private resolveTypeName
        (ctx: ExtractCtx)
        (openPrefixes: string list)
        (name: string)
        (arity: int)
        : Result<string, string> =
        let short =
            let dot = name.LastIndexOf '.'
            if dot < 0 then name else name.Substring(dot + 1)

        if ctx.QualifiedTypes.Contains name then
            Ok name
        else
            match ctx.Types.TryGetValue short with
            | true, (_, compiled) ->
                // Short-name hit. Arity disagreement is tolerated; the
                // recorded compiled name still beats a placeholder.
                ignore arity
                Ok compiled
            | _ ->
                let mutable hit = ValueNone

                for prefix in openPrefixes do
                    if hit.IsNone then
                        let candidate = prefix + "." + name

                        if ctx.QualifiedTypes.Contains candidate then
                            hit <- ValueSome candidate

                match hit with
                | ValueSome c -> Ok c
                | ValueNone -> Error(sprintf "Unresolved type name '%s'" name)

    let private isPrimitiveName (s: string) =
        match s with
        | "int"
        | "int8"
        | "int16"
        | "int32"
        | "int64"
        | "uint"
        | "uint8"
        | "uint16"
        | "uint32"
        | "uint64"
        | "byte"
        | "sbyte"
        | "nativeint"
        | "unativeint"
        | "float"
        | "float32"
        | "double"
        | "single"
        | "decimal"
        | "char"
        | "string"
        | "bool"
        | "unit"
        | "obj"
        | "objnull"
        | "voidptr"
        | "exn" -> true
        | _ -> false

    /// Source-text name of a typar (the part after `'` or `^`), or
    /// `ValueNone` for anonymous typars (whose constraint participation
    /// can't be addressed by name later).
    let private typarName (lexed: Lexed) (input: string) (t: Typar<SyntaxToken>) : string voption =
        match t with
        | Typar.Named(_, identTok)
        | Typar.Static(_, identTok) -> ValueSome(nameOfTok lexed input identTok)
        | Typar.Anon _ -> ValueNone

    /// Walk a `when …` clause and emit `RawConstraint` entries into the
    /// collector. Trait-style constraints (Equality/Comparison/etc.) flow
    /// through directly; SRTP member traits and defaults are captured as
    /// opaque markers for Phase 5b. Unsupported constraint shapes
    /// (`Coercion`, `Enum`, …) silently drop in v1.
    let private captureConstraints
        (lexed: Lexed)
        (input: string)
        (acc: ConstraintCollector)
        (clauses: TyparConstraints<SyntaxToken>)
        : unit =
        let (TyparConstraints(_, items, _)) = clauses

        for i in 0 .. items.Length - 1 do
            match items.[i] with
            | Constraint.Equality(t, _, _) ->
                match typarName lexed input t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.Equality))
                | ValueNone -> ()
            | Constraint.Comparison(t, _, _) ->
                match typarName lexed input t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.Comparison))
                | ValueNone -> ()
            | Constraint.Struct(t, _, _) ->
                match typarName lexed input t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.Struct))
                | ValueNone -> ()
            | Constraint.ReferenceType(t, _, _, _) ->
                match typarName lexed input t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.ReferenceType))
                | ValueNone -> ()
            | Constraint.Nullness(t, _, _) ->
                match typarName lexed input t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.Nullness))
                | ValueNone -> ()
            | Constraint.NotNull(t, _, _, _) ->
                match typarName lexed input t with
                | ValueSome n -> acc.Add(RawConstraint.Trait(n, SemanticConstraintKind.NotNull))
                | ValueNone -> ()
            | Constraint.MemberTrait(staticTypars, _, _, _, _, memberSig, _) ->
                let names =
                    match staticTypars with
                    | StaticTypars.Single t ->
                        match typarName lexed input t with
                        | ValueSome n -> [ n ]
                        | ValueNone -> []
                    | StaticTypars.OrList(_, items, _, _) ->
                        [
                            for k in 0 .. items.Length - 1 do
                                match typarName lexed input items.[k] with
                                | ValueSome n -> yield n
                                | ValueNone -> ()
                        ]

                if not (List.isEmpty names) then
                    // `static member (+) : ^T1 * ^T2 -> ^T3` parses as a
                    // `CurriedSig` (args as one asterisk-joined `ArgsSpec`).
                    // Property sigs (`Zero : ^T`) are a zero-arg curried sig.
                    let sign =
                        match memberSig with
                        | MemberSig.MethodOrPropSig(ident = ident; sign = s) -> ValueSome(ident, s)
                        | MemberSig.PropSig(ident = ident; sign = s) -> ValueSome(ident, s)

                    match sign with
                    | ValueNone -> ()
                    | ValueSome(ident, CurriedSig(args, retTy)) ->
                        match identOrOpName lexed input ident with
                        | ValueNone -> ()
                        | ValueSome mName ->
                            // F# trait sigs are tupled by convention
                            // (`^T * ^T -> ^T`), parsing as one ArgsSpec with
                            // N args; flatten it into the trait's arg list.
                            let argTys =
                                [
                                    for k in 0 .. args.Length - 1 do
                                        let struct (ArgsSpec(specs, _), _) = args.[k]

                                        for j in 0 .. specs.Length - 1 do
                                            let (ArgSpec(_, _, t)) = specs.[j]
                                            yield t
                                ]

                            acc.Add(RawConstraint.MemberTrait(names, mName, argTys, retTy))
            | Constraint.Default(_, t, _, target) ->
                match typarName lexed input t with
                | ValueSome n -> acc.Add(RawConstraint.Default(n, target))
                | ValueNone -> ()
            | Constraint.Coercion _
            | Constraint.DefaultConstructor _
            | Constraint.Enum _
            | Constraint.Unmanaged _
            | Constraint.Delegate _ ->
                // v1 silently drops; Phase 5b extends as needed.
                ()

    let rec private translateType
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (typars: TyparCollector)
        (constraints: ConstraintCollector)
        (typ: Type<SyntaxToken>)
        : Result<SemBuilder, string> =
        match typ with
        | Type.ParenType(_, inner, _) -> translateType ctx lexed input opens typars constraints inner

        | Type.FunctionType(a, _, b) ->
            match translateType ctx lexed input opens typars constraints a with
            | Error e -> Error e
            | Ok fa ->
                match translateType ctx lexed input opens typars constraints b with
                | Error e -> Error e
                | Ok fb -> Ok(fun ts -> TyFun(fa ts, fb ts))

        | Type.TupleType(parts, _)
        | Type.StructTupleType(_, _, parts, _, _) ->
            let mutable err = None
            let builders = ResizeArray<SemBuilder>(parts.Length)

            for i in 0 .. parts.Length - 1 do
                if err.IsNone then
                    match translateType ctx lexed input opens typars constraints parts.[i] with
                    | Error e -> err <- Some e
                    | Ok b -> builders.Add b

            match err with
            | Some e -> Error e
            | None ->
                let bs = builders.ToArray()
                Ok(fun ts -> TyTuple [ for b in bs -> b ts ])

        | Type.VarType(Typar.Named(_, identTok)) ->
            let name = nameOfTok lexed input identTok
            let idx = typars.IndexOf(name, TyparKind.Regular)
            Ok(fun ts -> ts.[idx])

        | Type.VarType(Typar.Static(_, identTok)) ->
            let name = nameOfTok lexed input identTok
            let idx = typars.IndexOf(name, TyparKind.Static)
            Ok(fun ts -> ts.[idx])

        | Type.VarType(Typar.Anon _) ->
            // Synthetic name so distinct anonymous typars don't collide.
            let synthetic = sprintf "_anon%d" typars.Count
            let idx = typars.IndexOf(synthetic, TyparKind.Regular)
            Ok(fun ts -> ts.[idx])

        | Type.NamedType li ->
            let name = longIdentName lexed input li

            if isPrimitiveName name then
                let ty = TyConst name
                Ok(fun _ -> ty)
            else
                match resolveTypeName ctx opens name 0 with
                | Error e -> Error e
                | Ok compiled ->
                    let ty = TyConst compiled
                    Ok(fun _ -> ty)

        | Type.GenericType(li, _, args, _, _) ->
            let name = longIdentName lexed input li
            let mutable err = None
            let builders = ResizeArray<SemBuilder>(args.Length)

            for i in 0 .. args.Length - 1 do
                if err.IsNone then
                    match args.[i] with
                    | TypeArg.Type t ->
                        match translateType ctx lexed input opens typars constraints t with
                        | Error e -> err <- Some e
                        | Ok b -> builders.Add b
                    | TypeArg.Measure _ -> err <- Some "Measure arg not supported"

            match err with
            | Some e -> Error e
            | None ->
                let bs = builders.ToArray()

                match resolveTypeName ctx opens name bs.Length with
                | Error e -> Error e
                | Ok compiled -> Ok(fun ts -> TyRecord(compiled, [ for b in bs -> b ts ]))

        | Type.SuffixedType(baseTy, li) ->
            // `'T list` ≡ `List<'T>`.
            let name = longIdentName lexed input li

            match translateType ctx lexed input opens typars constraints baseTy with
            | Error e -> Error e
            | Ok fb ->
                match resolveTypeName ctx opens name 1 with
                | Error e -> Error e
                | Ok compiled -> Ok(fun ts -> TyRecord(compiled, [ fb ts ]))

        | Type.ArrayType(baseTy, _, commas, _) ->
            // rank = commas + 1; key by `array<rank>` so unification stays simple.
            let rank = commas.Length + 1

            match translateType ctx lexed input opens typars constraints baseTy with
            | Error e -> Error e
            | Ok fb ->
                let name = if rank = 1 then "array" else sprintf "array%d" rank
                Ok(fun ts -> TyRecord(name, [ fb ts ]))

        | Type.WhenConstrainedType(inner, clauses) ->
            // Capture clauses now; the collector is resolved (name -> index)
            // only once the body is fully walked, so a clause referencing a
            // typar declared anywhere in the val still resolves.
            captureConstraints lexed input constraints clauses
            translateType ctx lexed input opens typars constraints inner

        | Type.SubtypeConstraint(_, _, inner)
        | Type.AnonymousSubtype(_, inner) -> translateType ctx lexed input opens typars constraints inner

        | Type.DottedType(baseTy, _, _) ->
            // Treat `T.NestedName` as opaque (pass the base); proper nested-type
            // modelling lands later.
            translateType ctx lexed input opens typars constraints baseTy

        | Type.UnionType _ -> Error "Union types (e.g. `obj | null`) not supported"
        | Type.Null _ -> Error "Null types not supported"
        | Type.ILIntrinsic _ -> Error "Inline IL not supported"
        | Type.MeasureType _ -> Error "Measure types not supported"
        | Type.AnonRecordType _ -> Error "Anonymous record types not supported"
        | Type.Missing -> Error "Missing type"
        | Type.SkipsTokens _ -> Error "Recovery-skipped type"

    let private translateArgsSpec
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (typars: TyparCollector)
        (constraints: ConstraintCollector)
        (argsSpec: ArgsSpec<SyntaxToken>)
        : Result<SemBuilder, string> =
        let (ArgsSpec(args, _)) = argsSpec

        if args.Length = 0 then
            Ok(fun _ -> TyConst "unit")
        elif args.Length = 1 then
            let (ArgSpec(_, _, t)) = args.[0]
            translateType ctx lexed input opens typars constraints t
        else
            let mutable err = None
            let builders = ResizeArray<SemBuilder>(args.Length)

            for i in 0 .. args.Length - 1 do
                if err.IsNone then
                    let (ArgSpec(_, _, t)) = args.[i]

                    match translateType ctx lexed input opens typars constraints t with
                    | Error e -> err <- Some e
                    | Ok b -> builders.Add b

            match err with
            | Some e -> Error e
            | None ->
                let bs = builders.ToArray()
                Ok(fun ts -> TyTuple [ for b in bs -> b ts ])

    let private translateCurriedSig
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (typars: TyparCollector)
        (constraints: ConstraintCollector)
        (sigCurried: CurriedSig<SyntaxToken>)
        : Result<SemBuilder, string> =
        let (CurriedSig(args, retTy)) = sigCurried

        match translateType ctx lexed input opens typars constraints retTy with
        | Error e -> Error e
        | Ok retBuilder ->
            // Args nest right-assoc:
            //   `int -> string -> bool` ≡ `TyFun(int, TyFun(string, bool))`.
            let mutable err = None
            let argBuilders = ResizeArray<SemBuilder>(args.Length)

            for i in 0 .. args.Length - 1 do
                if err.IsNone then
                    let (struct (argsSpec, _)) = args.[i]

                    match translateArgsSpec ctx lexed input opens typars constraints argsSpec with
                    | Error e -> err <- Some e
                    | Ok b -> argBuilders.Add b

            match err with
            | Some e -> Error e
            | None ->
                let argArr = argBuilders.ToArray()

                let final =
                    fun (ts: SemType[]) ->
                        let mutable acc = retBuilder ts

                        for k in argArr.Length - 1 .. -1 .. 0 do
                            acc <- TyFun(argArr.[k] ts, acc)

                        acc

                Ok final

    let private registerExplicitTypars
        (lexed: Lexed)
        (input: string)
        (typars: TyparCollector)
        (defns: TyparDefns<SyntaxToken> voption)
        : unit =
        match defns with
        | ValueNone -> ()
        | ValueSome(TyparDefns(_, items, _, _)) ->
            for i in 0 .. items.Length - 1 do
                let (TyparDefn(_, typar)) = items.[i]

                match typar with
                | Typar.Named(_, identTok) ->
                    let name = nameOfTok lexed input identTok
                    typars.IndexOf(name, TyparKind.Regular) |> ignore
                | Typar.Static(_, identTok) ->
                    let name = nameOfTok lexed input identTok
                    typars.IndexOf(name, TyparKind.Static) |> ignore
                | Typar.Anon _ -> ()

    let private isAccessible (access: Access<SyntaxToken> voption) : bool =
        match access with
        | ValueNone
        | ValueSome(Access.Public _) -> true
        | ValueSome(Access.Internal _)
        | ValueSome(Access.Private _) -> false

    /// Build the compiled name for a val. Respects `[<CompiledName(_)>]`
    /// (overrides the source ident) and the `ModuleSuffix` flag (already
    /// baked into `path`'s last segment by the walker).
    let private compiledNameForVal
        (lexed: Lexed)
        (input: string)
        (path: string list)
        (attrs: Attributes<SyntaxToken> voption)
        (ident: IdentOrOp<SyntaxToken>)
        : string voption =
        let identName =
            match tryCompiledName lexed input attrs with
            | ValueSome n -> ValueSome n
            | ValueNone -> identOrOpName lexed input ident

        match identName with
        | ValueNone -> ValueNone
        | ValueSome n ->
            let qualifier = String.concat "." (List.rev path)

            if qualifier.Length = 0 then
                ValueSome n
            else
                ValueSome(qualifier + "." + n)

    /// Resolve a `RawConstraint`'s typar names against the val's typar
    /// collector, dropping entries that reference an undeclared typar.
    /// `Trait` and `MemberTrait` entries fold to opaque markers; `Default`
    /// entries translate their target `Type<SyntaxToken>` through the same
    /// `translateType` path the val signature used, so the default's RHS
    /// resolves typar references through the same indexing scheme.
    /// Entries whose target fails translation (e.g. references an unknown
    /// type name) silently drop — the default would be unusable in
    /// Instantiate anyway.
    let private resolveConstraints
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (typars: TyparCollector)
        (raw: RawConstraint list)
        : ExternalConstraint list =
        // Sink for any constraints a target type carries (exotic, not
        // surfaced on the symbol).
        let throwaway = ConstraintCollector()

        raw
        |> List.choose (fun rc ->
            match rc with
            | RawConstraint.Trait(n, kind) ->
                match typars.TryIndexOf n with
                | ValueSome i -> Some(ExternalConstraint.Trait(i, kind))
                | ValueNone -> None
            | RawConstraint.MemberTrait(names, memberName, argTys, retTy) ->
                let indices =
                    names
                    |> List.choose (fun n ->
                        match typars.TryIndexOf n with
                        | ValueSome i -> Some i
                        | ValueNone -> None
                    )

                if List.isEmpty indices then
                    None
                else
                    // Any translation failure drops the whole entry — better
                    // to under-stamp the trait than to mis-stamp it.
                    let mutable failed = false
                    let argBuilders = ResizeArray<SemBuilder>(argTys.Length)

                    for t in argTys do
                        if not failed then
                            match translateType ctx lexed input opens typars throwaway t with
                            | Error _ -> failed <- true
                            | Ok b -> argBuilders.Add b

                    if failed then
                        None
                    else
                        match translateType ctx lexed input opens typars throwaway retTy with
                        | Error _ -> None
                        | Ok retBuilder ->
                            Some(
                                ExternalConstraint.MemberTrait(indices, memberName, argBuilders.ToArray(), retBuilder)
                            )
            | RawConstraint.Default(n, target) ->
                match typars.TryIndexOf n with
                | ValueNone -> None
                | ValueSome i ->
                    match translateType ctx lexed input opens typars throwaway target with
                    | Error _ -> None
                    | Ok builder -> Some(ExternalConstraint.Default(i, builder))
        )

    let private extractValSig
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (path: string list)
        // The *source* module path (no `ModuleSuffix` rewrite). Differs from
        // `path` only inside a `[<CompilationRepresentation(ModuleSuffix)>]`
        // module (`List` ⇒ compiled `ListModule`). The provider is probed with the
        // *source*-qualified name the front end writes (`List.fold`, not
        // `ListModule.fold`), so the val is additionally registered under that
        // name; the compiled-name entry stays for the desugar/Freeze paths that key
        // on it (and the FSharpLib `…Module.Map` tests).
        (sourcePath: string list)
        (valSig: ValSig<SyntaxToken>)
        : unit =
        let (ValSig(attrs, _, _, access, _, ident, typars, _, signature, _)) = valSig

        if not (isAccessible access) then
            ()
        else
            match compiledNameForVal lexed input path attrs ident with
            | ValueNone -> ()
            | ValueSome compiled ->
                let collector = TyparCollector()
                registerExplicitTypars lexed input collector typars
                let constraints = ConstraintCollector()

                match translateCurriedSig ctx lexed input opens collector constraints signature with
                | Error e ->
                    // Skip so unresolved names don't masquerade as opaque
                    // TyConsts. Per-val skips go to `Skipped`, not
                    // `Diagnostics` (reserved for file-level parse failures).
                    ctx.Skipped.Add(file, sprintf "%s: %s" compiled e)
                | Ok build ->
                    let typarCount = collector.Count

                    let resolved =
                        resolveConstraints ctx lexed input opens collector (constraints.Snapshot())

                    // Trait-style entries are applied at instantiation time;
                    // everything else stays on the symbol for diagnostics.
                    let traitConstraints =
                        resolved
                        |> List.choose (fun c ->
                            match c with
                            | ExternalConstraint.Trait(i, k) -> Some(i, k)
                            | _ -> None
                        )

                    // Applied at instantiation time so the source TyVar's
                    // `Defaults` list carries the resolved target;
                    // generalisation walks the list when a TyVar is still free
                    // and links it to the first concrete shape.
                    let defaultConstraints =
                        resolved
                        |> List.choose (fun c ->
                            match c with
                            | ExternalConstraint.Default(i, builder) -> Some(i, builder)
                            | _ -> None
                        )

                    // Stamped on every participating fresh TyVar's `SrtpBounds`;
                    // `Unification.drainSrtpBounds` fires the first time any of
                    // them gets a `Link`, sharing a `Resolved` ref so the others
                    // no-op.
                    let memberTraitConstraints =
                        resolved
                        |> List.choose (fun c ->
                            match c with
                            | ExternalConstraint.MemberTrait(idxs, name, argBs, retB) -> Some(idxs, name, argBs, retB)
                            | _ -> None
                        )

                    let instantiate =
                        if typarCount = 0 then
                            let semType = build [||]
                            fun _ -> semType
                        else
                            fun level ->
                                let freshTvs =
                                    Array.init
                                        typarCount
                                        (fun _ ->
                                            let tv = TypeVar()
                                            tv.Level <- level
                                            tv
                                        )

                                let fresh = freshTvs |> Array.map TyVar

                                for (i, kind) in traitConstraints do
                                    if i >= 0 && i < freshTvs.Length then
                                        // External symbols carry no source-side
                                        // NodeKey; stamp `Unknown` so diagnostics
                                        // attribute the constraint to the use site.
                                        let cstr: SemanticConstraint =
                                            {
                                                Kind = kind
                                                DeclKey = NodeKey.ofSource 0 NodeKind.Unknown
                                            }

                                        freshTvs.[i].Constraints <- cstr :: freshTvs.[i].Constraints

                                // Defaults accumulate newest-last so source
                                // order is preserved when generalisation later
                                // walks the list for the first concrete shape.
                                for (i, builder) in defaultConstraints do
                                    if i >= 0 && i < freshTvs.Length then
                                        let target = builder fresh
                                        let tv = freshTvs.[i]
                                        tv.Defaults <- tv.Defaults @ [ target ]

                                // Shared `Resolved` ref dedupes dispatch:
                                // whichever participating typar resolves first
                                // runs the drain; the others see it flipped and
                                // skip.
                                for (idxs, mName, argBs, retB) in memberTraitConstraints do
                                    let argTys = [ for b in argBs -> b fresh ]
                                    let retTy = retB fresh

                                    let sig_: MemberSignature =
                                        {
                                            MemberName = mName
                                            ArgTypes = argTys
                                            ReturnType = retTy
                                            Resolved = false
                                        }

                                    for i in idxs do
                                        if i >= 0 && i < freshTvs.Length then
                                            let tv = freshTvs.[i]
                                            tv.SrtpBounds <- sig_ :: tv.SrtpBounds

                                build fresh

                    let sym: ExternalSymbol =
                        {
                            Name = compiled
                            Instantiate = instantiate
                            Constraints = resolved
                            Origin = SymbolOrigin.Empty
                        }

                    ctx.Symbols.[compiled] <- sym

                    // Source-name alias for a `ModuleSuffix` module's members
                    // (`List.fold` alongside the compiled `ListModule.fold`), so a
                    // front-end probe of the source-qualified name resolves. Only
                    // when it differs from the compiled name and nothing already
                    // claims it (first registration wins, like the type index).
                    match compiledNameForVal lexed input sourcePath attrs ident with
                    | ValueSome source when source <> compiled && not (ctx.Symbols.ContainsKey source) ->
                        ctx.Symbols.[source] <- { sym with Name = source }
                    | _ -> ()

    let private registerPrefixTypars
        (lexed: Lexed)
        (input: string)
        (typars: TyparCollector)
        (prefix: PrefixTypars<SyntaxToken> voption)
        : unit =
        let register (t: Typar<SyntaxToken>) =
            match t with
            | Typar.Named(_, identTok) ->
                let name = nameOfTok lexed input identTok
                typars.IndexOf(name, TyparKind.Regular) |> ignore
            | Typar.Static(_, identTok) ->
                let name = nameOfTok lexed input identTok
                typars.IndexOf(name, TyparKind.Static) |> ignore
            | Typar.Anon _ -> ()

        match prefix with
        | ValueNone -> ()
        | ValueSome(PrefixTypars.Single t) -> register t
        | ValueSome(PrefixTypars.Multiple(_, items, _, _)) ->
            for i in 0 .. items.Length - 1 do
                register items.[i]

    let private typeNameTypars
        (defns: TyparDefns<SyntaxToken> voption)
        (prefix: PrefixTypars<SyntaxToken> voption)
        : int =
        let prefixCount =
            match prefix with
            | ValueSome(PrefixTypars.Single _) -> 1
            | ValueSome(PrefixTypars.Multiple(_, items, _, _)) -> items.Length
            | ValueNone -> 0

        let defnCount =
            match defns with
            | ValueSome(TyparDefns(_, items, _, _)) -> items.Length
            | ValueNone -> 0

        max prefixCount defnCount

    /// Register a `type` declaration's short name + qualified compiled name.
    /// Returns the (compiled name, declared arity) tuple so the body
    /// extractor below can populate `ctx.TypeShapes` against the same key.
    /// `ValueNone` indicates the declaration was malformed (no ident).
    let private registerTypeDecl
        (ctx: ExtractCtx)
        (lexed: Lexed)
        (input: string)
        (path: string list)
        (typeName: TypeName<SyntaxToken>)
        : struct (string * int) voption =
        let (TypeName(_, _, prefix, ident, defns, _)) = typeName

        if ident.Idents.Length = 0 then
            ValueNone
        else
            let short = nameOfTok lexed input ident.Idents.[ident.Idents.Length - 1]

            if short.Length = 0 then
                ValueNone
            else
                let arity = typeNameTypars defns prefix

                let qualifier = String.concat "." (List.rev path)

                let compiled =
                    if qualifier.Length = 0 then
                        short
                    else
                        qualifier + "." + short

                // First declaration wins on collision.
                if not (ctx.Types.ContainsKey short) then
                    ctx.Types.[short] <- (arity, compiled)

                ctx.QualifiedTypes.Add compiled |> ignore
                ValueSome(struct (compiled, arity))

    let private collectorForTypeName (lexed: Lexed) (input: string) (typeName: TypeName<SyntaxToken>) : TyparCollector =
        let (TypeName(_, _, prefix, _, defns, _)) = typeName
        let collector = TyparCollector()
        registerPrefixTypars lexed input collector prefix
        registerExplicitTypars lexed input collector defns
        collector

    /// `Ok` only if every typar the body touched was declared in the type's
    /// prefix / typar-defns — otherwise the shape would be instantiable with
    /// a wrong-length array. v1 skips the body in that case but keeps the
    /// short-name registration so other types can still reference it nominally.
    let private bodyTyparsOk (collector: TyparCollector) (declaredArity: int) : Result<unit, string> =
        if collector.Count <= declaredArity then
            Ok()
        else
            Error(sprintf "body references %d typars but only %d declared" collector.Count declaredArity)

    let private extractAbbrevBody
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (compiled: string)
        (arity: int)
        (typeName: TypeName<SyntaxToken>)
        (rhs: Type<SyntaxToken>)
        : unit =
        let collector = collectorForTypeName lexed input typeName
        let throwawayConstraints = ConstraintCollector()

        match translateType ctx lexed input opens collector throwawayConstraints rhs with
        | Error e -> ctx.Skipped.Add(file, sprintf "type %s body: %s" compiled e)
        | Ok build ->
            match bodyTyparsOk collector arity with
            | Error e -> ctx.Skipped.Add(file, sprintf "type %s body: %s" compiled e)
            | Ok() -> ctx.TypeShapes.[compiled] <- ExternalTypeShape.Abbrev(arity, build)

    let private extractRecordBody
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (compiled: string)
        (arity: int)
        (typeName: TypeName<SyntaxToken>)
        (fields: RecordFields<SyntaxToken>)
        : unit =
        let collector = collectorForTypeName lexed input typeName
        let throwawayConstraints = ConstraintCollector()
        let shapes = ResizeArray<ExternalFieldShape>(fields.Length)
        let mutable err = None

        for i in 0 .. fields.Length - 1 do
            if err.IsNone then
                let (RecordField(_, mutableTok, _, identTok, _, fieldTy)) = fields.[i]

                match translateType ctx lexed input opens collector throwawayConstraints fieldTy with
                | Error e -> err <- Some e
                | Ok b ->
                    shapes.Add
                        {
                            Name = nameOfTok lexed input identTok
                            IsMutable = mutableTok.IsSome
                            BuildType = b
                        }

        match err with
        | Some e -> ctx.Skipped.Add(file, sprintf "type %s body: %s" compiled e)
        | None ->
            match bodyTyparsOk collector arity with
            | Error e -> ctx.Skipped.Add(file, sprintf "type %s body: %s" compiled e)
            | Ok() -> ctx.TypeShapes.[compiled] <- ExternalTypeShape.Record(arity, shapes.ToArray())

    let private extractUnionBody
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (compiled: string)
        (arity: int)
        (typeName: TypeName<SyntaxToken>)
        (cases: UnionTypeCases<SyntaxToken>)
        : unit =
        let collector = collectorForTypeName lexed input typeName
        let throwawayConstraints = ConstraintCollector()
        let caseShapes = ResizeArray<ExternalCaseShape>(cases.Length)
        let mutable err = None

        let caseName (ioo: IdentOrOp<SyntaxToken>) : string voption =
            match ioo with
            | IdentOrOp.Ident tok -> ValueSome(nameOfTok lexed input tok)
            | _ ->
                // Operator-named cases (FSharp.Core list's `([])`/`(::)`) are
                // named by their compiled-op form to stay uniquely identified.
                identOrOpName lexed input ioo

        for i in 0 .. cases.Length - 1 do
            if err.IsNone then
                let (UnionTypeCase(_, data)) = cases.[i]

                match data with
                | UnionTypeCaseData.Nullary ident ->
                    match caseName ident with
                    | ValueNone -> err <- Some "unnamed case"
                    | ValueSome n ->
                        caseShapes.Add
                            {
                                Name = n
                                FieldNames = [||]
                                BuildFieldTypes = [||]
                            }

                | UnionTypeCaseData.Nary(ident, _, fields, _) ->
                    match caseName ident with
                    | ValueNone -> err <- Some "unnamed case"
                    | ValueSome n ->
                        let names = ResizeArray<string voption>(fields.Length)
                        let builds = ResizeArray<SemBuilder>(fields.Length)

                        for j in 0 .. fields.Length - 1 do
                            if err.IsNone then
                                let nameOpt, fieldTy =
                                    match fields.[j] with
                                    | UnionTypeField.Unnamed t -> ValueNone, t
                                    | UnionTypeField.Named(identTok, _, t) ->
                                        ValueSome(nameOfTok lexed input identTok), t

                                match translateType ctx lexed input opens collector throwawayConstraints fieldTy with
                                | Error e -> err <- Some(sprintf "case %s field: %s" n e)
                                | Ok b ->
                                    names.Add nameOpt
                                    builds.Add b

                        if err.IsNone then
                            caseShapes.Add
                                {
                                    Name = n
                                    FieldNames = names.ToArray()
                                    BuildFieldTypes = builds.ToArray()
                                }

                | UnionTypeCaseData.GadtNary _
                | UnionTypeCaseData.GadtNullary _ ->
                    // GADT cases need richer machinery (return-type binding
                    // of typars). Phase 4 v1 skips.
                    err <- Some "GADT cases not supported"

        match err with
        | Some e -> ctx.Skipped.Add(file, sprintf "type %s body: %s" compiled e)
        | None ->
            match bodyTyparsOk collector arity with
            | Error e -> ctx.Skipped.Add(file, sprintf "type %s body: %s" compiled e)
            | Ok() -> ctx.TypeShapes.[compiled] <- ExternalTypeShape.Union(arity, caseShapes.ToArray())

    let private extractTypeSig
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (path: string list)
        (ts: TypeSignature<SyntaxToken>)
        : unit =
        match ts with
        | TypeSignature.Abbrev(typeName, _, rhs) ->
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                extractAbbrevBody ctx file lexed input opens compiled arity typeName rhs

        | TypeSignature.Record(typeName = typeName; fields = fields) ->
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                extractRecordBody ctx file lexed input opens compiled arity typeName fields

        | TypeSignature.Union(typeName = typeName; cases = cases) ->
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                extractUnionBody ctx file lexed input opens compiled arity typeName cases

        | TypeSignature.Interface(typeName = typeName) ->
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                // A class/interface shape carries no body (its members are
                // resolved separately via `TryLookupMember`), but registering it
                // here is what makes a nominal external type — `Vesper.Fun`,
                // `EqualityComparer<_>` — resolve through `TryLookupType` instead
                // of `ValueNone` (symbol-resolution-plan §4). `Origin` is stamped
                // by the resolving source (e.g. `ReferencedProject`), not here.
                ctx.TypeShapes.[compiled] <- ExternalTypeShape.Class(arity, true, SymbolOrigin.Empty)

        | TypeSignature.Anon(typeName = typeName)
        | TypeSignature.Class(typeName = typeName)
        | TypeSignature.Struct(typeName = typeName)
        | TypeSignature.Extern(typeName = typeName)
        | TypeSignature.AbstractType typeName ->
            // Nominal types with no front-end-modelled body shape (an `extern`
            // primitive like `int`, a class, an opaque abstract type). Resolve as
            // a non-interface `Class` so codegen can mint a ref off the origin.
            match registerTypeDecl ctx lexed input path typeName with
            | ValueNone -> ()
            | ValueSome(struct (compiled, arity)) ->
                ctx.TypeShapes.[compiled] <- ExternalTypeShape.Class(arity, false, SymbolOrigin.Empty)

        | TypeSignature.Enum(typeName = typeName)
        | TypeSignature.Delegate(typeName = typeName)
        | TypeSignature.TypeExtension(typeName = typeName) ->
            // Enum / delegate / type-extension body shapes land later; v1 only
            // registers the name+arity so other types can reference them nominally.
            registerTypeDecl ctx lexed input path typeName |> ignore

    /// Harvest the `open Foo.Bar` clauses from a flat element list. The
    /// caller prepends to its inherited list so a scope's own opens are
    /// tried *first* (newest-first) during resolution.
    let private collectOpens
        (lexed: Lexed)
        (input: string)
        (elems: System.Collections.Immutable.ImmutableArray<ModuleSignatureElement<SyntaxToken>>)
        : string list =
        let acc = ResizeArray<string>()

        for i in 0 .. elems.Length - 1 do
            match elems.[i] with
            | ModuleSignatureElement.Import(ImportDecl.ImportDecl(_, li)) -> acc.Add(longIdentName lexed input li)
            | ModuleSignatureElement.Import(ImportDecl.ImportDeclType _) ->
                // `open type Foo` brings only Foo's static members into scope,
                // not Foo as a prefix. Ignore for v1.
                ()
            | _ -> ()

        // Newest first: a later `open` shadows earlier ones.
        List.ofSeq (Seq.rev acc)

    let rec private extractModuleSigElement
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (opens: string list)
        (path: string list)
        // The source-name twin of `path` (see `extractValSig`). Equal to `path`
        // except inside a `ModuleSuffix` module, where `path` carries the compiled
        // `…Module` segment and this carries the source segment.
        (sourcePath: string list)
        (elem: ModuleSignatureElement<SyntaxToken>)
        : unit =
        match elem with
        | ModuleSignatureElement.Val valSig -> extractValSig ctx file lexed input opens path sourcePath valSig

        | ModuleSignatureElement.Type(_, typeSigs) ->
            let (TypeSignatures(first, rest)) = typeSigs
            extractTypeSig ctx file lexed input opens path first

            for i in 0 .. rest.Length - 1 do
                let (_, ts) = rest.[i]
                extractTypeSig ctx file lexed input opens path ts

        | ModuleSignatureElement.Module moduleSig ->
            let (ModuleSignature(attrs, _, access, _, identTok, _, body)) = moduleSig

            if isAccessible access then
                let name = nameOfTok lexed input identTok

                let suffixed =
                    if hasModuleSuffix lexed input attrs then
                        name + "Module"
                    else
                        name

                let childPath = suffixed :: path
                let childSourcePath = name :: sourcePath
                let (ModuleSignatureBody(_, elems, _)) = body
                // The module's own qualified path is itself an implicit open
                // prefix, ahead of the inherited opens but behind the body's.
                let modulePath = String.concat "." (List.rev childPath)

                // An `[<AutoOpen>]` module contributes its qualified path to the
                // contract's ambient prefix set (symbol-resolution-handoff.md, open-resolution).
                if isAutoOpen lexed input attrs then
                    ctx.AutoOpenPrefixes.Add modulePath

                let childOpens = collectOpens lexed input elems @ (modulePath :: opens)

                for i in 0 .. elems.Length - 1 do
                    extractModuleSigElement ctx file lexed input childOpens childPath childSourcePath elems.[i]

        | _ -> ()

    let private extractNamespaceGroup
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (fileOpens: string list)
        (group: NamespaceDeclGroupSignature<SyntaxToken>)
        : unit =
        let nsPath, elems =
            match group with
            | NamespaceDeclGroupSignature.Named(_, _, li, els) ->
                let parts =
                    [ for i in 0 .. li.Idents.Length - 1 -> nameOfTok lexed input li.Idents.[i] ]

                List.rev parts, els
            | NamespaceDeclGroupSignature.Global(_, _, els) -> [], els

        let nsName = String.concat "." (List.rev nsPath)
        let ownOpens = collectOpens lexed input elems
        // The namespace's qualified path is implicitly in scope; top-level
        // opens outside any namespace group are inherited.
        let opens =
            if nsName.Length = 0 then
                ownOpens @ fileOpens
            else
                ownOpens @ (nsName :: fileOpens)

        for i in 0 .. elems.Length - 1 do
            // A namespace path carries no `ModuleSuffix` rewrite, so source == compiled.
            extractModuleSigElement ctx file lexed input opens nsPath nsPath elems.[i]

    let private extractNamedModuleSig
        (ctx: ExtractCtx)
        (file: LibFile)
        (lexed: Lexed)
        (input: string)
        (fileOpens: string list)
        (nm: NamedModuleSignature<SyntaxToken>)
        : unit =
        let (NamedModuleSignature(attrs, _, access, _, li, elems)) = nm

        if isAccessible access then
            let suffix = hasModuleSuffix lexed input attrs

            let sourcePathRev =
                [ for i in 0 .. li.Idents.Length - 1 -> nameOfTok lexed input li.Idents.[i] ]
                |> List.rev

            // ModuleSuffix applies to the innermost segment only.
            let pathRev =
                match sourcePathRev, suffix with
                | head :: rest, true -> (head + "Module") :: rest
                | _ -> sourcePathRev

            let qualifiedSelf = String.concat "." (List.rev pathRev)

            if isAutoOpen lexed input attrs then
                ctx.AutoOpenPrefixes.Add qualifiedSelf

            let ownOpens = collectOpens lexed input elems
            let opens = ownOpens @ (qualifiedSelf :: fileOpens)

            for i in 0 .. elems.Length - 1 do
                extractModuleSigElement ctx file lexed input opens pathRev sourcePathRev elems.[i]

    let extractSymbols (ctx: ExtractCtx) (parsed: ParsedFile) : unit =
        match parsed.Ast with
        | FSharpAst.SignatureFile sf ->
            match sf with
            | SignatureFile.Namespaces groups ->
                for i in 0 .. groups.Length - 1 do
                    // Each namespace decl group starts a fresh open scope.
                    extractNamespaceGroup ctx parsed.File parsed.Lexed parsed.Input [] groups.[i]
            | SignatureFile.NamedModule nm -> extractNamedModuleSig ctx parsed.File parsed.Lexed parsed.Input [] nm
            | SignatureFile.AnonymousModule elems ->
                let opens = collectOpens parsed.Lexed parsed.Input elems

                for i in 0 .. elems.Length - 1 do
                    extractModuleSigElement ctx parsed.File parsed.Lexed parsed.Input opens [] [] elems.[i]
        | _ -> ctx.Diagnostics.Add(parsed.File, "Skipped: not a signature file")

    /// Builds an `IExternalSymbolProvider` backed by XParsec.FSharp.Lib.
    /// Returns the provider plus per-file errors so callers can decide
    /// whether to proceed with a partial table or fail loudly. Files that
    /// fail to parse contribute no symbols but do not abort the build.
    let buildProvider (libRoot: string) : Result<IExternalSymbolProvider * (LibFile * string) list, string> =
        match loadAll libRoot with
        | Error e -> Error e
        | Ok loaded ->
            let ctx = ExtractCtx.empty ()

            for file in loaded.Files do
                match parseFileFull file with
                | Error e -> ctx.Diagnostics.Add(file, e)
                | Ok parsed -> extractSymbols ctx parsed

            Ok(ExtractCtx.toProvider ctx, List.ofSeq ctx.Diagnostics)

    /// Composes two providers: tries `primary` first, falls back to
    /// `secondary` (e.g. `MockBuiltins.provider` as a backstop for whatever
    /// `extractSymbols` doesn't cover yet). The 2-deep special case of
    /// `ExternalSymbols.composite`, kept for its callers' readability.
    let chain (primary: IExternalSymbolProvider) (secondary: IExternalSymbolProvider) : IExternalSymbolProvider =
        ExternalSymbols.composite [ primary; secondary ]

    /// Lazy cache keyed by `libRoot` so repeated callers parse the lib at
    /// most once per root. Thread-safe via `Lazy<_>` publication.
    let private cachedProviders =
        System.Collections.Concurrent.ConcurrentDictionary<
            string,
            Lazy<Result<IExternalSymbolProvider * (LibFile * string) list, string>>
         >(
            StringComparer.Ordinal
        )

    /// Production-path entry point: caches `buildProvider` per `libRoot`.
    /// Tests that need a fresh provider should call `buildProvider` instead.
    let defaultProvider (libRoot: string) : Result<IExternalSymbolProvider * (LibFile * string) list, string> =
        let normalised = Path.GetFullPath libRoot

        let entry =
            cachedProviders.GetOrAdd(normalised, (fun root -> lazy (buildProvider root)))

        entry.Value
