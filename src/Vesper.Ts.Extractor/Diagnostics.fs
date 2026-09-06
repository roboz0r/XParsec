/// Per-type degradation diagnostics + the threaded mapping context. A TS construct
/// with no faithful schema form warns and degrades to a nameable `TypeRef`; plumbing
/// failures (not a module, unreadable source, unresolvable specifier) still THROW.
module Vesper.Ts.Extractor.Diagnostics

open Fable.Core

open TypeScript
open Vesper.Ts.Manifest

open Vesper.Ts.Extractor.TsInterop

/// The context threaded through the whole type/member/export walk.
type MapCtx =
    {
        Checker: Ts.TypeChecker
        /// Source-file ORIGIN oracle: `isSourceFileDefaultLibrary` / `isSourceFileFromExternalLibrary`.
        Program: Ts.Program
        Diags: ResizeArray<Schema.Diagnostic>
        /// Foreign named references, keyed by the bare name in the emitted `Named`.
        Refs: ResizeArray<string * Schema.RefEntry>
        /// Type-scope typars: the enclosing type's typars for a member,
        /// or a FREE FUNCTION's own typars (its single index space).
        DeclaringEnv: Ts.Symbol list
        /// Method scope: a generic MEMBER's own typars. Empty for a free
        /// function / property / heritage walk.
        MethodEnv: Ts.Symbol list
        /// OVERRIDE for a foreign ref's home: one manifest per quoted `declare module
        /// "…"`, so a ref to a SIBLING module's type homes by that module's specifier
        /// (`node/events`). `None` = same-module or non-ambient; file origin decides.
        ModuleHome: Ts.Symbol -> string option
        /// SHARED type-mapping recursion depth — every `{ ctx with … }` copy aliases the
        /// same cell, so it counts total nesting across all axes. Past the bound a
        /// self-recursive conditional (`Awaited<T>`) degrades to `obj` instead of overflowing.
        Depth: int ref
    }

    static member Root
        (checker: Ts.TypeChecker)
        (program: Ts.Program)
        (diags: ResizeArray<Schema.Diagnostic>)
        (refs: ResizeArray<string * Schema.RefEntry>)
        : MapCtx =
        {
            Checker = checker
            Program = program
            Diags = diags
            Refs = refs
            DeclaringEnv = []
            MethodEnv = []
            ModuleHome = fun _ -> None
            Depth = ref 0
        }

/// Capital `lib.es*` wrapper interfaces that OVERLAP Vesper's own representation of the
/// same runtime values (numerics/string/bool are carried on `IntrinsicRepr`, `obj` is the universal
/// supertype, arrays are native). Never homed as a ref, never emitted as an export.
let intrinsicOverlapNames: Set<string> =
    Set.ofList
        [
            "String"
            "Number"
            "Boolean"
            "Object"
            "Function"
            "Array"
            "Symbol"
            "BigInt"
        ]

/// `File` is the absolute source path here; it is relativized once, at finalize.
let spanOfNode (node: Ts.Node) : Schema.Span =
    {
        File = (node.getSourceFile ()).fileName
        Start = int (node.getStart ())
        End = int (node.getEnd ())
    }

let emitWarning
    (ctx: MapCtx)
    (code: Schema.DiagCode)
    (symbol: string)
    (span: Schema.Span option)
    (message: string)
    : unit =
    ctx.Diags.Add
        {
            Severity = Schema.Severity.Warning
            Code = code
            Symbol = symbol
            Span = span
            Message = message
        }

/// Rewrite each span's `File` relative to `baseDir` with forward slashes, so the
/// manifest relocates across machines, then collapse diagnostics identical in
/// (Code, Symbol, Span) — one hard type reached from N sites warns N times.
let finalizeDiagnostics (baseDir: string) (diags: ResizeArray<Schema.Diagnostic>) : Schema.Diagnostic list =
    let relFile (file: string) =
        let rel = normalizeSlashes (pathRelative baseDir file)
        // An empty/absolute result (different drive) is not portable — use the basename.
        if rel = "" || rel.Contains ":" || rel.StartsWith "/" then
            normalizeSlashes (pathBasename file)
        else
            rel

    diags
    |> Seq.map (fun d ->
        match d.Span with
        | Some s ->
            { d with
                Span = Some { s with File = relFile s.File }
            }
        | None -> d
    )
    |> List.ofSeq
    |> List.distinctBy (fun d -> d.Code, d.Symbol, d.Span)

// ─── foreign-reference classification (home + kind + arity) ────────────────────
// IDENTITY ONLY — never the foreign type's shape or members, and nothing here
// EVALUATES a type (no `getConstraint`, no keyof-fold).

/// The referenced type's HOME, read off its DECLARATION's source file: a default-library
/// file → `"es2015"` (every `lib.es*` version flattened into one home); a `node_modules`
/// file → that package's own `name`; otherwise LOCAL → `None`, and no ref is recorded.
let private classifyHome (ctx: MapCtx) (sym: Ts.Symbol) : string option =
    match ctx.ModuleHome sym with
    | Some home -> Some home
    | None ->

        match tryDeclOf sym with
        | None -> None // an intrinsic type has no declaration → not a foreign nominal ref
        | Some decl ->
            let sf = decl.getSourceFile ()

            if ctx.Program.isSourceFileDefaultLibrary sf then
                Some "es2015"
            elif ctx.Program.isSourceFileFromExternalLibrary sf then
                let rec walk (dir: string) : string option =
                    let pj = pathJoin dir "package.json"

                    if existsSync pj then
                        jsonNameField (JS.JSON.parse (readFileSyncUtf8 pj "utf8"))
                    else
                        let parent = pathDirname dir

                        if parent = dir then None else walk parent

                walk (pathDirname sf.fileName)
            else
                None // LOCAL: resolved through the own-registry path

/// The referenced type's KIND, from its `SymbolFlags`. An `Interface` that also carries
/// a VALUE meaning is a FUSED pair (`interface Map<K,V>` + `declare var Map:
/// MapConstructor`) — `new`-able, so `Class`. A bare value in type position → `None`.
let private classifyKind (sym: Ts.Symbol) : Schema.RefKind option =
    let flags = sym.getFlags ()

    if hasFlag flags Ts.SymbolFlags.Class then
        Some Schema.RefKind.Class
    elif hasFlag flags Ts.SymbolFlags.Enum then
        Some Schema.RefKind.Enum
    elif hasFlag flags Ts.SymbolFlags.TypeAlias then
        Some Schema.RefKind.Alias
    elif hasFlag flags Ts.SymbolFlags.Interface then
        if hasFlag flags Ts.SymbolFlags.Value then
            Some Schema.RefKind.Class
        else
            Some Schema.RefKind.Interface
    else
        None

/// The referenced type's DECLARED arity: the MAX type-parameter count over its
/// declarations, read syntactically off each node. The max picks the real arity of a
/// fused pair — `declare var Map` declares none, `interface Map<K,V>` declares two.
let private refArity (sym: Ts.Symbol) : int =
    match sym.declarations with
    | Some ds when ds.Count > 0 ->
        ds
        |> Seq.map (fun d -> (ts.getEffectiveTypeParameterDeclarations (unbox d)).Count)
        |> Seq.max
    | _ -> 0

/// `name` is the BARE name carried in the emitted `Named` node (the refs key); `sym` is
/// the referenced type's symbol, followed through a re-export alias to the real symbol
/// whose declarations home it. A non-homeable symbol records nothing, never throws.
let recordForeignRef (ctx: MapCtx) (name: string) (sym: Ts.Symbol) : unit =
    if intrinsicOverlapNames.Contains name then
        ()
    else

        let resolved =
            if hasFlag (sym.getFlags ()) Ts.SymbolFlags.Alias then
                ctx.Checker.getAliasedSymbol sym
            else
                sym

        match classifyHome ctx resolved with
        | None -> ()
        | Some home ->
            match classifyKind resolved with
            | None -> ()
            | Some kind ->
                ctx.Refs.Add(
                    name,
                    {
                        Home = home
                        Kind = kind
                        TyparArity = refArity resolved
                    }
                )

/// DEDUPE by bare name keeping FIRST-SEEN order, so the golden stays deterministic. A
/// name recurs with the same identity, so keeping the first occurrence is lossless.
let finalizeRefs (refs: ResizeArray<string * Schema.RefEntry>) : (string * Schema.RefEntry) list =
    refs |> List.ofSeq |> List.distinctBy fst
