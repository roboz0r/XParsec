/// Per-type degradation diagnostics + the threaded mapping context.
///
/// A per-TYPE mapping failure (a TS construct with no faithful schema form but that
/// CAN still be named) records a `Warning` and DEGRADES to a representable `TypeRef`,
/// rather than aborting the whole extraction. The accumulator is a plain `ResizeArray`
/// created once per extraction in `extractFile`/`extractPackage` and threaded — like
/// `checker` — through the walk (as `MapCtx.Diags`), then finalized into the manifest's
/// `Diagnostics`. Single-threaded Node, so a shared mutable buffer is safe. FATAL
/// plumbing failures (not a module, unreadable source, unresolvable specifier) still
/// THROW: a degraded type is meaningful; a half-loaded program is not.
module Vesper.Ts.Extractor.Diagnostics

open Fable.Core

open TypeScript
open Vesper.Ts.Manifest

open Vesper.Ts.Extractor.TsInterop

/// The context threaded through the whole type/member/export walk: the checker
/// oracle, the per-extraction diagnostic accumulator, and the TWO typar axes.
/// The two envs are identically typed, so positional threading was an axis-swap
/// hazard (a swap typechecks and silently maps typars to the wrong axis) — named
/// fields make axis membership un-swappable and let every recursion site pass ONE
/// value. A typar is resolved by SYMBOL identity (reference equality), NOT by
/// name — a method-axis `<U>` that shadows a declaring `<T>` must not alias to it.
type MapCtx =
    {
        Checker: Ts.TypeChecker
        /// The whole program — the ORACLE for a source file's ORIGIN
        /// (`isSourceFileDefaultLibrary` / `isSourceFileFromExternalLibrary`), which
        /// homes a foreign named reference into the refs table. The checker alone
        /// cannot answer origin; hence it rides here alongside it.
        Program: Ts.Program
        Diags: ResizeArray<Schema.Diagnostic>
        /// The foreign-reference accumulator (the `TypeRef`/`AssemblyRef` analog),
        /// filled as `mapType` emits each foreign `Named` and DEDUPED by bare name at
        /// finalize (`finalizeRefs`). A shared mutable buffer, like `Diags` — single-threaded
        /// Node makes it safe; created once per extraction and threaded through the walk.
        Refs: ResizeArray<string * Schema.RefEntry>
        /// Declaring-axis typar scope: the enclosing type's typars for a member,
        /// or a FREE FUNCTION's own typars (its single index space).
        DeclaringEnv: Ts.Symbol list
        /// Method-axis scope: a generic MEMBER's own typars. Empty for a free
        /// function / property / heritage walk.
        MethodEnv: Ts.Symbol list
        /// Per-manifest OVERRIDE for a foreign ref's home. The ambient-modules entry
        /// (decision A) extracts ONE manifest per quoted `declare module "…"`, so a
        /// reference FROM one such module TO a type declared in a SIBLING module is
        /// cross-manifest — even though both sit in the same physical `.d.ts` file, which
        /// `classifyHome`'s file-origin oracle would call LOCAL. This resolver, closed
        /// over the ambient-module set + the module CURRENTLY being extracted, homes such
        /// a ref by its DECLARING module specifier (`node/events`), returning `None` for a
        /// same-module (local) or non-ambient-module symbol so `classifyHome` falls
        /// through to its default file-origin logic. The module paths (`extractFile`/
        /// `extractPackage`) supply the always-`None` identity, so they are unchanged.
        ModuleHome: Ts.Symbol -> string option
        /// SHARED `mapType` recursion depth (a single mutable cell threaded like `Diags`
        /// — the `{ ctx with … }` copies alias the SAME ref, so it counts total nesting
        /// across every axis). Guards a self-recursive conditional type (`Awaited<T>`)
        /// from blowing the JS stack: `mapType` degrades to `obj` past the bound.
        Depth: int ref
    }

    /// The walk root: empty typar axes (per-export arms seed `DeclaringEnv`).
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

/// The PRIMITIVE-OVERLAP skip-list (Step 3). `lib.es5`/`lib.es2015` declare capital
/// wrapper/library interfaces — `String`/`Number`/`Boolean`/`Object`/`Function`/
/// `Array`/`Symbol`/`BigInt` — that OVERLAP Vesper's own intrinsic representation of
/// the same runtime values: the numeric/string/bool primitives ride `IntrinsicRepr`
/// (canon `.fsi` names, per-target platform reprs), `obj` is the universal supertype,
/// and Vesper arrays ARE native JS arrays. Registering these as `Js.*` nominals would
/// DOUBLE-REPRESENT them and fight the intrinsic subsumes/canonName machinery.
/// This list MIRRORS the front-end
/// intrinsic set — `RuntimeNames.numericTypeNames` ∪ {string, bool, unit, obj} ∪ the
/// native array `[]` — but spelled with the TS-lib INTERFACE names (capitalised
/// wrappers). The lowercase primitive TYPES (`string`/`number`/`boolean`/`void`) are
/// already remapped upstream in `mapType` (→ `string`/`float`/`bool`/`unit`) and never
/// reach the `Named` fallthrough, so only the capital interfaces need listing here.
/// Applied at TWO sites: `recordForeignRef` (a skip-list name is NOT homed — it stays a
/// carried `Named`→`FTConst`, never minting a dangling homed `FTClass`) and
/// `extractGlobals` (a skip-list name is NOT emitted as an export — the ref pack does
/// not REGISTER the intrinsic-repr'd names; their member surface is a later question).
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

/// A source `Span` anchored on a `ts.Node` — its source-file name + start/end
/// offsets (the binding returns the offsets as `float`; the wire carries `int`).
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

/// The single diagnostics DRAIN: relativize + dedup the accumulated diagnostics
/// before they land in the manifest.
///
/// RELATIVIZE: the manifest is a relocatable per-package cache artifact, so a
/// span's `File` must be RELATIVE to the extraction base (the `.d.ts`'s directory /
/// the package resolve dir), normalized to forward slashes — never the producer
/// machine's absolute path (which would break the cross-machine Node golden-diff).
/// A path that cannot be relativized (a different Windows drive — node's `relative`
/// then returns an absolute path) falls back to its basename. Done ONCE here, so
/// `spanOfNode` stays anchored on the absolute `fileName`.
///
/// DEDUP: the same degraded type referenced from N sites records N identical
/// diagnostics (a `keyof Events` reached from `all`, the `mitt` param, and the
/// alias is still ONE genuinely-hard type). Collapse by (Code, Symbol, Span) —
/// distinct spans stay distinct (a real second occurrence). Runs AFTER
/// relativization so spans are compared in their portable form.
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
//
// Every foreign `Named` the encoder emits records its IDENTITY — home + kind + arity —
// so the provider can re-mint a HOMED `FTClass`/abbrev. IDENTITY ONLY: never the
// foreign type's shape/members (the staleness trap the design forbids). The
// classification is RESILIENT (the standing extractor rule): a symbol whose home
// cannot be determined — an intrinsic with no declaration (`symbol`, `never`,
// `unknown`), or a LOCAL type declared in the package under extraction — is simply
// NOT recorded (a LOCAL type rides the own-registry path; homing it would be wrong).
// Nothing here EVALUATES a type (no `getConstraint`, no keyof-fold).

/// The referenced type's HOME:
///   • declared in a DEFAULT-LIBRARY source file → the reserved ES-core home
///     `"es2015"` (the plan flattens TS's `lib.es*` version grouping into one home; a
///     finer lib-file→home split is a later tranche — do not over-engineer it here);
///   • declared in an EXTERNAL library (a `node_modules` package) → that package's
///     specifier (its own `package.json`'s `name`, nearest walking up from the decl);
///   • otherwise LOCAL (declared in the package being extracted) → `None`, no ref.
/// Origin is read off the symbol's DECLARATION source file via the program oracle,
/// never a numeric flag.
let private classifyHome (ctx: MapCtx) (sym: Ts.Symbol) : string option =
    // The ambient-modules per-manifest override wins FIRST (decision A): a ref to a type
    // declared in a SIBLING quoted module homes by that module's specifier, not the
    // enclosing package name a file-origin walk would find. `None` (the module-path
    // identity, or a same-module/non-ambient symbol) falls through to file-origin logic.
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
                // Nearest `package.json` name walking up from the declaration's file — the
                // external package's own manifest sits closest (mirrors `packageVersionOf`'s
                // walk). A package.json missing a `name` degrades to `None` (no ref recorded).
                let rec walk (dir: string) : string option =
                    let pj = pathJoin dir "package.json"

                    if existsSync pj then
                        jsonNameField (JS.JSON.parse (readFileSyncUtf8 pj "utf8"))
                    else
                        let parent = pathDirname dir

                        if parent = dir then None else walk parent

                walk (pathDirname sf.fileName)
            else
                None // LOCAL: rides the own-registry path

/// The referenced type's KIND, mirroring what its home manifest's export arm would
/// emit (so the provider's re-mint dispatch matches). Classified by the SAME
/// `SymbolFlags` named-constant predicates the export arms use (never raw numerics):
///   • `Class` flag → `Class`; `Enum` → `Enum`; `TypeAlias` → `Alias`;
///   • `Interface` flag → `Interface`, UNLESS the symbol ALSO carries a VALUE meaning
///     (`SymbolFlags.Value`) — a class-like FUSED pair (`interface Map<K,V>` +
///     `declare var Map: MapConstructor`) is `new`-able and mints an `FTClass`, so it
///     is a `Class` at the seam, distinct from a pure type-only interface.
/// A symbol that is neither a type nor a class-like value (a bare value reached in a
/// type position) yields `None` — not a nominal type reference.
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

/// The referenced type's DECLARED generic arity: the MAX type-parameter count over
/// the symbol's declarations, read SYNTACTICALLY off each declaration node's effective
/// type-parameter list. The max handles a class-like fused pair whose value-side
/// (`declare var Map`) declares NO type params while its type-side (`interface
/// Map<K,V>`) declares two — the interface's count is the real arity. Never
/// `getConstraint()` (it EVALUATES) nor the declared type (avoid resolving).
let private refArity (sym: Ts.Symbol) : int =
    match sym.declarations with
    | Some ds when ds.Count > 0 ->
        ds
        |> Seq.map (fun d -> (ts.getEffectiveTypeParameterDeclarations (unbox d)).Count)
        |> Seq.max
    | _ -> 0

/// Record a foreign named reference's identity in the refs accumulator. `name` is the
/// BARE name carried in the emitted `Named` node (the refs key); `sym` is the
/// referenced type's symbol (the generic-instantiation target, or a bare nominal's own
/// symbol). Follows a re-export alias to the REAL symbol whose declarations home it.
/// Degrades silently (records nothing) for a non-homeable symbol — never throws.
let recordForeignRef (ctx: MapCtx) (name: string) (sym: Ts.Symbol) : unit =
    // Primitive-overlap skip (Step 3): a TS-lib intrinsic-overlap interface (`Array`,
    // `String`, …) is NOT homed — it stays a carried `Named`→`FTConst` (its pre-Step-1B
    // behaviour), so the extractor never mints a dangling homed `FTClass` for a name
    // Vesper already represents intrinsically (and that no stacked es2015 provider
    // registers). `Map`/`Set`/… are absent from the list and home normally.
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

/// Finalize the accumulated foreign refs into the manifest table: DEDUPE by bare name
/// (the same foreign type referenced N times → ONE entry) keeping FIRST-SEEN order, so
/// the golden stays deterministic (the walk order is deterministic). A name recurs with
/// the same identity, so keeping the first occurrence is lossless.
let finalizeRefs (refs: ResizeArray<string * Schema.RefEntry>) : (string * Schema.RefEntry) list =
    refs |> List.ofSeq |> List.distinctBy fst
