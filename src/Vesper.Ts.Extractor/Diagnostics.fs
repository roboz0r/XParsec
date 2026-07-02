/// Per-type degradation diagnostics + the threaded mapping context.
///
/// A per-TYPE mapping failure (a TS construct with no faithful schema form but that
/// CAN still be named) records a `Warning` and DEGRADES to a representable `TypeRef`,
/// rather than aborting the whole extraction. The accumulator is a plain `ResizeArray`
/// created once per extraction in `extractFile`/`extractPackage` and threaded — like
/// `checker` — through the walk (as `MapCtx.Diags`), then drained into the manifest's
/// `Diagnostics`. Single-threaded Node, so a shared mutable buffer is safe. FATAL
/// plumbing failures (not a module, unreadable source, unresolvable specifier) still
/// THROW: a degraded type is meaningful; a half-loaded program is not.
module Vesper.Ts.Extractor.Diagnostics

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
        Diags: ResizeArray<Schema.Diagnostic>
        /// Declaring-axis typar scope: the enclosing type's typars for a member,
        /// or a FREE FUNCTION's own typars (its single index space).
        DeclaringEnv: Ts.Symbol list
        /// Method-axis scope: a generic MEMBER's own typars. Empty for a free
        /// function / property / heritage walk.
        MethodEnv: Ts.Symbol list
    }

    /// The walk root: empty typar axes (per-export arms seed `DeclaringEnv`).
    static member Root (checker: Ts.TypeChecker) (diags: ResizeArray<Schema.Diagnostic>) : MapCtx =
        {
            Checker = checker
            Diags = diags
            DeclaringEnv = []
            MethodEnv = []
        }

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
let drainDiagnostics (baseDir: string) (diags: ResizeArray<Schema.Diagnostic>) : Schema.Diagnostic list =
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
