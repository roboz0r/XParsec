namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Generic
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Shared mutable state threaded through the extractor:
///
///   - `TyparCollector` — order-preserving typar-name → index map. Explicit
///     `<'T>` typars are seeded first by the caller so they keep declared
///     positions; implicit body-only typars pick up indices in source order.
///   - `RawConstraint` / `ConstraintCollector` — captured `when …` clauses
///     before typar names are mapped to indices. Trait constraints flow
///     straight through; SRTP / `default` entries hold their raw `Type`
///     until the body walk has registered every typar they might reference.
///   - `ExtractCtx` — the per-build accumulator: symbol / type-shape tables,
///     diagnostics, ambient open-prefix set. `ExtractCtx.toProvider` lifts
///     it to an `IExternalSymbolProvider` (surfacing the ambient prefixes
///     via its `AmbientOpenPrefixes` member).
module VesperLibTyparCapture =

    /// SemType template parameterised over a fresh-TyVar array (one per
    /// declared typar in the val's merged typar list). Independent
    /// invocations of `Instantiate level` allocate a fresh array, so two
    /// call sites of the same val never share TyVars.
    type SemBuilder = SemType[] -> SemType

    [<RequireQualifiedAccess>]
    type TyparKind =
        | Regular
        | Static

    /// Order-preserving typar collector. The first time a name is seen it
    /// gets the next index; subsequent occurrences re-use it. Explicit
    /// `<'T>` typars are added first by the caller before the body walk,
    /// so they keep their declared positions; implicit (body-only) typars
    /// pick up indices in source order.
    [<Sealed>]
    type TyparCollector() =
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
    type RawConstraint =
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
        /// `when 'e :> <type>`. `target` is the raw RHS, translated later
        /// through the same `translateType` path the val signature used
        /// (it may reference typars not yet registered when captured).
        | Coercion of typarName: string * target: Type<SyntaxToken>

    [<Sealed>]
    type ConstraintCollector() =
        let items = ResizeArray<RawConstraint>()
        member _.Add(c: RawConstraint) = items.Add c
        member _.Snapshot() = List.ofSeq items
        member _.Count = items.Count

    /// Mutable accumulator threaded through `extractSymbols` across every
    /// file in `LoadedLib.Files`. Cross-bucket references resolve through
    /// the accumulated tables.
    [<Sealed>]
    type ExtractCtx() =
        member val Symbols = Dictionary<string, ExternalSymbol>(StringComparer.Ordinal) with get
        /// File-level diagnostics — parse failures, AST-shape rejections.
        /// Kept distinct from `Skipped`; tests filter against them to detect
        /// parsing regressions.
        member val Diagnostics = ResizeArray<VesperLibManifest.LibFile * string>() with get
        /// Per-val extraction failures. Captured separately from
        /// `Diagnostics` so file-level health stays visible: an .fsi can
        /// extract 90% of its vals fine and still parse cleanly.
        member val Skipped = ResizeArray<VesperLibManifest.LibFile * string>() with get
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
        /// Instance/static members extracted from a type's augmentation block —
        /// the `member`s a `.fsi` declares inside a union/record/class body
        /// (`Option.IsSome` / `.Value` / `.IsNone`). Keyed by the declaring
        /// type's qualified compiled name (the same key `TypeShapes` uses);
        /// each list preserves declaration order. Surfaced through `toProvider`'s
        /// `TryLookupMember` / `TryLookupMembers` so a cross-package use site can
        /// type `o.IsSome` against the contract (vesper-lib-test-plan Gap 2
        /// Layer A). Empty for types with no augmentation members.
        member val TypeMembers = Dictionary<string, ResizeArray<ExternalMember>>(StringComparer.Ordinal) with get
        /// Intrinsic-representation index: *short* type name -> CLI repr string,
        /// harvested from the package's per-target `.fs` companions
        /// (`type exn = (# "System.Exception" #)` ⇒ `"exn" -> "System.Exception"`).
        /// Populated BEFORE `.fsi` extraction so the `extern` arm of
        /// `extractTypeSig` can publish a matching extern as
        /// `ExternalTypeShape.Intrinsic repr` instead of an opaque `Class`
        /// (intrinsic-repr-handoff.md — the `.fsi`/`.fs` pairing moves here from
        /// the codegen-layer harvest). Empty for callers with no `.fs` companions
        /// (e.g. `VesperLib.buildProvider` over the signature-only FSharp.Core
        /// port), so every extern stays a `Class` exactly as before.
        member val IntrinsicReprs = Dictionary<string, string>(StringComparer.Ordinal) with get
        /// Qualified names of `[<AutoOpen>]` modules encountered during
        /// extraction, in source order (`"Vesper.ArithmeticOperators"`). A
        /// referenced contract surfaces these as its ambient open-prefix set so a
        /// consumer resolves `op_Addition` / `hash` with no explicit `open`
        /// (symbol-resolution-handoff.md, open-resolution). The hardcoded FSharp.Core prelude list
        /// stays separate (compiler-magic opens not expressible as `[<AutoOpen>]`).
        member val AutoOpenPrefixes = ResizeArray<string>() with get
        /// Type shapes contributed by already-extracted dependency packages (dependency-ordered).
        /// Read-only here: extraction never writes a dependency's shape, only
        /// consults it (through `shapeOf`) to kind a cross-package nominal head.
        /// `ReferencedProject.buildProviderWith` seeds it from the composite
        /// `TryLookupType` of this package's dependency providers *and* layer-2
        /// metadata (the BCL) — the same shapes the consumer would see, minus this
        /// package. Layer-2 is included so a contract naming a raw BCL nominal head
        /// not aliased in its own package kinds correctly at bake time instead of
        /// baking a spurious `TyUnknown`.
        /// The default (`fun _ -> ValueNone`) is the dependency-free, no-metadata
        /// case: any caller that builds a context in isolation.
        member val AmbientShapes: (string -> ExternalTypeShape voption) = (fun _ -> ValueNone) with get, set

    module ExtractCtx =
        let empty () = ExtractCtx()

        /// The in-scope type shape for compiled name `compiled` during
        /// extraction: this package's own shapes first (registered as its files
        /// are walked), then the dependency-contributed `AmbientShapes`. The
        /// single lookup Phase 3's `translateType` kinding consults — own shapes
        /// shadow a dependency's on a name clash, matching the consumer composite's
        /// first-source-wins priority.
        ///
        /// A *forward* reference within this package (a name whose shape is
        /// registered later in the file walk) misses here unless it is legal: an
        /// intra-package forward reference is only valid inside a `type … and …`
        /// group or a `rec` namespace/module, and those shapes are registered
        /// together before any signature body is kinded. Any other unresolved name
        /// is a genuine `TyUnknown` (Phase 4), not an ordering artefact.
        let shapeOf (ctx: ExtractCtx) (compiled: string) : ExternalTypeShape voption =
            match ctx.TypeShapes.TryGetValue compiled with
            | true, s -> ValueSome s
            | _ -> ctx.AmbientShapes compiled

        /// Provider over the extracted symbol / type-shape tables, exposing
        /// `ctx.AutoOpenPrefixes` as its ambient. The pipeline seeds the
        /// ambient into the open scope and probes it BEHIND explicit
        /// `open`s — `1 + 2`'s desugared `op_Addition` lives in
        /// `Microsoft.FSharp.Core.Operators`, not at the root, and resolves
        /// through the ambient (symbol-resolution-handoff.md, open-resolution).
        /// The extractor populates `ctx.AutoOpenPrefixes` from `[<AutoOpen>]`
        /// attributes on modules; `buildProvider` supplements it with any
        /// library-specific prelude (e.g. F#'s implicit `Microsoft.FSharp.*`
        /// namespace opens) before calling `toProvider`. The same mechanism
        /// `ReferencedProject` uses for Vesper packages.
        let toProvider (ctx: ExtractCtx) : IExternalSymbolProvider =
            // Reverse case-name index for `TryLookupUnionCase` (Gap 2 Layer B):
            // bare case name -> (declaring union compiled name, arity, case
            // shape). Built once here, after extraction has fully populated
            // `ctx.TypeShapes`. First declaration wins on a name collision (the
            // same warn-and-take-first rule the short-name type index uses); the
            // actual Vesper packages have disjoint case names across unions, so
            // collisions don't arise in practice.
            let unionCaseIndex =
                let d = Dictionary<string, string * int * ExternalCaseShape>(StringComparer.Ordinal)

                for kv in ctx.TypeShapes do
                    match kv.Value with
                    | ExternalTypeShape.Union(arity, cases, _) ->
                        for case in cases do
                            if not (d.ContainsKey case.Name) then
                                d.[case.Name] <- (kv.Key, arity, case)
                    | _ -> ()

                d

            { new IExternalSymbolProvider with
                member _.TryLookup(name) =
                    match ctx.Symbols.TryGetValue name with
                    | true, sym -> ValueSome sym
                    | _ -> ValueNone

                member _.TryLookupType(name) =
                    match ctx.TypeShapes.TryGetValue name with
                    | true, shape -> ValueSome shape
                    | _ -> ValueNone

                member _.TryLookupMember(typeName, memberName) =
                    match ctx.TypeMembers.TryGetValue typeName with
                    | true, members ->
                        let mutable found = ValueNone
                        let mutable i = 0

                        while found.IsNone && i < members.Count do
                            if members.[i].Name = memberName then
                                found <- ValueSome members.[i]

                            i <- i + 1

                        found
                    | _ -> ValueNone

                member _.TryLookupMembers(typeName, memberName) =
                    match ctx.TypeMembers.TryGetValue typeName with
                    | true, members ->
                        [|
                            for m in members do
                                if m.Name = memberName then
                                    m
                        |]
                    | _ -> [||]

                member _.TryLookupUnionCase caseName =
                    match unionCaseIndex.TryGetValue caseName with
                    | true, hit -> ValueSome hit
                    | _ -> ValueNone

                member _.AmbientOpenPrefixes = List.ofSeq ctx.AutoOpenPrefixes
            }
