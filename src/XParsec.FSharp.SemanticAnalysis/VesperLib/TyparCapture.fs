namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Generic
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// The mutable state threaded through the `.fsi` contract extractor: typar interning,
/// raw `when` clauses, and the accumulating symbol / type-shape tables.
module VesperLibTyparCapture =

    /// First sight of a name takes the next index; later occurrences re-use it.
    [<Sealed>]
    type TyparCollector() =
        let dict = Dictionary<string, int>(StringComparer.Ordinal)

        member _.IndexOf(name: string) : int =
            match dict.TryGetValue name with
            | true, idx -> idx
            | _ ->
                let idx = dict.Count
                dict.[name] <- idx
                idx

        /// Lookup that does NOT intern, so resolving a `when` clause adds no typar of its own.
        member _.TryIndexOf(name: string) : int voption =
            match dict.TryGetValue name with
            | true, idx -> ValueSome idx
            | _ -> ValueNone

        member _.Count = dict.Count

    /// What the finalize pass needs to re-walk a stashed CST. `Typars` is the same collector
    /// the extraction walk interned into, so both walks read one set of typar indices.
    type DeferredCtx =
        {
            Lexed: Lexed
            Opens: string list
            Typars: TyparCollector
        }

    /// A type body's CST, parked between extraction and the finalize pass: it may forward-
    /// reference a type declared later in the package, so it freezes only once every
    /// declaration is registered.
    [<RequireQualifiedAccess>]
    type DeferredBody =
        | Abbrev of DeferredCtx * rhs: Type<SyntaxToken>
        /// One CST per record field, in declaration order.
        | Record of DeferredCtx * fields: Type<SyntaxToken>[]
        /// One field-type CST array per union case, in case order, each indexed by that
        /// case's fields.
        | Union of DeferredCtx * cases: Type<SyntaxToken>[][] * interfaces: Type<SyntaxToken> list
        /// `ctors` holds each `new: … -> T` signature as `(param-type CSTs, return-type CST)`.
        | Class of
            DeferredCtx *
            baseType: Type<SyntaxToken> voption *
            interfaces: Type<SyntaxToken> list *
            ctors: (Type<SyntaxToken>[] * Type<SyntaxToken>) list

    type DeferredMember =
        {
            Ctx: DeferredCtx
            Signature: CurriedSig<SyntaxToken>
            /// `P: T with get, set` writes ONE signature, the getter's. The `set` half freezes
            /// from it as `args… -> T -> unit`; there is no `unit` in the source to translate.
            IsSetter: bool
        }

    /// One `val` signature CST, frozen into a complete `ExternalSymbol` by the finalize pass.
    type DeferredVal =
        {
            Ctx: DeferredCtx
            Key: BindingKey
            Source: string voption
            File: OriginPath
            Signature: CurriedSig<SyntaxToken>
        }

    /// Raw `when`-clause capture, before typar names are mapped to indices. A target stays
    /// a CST because it may name a typar the body walk has not interned yet.
    [<RequireQualifiedAccess>]
    type RawConstraint =
        | Trait of typarName: string * kind: SemanticConstraintKind
        /// `when (^T or ^U) : (static member (+) : ^T * ^U -> ^V)`. `memberName` is the
        /// compiled name (`op_Addition`).
        | MemberTrait of
            typarNames: string list *
            memberName: string *
            argTypes: Type<SyntaxToken> list *
            returnType: Type<SyntaxToken>
        /// `target` is the RHS of `default ^T : <type>`.
        | Default of typarName: string * target: Type<SyntaxToken>
        /// `target` is the RHS of `when 'e :> <type>`.
        | Coercion of typarName: string * target: Type<SyntaxToken>

    [<Sealed>]
    type ConstraintCollector() =
        let items = ResizeArray<RawConstraint>()
        member _.Add(c: RawConstraint) = items.Add c
        member _.Snapshot() = List.ofSeq items

    /// Mutable accumulator threaded through the extraction of every file in one package;
    /// a reference across files resolves through the tables it has filled so far.
    [<Sealed>]
    type ExtractCtx(target: string) =
        member _.Target: string = target
        member val Symbols = Dictionary<string, ExternalSymbol>(StringComparer.Ordinal) with get
        /// File-level failures: a file that would not lex, parse, or match an AST shape.
        member val Diagnostics = ResizeArray<OriginPath * string>() with get
        /// Per-val extraction failures: an `.fsi` that parses cleanly can still lose vals.
        member val Skipped = ResizeArray<OriginPath * string>() with get
        /// Type-name index: short name -> (arity, compiledName). First declaration wins
        /// when two types share a short name.
        member val Types = Dictionary<string, int * string>(StringComparer.Ordinal) with get
        /// The identity of every type this package declares, indexed by its canonical
        /// metadata name, the same key the shape tables below use.
        member val TypeKeys = Dictionary<string, TypeKey>(StringComparer.Ordinal) with get
        /// Every `module` this package declares, keyed by the DOTTED path the source writes
        /// -> the container a type declared in it sits in, which carries the compiled chain.
        member val ModuleContainers = Dictionary<string, TypeContainer>(StringComparer.Ordinal) with get
        /// Type-shape index: qualified compiled name -> body shape.
        member val TypeShapes = Dictionary<string, ExternalTypeShape>(StringComparer.Ordinal) with get
        /// The `member`s a `.fsi` declares inside a union/record/class body, keyed by the
        /// declaring type's qualified compiled name as `TypeShapes` is, in declaration order.
        member val TypeMembers = Dictionary<string, ResizeArray<ExternalMember>>(StringComparer.Ordinal) with get
        /// The deferred body CST for a `TypeShapes` entry, keyed the same way. A shape whose
        /// body holds no type syntax (an `extern` with no base, interface or ctor) has none.
        member val DeferredBodies = Dictionary<string, DeferredBody>(StringComparer.Ordinal) with get
        /// The deferred signature CST for each `TypeMembers` entry, keyed the same way and
        /// index-aligned with that type's member list.
        member val DeferredMembers = Dictionary<string, ResizeArray<DeferredMember>>(StringComparer.Ordinal) with get
        /// The deferred `val` signatures, in source order.
        member val DeferredVals = ResizeArray<DeferredVal>() with get
        /// Short type name -> the platform repr for the target being compiled, read out of
        /// the package's per-target `.fs` companion (`prim-types-int.js.fs` ⇒ `int` -> `number`).
        member val IntrinsicReprs = Dictionary<string, string>(StringComparer.Ordinal) with get

        /// Heritable primitives (`extern class with …`: `obj`/`exn`) awaiting republish as an
        /// `Intrinsic`: compiled name -> (canon key, platform repr). A side table because
        /// `DeferredBody.Class` carries no kind tag.
        member val PendingIntrinsicClasses =
            Dictionary<string, struct (TypeKey * string)>(StringComparer.Ordinal) with get

        /// Capability interfaces (`disposable`/`equatable`/`comparable`) awaiting republish as
        /// an `IntrinsicInterface`: compiled name -> (canon key, platform repr).
        member val PendingCapabilityInterfaces =
            Dictionary<string, struct (TypeKey * string)>(StringComparer.Ordinal) with get

        /// Qualified names of the `[<AutoOpen>]` modules this package declares, in source
        /// order. Published as the contract's ambient prefixes, so a consumer resolves their
        /// members with no explicit `open`.
        member val AutoOpenPrefixes = ResizeArray<string>() with get
        /// Qualified compiled names of the `[<RequireQualifiedAccess>]` unions this package
        /// declares; every case built from one is stamped, so a bare case name is rejected.
        member val RqaTypes = HashSet<string>(StringComparer.Ordinal) with get
        /// Type shapes from this package's already-extracted dependencies, plus BCL metadata,
        /// consulted to kind a nominal type constructor this package only names.
        /// The default answers nothing: a package with no dependencies.
        member val AmbientShapes: (string -> ExternalTypeShape voption) = (fun _ -> ValueNone) with get, set
        /// The ambient prefixes this package's dependencies publish, probed LAST during this
        /// package's own extraction, so a `.fsi` can name a dependency's type unqualified.
        member val DependencyAmbientPrefixes: string list = [] with get, set

    module ExtractCtx =
        let empty (target: string) = ExtractCtx(target)

        /// The shape in scope for compiled name `compiled`: this package's own first, then a
        /// dependency's. A name whose shape is registered later in the file walk misses.
        let shapeOf (ctx: ExtractCtx) (compiled: string) : ExternalTypeShape voption =
            match ctx.TypeShapes.TryGetValue compiled with
            | true, s -> ValueSome s
            | _ -> ctx.AmbientShapes compiled

        /// The identity a written type name denotes in this package. Two spellings arrive: the
        /// canonical metadata name, a direct hit; and the dotted spelling source writes for a
        /// module-held type (`M.T`), resolved through the declared modules.
        let tryTypeKey (ctx: ExtractCtx) (probe: string) : TypeKey voption =
            let exact (name: string) =
                match ctx.TypeKeys.TryGetValue name with
                | true, key -> ValueSome key
                | _ -> ValueNone

            let moduleContainer (path: string) =
                match ctx.ModuleContainers.TryGetValue path with
                | true, container -> ValueSome container
                | _ -> ValueNone

            SymbolKeyOps.tryDottedInModule exact moduleContainer probe

        /// Read a FINALIZED context out as the surface it publishes, `AutoOpenPrefixes` its
        /// ambient. Precondition: every deferred body / member is already frozen into its
        /// shape, and every table below is read once rather than closed over.
        let toSurface (ctx: ExtractCtx) : PublishedSurfaceBuilder =
            let surface = PublishedSurfaceBuilder.create ()

            // Bare case name -> declaring union + case shape, for a consumer writing `Some x`
            // with no union named. First declaration wins on a name collision.
            for kv in ctx.TypeShapes do
                match kv.Value with
                // A list is written only as `[]` / `::`, so indexing the cons-list's case
                // names would only shadow a user union declaring a case of the same name.
                | ExternalTypeShape.Union _ when RuntimeNames.isVesperListName kv.Key -> ()
                | ExternalTypeShape.Union(arity, cases, _, origin) ->
                    let rqa = ctx.RqaTypes.Contains kv.Key

                    for case in cases do
                        if not (surface.UnionCases.ContainsKey case.Name) then
                            surface.UnionCases.[case.Name] <-
                                {
                                    UnionName = kv.Key
                                    TyparArity = arity
                                    Origin = origin
                                    Case = case
                                    IsRequireQualifiedAccess = rqa
                                }
                | _ -> ()

            // What resolves a consumer's bare `{ x = 1 }` to a record this signature publishes.
            for KeyValue(compiled, typeKey) in ctx.TypeKeys do
                match ctx.TypeShapes.TryGetValue compiled with
                | true, ExternalTypeShape.Record(arity, fields, origin, _) ->
                    let candidate =
                        {
                            TypeKey = typeKey
                            TyparArity = arity
                            Origin = origin
                            FieldNames = fields |> EqArray.map (fun f -> f.Name)
                            IsRequireQualifiedAccess = ctx.RqaTypes.Contains compiled
                        }

                    for f in fields do
                        match surface.RecordFields.TryGetValue f.Name with
                        | true, buf -> buf.Add candidate
                        | _ ->
                            let buf = ResizeArray<ExternalRecordCandidate>()
                            buf.Add candidate
                            surface.RecordFields.[f.Name] <- buf
                | _ -> ()

            // The intrinsic axis, off the `Intrinsic` shapes alone: a CAPABILITY interface
            // carries its platform name on its own identity and is deliberately absent here.
            surface.Intrinsics <-
                ctx.TypeShapes
                |> Seq.choose (fun kv ->
                    match kv.Value with
                    | ExternalTypeShape.Intrinsic { Id = id } ->
                        Some
                            {
                                Canon = id.Canon
                                Platform = id.Platform
                            }
                    | _ -> None
                )
                |> IntrinsicTypeMap.ofSeq

            // Extraction registers by compiled NAME throughout and only reaches identities
            // here, through the one table that holds them.
            for KeyValue(compiled, typeKey) in ctx.TypeKeys do
                surface.TypesByName.[compiled] <- typeKey

                match ctx.TypeShapes.TryGetValue compiled with
                | true, shape -> surface.ShapesByKey.[typeKey] <- shape
                | _ -> ()

                match ctx.TypeMembers.TryGetValue compiled with
                | true, members -> surface.MembersByKey.[typeKey] <- members
                | _ -> ()

            for KeyValue(path, container) in ctx.ModuleContainers do
                surface.ModuleContainers.[path] <- container

            for KeyValue(name, sym) in ctx.Symbols do
                surface.Symbols.[name] <- sym

            surface.AmbientOpenPrefixes <- List.ofSeq ctx.AutoOpenPrefixes
            surface

        let toProvider (ctx: ExtractCtx) : IExternalSymbolProvider =
            PublishedSurface.toProvider (PublishedSurface.ofBuilder (toSurface ctx))
