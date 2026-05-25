namespace XParsec.FSharp.SemanticAnalysis

// The full FSharp.Core (+) story (SRTPs + static-optimisation clauses +
// per-target inline IL) is firmly future work — see
// [[project_inline_il_target_specific]] for why we don't model it here.

/// Where a resolved symbol physically lives — enough for codegen to mint a ref
/// without re-resolving. `Assembly` is a simple name keyed into a `ProjectInfo`'s
/// resolved reference set; `None` ⇒ defined in the project being compiled. See
/// symbol-resolution-plan §4.
type SymbolOrigin =
    {
        Assembly: string option
        Namespace: string
        DeclaringType: string option
    }

    /// The default carried by symbols that don't (yet) record an origin —
    /// project-local, root namespace, no declaring type. P0 stamps this
    /// everywhere; later phases fill it from the resolving source.
    static member Empty =
        {
            Assembly = None
            Namespace = ""
            DeclaringType = None
        }

/// Platform-agnostic, scope-unambiguous symbol identity (symbol-resolution-plan
/// §7.3). All strings/ints — never a CLR `EntityHandle` or `System.Type` (those
/// are per-context and target-specific). The discriminator is the *origin*
/// (assembly, namespace[, declaring type]), not the bare name, so a project-local
/// `List` and `System.Collections.Generic.List`1` get different keys by
/// construction. Keyed on the open generic *definition* (`name` includes the
/// `` `arity `` suffix); instantiation is the cheap per-use substitution.
[<RequireQualifiedAccess>]
type SymbolKey =
    /// A type definition. `name` includes the arity suffix (`` IEnumerable`1 ``).
    | TypeKey of asm: string option * ns: string * name: string
    /// A value (module-level binding / operator).
    | ValueKey of asm: string option * ns: string * name: string
    /// A member on a type. `argSig` is written in the declaring type's OPEN
    /// typars (`!0`, …) and disambiguates overloads (`GetHashCode()` vs
    /// `GetHashCode(!0)`).
    | MemberKey of decl: SymbolKey * memberName: string * argSig: string list

/// SRTP / trait / default constraint captured on an external symbol's typar
/// list. Member-trait clauses are recorded as opaque markers; default clauses
/// carry a SemBuilder over the symbol's typar list so `Instantiate` can stamp
/// the default target onto the freshly minted TyVar for generalisation-time
/// defaulting.
[<RequireQualifiedAccess>]
type ExternalConstraint =
    /// `when 'T : equality` etc. — directly stamps a `SemanticConstraint` on
    /// the fresh `TypeVar` allocated for the typar at instantiation time.
    | Trait of typarIndex: int * kind: SemanticConstraintKind
    /// `when (^T or ^U) : (static member (+) : ^T * ^U -> ^V)` — SRTP
    /// member trait. `typarIndices` are the participating typar slots
    /// (the LHS of the trait). `memberName` is the compiled name. At
    /// `Instantiate` time the caller passes the fresh-TyVar array, and the
    /// closures produce the SemTypes describing the trait's expected member
    /// signature. The Unification pass drains the captured signature when any
    /// participating fresh TyVar is linked to a concrete shape — see
    /// `Unification.drainSrtpBounds`.
    | MemberTrait of
        typarIndices: int list *
        memberName: string *
        buildArgTypes: (SemType[] -> SemType)[] *
        buildReturnType: (SemType[] -> SemType)
    /// `default ^T : <ty>` — typar defaulting at generalisation. The
    /// `buildTarget` closure takes the symbol's fresh-TyVar array (one
    /// entry per declared typar) and returns the target `SemType` —
    /// usually another fresh TyVar (`default ^T3 : ^T1`) or a concrete
    /// shape (`default ^T1 : int`). `Instantiate` stamps the resolved
    /// target onto the source TyVar's `Defaults` list so generalisation
    /// can chase the chain and pick the first concrete shape it reaches.
    | Default of typarIndex: int * buildTarget: (SemType[] -> SemType)

type ExternalSymbol =
    {
        Name: string
        /// Returns a fresh instantiation of the symbol's type each call.
        /// `level` is the let-depth at which the instantiation happens; fresh
        /// TyVars must be stamped with it so Rémy's level-based generalisation
        /// can decide which to quantify. Monomorphic symbols return the same
        /// SemType every time and ignore the level. Polymorphic symbols
        /// allocate fresh TypeVars at `level` per call so independent use-sites
        /// don't unify with each other through the shared scheme.
        ///
        /// `Instantiate` is also responsible for applying any `Constraints` to
        /// the fresh TyVars it mints; callers don't drain the list separately.
        Instantiate: int -> SemType
        /// Empty for the overwhelming majority of symbols. Surfaced out of the
        /// closure for diagnostic introspection and to let future passes audit
        /// which constraints are still unimplemented.
        Constraints: ExternalConstraint list
        /// Where the symbol lives — the bridge to codegen (symbol-resolution-plan
        /// §4). `SymbolOrigin.Empty` until a resolving source fills it.
        Origin: SymbolOrigin
    }

/// Per-field shape inside an `ExternalTypeShape.Record`. Field types are
/// closure-builders parameterised over the enclosing type's typars: callers
/// pass an `SemType[]` (one entry per declared typar, in declaration order)
/// and the builder substitutes them through.
type ExternalFieldShape =
    {
        Name: string
        IsMutable: bool
        BuildType: SemType[] -> SemType
    }

/// Per-case shape inside an `ExternalTypeShape.Union`. `FieldNames` is
/// `ValueNone` for positional fields and `ValueSome name` for `of x: int`-
/// style named fields. The arrays line up: `FieldNames[i]` describes
/// `BuildFieldTypes[i]`'s source-side label.
type ExternalCaseShape =
    {
        Name: string
        FieldNames: string voption[]
        BuildFieldTypes: (SemType[] -> SemType)[]
    }

/// A resolved member (static/instance method or property getter) on an external
/// type (symbol-resolution-plan §4). `BuildSignature` is parameterised over the
/// *enclosing type's* typars, exactly like `ExternalFieldShape.BuildType`:
/// callers pass a `SemType[]` (one entry per declared typar) and the builder
/// substitutes them through, yielding the curried `arg → … → ret` signature.
type ExternalMember =
    {
        Name: string
        IsStatic: bool
        IsProperty: bool
        BuildSignature: SemType[] -> SemType
        Origin: SymbolOrigin
        /// The interned identity (symbol-resolution-plan §7.2/§7.3): a
        /// `SymbolKey.MemberKey` over the *open* declaring type (its `argSig` in
        /// `!0`-typars), minted by the resolving source. Freeze stamps it into
        /// `TExpr.ExternalMember` so codegen reads the binding off the node.
        Key: SymbolKey
    }

/// Type-declaration shape carried by `IExternalSymbolProvider.TryLookupType`.
/// `arity` is the number of declared typars (same length the builder
/// arrays expect at instantiation). enum/delegate types are deferred and
/// currently return `ValueNone` from the provider.
[<RequireQualifiedAccess>]
type ExternalTypeShape =
    | Abbrev of arity: int * body: (SemType[] -> SemType)
    /// Field order matches source.
    | Record of arity: int * fields: ExternalFieldShape[]
    /// Case order matches source.
    | Union of arity: int * cases: ExternalCaseShape[]
    /// A class or interface (the gap that makes `EqualityComparer<_>` resolve to
    /// `ValueNone` today). Members are resolved separately via `TryLookupMember`.
    | Class of arity: int * isInterface: bool * origin: SymbolOrigin

/// **Thread-safety:** `TryLookup` and `TryLookupType` must be safe to call
/// concurrently from multiple threads. Implementations that cache lazily must
/// guard their internal mutation. Per-file pipelines run independent
/// `PassContext`s in parallel and may hit the same provider from any of them —
/// see [`docs/architecture.md`](docs/architecture.md#parallelism).
type IExternalSymbolProvider =
    /// `name` is the compiled name ("op_Addition", not "(+)").
    abstract TryLookup: name: string -> ExternalSymbol voption
    /// Look up the body of a `type` declaration by canonical compiled name.
    /// Returns `ValueNone` for unknown names or for types whose body shape the
    /// provider doesn't (yet) model — enums, delegates, etc. Consumers fall back
    /// to `TyConst` / `TyRecord` nominal behaviour when this returns `ValueNone`.
    abstract TryLookupType: name: string -> ExternalTypeShape voption
    /// Look up a static/instance member on an external type by the declaring
    /// type's compiled name and the member name. This is what types
    /// `EqualityComparer<'T>.Default` (static property) and `.GetHashCode`
    /// (instance method). Defaults to `ValueNone` for providers that don't model
    /// members (symbol-resolution-plan §4).
    abstract TryLookupMember: typeName: string * memberName: string -> ExternalMember voption

/// Optional capability a provider may implement to contribute an *ambient*
/// (implicit) open-prefix set — the prelude / referenced-contract `[<AutoOpen>]`
/// modules. The pipeline seeds `PassContext.AmbientOpenScope` from it, where it
/// is probed strictly BEHIND explicit `open`s: a short name tries its bare form
/// and every explicit open first, and only then these ambient prefixes
/// (symbol-resolution-handoff.md, open-resolution — auto-opens resolve as if behind explicit opens).
/// Providers with no implicit prelude (`MockBuiltins`, inline test fakes) simply
/// don't implement it, so they contribute an empty ambient and behave exactly as
/// before. Kept separate from `IExternalSymbolProvider` for that reason: every
/// inline provider would otherwise have to implement it.
type IAmbientOpenScope =
    /// Dotted namespace/module prefixes in priority order (earliest wins on a
    /// collision), e.g. `["Vesper.ArithmeticOperators"; "Vesper"]`. Tried as
    /// candidate qualifiers for a short name after the bare name and all
    /// explicit opens have missed.
    abstract AmbientOpenPrefixes: string list

module ExternalSymbols =

    let mono (name: string) (ty: SemType) : ExternalSymbol =
        {
            Name = name
            Instantiate = fun _ -> ty
            Constraints = []
            Origin = SymbolOrigin.Empty
        }

    /// `build level` is invoked per lookup so any `TypeVar` it allocates is
    /// fresh and stamped at the caller's let-depth.
    let poly (name: string) (build: int -> SemType) : ExternalSymbol =
        {
            Name = name
            Instantiate = build
            Constraints = []
            Origin = SymbolOrigin.Empty
        }

    /// Like `poly` but carries constraints. The `build` closure is responsible
    /// for applying them to the fresh TyVars it allocates; this helper just
    /// records the structured shape on the symbol for introspection use.
    let polyWith (name: string) (build: int -> SemType) (constraints: ExternalConstraint list) : ExternalSymbol =
        {
            Name = name
            Instantiate = build
            Constraints = constraints
            Origin = SymbolOrigin.Empty
        }

    /// For tests that want to isolate behavior from external-symbol noise.
    let nullProvider: IExternalSymbolProvider =
        { new IExternalSymbolProvider with
            member _.TryLookup _ = ValueNone
            member _.TryLookupType _ = ValueNone
            member _.TryLookupMember(_, _) = ValueNone
        }

    /// First-hit-wins down the list; `[]` ⇒ `nullProvider`, a singleton ⇒ that
    /// provider unwrapped. Priority encodes shadowing among *external* sources
    /// (a referenced project beats a referenced assembly — symbol-resolution-plan
    /// §5). Project-local symbols are not here: `PassContext` resolves them before
    /// the provider is ever consulted. `FSharpLib.chain` is the 2-deep special
    /// case (`composite [primary; secondary]`).
    let composite (sources: IExternalSymbolProvider list) : IExternalSymbolProvider =
        match sources with
        | [] -> nullProvider
        | [ single ] -> single
        | _ ->
            // Snapshot to an array so the hot lookup is an index loop, not list
            // traversal, on a provider hit from many parallel PassContexts.
            let sources = List.toArray sources

            // The composed ambient prelude: each source's `[<AutoOpen>]` /
            // prelude prefixes, concatenated in source priority order (so a
            // higher-priority provider's auto-opens shadow a lower one's on a
            // name collision, same first-hit-wins ordering as lookups). Computed
            // once; the pipeline seeds `PassContext.AmbientOpenScope` from it.
            let ambientPrefixes =
                [
                    for s in sources do
                        match box s with
                        | :? IAmbientOpenScope as a -> yield! a.AmbientOpenPrefixes
                        | _ -> ()
                ]

            { new IExternalSymbolProvider with
                member _.TryLookup name =
                    let mutable result = ValueNone
                    let mutable i = 0

                    while result.IsNone && i < sources.Length do
                        result <- sources.[i].TryLookup name
                        i <- i + 1

                    result

                member _.TryLookupType name =
                    let mutable result = ValueNone
                    let mutable i = 0

                    while result.IsNone && i < sources.Length do
                        result <- sources.[i].TryLookupType name
                        i <- i + 1

                    result

                member _.TryLookupMember(typeName, memberName) =
                    let mutable result = ValueNone
                    let mutable i = 0

                    while result.IsNone && i < sources.Length do
                        result <- sources.[i].TryLookupMember(typeName, memberName)
                        i <- i + 1

                    result

              interface IAmbientOpenScope with
                  member _.AmbientOpenPrefixes = ambientPrefixes
            }

/// **Production-path code wires `FSharpLib.buildProvider` instead** — this
/// module exists as a test fixture and as the simplest possible example of the
/// provider interface. It stays for test isolation (most tests want a known
/// minimal surface so a failure is unambiguously the unifier's, not the
/// extractor's) and as a legible "fake target" provider slot.
///
/// Divergence risk: this provider declares some ops monomorphically
/// (`(+) : int -> int -> int`) where the real lib declares them
/// polymorphically with SRTP. Tests that need real-world behaviour should
/// wire `FSharpLib.buildProvider` directly or chain it in front of this
/// one — see `FSharpLib.chain`.
module MockBuiltins =

    let tyInt: SemType = TyConst "int"
    let tyInt64: SemType = TyConst "int64"
    let tyByte: SemType = TyConst "byte"
    let tyFloat: SemType = TyConst "float"
    let tyBool: SemType = TyConst "bool"
    let tyChar: SemType = TyConst "char"
    let tyDecimal: SemType = TyConst "decimal"
    let tyUnit: SemType = TyConst "unit"
    let tyString: SemType = TyConst "string"
    /// Placeholder for `seq<int>` — the result type of int range expressions
    /// (`1..10`, `1..2..10`). Until generic types are modelled this is an
    /// opaque TyConst that only unifies with itself.
    let tySeqInt: SemType = TyConst "seq<int>"

    let private tyBinOp (a: SemType) (b: SemType) (r: SemType) : SemType = TyFun(a, TyFun(b, r))

    let private tyUnaryOp (ty: SemType) : SemType = TyFun(ty, ty)

    let private monoOps =
        let intInfix = tyBinOp tyInt tyInt tyInt
        let intCmp = tyBinOp tyInt tyInt tyBool
        let boolInfix = tyBinOp tyBool tyBool tyBool

        [
            "op_Addition", intInfix
            "op_Subtraction", intInfix
            "op_Multiply", intInfix
            "op_Division", intInfix
            "op_Modulus", intInfix
            "op_UnaryNegation", tyUnaryOp tyInt
            "op_LessThan", intCmp
            "op_GreaterThan", intCmp
            "op_LessThanOrEqual", intCmp
            "op_GreaterThanOrEqual", intCmp
            "op_Equality", intCmp
            "op_Inequality", intCmp
            "op_BooleanAnd", boolInfix
            "op_BooleanOr", boolInfix
        ]
        |> List.map (fun (n, ty) -> n, ExternalSymbols.mono n ty)

    /// Polymorphic operators built from `FSharp.Core`. Each call mints fresh
    /// `TypeVar`s stamped at the caller's let-depth so two use-sites don't
    /// share variables and generalisation can quantify them at the right scope.
    let private polyOps =
        let freshAt (level: int) : SemType =
            let tv = TypeVar()
            tv.Level <- level
            TyVar tv

        [
            // val (|>) : 'a -> ('a -> 'b) -> 'b
            "op_PipeRight",
            fun level ->
                let a = freshAt level
                let b = freshAt level
                TyFun(a, TyFun(TyFun(a, b), b))
            // val (<|) : ('a -> 'b) -> 'a -> 'b
            "op_PipeLeft",
            fun level ->
                let a = freshAt level
                let b = freshAt level
                TyFun(TyFun(a, b), TyFun(a, b))
            // val (>>) : ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)
            "op_ComposeRight",
            fun level ->
                let a = freshAt level
                let b = freshAt level
                let c = freshAt level
                TyFun(TyFun(a, b), TyFun(TyFun(b, c), TyFun(a, c)))
            // val (<<) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
            "op_ComposeLeft",
            fun level ->
                let a = freshAt level
                let b = freshAt level
                let c = freshAt level
                TyFun(TyFun(b, c), TyFun(TyFun(a, b), TyFun(a, c)))
        ]
        |> List.map (fun (n, build) -> n, ExternalSymbols.poly n build)

    /// Collection module functions used by the codegen slices. Registered
    /// under their *source* qualified name (`List.fold`) because that is the
    /// key NameResolution / Unification look the provider up with — the
    /// compiled name (`…ListModule.Fold`) only matters to a target backend.
    let private listFns =
        let freshAt (level: int) : SemType =
            let tv = TypeVar()
            tv.Level <- level
            TyVar tv

        [
            // val fold<'T,'State> : ('State -> 'T -> 'State) -> 'State -> 'T list -> 'State
            //
            // The list parameter is the *Vesper* list (R3): a bare program's `[…]`
            // literal is flexible (`Unification.listLiteralTy`), so this drives it to
            // `Vesper.Collections.List` — the literal then emits BCL-only and the
            // fold runs over the Vesper list. (A literal nothing pins this way, e.g.
            // under `%A`, defaults back to FSharp.Core's `list`.)
            "List.fold",
            fun level ->
                let state = freshAt level
                let t = freshAt level
                let folder = TyFun(state, TyFun(t, state))
                let listOfT = TyRecord("Vesper.Collections.List", [ t ])
                TyFun(folder, TyFun(state, TyFun(listOfT, state)))
        ]
        |> List.map (fun (n, build) -> n, ExternalSymbols.poly n build)

    /// printf-family entry points. Registered with their *generic* signature
    /// `… -> PrintfFormat<'T, …> -> 'T` so plain name resolution and the
    /// non-literal-format fallback type sensibly; the literal-format typing
    /// rule lives in `Unification` and bypasses this signature. See
    /// [front-end-gaps-plan](docs/front-end-gaps-plan.md) §B.
    let private printfOps =
        let freshAt (level: int) : SemType =
            let tv = TypeVar()
            tv.Level <- level
            TyVar tv

        [
            for KeyValue(name, fam) in PrintfSpec.families ->
                name, ExternalSymbols.poly name (fun level -> PrintfSpec.genericSignature (fun () -> freshAt level) fam)
        ]

    /// Core `Operators` functions the codegen slices use. `failwith` is
    /// polymorphic in its result (`string -> 'T`); the CLR backend lowers it to
    /// a BCL-only `throw new System.Exception(msg)` (P3d.3), so it pins no
    /// FSharp.Core dependency.
    let private coreFns =
        let freshAt (level: int) : SemType =
            let tv = TypeVar()
            tv.Level <- level
            TyVar tv

        [
            // val failwith : string -> 'T
            "failwith", fun level -> TyFun(tyString, freshAt level)
            // val hash : 'T -> int  (Operators.hash, CompiledName "Hash"). The
            // `when 'T: equality` constraint the real `.fsi` carries is the
            // contract provider's concern; MockBuiltins is the codegen test
            // provider, so it only needs the shape — `hash x` resolving and
            // typing — to drive the backend (docs/core-operators-handoff.md).
            "hash", fun level -> TyFun(freshAt level, tyInt)
        ]
        |> List.map (fun (n, build) -> n, ExternalSymbols.poly n build)

    let private builtins =
        (monoOps @ polyOps @ listFns @ printfOps @ coreFns) |> Map.ofList

    let provider: IExternalSymbolProvider =
        { new IExternalSymbolProvider with
            member _.TryLookup(name) =
                match Map.tryFind name builtins with
                | Some s -> ValueSome s
                | None -> ValueNone

            member _.TryLookupType _ = ValueNone
            member _.TryLookupMember(_, _) = ValueNone
        }
