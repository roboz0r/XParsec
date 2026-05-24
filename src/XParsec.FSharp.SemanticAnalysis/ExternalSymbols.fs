namespace XParsec.FSharp.SemanticAnalysis

// The full FSharp.Core (+) story (SRTPs + static-optimisation clauses +
// per-target inline IL) is firmly future work — see
// [[project_inline_il_target_specific]] for why we don't model it here.

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

/// Type-declaration shape carried by `IExternalSymbolProvider.TryLookupType`.
/// `arity` is the number of declared typars (same length the builder
/// arrays expect at instantiation). class/interface/enum/delegate types are
/// deferred and currently return `ValueNone` from the provider.
[<RequireQualifiedAccess>]
type ExternalTypeShape =
    | Abbrev of arity: int * body: (SemType[] -> SemType)
    /// Field order matches source.
    | Record of arity: int * fields: ExternalFieldShape[]
    /// Case order matches source.
    | Union of arity: int * cases: ExternalCaseShape[]

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
    /// provider doesn't (yet) model — classes, interfaces, enums, delegates,
    /// etc. Consumers fall back to `TyConst` / `TyRecord` nominal behaviour
    /// when this returns `ValueNone`.
    abstract TryLookupType: name: string -> ExternalTypeShape voption

module ExternalSymbols =

    let mono (name: string) (ty: SemType) : ExternalSymbol =
        {
            Name = name
            Instantiate = fun _ -> ty
            Constraints = []
        }

    /// `build level` is invoked per lookup so any `TypeVar` it allocates is
    /// fresh and stamped at the caller's let-depth.
    let poly (name: string) (build: int -> SemType) : ExternalSymbol =
        {
            Name = name
            Instantiate = build
            Constraints = []
        }

    /// Like `poly` but carries constraints. The `build` closure is responsible
    /// for applying them to the fresh TyVars it allocates; this helper just
    /// records the structured shape on the symbol for introspection use.
    let polyWith (name: string) (build: int -> SemType) (constraints: ExternalConstraint list) : ExternalSymbol =
        {
            Name = name
            Instantiate = build
            Constraints = constraints
        }

    /// For tests that want to isolate behavior from external-symbol noise.
    let nullProvider: IExternalSymbolProvider =
        { new IExternalSymbolProvider with
            member _.TryLookup _ = ValueNone
            member _.TryLookupType _ = ValueNone
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
            "List.fold",
            fun level ->
                let state = freshAt level
                let t = freshAt level
                let folder = TyFun(state, TyFun(t, state))
                let listOfT = TyRecord("Microsoft.FSharp.Collections.list", [ t ])
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
        }
