namespace XParsec.FSharp.SemanticAnalysis

// The full FSharp.Core (+) story (SRTPs + static-optimisation clauses +
// per-target inline IL) is firmly future work — see
// [[project_inline_il_target_specific]] for why we don't model it here.

/// Type-name lookup is a separate concern (handled by named-type resolution
/// in the type checker, not by this interface).
type ExternalSymbol =
    {
        Name: string
        /// Returns a fresh instantiation of the symbol's type each call.
        /// Monomorphic symbols (`op_Addition`, `op_LessThan`) return the
        /// same SemType every time. Polymorphic symbols (`op_PipeRight`,
        /// `op_ComposeRight`, eventually `List.map`) allocate fresh
        /// TypeVars per call so independent use-sites don't unify with
        /// each other through the shared scheme.
        Instantiate: unit -> SemType
    }

/// Each target supplies its own provider implementation.
///
/// **Thread-safety:** `TryLookup` must be safe to call concurrently from
/// multiple threads. Implementations that cache lazily (e.g. a real
/// `FSharp.Core.dll` reader, or a wrapper exposing another file's
/// post-analysis schemes) must guard their internal mutation. Per-file
/// pipelines run independent `PassContext`s in parallel and may hit the
/// same provider from any of them — see [`docs/architecture.md`](docs/architecture.md#parallelism).
type IExternalSymbolProvider =
    /// `name` is the compiled name ("op_Addition", not "(+)").
    abstract TryLookup: name: string -> ExternalSymbol voption

module ExternalSymbols =

    /// Build a monomorphic symbol — the same `ty` every call.
    let mono (name: string) (ty: SemType) : ExternalSymbol =
        {
            Name = name
            Instantiate = fun () -> ty
        }

    /// Build a polymorphic symbol — `build` is invoked per lookup so any
    /// `TypeVar` it allocates is fresh.
    let poly (name: string) (build: unit -> SemType) : ExternalSymbol = { Name = name; Instantiate = build }

    /// For tests that want to isolate behavior from external-symbol noise.
    let nullProvider: IExternalSymbolProvider =
        { new IExternalSymbolProvider with
            member _.TryLookup _ = ValueNone
        }

/// TODO: replace with FSharp.Core.dll-derived equivalents when .NET
/// integration comes online; the interface above stays the same.
module MockBuiltins =

    let tyInt: SemType = TyConst "int"
    let tyInt64: SemType = TyConst "int64"
    let tyByte: SemType = TyConst "byte"
    let tyFloat: SemType = TyConst "float"
    let tyBool: SemType = TyConst "bool"
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
            // Comparison ops are monomorphic int-only for the tiny subset.
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
    /// `TypeVar`s so two use-sites don't accidentally share variables.
    let private polyOps =
        let fresh () = TyVar(TypeVar())

        [
            // val (|>) : 'a -> ('a -> 'b) -> 'b
            "op_PipeRight",
            fun () ->
                let a = fresh ()
                let b = fresh ()
                TyFun(a, TyFun(TyFun(a, b), b))
            // val (<|) : ('a -> 'b) -> 'a -> 'b
            "op_PipeLeft",
            fun () ->
                let a = fresh ()
                let b = fresh ()
                TyFun(TyFun(a, b), TyFun(a, b))
            // val (>>) : ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)
            "op_ComposeRight",
            fun () ->
                let a = fresh ()
                let b = fresh ()
                let c = fresh ()
                TyFun(TyFun(a, b), TyFun(TyFun(b, c), TyFun(a, c)))
            // val (<<) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
            "op_ComposeLeft",
            fun () ->
                let a = fresh ()
                let b = fresh ()
                let c = fresh ()
                TyFun(TyFun(b, c), TyFun(TyFun(a, b), TyFun(a, c)))
        ]
        |> List.map (fun (n, build) -> n, ExternalSymbols.poly n build)

    let private builtins = (monoOps @ polyOps) |> Map.ofList

    let provider: IExternalSymbolProvider =
        { new IExternalSymbolProvider with
            member _.TryLookup(name) =
                match Map.tryFind name builtins with
                | Some s -> ValueSome s
                | None -> ValueNone
        }
