namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.BuiltinTypes

/// Test fixture: the symbol-provider `provider` plus its op / printf / list /
/// core tables. The contract-as-provider demotion removed this module from the
/// codegen stack entirely (see `project_contract_demotion` /
/// docs/symbol-resolution-handoff.md); it now lives in the test assembly so
/// production code can no longer import it. Tests that need real-world
/// behaviour should wire `VesperLib.buildProvider` or
/// `ReferencedProject.buildProvider` directly.
///
/// Divergence risk to be aware of: this provider declares some ops
/// monomorphically (`(+) : int -> int -> int`) where the real contract
/// declares them polymorphically with SRTP.
module MockBuiltins =

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
                let listOfT = TyRecord("Vesper.Collections.List", EqArray.singleton t)
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
    /// a BCL-only `throw new System.Exception(msg)`, so it pins no
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
            // typing — to drive the backend (docs/operators-plan.md).
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
            member _.TryLookupMembers(_, _) = [||]
        }
