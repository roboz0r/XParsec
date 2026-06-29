namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.BuiltinTypes

/// Test fixture: the symbol-provider `provider` plus its op / printf / list /
/// core tables. The contract-as-provider demotion removed this module from the
/// codegen stack entirely (see `project_contract_demotion`); it now lives in the test assembly so
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
        |> List.map (fun (n, ty) -> n, ExternalSymbols.monoFrozen n (FrozenTypeBridge.toFrozen ty))

    // FrozenType scheme helpers: a declaring-axis typar marker and an arrow. The
    // scheme builder (`ExternalSymbols.scheme`) freshens these typars per use-site,
    // so the data form replaces the former `freshAt`-closure builders.
    let private tv (i: int) : FrozenType = FTTypar(TyparAxis.Declaring, i)
    let private fn (a: FrozenType) (b: FrozenType) : FrozenType = FTFun(a, b)

    /// Polymorphic operators built from `FSharp.Core`, as `FrozenType` schemes over
    /// their own typars; `ExternalSymbols.scheme` mints fresh `TypeVar`s per use-site
    /// (stamped at the caller's let-depth) so two use-sites don't share variables.
    let private polyOps =
        [
            // val (|>) : 'a -> ('a -> 'b) -> 'b
            "op_PipeRight", fn (tv 0) (fn (fn (tv 0) (tv 1)) (tv 1)), 2
            // val (<|) : ('a -> 'b) -> 'a -> 'b
            "op_PipeLeft", fn (fn (tv 0) (tv 1)) (fn (tv 0) (tv 1)), 2
            // val (>>) : ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)
            "op_ComposeRight", fn (fn (tv 0) (tv 1)) (fn (fn (tv 1) (tv 2)) (fn (tv 0) (tv 2))), 3
            // val (<<) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
            "op_ComposeLeft", fn (fn (tv 1) (tv 2)) (fn (fn (tv 0) (tv 1)) (fn (tv 0) (tv 2))), 3
        ]
        |> List.map (fun (n, frozen, arity) -> n, ExternalSymbols.scheme n frozen arity [])

    /// Collection module functions used by the codegen slices. Registered
    /// under their *source* qualified name (`List.fold`) because that is the
    /// key NameResolution / Unification look the provider up with — the
    /// compiled name (`…ListModule.Fold`) only matters to a target backend.
    let private listFns =
        // val fold<'T,'State> : ('State -> 'T -> 'State) -> 'State -> 'T list -> 'State
        // (typars: 'State = 0, 'T = 1).
        //
        // The list parameter is the *Vesper* list (R3): a bare program's `[…]`
        // literal is flexible (`Unification.listLiteralTy`), so this drives it to
        // `Vesper.Collections.List` — the literal then emits BCL-only and the
        // fold runs over the Vesper list. (A literal nothing pins this way, e.g.
        // under `%A`, defaults back to FSharp.Core's `list`.)
        let listOfT =
            FTRecord(SymbolKeyOps.qualifiedTypeKey "Vesper.Collections.List" 1, EqArray.singleton (tv 1))

        let folder = fn (tv 0) (fn (tv 1) (tv 0))
        let foldScheme = fn folder (fn (tv 0) (fn listOfT (tv 0)))

        [ "List.fold", ExternalSymbols.scheme "List.fold" foldScheme 2 [] ]

    /// printf-family entry points. Registered with their *generic* signature
    /// `… -> PrintfFormat<'T, …> -> 'T` so plain name resolution and the
    /// non-literal-format fallback type sensibly; the literal-format typing
    /// rule lives in `Unification` and bypasses this signature. See
    /// [front-end-gaps-plan](docs/front-end-gaps-plan.md) §B.
    let private printfOps =
        [
            for KeyValue(name, fam) in PrintfSpec.families ->
                // One-typar scheme `… -> PrintfFormat<'T, …> -> 'T`; `scheme` freshens
                // the printer typar per use-site (replaces the former freshAt closure).
                name, ExternalSymbols.scheme name (PrintfSpec.genericSignatureFrozen fam) 1 []
        ]

    /// Core `Operators` functions the codegen slices use. `failwith` is
    /// polymorphic in its result (`string -> 'T`); the CLR backend lowers it to
    /// a BCL-only `throw new System.Exception(msg)`, so it pins no
    /// FSharp.Core dependency.
    let private coreFns =
        let frozenString = FrozenTypeBridge.toFrozen tyString
        let frozenInt = FrozenTypeBridge.toFrozen tyInt

        [
            // val failwith : string -> 'T
            "failwith", fn frozenString (tv 0), 1
            // val hash : 'T -> int  (Operators.hash, CompiledName "Hash"). The
            // `when 'T: equality` constraint the real `.fsi` carries is the
            // contract provider's concern; MockBuiltins is the codegen test
            // provider, so it only needs the shape — `hash x` resolving and
            // typing — to drive the backend.
            "hash", fn (tv 0) frozenInt, 1
        ]
        |> List.map (fun (n, frozen, arity) -> n, ExternalSymbols.scheme n frozen arity [])

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
            member _.TryLookupUnionCase _ = ValueNone
            member _.AmbientOpenPrefixes = []
            member _.TryLookupInlineBody _ = ValueNone
            member _.TryLookupInlineBodyByName _ = ValueNone
            member _.IntrinsicReverseCanon = Map.empty
            member _.IntrinsicForwardRepr = Map.empty
        }
