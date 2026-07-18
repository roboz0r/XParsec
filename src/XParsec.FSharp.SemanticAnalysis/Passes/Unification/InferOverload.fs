namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate

module UnificationInferOverload =

    /// A union whose every member is a structural literal — kept by-VALUE in overload
    /// filtering (its `argSigOf` spelling is sharp), unlike a union with a function /
    /// carried-node member, which is applicability-opaque.
    let isPureLiteralUnion (ms: UnionMembers) : bool =
        ms.Members
        |> EqSet.forall (fun m ->
            match zonk m with
            | TyLiteral _ -> true
            | _ -> false
        )

    /// Does `t` carry a not-yet-ground type-level computation anywhere inside it?
    /// Only such a type is genuinely applicability-OPAQUE; a plain nominal /
    /// primitive union must NOT act as a filtering wildcard (it would perturb BCL
    /// overload sets — any union-typed argument would match every same-arity
    /// parameter of every overloaded external method).
    let rec hasCarriedNode (t: SemType) : bool =
        match zonk t with
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ -> true
        | t -> SemType.existsChild hasCarriedNode t

    /// The trial substitution accumulated by `matchTypes` for ONE candidate — the
    /// scratch that makes the applicability pre-check exact WITHOUT touching the
    /// shared graph. Two bindable leaves, each keyed to match its identity:
    ///   * `MethodTypars` — the candidate's OWN method typars, keyed by Method-axis
    ///     index. `M<'T>('T, 'T)` opens (`ExternalSymbols.openSignature`) to the SAME
    ///     `TyTypar(Method, 0)` at every position, so consistency across argument
    ///     positions is index equality — this is what forgetting per-position let the
    ///     over-accept bug through.
    ///   * `CallerVars` — the caller-side free metavars the trial touches, keyed by
    ///     union-find ROOT identity (`TypeVar` is a sealed class, so `Dictionary`
    ///     uses reference equality; `HashIdentity.Reference` states it).
    /// Local to the trial and DROPPED on failure. `matchTypes` writes only here — never
    /// `Link`/`union`/`drainAll` — so a failed trial leaves no residue in the shared
    /// union-find (all of `unify`'s mutation is confined to its metavar arms, `Engine.fs`,
    /// reachable only through a `Link` write, which this never performs).
    type private TrialBindings =
        {
            MethodTypars: Dictionary<int, SemType>
            CallerVars: Dictionary<TypeVar, SemType>
        }

        static member Create() =
            {
                MethodTypars = Dictionary()
                CallerVars = Dictionary(HashIdentity.Reference)
            }

    /// The bindings-accumulating generalisation of `applicabilityMatches` (below): the
    /// SAME arm structure, kept auditably parallel to it AND to `unify`'s concrete-head
    /// arms (`Engine.fs`, `TyConst`/`TyRecord`/`TyUnion`/`TyClass`/`TyFun`/`TyTuple`/…),
    /// so a future reader can diff the three. The one difference is that the two arms
    /// which are genuinely BINDABLE — an open method typar and a free metavar — RECORD
    /// their binding in `binds` and check it for consistency, instead of returning `true`
    /// unconditionally. That fixes the over-accept (a shared `'T` now remembers its first
    /// binding) and the under-accept (a free caller `TyVar` against a concrete parameter
    /// now BINDS rather than falling to `| _ -> false`).
    ///
    /// The other wildcard arms STAY "matches anything": the carried type-level nodes and
    /// the non-pure / carried-node unions are applicability-OPAQUE, not bindable — their
    /// real admission is still the `unifyAppliedSig` commit seam. No subtyping / `subsumes`
    /// enters here (that is a later change); `isObjectTy` stays the caller's obj policy.
    ///
    /// Read-only w.r.t. the shared graph. The head `zonk` follows any already-committed
    /// `Link` (subsuming `resolveStep`), so a bound caller var is resolved BEFORE the
    /// metavar arm; a still-free var reaches the arm and is looked up two-tier — the
    /// scratch `binds.CallerVars` after the graph — binding it if unseen.
    let rec private matchTypes (canon: SymbolKey -> SymbolKey) (binds: TrialBindings) (a: SemType) (b: SemType) : bool =
        match zonk a, zonk b with
        // Binder arm (was the `-> true` wildcard): a generic method's own typar
        // (`TyTypar(Method, i)`) binds to whatever it first meets and must AGREE at every
        // later occurrence — index equality across positions.
        | TyTypar(TyparAxis.Method, i), other
        | other, TyTypar(TyparAxis.Method, i) -> matchMethodTypar canon binds i other
        // Applicability-OPAQUE, not bindable: their structural identity can't be decided
        // until a call site grounds them, so they stay "matches anything" (the
        // `unifyAppliedSig` commit seam does the real work).
        | (TyKeyOf _ | TyIndexedAccess _ | TyConditional _), _
        | _, (TyKeyOf _ | TyIndexedAccess _ | TyConditional _) -> true
        // A non-literal union parameter / a carried-node union argument stay opaque, exactly
        // as in `applicabilityMatches` — their members can carry a not-yet-ground node.
        | _, TyOr ms when not (isPureLiteralUnion ms) -> true
        | TyOr ms, _ when EqSet.exists hasCarriedNode ms.Members -> true
        | TyLiteral v1, TyLiteral v2 -> v1 = v2
        | TyConst(k1, xs), TyConst(k2, ys) -> k1 = k2 && EqArray.forall2 (matchTypes canon binds) xs ys
        // Binder arm (was `ReferenceEquals(find x, find y)`, which failed a free caller
        // var against a concrete parameter — the under-accept bug): a free metavar on
        // EITHER side binds to the opposite type, or agrees if already bound.
        | TyVar tv, other
        | other, TyVar tv -> matchVar canon binds tv other
        | TyFun(a1, r1), TyFun(a2, r2) -> matchTypes canon binds a1 a2 && matchTypes canon binds r1 r2
        | TyTuple xs, TyTuple ys -> EqArray.forall2 (matchTypes canon binds) xs ys
        | TyRecord(n1, xs), TyRecord(n2, ys)
        | TyUnion(n1, xs), TyUnion(n2, ys)
        | TyClass(n1, xs), TyClass(n2, ys) ->
            (n1 = n2 || canon (SymbolKey.Type n1) = canon (SymbolKey.Type n2))
            && EqArray.forall2 (matchTypes canon binds) xs ys
        | _ -> false

    /// A method typar binds on first sight and must agree thereafter (index equality
    /// carries the shared-`'T` constraint the old per-position wildcard forgot).
    and private matchMethodTypar
        (canon: SymbolKey -> SymbolKey)
        (binds: TrialBindings)
        (i: int)
        (other: SemType)
        : bool =
        match binds.MethodTypars.TryGetValue i with
        | true, bound -> matchTypes canon binds bound other
        | _ ->
            binds.MethodTypars.[i] <- other
            true

    /// A free caller metavar: two-tier lookup — the shared graph was already consulted by
    /// `matchTypes`'s head `zonk` (so a committed `Link` never reaches here), leaving only
    /// the scratch `binds.CallerVars`. Bound ⇒ recurse against the binding; unseen ⇒ record
    /// it (writing ONLY the scratch dictionary). Two free vars already unified in the graph
    /// match with no new binding — the success case of the old `ReferenceEquals` arm.
    and private matchVar (canon: SymbolKey -> SymbolKey) (binds: TrialBindings) (tv: TypeVar) (other: SemType) : bool =
        let root = UnionFind.find tv

        match binds.CallerVars.TryGetValue root with
        | true, bound -> matchTypes canon binds bound other
        | _ ->
            match other with
            | TyVar tv2 when System.Object.ReferenceEquals(UnionFind.find tv2, root) -> true
            | _ ->
                binds.CallerVars.[root] <- other
                true

    /// NOT an equality: the structural-match relation overload FILTERING uses.
    /// Wildcard arms (open method typars, carried type-level nodes, opaque unions)
    /// deliberately return `true` for anything — "indistinguishable during
    /// filtering", with the real admission at the `unifyAppliedSig` commit seam.
    /// `canon` normalises a nominal key to its capability-canonical face (identity for
    /// every non-capability key) so a `seq` argument admits a BCL `IEnumerable\`1`
    /// parameter of an overloaded external method (`Enumerable.Take`) — the overload-
    /// filter mirror of the `unify` / `subsumes` capability reconciliation. The
    /// `SemType`-free codegen path passes `id` (frozen overloads carry no capability
    /// gap; a mismatch there just falls back to the first arity match).
    let rec applicabilityMatches (canon: SymbolKey -> SymbolKey) (a: SemType) (b: SemType) : bool =
        match zonk a, zonk b with
        // A generic method's own typar (`Take<TSource>` ⇒ `TyTypar(Method, _)`,
        // kept as a wildcard in the open signature by `ExternalSymbols.openSignature`)
        // is unconstrained — it matches any argument during applicability filtering,
        // so a generic external method resolves against concrete call-site types.
        // The eventual instantiation is recovered by
        // `tryInferExternalStaticMethodCall` (front end) / `recoverTypeArgs`
        // (codegen); here it is a wildcard at any structural depth.
        | TyTypar(TyparAxis.Method, _), _
        | _, TyTypar(TyparAxis.Method, _) -> true
        // A CARRIED TS type-level computation (`keyof`/`T[K]`/conditional) is
        // applicability-OPAQUE — its structural identity can't be decided until a call
        // site grounds it (R4a step 3), so during overload FILTERING it matches any
        // argument, exactly like an open method typar. The real solution happens at the
        // `unifyAppliedSig` commit seam (where the fold fires). External-vocabulary only,
        // so this never perturbs a BCL overload set (none carry these nodes).
        | (TyKeyOf _ | TyIndexedAccess _ | TyConditional _), _
        | _, (TyKeyOf _ | TyIndexedAccess _ | TyConditional _) -> true
        // A non-literal UNION parameter (off's optional `Handler<Events[Key]> | undefined`)
        // is applicability-OPAQUE during filtering, same as the carried-node / open-typar
        // wildcards above: its members can carry a not-yet-ground `T[K]` and the real
        // admission (member subsumption + carried-node fold) happens at the
        // `unifyAppliedSig` commit seam. A PURE literal union stays by-VALUE below so
        // literal-union overload specificity (the sharp `argSigOf` spelling) is preserved.
        // A `TyOr` only ever arises from TS vocabulary, so a param-side wildcard cannot
        // perturb a BCL overload set.
        | _, TyOr ms when not (isPureLiteralUnion ms) -> true
        // The ARGUMENT side is a wildcard ONLY when the union genuinely carries a
        // not-yet-ground node — the actual motivation. A plain nominal/primitive union
        // argument (`string | MyClass` by annotation) must fall through to structural
        // comparison, not match every same-arity parameter.
        | TyOr ms, _ when EqSet.exists hasCarriedNode ms.Members -> true
        // Two structural literals are equal by VALUE (so `on("*", …)` prefers the
        // literal-`'*'` overload over a same-position typar); a literal vs a non-literal
        // falls through to `false` (a plain `string` is not a specific literal).
        | TyLiteral v1, TyLiteral v2 -> v1 = v2
        | TyConst(k1, xs), TyConst(k2, ys) -> k1 = k2 && EqArray.forall2 (applicabilityMatches canon) xs ys
        | TyVar x, TyVar y -> System.Object.ReferenceEquals(UnionFind.find x, UnionFind.find y)
        | TyFun(a1, r1), TyFun(a2, r2) -> applicabilityMatches canon a1 a2 && applicabilityMatches canon r1 r2
        | TyTuple xs, TyTuple ys -> EqArray.forall2 (applicabilityMatches canon) xs ys
        // The `canon n1 = canon n2` fallback fires only for a capability interface (its two
        // faces canonicalise equal); `canon` is identity for records/unions and every
        // non-capability class, so the common `n1 = n2` short-circuits unchanged.
        | TyRecord(n1, xs), TyRecord(n2, ys)
        | TyUnion(n1, xs), TyUnion(n2, ys)
        | TyClass(n1, xs), TyClass(n2, ys) ->
            (n1 = n2 || canon (SymbolKey.Type n1) = canon (SymbolKey.Type n2))
            && EqArray.forall2 (applicabilityMatches canon) xs ys
        | _ -> false

    /// `object`/`obj` is the only supertype we model — no other reference
    /// hierarchy, so a non-`object` param only matches an arg it equals.
    and isObjectTy (t: SemType) : bool =
        match zonk t with
        | TyObj -> true
        | _ -> false

    and asSpecificOrEq (canon: SymbolKey -> SymbolKey) (aTy: SemType) (bTy: SemType) : bool =
        applicabilityMatches canon aTy bTy || isObjectTy bTy

    /// Flattens the tupled signature back to N parameters. The `argSig` length — the
    /// member's own identity — distinguishes a flattened N-param method from a genuine
    /// single tuple param; the signature alone cannot.
    and memberParamTypes (typeArgs: SemType[]) (m: ExternalMember) : SemType list =
        let n = m.Key.ArgSig.Length

        match zonk (ExternalSymbols.openSignature m typeArgs) with
        | TyFun(TyTuple elems, _) when n >= 2 && elems.Length = n -> EqArray.toList elems
        | TyFun(TyUnit, _) when n = 0 -> []
        | TyFun(p, _) -> [ p ]
        | _ -> []

    /// `ValueNone` = none applicable, or no unique best (ambiguous — the caller diagnoses).
    /// Static/instance/ctor agnostic: pure arity + `argAssignable` + specificity ranking
    /// over any `ExternalMember[]` candidate set (callers pre-filter by static-ness).
    and pickBestOverload
        (canon: SymbolKey -> SymbolKey)
        (typeArgs: SemType[])
        (candidates: ExternalMember[])
        (argElems: SemType list)
        : ExternalMember voption =
        let arity = List.length argElems

        let applicable =
            candidates
            |> Array.filter (fun m ->
                m.Key.ArgSig.Length = arity
                && (let ps = memberParamTypes typeArgs m

                    List.length ps = arity
                    // One scratch substitution per candidate trial: a shared method typar
                    // must bind consistently across positions, and a free caller var binds
                    // rather than failing. `binds` is dropped when this `forall2` short-
                    // circuits false, so a rejected trial leaves no residue. `isObjectTy`
                    // keeps its no-pin obj absorption (bypassing the trial, as before).
                    && (let binds = TrialBindings.Create()
                        List.forall2 (fun a p -> matchTypes canon binds a p || isObjectTy p) argElems ps))
            )

        // The single applicable survivor short-circuits before the betterness ranking —
        // fsc's single-candidate fast path (a lone name/arity candidate never reaches the
        // picker at all: `InferExternalCall` declines to the single-pick + commit-seam path
        // when a name has ≤1 overload, so a lone shared-typar mismatch surfaces as a commit
        // error, not an applicability rejection).
        match applicable with
        | [||] -> ValueNone
        | [| only |] -> ValueSome only
        | many ->
            let betterThan (a: ExternalMember) (b: ExternalMember) =
                let pa = memberParamTypes typeArgs a
                let pb = memberParamTypes typeArgs b

                List.forall2 (asSpecificOrEq canon) pa pb
                && List.exists2 (fun x y -> not (applicabilityMatches canon x y)) pa pb

            let best =
                many
                |> Array.filter (fun a ->
                    many
                    |> Array.forall (fun b -> System.Object.ReferenceEquals(a, b) || betterThan a b)
                )

            match best with
            | [| unique |] -> ValueSome unique
            | _ -> ValueNone
