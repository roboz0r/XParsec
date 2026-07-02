namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore

/// The DIRECTIONAL layer: the read-only subtyping query `subsumes` and the
/// ground-evaluation of the carried TS type-level computations
/// (`keyof`/`T[K]`/conditional) — mutually recursive, since a conditional's
/// `extends` test IS `subsumes` and a union member folds before it is compared.
/// The dependency rule is one-way — `UnificationEngineCore` <- `UnificationSubsume`
/// <- `UnificationEngine`; nothing here calls back into `unify`.
module UnificationSubsume =

    /// The result of the subtyping query `subsumes`: `Equal` when the two
    /// types are the same nominal type (with invariant args in v1), `Subtype`
    /// when `src` is a strict descendant of `tgt` — either along the `inherit`
    /// chain or because `src` (or a base) declares `tgt` as an interface —
    /// `Unrelated` otherwise.
    [<RequireQualifiedAccess>]
    type SubsumeOutcome =
        | Equal
        | Subtype
        | Unrelated

    /// Does the string ENUM `enumKey`'s case-VALUE set sit ⊆ the string-literal
    /// members of a union? Reads the enum's resolved string case values off
    /// `EnumTypeInfo.CaseStringValues` (populated at NameResolution — the values are
    /// available before Elaborate for the simple string-literal case this admission
    /// needs). Declines (`false`) for a numeric/mixed enum, an unresolved enum, or a
    /// union with no literal members covering every value — driving the `subsumes`
    /// arm to fall through to `Unrelated` (the honest rejection for a non-matching
    /// enum). Non-literal union members are simply ignored (they can't cover a value).
    let private enumAdmitsIntoLiteralUnion (ctx: PassContext) (enumKey: SymbolKey) (members: EqSet<SemType>) : bool =
        let litValues = HashSet<string>()

        for m in members do
            match resolveStep m with
            | TyLiteral(LiteralConst.String s) -> litValues.Add s |> ignore
            | _ -> ()

        let enumName = SymbolKeyOps.bareName (SymbolKeyOps.qualifiedName enumKey)

        match TypeRegistry.tryEnum ctx.Types enumName with
        // The registry is keyed by bare name but `enumKey` is the full identity axis
        // (home assembly + namespace) — require them to agree, so a same-named enum
        // from another namespace/package never admits against this one's value set.
        | ValueSome info when info.Key = enumKey ->
            match info.CaseStringValues with
            | ValueSome vals when vals.Length > 0 -> vals |> Array.forall litValues.Contains
            | _ -> false
        | _ -> false

    // ─── R4a step 3: ground-evaluation of the carried TS type-level computations ───
    //
    // `keyof T`, `T[K]`, and `check extends extends_ ? whenTrue : whenFalse` ride the
    // manifest FAITHFULLY as inert `TyKeyOf`/`TyIndexedAccess`/`TyConditional` carrier
    // nodes (step 2). The front end GROUND-EVALUATES them here (design §"keyof … ride on
    // top") once their inputs are concrete; an unground node stays carried (a deferred
    // node — it must NOT poison unification, so `unify` only re-enters on a node that
    // actually folded to a non-carrier type). External-vocabulary only: Vesper inference
    // never mints one, so a fold can only ARISE from an instantiated external signature.

    /// Ground member NAMES of a record / interface / class `t` (for `keyof`), or
    /// `ValueNone` when `t` is not a nominal whose members are known here (a free var, a
    /// primitive, a still-carried node) so `keyof` stays inert. Project-local records read
    /// `RecordTypeInfo.Fields`; an external manifest interface/class reads its provider
    /// shape's INSTANCE members (declared `.d.ts` order preserved, deduped across method
    /// overloads).
    let private groundMemberNames (ctx: PassContext) (t: SemType) : string list voption =
        match resolveStep t with
        | TyRecord(key, _) ->
            match TypeRegistry.tryRecordByKey ctx.Types key with
            | ValueSome info -> ValueSome [ for f in info.Fields -> f.Name ]
            | ValueNone -> ValueNone
        | TyClass(key, _) when (TypeRegistry.tryClassByKey ctx.Types key).IsNone ->
            match ctx.Provider.TryLookupType(SymbolKeyOps.qualifiedName key) with
            | ValueSome(ExternalTypeShape.Class shape) ->
                ValueSome(
                    shape.Members
                    |> Array.filter (fun m -> not m.IsStatic)
                    |> Array.map (fun m -> m.Name)
                    |> Array.distinct
                    |> Array.toList
                )
            | _ -> ValueNone
        | _ -> ValueNone

    /// The ground type of member `name` on record / interface / class `t` (for `T[K]`),
    /// or `ValueNone` when `t` is not a known nominal or has no such member. A
    /// project-local field's declared type is substituted against the receiver's args; an
    /// external value member is realised through `instantiateSignature`.
    let private groundMemberType (ctx: PassContext) (t: SemType) (name: string) : SemType voption =
        match resolveStep t with
        | TyRecord(key, args) ->
            match TypeRegistry.tryRecordByKey ctx.Types key with
            | ValueSome info ->
                match info.Fields |> Array.tryFind (fun f -> f.Name = name) with
                | Some field -> ValueSome(instantiateMember (info.TypeParams, args) field.Type)
                | None -> ValueNone
            | ValueNone -> ValueNone
        | TyClass(key, args) when (TypeRegistry.tryClassByKey ctx.Types key).IsNone ->
            match ctx.Provider.TryLookupMember(SymbolKeyOps.qualifiedName key, name) with
            | ValueSome m when m.IsValueMember && not m.IsStatic ->
                ValueSome(
                    ExternalSymbols.instantiateSignature m (args |> EqArray.toList |> List.toArray) ctx.CurrentLevel
                )
            | _ -> ValueNone
        | _ -> ValueNone

    /// The literal members of `t` when it is a PURE literal shape: a single
    /// `TyLiteral` yields a singleton, a `TyOr` yields its members iff EVERY member
    /// is a literal, anything else is `ValueNone`. THE one pure-literal-union
    /// collector — the admission seams (`T[K]` index keys, keyof-bound literal
    /// sets, literal parameter slots) all specialise this rather than re-rolling
    /// the collect-and-check loop. Callers pre-fold (`evalTypeLevel`) per their
    /// own seam; this only `resolveStep`s.
    let tryLiteralMembers (t: SemType) : LiteralConst list voption =
        match resolveStep t with
        | TyLiteral v -> ValueSome [ v ]
        | TyOr ms ->
            let acc = ResizeArray<LiteralConst>()
            let mutable allLit = true

            for m in ms.Members do
                match resolveStep m with
                | TyLiteral v -> acc.Add v
                | _ -> allLit <- false

            if allLit && acc.Count > 0 then
                ValueSome(List.ofSeq acc)
            else
                ValueNone
        | _ -> ValueNone

    /// `tryLiteralMembers` narrowed to ALL-STRING literals (`ValueNone` when any
    /// member is a non-string literal or a non-literal).
    let tryLiteralStrings (t: SemType) : string list voption =
        match tryLiteralMembers t with
        | ValueSome vs ->
            let strings =
                vs
                |> List.choose (
                    function
                    | LiteralConst.String s -> Some s
                    | LiteralConst.Int _ -> None
                )

            if List.length strings = List.length vs then
                ValueSome strings
            else
                ValueNone
        | ValueNone -> ValueNone

    /// The STRING literal keys an indexed-access index selects: a single `TyLiteral`, or a
    /// union of them (`T[keyof T]`). `ValueNone` for a non-literal / mixed index, so the
    /// access stays carried (the documented precision fallback, not a hard error).
    let private indexLiteralKeys (index: SemType) : string list voption = tryLiteralStrings index

    /// No free `TyVar` and no still-carried type-level node anywhere in `t` — the gate a
    /// conditional's `check`/`extends` must pass before its `extends` test can decide.
    let rec private isGroundEval (t: SemType) : bool =
        match resolveStep t with
        | TyVar _
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ -> false
        | t -> SemType.forallChildren isGroundEval t

    /// A carried type-level node (`keyof`/`T[K]`/conditional) occurs anywhere in
    /// `t` — the gate behind which `foldMemberCarried` pays for `evalTypeLevel`.
    let rec private hasCarriedNode (t: SemType) : bool =
        match resolveStep t with
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ -> true
        | t -> SemType.existsChild hasCarriedNode t

    /// Subtyping query distinct from `unify`: does a value of type `src`
    /// coerce to the statically-known type `tgt`? A **pure read** of
    /// `ctx.Types.Class` — never mutates `Link` / `Constraints`, so it's safe
    /// to call from the read-only coercion site (`:?`) without an undo trace
    ///
    /// Reflexivity is `Equal` (callers distinguish a redundant cast from a real
    /// one); the parent-chain / interface walk yields `Subtype`. Args are
    /// invariant in v1 — `List<Circle>` does not subsume `List<Shape>`. The
    /// argument / `:>` coercion sites use `tryCoerceUpcast` instead, which
    /// *unifies* the witness's type args (so a free var in the target, e.g. the
    /// `_` in `this :> seq<_>`, is pinned).
    ///
    /// Layered on `tryUpcastWitness` — the witness is reflexive and stops at the
    /// first name match, exactly `subsumes`' semantics — so the subtype traversal
    /// lives in one place. `src` subsumes `tgt` iff `src` reaches `tgt`'s nominal
    /// with invariant-equal args; reflexive (same root nominal) is `Equal`, a
    /// base/interface hop is `Subtype`. Non-nominal operands fall back to identity.
    ///
    /// This is the union-aware dispatcher: a `TyOr` on either side resolves
    /// structurally (member set ⊆ member set, value ∈ member set) and the
    /// non-union case delegates to `subsumesNominal`, which carries the original
    /// inherit/interface walk. Splitting the two keeps the nominal traversal flat
    /// rather than nested under a union fallthrough. Union members fold their
    /// nested carried type-level nodes before comparison (`foldMemberCarried`),
    /// so callers never pre-fold a union operand.
    let rec subsumes (ctx: PassContext) (src: SemType) (tgt: SemType) : SubsumeOutcome =
        match resolveStep src, resolveStep tgt with
        // union → union (`A | B ≤ A | B | C`, order-insensitive): every member of
        // the source must land in some member of the target. Identical canonical
        // member sets are `Equal` (reflexivity, sound because both are sorted/deduped);
        // a member-wise subset is `Subtype`. `A | B ⋠ A | C` ⇒ `Unrelated`.
        | TyOr ss, TyOr ts ->
            let ssm = ss.Members
            let tsm = ts.Members

            if ssm = tsm then
                SubsumeOutcome.Equal
            elif
                ssm
                |> EqSet.forall (fun s ->
                    tsm
                    |> EqSet.exists (fun t -> subsumes ctx s (foldMemberCarried ctx t) <> SubsumeOutcome.Unrelated)
                )
            then
                SubsumeOutcome.Subtype
            else
                SubsumeOutcome.Unrelated
        // A Vesper string ENUM admits into a literal union when its case-VALUE set
        // ⊆ the union's literal set (design §"a Vesper string ENUM … admits when its
        // case-VALUE set ⊆ the union"). The nominal companion for code that wants to
        // name the literal type. ADDITIVE — an enum previously fell to the generic
        // `src', TyOr ts` arm and was `Unrelated`; this only widens the match, never
        // narrows non-literal behaviour. Non-string enums / unions with a non-literal
        // member decline (the guard fails) and fall through to `Unrelated`.
        | TyEnum ek, TyOr ts when enumAdmitsIntoLiteralUnion ctx ek ts.Members -> SubsumeOutcome.Subtype
        // member → union (`A ≤ A | B`): `Equal` when `src` *is* a member by
        // structural `=`, `Subtype` when it subsumes into some member (e.g. a
        // subclass of a member, or a literal widening into a base-primitive member).
        // `src` is necessarily non-union here (the union → union arm above caught it).
        | src', TyOr ts ->
            let tsm = ts.Members

            if tsm |> EqSet.exists (fun t -> foldMemberCarried ctx t = src') then
                SubsumeOutcome.Equal
            elif
                tsm
                |> EqSet.exists (fun t -> subsumes ctx src' (foldMemberCarried ctx t) <> SubsumeOutcome.Unrelated)
            then
                SubsumeOutcome.Subtype
            else
                SubsumeOutcome.Unrelated
        // union → member/other (`A | B ⋠ A`): coerces only when *every* member
        // subsumes the target (target = `obj` or a wider type) — otherwise the
        // consumer must narrow first. `never` (`TyOr []`) subsumes into everything
        // (`forall` over the empty set). A literal-union widening to its base
        // primitive (`("a"|"b") ≤ string`) falls out here via the `TyLiteral` arm.
        | TyOr ss, _ ->
            if
                ss.Members
                |> EqSet.forall (fun s -> subsumes ctx (foldMemberCarried ctx s) tgt <> SubsumeOutcome.Unrelated)
            then
                SubsumeOutcome.Subtype
            else
                SubsumeOutcome.Unrelated
        // OUTWARD widening: a structural literal widens to its BASE primitive
        // (design §"outward, a literal (union) WIDENS to its base primitive"), so
        // reading a literal-typed value back into Vesper needs nothing new. This is
        // DIRECTIONAL — the converse (plain `string` into a literal) is NOT admitted
        // here (a `string` source hits `subsumesNominal` → `Unrelated`); the only
        // inward path is the syntactic-constant consultation at the external-arg seam.
        | TyLiteral v, TyConst(n, _) when n = v.BaseName -> SubsumeOutcome.Subtype
        // The arrow↔`Fun` correspondence: a structural arrow
        // `TyFun(a,b)` IS a subtype of the canonical `Vesper.Fun`2<a,b>` interface.
        // This is the ONE place the two layers meet — the unifier keeps seeing
        // `TyFun` as the structural arrow everywhere else (function-representation
        // §"Two layers"); only a `'TF :> Fun<…>` constrained-typar slot discharges
        // through here. Args are invariant (same rule as `subsumesNominal`): the
        // arrow's domain/codomain must each be `Equal` to the `Fun`'s type args. This
        // is a read-only check, not a `unify` — grounding a still-free `Fun`-arg FROM
        // the arrow is deferred, not yet exercised. Arity-1 `Fun`2`
        // — a curried `TyFun(a, TyFun(b,c))` against `Fun`2` falls out naturally
        // (codomain = the inner arrow), with no flat-`Fun2`/`Fun3` special-casing.
        | TyFun(a, b), (TyClass(tk, targs)) when
            SymbolKeyOps.bareName (SymbolKeyOps.qualifiedName tk) = funInterfaceQualifiedName
            && targs.Length = 2
            ->
            if
                subsumes ctx a targs.[0] = SubsumeOutcome.Equal
                && subsumes ctx b targs.[1] = SubsumeOutcome.Equal
            then
                SubsumeOutcome.Subtype
            else
                SubsumeOutcome.Unrelated
        // The FLAT-2 arrow↔`Fun2` correspondence: a CURRIED arrow
        // `TyFun(a, TyFun(b,c))` IS a subtype of the canonical
        // `Vesper.Fun2`3<a,b,c>` interface — a saturated 2-arg slot. Sibling of the
        // arity-1 `Vesper.Fun` arm above (`Fun2` does NOT inherit `Fun`,
        // so the two arms are independent). Same read-only, invariant-arg discipline:
        // the two arrow domains and the final codomain must each be `Equal` to the
        // `Fun2`'s three type args. The caller records the arity-2 verdict for the
        // lambda node (`inferApp`), keyed for the value-struct flat-`Invoke` lowering.
        | TyFun(a, TyFun(b, c)), (TyClass(tk, targs)) when
            SymbolKeyOps.bareName (SymbolKeyOps.qualifiedName tk) = fun2InterfaceQualifiedName
            && targs.Length = 3
            ->
            if
                subsumes ctx a targs.[0] = SubsumeOutcome.Equal
                && subsumes ctx b targs.[1] = SubsumeOutcome.Equal
                && subsumes ctx c targs.[2] = SubsumeOutcome.Equal
            then
                SubsumeOutcome.Subtype
            else
                SubsumeOutcome.Unrelated
        // Neither operand is a union: the nominal subtype walk.
        | _ -> subsumesNominal ctx src tgt

    /// The nominal core of `subsumes` (no union operands): `src` subsumes `tgt`
    /// iff `src` reaches `tgt`'s nominal via the inherit/interface witness with
    /// invariant-equal args — reflexive (same root nominal) is `Equal`, a
    /// base/interface hop is `Subtype`. Non-nominal operands (vars, funcs, tuples)
    /// fall back to identity. Mutually recursive with `subsumes` only through the
    /// invariant-arg check, which may itself face union args.
    and subsumesNominal (ctx: PassContext) (src: SemType) (tgt: SemType) : SubsumeOutcome =
        match subtypeNominalOf ctx src, subtypeNominalOf ctx tgt with
        | ValueSome(struct (s, _)), ValueSome(struct (t, ta)) ->
            match tryUpcastWitness ctx src t with
            // v1 args are invariant: every witnessed arg must itself be `Equal`.
            // The length guard is belt-and-suspenders — a name match implies equal
            // arity in a well-formed program.
            | ValueSome wargs when
                wargs.Length = ta.Length
                && EqArray.forall2 (fun a b -> subsumes ctx a b = SubsumeOutcome.Equal) wargs ta
                ->
                if s = t then
                    SubsumeOutcome.Equal
                else
                    SubsumeOutcome.Subtype
            | _ -> SubsumeOutcome.Unrelated
        | _ ->
            // Non-nominal operands (vars, funcs, tuples): identity only.
            if resolveStep src = resolveStep tgt then
                SubsumeOutcome.Equal
            else
                SubsumeOutcome.Unrelated

    /// Deep-fold carried type-level nodes NESTED inside a union member before the
    /// union arms compare against it. A member that WRAPS an arrow (mitt off's
    /// optional `Handler<Events[Key]> | undefined` → `TyOr`) is compared WHOLE, so
    /// its nested access must fold here (`Handler<Events[Key]>` → `(int) -> unit`)
    /// for the member to match; a bare-arrow parameter already folds at its leaf
    /// via `unify`'s structural descent. Gated on an actual carrier occurrence so
    /// the common (carrier-free) member pays nothing. `evalTypeLevel` is read-only
    /// apart from fresh-var minting in `groundMemberType`'s `instantiateSignature`
    /// (pre-existing fold behaviour), so `subsumes` stays a pure read.
    and private foldMemberCarried (ctx: PassContext) (m: SemType) : SemType =
        if hasCarriedNode m then evalTypeLevel ctx m else m

    /// Ground-evaluate a carried type-level computation as far as its inputs allow.
    /// Returns the FOLDED type when a rule fires; otherwise returns the (child-eval'd)
    /// carrier unchanged so it stays inert. Never mints a literal for a Vesper expression
    /// — the folds only rewrite nodes that already exist in an external signature.
    and evalTypeLevel (ctx: PassContext) (t: SemType) : SemType =
        match resolveStep t with
        // `keyof T` → the union of `T`'s member NAMES as string literals.
        | TyKeyOf inner ->
            let inner = evalTypeLevel ctx inner

            match groundMemberNames ctx inner with
            | ValueSome names -> SemType.MkUnion [ for n in names -> TyLiteral(LiteralConst.String n) ]
            | ValueNone -> TyKeyOf inner
        // `T[K]` → the (union of the) addressed member type(s), when `T` is a known
        // nominal and `K` is a literal / literal union. A missing key leaves it carried.
        | TyIndexedAccess(objTy, index) ->
            let objTy = evalTypeLevel ctx objTy
            let index = evalTypeLevel ctx index

            match indexLiteralKeys index with
            | ValueSome keys ->
                let tys = ResizeArray<SemType>()
                let mutable allFound = true

                for k in keys do
                    match groundMemberType ctx objTy k with
                    | ValueSome ty -> tys.Add ty
                    | ValueNone -> allFound <- false

                if allFound && tys.Count > 0 then
                    SemType.MkUnion(List.ofSeq tys)
                else
                    TyIndexedAccess(objTy, index)
            | ValueNone -> TyIndexedAccess(objTy, index)
        // `check extends extends_ ? whenTrue : whenFalse` → pick a branch once `check` and
        // `extends_` are ground; the `extends` test is the directional `subsumes`
        // membership/subtype query (a ground union's membership included). mitt's
        // `undefined extends Events[Key] ? Key : never` is the pinned stress case.
        | TyConditional(check, extends, whenTrue, whenFalse) ->
            let check = evalTypeLevel ctx check
            let extends = evalTypeLevel ctx extends

            if isGroundEval check && isGroundEval extends then
                match subsumes ctx check extends with
                | SubsumeOutcome.Unrelated -> evalTypeLevel ctx whenFalse
                | _ -> evalTypeLevel ctx whenTrue
            else
                TyConditional(check, extends, evalTypeLevel ctx whenTrue, evalTypeLevel ctx whenFalse)
        // COMPOUND types recurse so a carried node NESTED inside them folds too — a
        // `keyof`/`T[K]` under a `TyFun`, `TyOr`, tuple, or nominal argument. A bare
        // `TyFun` parameter (mitt's `on` handler `(Events[Key]) -> unit`) already folds at
        // its leaf via `unify`'s structural descent, but a param that WRAPS the arrow (off's
        // optional `Handler<Events[Key]> | undefined` → `TyOr`) is admitted by `subsumes`,
        // which compares members whole — so its nested access must be folded HERE for the
        // member to match. `mapChildren` routes `TyOr` through its smart constructor
        // (a folded member can collapse/reorder the set).
        | t -> SemType.mapChildren (evalTypeLevel ctx) t

    /// A carried type-level node folded to a CONCRETE (non-carrier) type, or `ValueNone`
    /// when it is not a carrier or is still inert — the guard `unify`/`subsumes` re-enter
    /// on. The `ValueSome` result is guaranteed non-carrier, so re-entry makes progress
    /// (no loop on a still-deferred node).
    let tryFoldCarried (ctx: PassContext) (t: SemType) : SemType voption =
        match t with
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ ->
            match evalTypeLevel ctx t with
            | TyKeyOf _
            | TyIndexedAccess _
            | TyConditional _ -> ValueNone
            | folded -> ValueSome folded
        | _ -> ValueNone
