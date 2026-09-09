namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open Vesper
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore

/// The DIRECTIONAL layer: the read-only subtyping query `subsumes` and the ground
/// evaluation of the carried type-level computations (`keyof`/`T[K]`/conditional). The two
/// are mutually recursive, since a conditional's `extends` test IS `subsumes`.
module UnificationSubsume =

    /// The result of the subtyping query `subsumes`: `Equal` for the same nominal type
    /// (args invariant in v1), `Subtype` when `src` reaches `tgt` along the `inherit`
    /// chain or by declaring it as an interface, `Unrelated` otherwise.
    [<RequireQualifiedAccess>]
    type SubsumeOutcome =
        | Equal
        | Subtype
        | Unrelated

    /// Does the string ENUM `enumKey`'s case-VALUE set sit ⊆ the string-literal disjuncts
    /// of a union? `false` for a numeric/mixed enum, an unresolved enum, or a union whose
    /// literals miss a value; a non-literal disjunct covers nothing, so it is ignored.
    let private enumAdmitsIntoLiteralUnion (ctx: PassContext) (enumKey: TypeKey) (disjuncts: EqSet<SemType>) : bool =
        let litValues = HashSet<string>()

        for d in disjuncts do
            match resolveStep ctx.Store d with
            | TyLiteral(LiteralConst.String s) -> litValues.Add s |> ignore
            | _ -> ()

        // Key-addressed (home assembly + namespace + arity), so a same-named enum from
        // another namespace never admits against this one's value set.
        match TypeRegistry.tryEnumByKey ctx.Types enumKey with
        | ValueSome info ->
            match info.CaseStringValues with
            | ValueSome vals when vals.Length > 0 -> vals |> Array.forall litValues.Contains
            | _ -> false
        | _ -> false

    /// Ground member NAMES of a record / interface / class `t` (for `keyof`), or `ValueNone`
    /// when `t`'s members are not known here (a free var, a primitive, a still-carried node),
    /// so `keyof` stays inert. External shapes contribute INSTANCE members, overloads deduped.
    let private groundMemberNames (ctx: PassContext) (t: SemType) : string list voption =
        match resolveStep ctx.Store t with
        | TyRecord(key, _) ->
            match TypeRegistry.tryRecordByKey ctx.Types key with
            | ValueSome info -> ValueSome [ for f in info.Fields -> f.Name ]
            | ValueNone -> ValueNone
        | TyClass(key, _) when (TypeRegistry.tryClassByKey ctx.Types key).IsNone ->
            match ctx.Provider.TryLookupType key with
            // `keyof` reads any external nominal's members, a plain class as well as an
            // interface, so it takes the un-guarded member surface.
            | ValueSome(ExternalSymbols.ExternalMembers members) ->
                ValueSome(
                    members
                    |> Block.filter (fun m -> not m.IsStatic)
                    |> Block.map (fun m -> m.Name)
                    |> Block.distinct
                    |> Block.toList
                )
            | _ -> ValueNone
        | _ -> ValueNone

    /// The ground type of member `name` on record / interface / class `t` (for `T[K]`), or
    /// `ValueNone` when `t` is not a known nominal or has no such member. A read-only fold
    /// query: `openSignature` leaves a member's own typars as `TyTypar(Member _, j)`, minting
    /// no vars.
    let private groundMemberType (ctx: PassContext) (t: SemType) (name: string) : SemType voption =
        match resolveStep ctx.Store t with
        | TyRecord(key, args) ->
            match TypeRegistry.tryRecordByKey ctx.Types key with
            | ValueSome info ->
                match info.Fields |> Array.tryFind (fun f -> f.Name = name) with
                | Some field -> ValueSome(instantiateMember ctx.Store (info.TypeParams, args) field.Type)
                | None -> ValueNone
            | ValueNone -> ValueNone
        | TyClass(key, args) when (TypeRegistry.tryClassByKey ctx.Types key).IsNone ->
            match ctx.Provider.TryLookupMember(key, name) with
            | ValueSome m when m.IsValueMember && not m.IsStatic ->
                ValueSome(ExternalSymbols.openSignature ctx m (Block.toArray args))
            | _ -> ValueNone
        | _ -> ValueNone

    /// The literal disjuncts of `t` when it is a PURE literal shape: a single `TyLiteral`
    /// yields a singleton, a `TyOr` yields its disjuncts iff EVERY one is a literal,
    /// anything else is `ValueNone`. Only `resolveStep`s, so a caller needing a fold pre-folds.
    let tryLiteralDisjuncts (store: TypeStore) (t: SemType) : LiteralConst list voption =
        match resolveStep store t with
        | TyLiteral v -> ValueSome [ v ]
        | TyOr ds ->
            let acc = ResizeArray<LiteralConst>()
            let mutable allLit = true

            for d in ds.Disjuncts do
                match resolveStep store d with
                | TyLiteral v -> acc.Add v
                | _ -> allLit <- false

            if allLit && acc.Count > 0 then
                ValueSome(List.ofSeq acc)
            else
                ValueNone
        | _ -> ValueNone

    /// `tryLiteralDisjuncts` narrowed to ALL-STRING literals (`ValueNone` when any
    /// disjunct is a non-string literal or a non-literal).
    let tryLiteralStrings (store: TypeStore) (t: SemType) : string list voption =
        match tryLiteralDisjuncts store t with
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

    /// No free `TyVar` and no still-carried type-level node anywhere in `t`, the gate a
    /// conditional's `check`/`extends` must pass before its `extends` test can decide.
    let rec private isGroundEval (store: TypeStore) (t: SemType) : bool =
        match resolveStep store t with
        | TyVar _
        | TyCarrier -> false
        | t -> SemType.forallChildren (isGroundEval store) t

    /// A carried type-level node (`keyof`/`T[K]`/conditional) occurs anywhere in `t`. Only such
    /// a type is applicability-OPAQUE to overload filtering; a plain nominal / primitive union
    /// must NOT act as a wildcard, or every union-typed argument matches every same-arity slot.
    let rec hasCarriedNode (store: TypeStore) (t: SemType) : bool =
        match resolveStep store t with
        | TyCarrier -> true
        | t -> SemType.existsChild (hasCarriedNode store) t

    /// Subtyping query distinct from `unify`: does a value of type `src` coerce to `tgt`? It
    /// never mutates `State` / `Constraints`, so a read-only `:?` site needs no undo trace.
    /// A `TyOr` operand resolves structurally, with its disjuncts folded first.
    let rec subsumes (ctx: PassContext) (src: SemType) (tgt: SemType) : SubsumeOutcome =
        match resolveStep ctx.Store src, resolveStep ctx.Store tgt with
        // union → union (`A | B ≤ A | B | C`): identical disjunct sets are `Equal` (`EqSet`
        // equality is order-independent and deduped, so declared order does not matter);
        // every source disjunct landing in some target disjunct is `Subtype`.
        | TyOr ss, TyOr ts ->
            let ssd = ss.Disjuncts
            let tsd = ts.Disjuncts

            if ssd = tsd then
                SubsumeOutcome.Equal
            elif
                ssd
                |> EqSet.forall (fun s ->
                    tsd
                    |> EqSet.exists (fun t -> subsumes ctx s (foldDisjunctCarried ctx t) <> SubsumeOutcome.Unrelated)
                )
            then
                SubsumeOutcome.Subtype
            else
                SubsumeOutcome.Unrelated
        // A Vesper string ENUM admits into a literal union when its case-VALUE set ⊆ the
        // union's literal set, so an enum can be the nominal companion for code that wants to
        // refer to the literal type. A non-string enum declines the guard and takes the arm below.
        | TyEnum ek, TyOr ts when enumAdmitsIntoLiteralUnion ctx ek ts.Disjuncts -> SubsumeOutcome.Subtype
        // disjunct → union (`A ≤ A | B`): `Equal` when `src` *is* a disjunct by structural `=`,
        // `Subtype` when it subsumes into some disjunct (a subclass of one, or a literal
        // widening into a base-primitive one).
        | src', TyOr ts ->
            let tsd = ts.Disjuncts

            if tsd |> EqSet.exists (fun t -> foldDisjunctCarried ctx t = src') then
                SubsumeOutcome.Equal
            elif
                tsd
                |> EqSet.exists (fun t -> subsumes ctx src' (foldDisjunctCarried ctx t) <> SubsumeOutcome.Unrelated)
            then
                SubsumeOutcome.Subtype
            else
                SubsumeOutcome.Unrelated
        // union → disjunct/other (`A | B ⋠ A`): coerces only when *every* disjunct subsumes the
        // target (`obj` or a wider type); otherwise the consumer must narrow first.
        // `never` (`TyOr []`) subsumes into everything (`forall` over the empty set).
        | TyOr ss, _ ->
            if
                ss.Disjuncts
                |> EqSet.forall (fun s -> subsumes ctx (foldDisjunctCarried ctx s) tgt <> SubsumeOutcome.Unrelated)
            then
                SubsumeOutcome.Subtype
            else
                SubsumeOutcome.Unrelated
        // OUTWARD widening: a structural literal widens to its BASE primitive (`"a" ≤
        // string`). The converse, plain `string` into a literal, is not admitted here, because
        // a `string` source reaches `subsumesNominal` and is `Unrelated`.
        | TyLiteral v, TyConst(key, _) when key = RuntimeNames.literalBaseKey v -> SubsumeOutcome.Subtype
        // A structural `TyFun(a, …)` IS a subtype of `Vesper.Fun`(k+1)<a1..ak, r>` when each
        // peeled domain is invariant-`Equal` to its `Fun` arg and the residual codomain
        // matches WHOLE (it may be a further curried function).
        | TyFun(a, b), (TyClass(tk, targs)) ->
            match tryFunSlotPeel ctx.Store tk targs a b with
            | ValueSome tys when
                List.forall2 (fun s t -> subsumes ctx s t = SubsumeOutcome.Equal) tys (Block.toList targs)
                ->
                SubsumeOutcome.Subtype
            | ValueSome _ -> SubsumeOutcome.Unrelated
            | ValueNone -> subsumesNominal ctx src tgt
        | _ -> subsumesNominal ctx src tgt

    /// The nominal core of `subsumes` (no union operands): `src` subsumes `tgt` iff it
    /// reaches `tgt`'s nominal via the inherit/interface witness with invariant-equal args.
    /// Non-nominal operands (vars, funcs, tuples) fall back to identity.
    and subsumesNominal (ctx: PassContext) (src: SemType) (tgt: SemType) : SubsumeOutcome =
        match subtypeNominalOf ctx src, subtypeNominalOf ctx tgt with
        | ValueSome(struct (s, _)), ValueSome(struct (t, ta)) ->
            match tryUpcastWitness ctx src t with
            // v1 args are invariant: every witnessed arg must itself be `Equal`.
            | ValueSome wargs when
                wargs.Length = ta.Length
                && Block.forall2 (fun a b -> subsumes ctx a b = SubsumeOutcome.Equal) wargs ta
                ->
                // A capability's two names (a `seq` source vs an `IEnumerable\`1` target)
                // must read as `Equal`, not a spurious `Subtype`.
                if sameNominalKey ctx s t then
                    SubsumeOutcome.Equal
                else
                    SubsumeOutcome.Subtype
            | _ -> SubsumeOutcome.Unrelated
        | _ ->
            // Non-nominal operands (vars, funcs, tuples): identity only.
            if resolveStep ctx.Store src = resolveStep ctx.Store tgt then
                SubsumeOutcome.Equal
            else
                SubsumeOutcome.Unrelated

    /// Deep-fold carried type-level nodes NESTED inside a union disjunct, since the union
    /// arms compare a disjunct WHOLE: `Handler<Events[Key]> | undefined` only matches once
    /// `Handler<Events[Key]>` has folded to `(int) -> unit`.
    and private foldDisjunctCarried (ctx: PassContext) (d: SemType) : SemType =
        if hasCarriedNode ctx.Store d then
            evalTypeLevel ctx d
        else
            d

    /// Ground-evaluate a carried type-level computation as far as its inputs allow: the
    /// FOLDED type when a rule fires, otherwise the (child-eval'd) carrier unchanged so it
    /// stays inert. Only rewrites nodes that an external signature already carries.
    and evalTypeLevel (ctx: PassContext) (t: SemType) : SemType =
        match resolveStep ctx.Store t with
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

            // A single `TyLiteral` index, or a union of them (`T[keyof T]`); a non-literal
            // or mixed index yields `ValueNone` and leaves the access carried.
            match tryLiteralStrings ctx.Store index with
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
        // membership/subtype query, e.g. `undefined extends Events[Key] ? Key : never`.
        | TyConditional c ->
            let check = evalTypeLevel ctx c.Check
            let extends = evalTypeLevel ctx c.Extends

            if isGroundEval ctx.Store check && isGroundEval ctx.Store extends then
                match subsumes ctx check extends with
                | SubsumeOutcome.Unrelated -> evalTypeLevel ctx c.WhenFalse
                | _ -> evalTypeLevel ctx c.WhenTrue
            else
                TyConditional
                    {
                        Check = check
                        Extends = extends
                        WhenTrue = evalTypeLevel ctx c.WhenTrue
                        WhenFalse = evalTypeLevel ctx c.WhenFalse
                    }
        // COMPOUND types recurse so a carrier NESTED inside them folds too, such as a `keyof`/`T[K]`
        // under a `TyFun`, `TyOr`, tuple, or nominal argument. `mapChildren` routes `TyOr`
        // through its smart constructor, since a folded member can collapse or reorder the set.
        | t when hasCarriedNode ctx.Store t -> SemType.mapChildren (evalTypeLevel ctx) t
        | t -> t

    /// A carried type-level node folded to a CONCRETE (non-carrier) type, or `ValueNone`
    /// when it is not a carrier or is still inert. A `ValueSome` is never itself a carrier,
    /// so a caller re-entering on it makes progress rather than looping.
    let tryFoldCarried (ctx: PassContext) (t: SemType) : SemType voption =
        match t with
        | TyCarrier ->
            match evalTypeLevel ctx t with
            | TyCarrier -> ValueNone
            | folded -> ValueSome folded
        | _ -> ValueNone
