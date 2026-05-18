namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  ctx.Desugared and ctx.Binding populated.
// Post: ctx.TypeVar populated; every TypeVar's Link reaches its solved type
//       via UnionFind.find. ctx.Scheme populated for every generalisable
//       `let`-bound name (single-name headPats — see `shouldGeneralise`).
//
// Algorithm J + Rémy's levels: fresh TypeVar per AST node, constraints
// generated and unified on the fly. Generalisation happens at the close of
// each binding group; instantiation at every use of a scheme-bearing name.
//
// Tiny-subset omissions (TODO):
//   - Value restriction is split: the *generalisation gate* on
//     `mutableToken` lives here (`shouldGeneralise`), but the *diagnostic*
//     for a mutable binding whose resolved type still has free TyVars at
//     end of analysis lives in Validation — by then every use site has
//     had a chance to pin them via unification. See docs/mutable-plan.md.
//   - `ref` cells / refs-as-values still generalise without a check; lands
//     when the `Ref<'a>` provider entry does (see mutable-plan §Open questions).
//   - SRTP / IWSAM bound resolution. The on-unified callbacks per
//     docs/typevar.md aren't wired yet.
//   - Binding-level return-type annotations (`let f x : int = ...`). Only
//     Expr.TypeAnnotation (`(e : t)`) is handled today.

module Unification =

    /// Walk TyVar links to the equivalence-class representative; if the rep
    /// has a Link, return its target (one level deep — call recursively for
    /// full resolution). Stops at a measure-bearing root so the measure
    /// stays attached: `unify` and `unitsOf` need the TyVar wrapper to
    /// see Units, and following Link straight through to the bare carrier
    /// would drop them.
    let private resolveStep (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome t' when root.Units.IsNone -> t'
            | _ -> TyVar root
        | _ -> t

    /// Fully resolve a SemType: walk all TyVar chains AND recurse into TyFun
    /// arms. Used by Freeze (and tests) to materialise the final inferred
    /// type for a node. A measure-bearing TyVar (`Units` set on its root)
    /// is preserved as a TyVar rather than collapsed into its carrier —
    /// the measure rides on the root, so downstream consumers can read it
    /// off the returned `TyVar` (already a root).
    let rec zonk (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome t' when root.Units.IsNone -> zonk t'
            | _ -> TyVar root
        | TyConst _ -> t
        | TyFun(a, r) -> TyFun(zonk a, zonk r)
        | TyTuple items -> TyTuple(List.map zonk items)
        | TyRecord(n, args) -> TyRecord(n, List.map zonk args)
        | TyUnion(n, args) -> TyUnion(n, List.map zonk args)
        | TyClass(n, args) -> TyClass(n, List.map zonk args)

    /// Merge `source.Constraints` into `target.Constraints`, collapsing
    /// any pair whose `Kind` already appears on the target. Two
    /// constraints with the same `Kind` discharge to the same predicate;
    /// keeping both would fire the diagnostic twice for what is, from
    /// the satisfaction-checker's point of view, one rule.
    let private mergeConstraints (target: TypeVar) (additions: SemanticConstraint list) : unit =
        let mutable acc = target.Constraints

        for c in additions do
            if not (acc |> List.exists (fun existing -> existing.Kind = c.Kind)) then
                acc <- c :: acc

        target.Constraints <- acc

    /// Move pending deferred-constraint state from `source` onto `target`.
    /// Called whenever a TyVar is no longer the equivalence-class
    /// representative (either after union-find collapse, or when its Link is
    /// set). Bounds attached to a non-representative would otherwise never
    /// fire their on-unified callbacks.
    let private migrateBounds (target: TypeVar) (source: TypeVar) : unit =
        if not (System.Object.ReferenceEquals(target, source)) then
            if not (List.isEmpty source.Constraints) then
                mergeConstraints target source.Constraints
                source.Constraints <- []

            if not (List.isEmpty source.SrtpBounds) then
                target.SrtpBounds <- source.SrtpBounds @ target.SrtpBounds
                source.SrtpBounds <- []

            if not (List.isEmpty source.PendingDotAccess) then
                target.PendingDotAccess <- source.PendingDotAccess @ target.PendingDotAccess
                source.PendingDotAccess <- []
    // TODO: fire on-unified callbacks for newly-stable SRTP bounds once
    // the SRTP / IWSAM resolution machinery exists.

    /// Two passes folded into one walk:
    /// (a) **Occurs check** — does `target` (already a union-find root) appear
    ///     anywhere inside `t`? Stops the `let rec f x = f` / `let rec g = g g`
    ///     family from cycling Link pointers and making zonk loop.
    /// (b) **Level adjustment** — when `target` is about to be linked to `t`,
    ///     every TyVar reachable from `t` becomes co-scoped with `target`.
    ///     Lower any reachable level above `target.Level` down to it so
    ///     generalisation at the enclosing scope sees the right "free" set.
    /// Resolves through Links and recurses into compound shapes. The `||`
    /// short-circuit on occurs-fail leaves some reachable TyVars unadjusted,
    /// but a failed unification produces a diagnostic and there's nothing
    /// to generalise after; adjusting them would be wasted work.
    let rec private occursAndAdjust (target: TypeVar) (t: SemType) : bool =
        match resolveStep t with
        | TyVar tv ->
            let root = UnionFind.find tv

            if System.Object.ReferenceEquals(root, target) then
                true
            else
                if root.Level > target.Level then
                    root.Level <- target.Level

                false
        | TyConst _ -> false
        | TyFun(a, r) -> occursAndAdjust target a || occursAndAdjust target r
        | TyTuple items -> List.exists (occursAndAdjust target) items
        | TyRecord(_, args) -> List.exists (occursAndAdjust target) args
        | TyUnion(_, args) -> List.exists (occursAndAdjust target) args
        | TyClass(_, args) -> List.exists (occursAndAdjust target) args

    /// Merge the Units field of two union-find roots after they've been
    /// joined into `newRoot`. Two non-equal measures emit a diagnostic; one
    /// of them is kept on the survivor so further unifications against it
    /// stay coherent. ValueNone on one side propagates from the other.
    let private mergeUnits
        (ctx: PassContext)
        (key: NodeKey)
        (newRoot: TypeVar)
        (unitsA: MeasureTerm voption)
        (unitsB: MeasureTerm voption)
        : unit =
        match unitsA, unitsB with
        | ValueNone, ValueNone -> ()
        | ValueSome m, ValueNone
        | ValueNone, ValueSome m -> newRoot.Units <- ValueSome m
        | ValueSome m1, ValueSome m2 when m1.Equals(m2) -> newRoot.Units <- ValueSome m1
        | ValueSome m1, ValueSome m2 ->
            newRoot.Units <- ValueSome m1

            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = sprintf "Measure mismatch: <%O> vs <%O>" m1 m2
                    Severity = Error
                }

    /// Substitute TyVar roots that appear as keys in `subst` with their
    /// target `SemType`. Walks compound shapes (TyFun, TyTuple, TyRecord,
    /// TyUnion args) so a nested `'a` deep inside a named-type arg gets
    /// rewritten the same way a top-level `'a` would. Other TyVars are
    /// returned unchanged (followed through union-find but not their
    /// `Link`s — that's `zonk`'s job). Used by `instantiate` to swap
    /// quantified typars for fresh ones, and by record / union code to
    /// substitute a type's `TypeParams` with the receiver's arg list at
    /// every use site. Public so Freeze can reuse the same substitution
    /// when reading field types off a generic receiver.
    let rec substituteWith (subst: Dictionary<TypeVar, SemType>) (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match subst.TryGetValue root with
            | true, target -> target
            | false, _ ->
                // Field / case-arg types stored on the registry start as
                // *placeholder* TyVars that Unification's fill-in linked
                // to the real declared type. The placeholder's own root
                // is never in `subst` — substitute keys are the
                // declared type's `TypeParams`. Follow `Link` so a
                // placeholder whose target is `TyVar typarRoot` resolves
                // to whatever `subst[typarRoot]` says.
                //
                // Stop at measure-bearing roots (same rule `zonk` uses):
                // a measured TyVar's `Link` carries the bare carrier, and
                // following through would drop the `Units` attached to
                // the root.
                match root.Link with
                | ValueSome target when root.Units.IsNone -> substituteWith subst target
                | _ -> TyVar root
        | TyConst _ -> t
        | TyFun(a, r) -> TyFun(substituteWith subst a, substituteWith subst r)
        | TyTuple xs -> TyTuple [ for x in xs -> substituteWith subst x ]
        | TyRecord(n, args) -> TyRecord(n, [ for a in args -> substituteWith subst a ])
        | TyUnion(n, args) -> TyUnion(n, [ for a in args -> substituteWith subst a ])
        | TyClass(n, args) -> TyClass(n, [ for a in args -> substituteWith subst a ])

    /// Pair a named type's declared `TypeParams` with the args provided
    /// at a use site. Empty when the lengths don't match — the caller has
    /// already (or should) emit an arity diagnostic, and an empty subst
    /// keeps the field types unsubstituted rather than silently mismatching.
    /// Public so Freeze can rebuild the same substitution when projecting
    /// fields off a generic receiver in a field-chain.
    let mkNamedTypeSubst (typeParams: (string * TypeVar) list) (args: SemType list) : Dictionary<TypeVar, SemType> =
        let subst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)

        if List.length typeParams = List.length args then
            List.iter2 (fun (_, tp) arg -> subst.[UnionFind.find tp] <- arg) typeParams args

        subst

    /// Walk a `SemType` through TyVar Links to surface a `TyRecord _` if
    /// the type has resolved to one. Returns ValueNone for free TyVars and
    /// non-record concrete types. The arg list rides along so
    /// `drainPendingDotAccess` can substitute the record's typars when
    /// resolving deferred field accesses.
    let rec private tryResolveRecord (t: SemType) : (string * SemType list) voption =
        match t with
        | TyRecord(n, args) -> ValueSome(n, args)
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome target -> tryResolveRecord target
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Mirror of `tryResolveRecord` for `TyClass`. Used by the drain path
    /// so a deferred dot-access against a now-pinned class receiver picks
    /// up the right member registry.
    let rec private tryResolveClass (t: SemType) : (string * SemType list) voption =
        match t with
        | TyClass(n, args) -> ValueSome(n, args)
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome target -> tryResolveClass target
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Three-valued result of evaluating a constraint against a candidate
    /// type. `Defer` is the "I don't know yet" answer: the target is
    /// still free (or compound-with-free-args) and a future unification
    /// might pin it. `drainConstraints` keeps deferred constraints on
    /// the TyVar so they re-fire on the next `Link` change.
    type private ConstraintOutcome =
        | Satisfied
        | Violated
        | Defer

    /// The primitive carrier set used by `primitiveSupports`. Includes
    /// every non-string primitive `MockBuiltins` mints — `string` is
    /// handled separately since it's a reference type.
    let private primitiveValueTypes =
        Set.ofList [ "int"; "int64"; "byte"; "bool"; "float"; "float32"; "char"; "unit" ]

    /// Source-text rendering of a `SemanticConstraintKind` for diagnostics.
    let private constraintKindName (k: SemanticConstraintKind) : string =
        match k with
        | SemanticConstraintKind.Equality -> "equality"
        | SemanticConstraintKind.Comparison -> "comparison"
        | SemanticConstraintKind.Struct -> "struct"
        | SemanticConstraintKind.ReferenceType -> "not struct"
        | SemanticConstraintKind.Nullness -> "null"
        | SemanticConstraintKind.NotNull -> "not null"

    let rec private unify (ctx: PassContext) (key: NodeKey) (a: SemType) (b: SemType) =
        let a = resolveStep a
        let b = resolveStep b

        match a, b with
        | TyConst n1, TyConst n2 when n1 = n2 -> ()
        | TyRecord(n1, a1), TyRecord(n2, a2) when n1 = n2 && a1.Length = a2.Length -> List.iter2 (unify ctx key) a1 a2
        | TyUnion(n1, a1), TyUnion(n2, a2) when n1 = n2 && a1.Length = a2.Length -> List.iter2 (unify ctx key) a1 a2
        | TyClass(n1, a1), TyClass(n2, a2) when n1 = n2 && a1.Length = a2.Length -> List.iter2 (unify ctx key) a1 a2
        | TyFun(a1, r1), TyFun(a2, r2) ->
            unify ctx key a1 a2
            unify ctx key r1 r2
        | TyTuple xs, TyTuple ys when xs.Length = ys.Length -> List.iter2 (unify ctx key) xs ys
        | TyVar tv1, TyVar tv2 when System.Object.ReferenceEquals(tv1, tv2) -> ()
        | TyVar tv1, TyVar tv2 ->
            let r1 = UnionFind.find tv1
            let r2 = UnionFind.find tv2
            let unitsA = r1.Units
            let unitsB = r2.Units
            let linkA = r1.Link
            let linkB = r2.Link
            UnionFind.union r1 r2
            // After union, exactly one of r1/r2 still has Parent = ValueNone.
            let newRoot = UnionFind.find r1

            let merged =
                if System.Object.ReferenceEquals(newRoot, r1) then
                    r2
                else
                    r1

            migrateBounds newRoot merged
            mergeUnits ctx key newRoot unitsA unitsB
            // Migrate Link: if one side carried a concrete target and the
            // survivor doesn't, copy it across. If both sides carried links,
            // unify them so the carrier types agree.
            match linkA, linkB with
            | ValueNone, ValueNone -> ()
            | ValueSome _, ValueNone ->
                newRoot.Link <- linkA

                match linkA with
                | ValueSome t ->
                    drainPendingDotAccess ctx newRoot t
                    drainConstraints ctx key newRoot t
                | ValueNone -> ()
            | ValueNone, ValueSome _ ->
                newRoot.Link <- linkB

                match linkB with
                | ValueSome t ->
                    drainPendingDotAccess ctx newRoot t
                    drainConstraints ctx key newRoot t
                | ValueNone -> ()
            | ValueSome a, ValueSome b ->
                newRoot.Link <- linkA
                unify ctx key a b

                match linkA with
                | ValueSome t ->
                    drainPendingDotAccess ctx newRoot t
                    drainConstraints ctx key newRoot t
                | ValueNone -> ()
        | TyVar tv, other
        | other, TyVar tv ->
            let root = UnionFind.find tv

            if occursAndAdjust root other then
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message =
                            sprintf
                                "Occurs check: cannot construct infinite type %A = %A"
                                (zonk (TyVar root))
                                (zonk other)
                        Severity = Error
                    }
            else
                // Linking to a plain TyConst (a dimensionless carrier) when
                // the variable is already known to be measured is a
                // dimensionless-vs-measured mismatch.
                match root.Units, other with
                | ValueSome m, TyConst _ when not m.IsDimensionless ->
                    ctx.Diagnostics.Add
                        {
                            Key = key
                            Message = sprintf "Dimensionless %A used where <%O> expected" other m
                            Severity = Error
                        }
                | _ -> ()

                root.Link <- ValueSome other
                drainPendingDotAccess ctx root other
                drainConstraints ctx key root other
        | _ ->
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = sprintf "Type mismatch: %A vs %A" (zonk a) (zonk b)
                    Severity = Error
                }

    /// When a TyVar's Link is set to (or resolves to) a `TyRecord T` or
    /// `TyClass T`, resolve any dot-access constraints that were parked
    /// on it. Each entry unifies the access expression's result TyVar
    /// with the field's / member's declared type; a missing field /
    /// member produces a diagnostic. When `T` is generic, the receiver's
    /// arg list substitutes for the type's declared typars so
    /// `(b : Box<int>).Value` resolves to `int`, not `Box`'s prototype
    /// `'a`.
    and private drainPendingDotAccess (ctx: PassContext) (root: TypeVar) (linkTarget: SemType) : unit =
        if List.isEmpty root.PendingDotAccess then
            ()
        else
            match tryResolveRecord linkTarget with
            | ValueSome(recName, args) ->
                let pending = root.PendingDotAccess
                root.PendingDotAccess <- []

                match ctx.RecordTypes.TryGetValue recName with
                | true, info ->
                    let subst = mkNamedTypeSubst info.TypeParams args

                    for (fieldName, useKey, resultTv) in pending do
                        match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                        | Some field -> unify ctx useKey (TyVar resultTv) (substituteWith subst field.Type)
                        | None ->
                            ctx.Diagnostics.Add
                                {
                                    Key = useKey
                                    Message = sprintf "Type '%s' has no field '%s'" recName fieldName
                                    Severity = Error
                                }
                | false, _ ->
                    for (_, useKey, _) in pending do
                        ctx.Diagnostics.Add
                            {
                                Key = useKey
                                Message = sprintf "Unknown record type '%s'" recName
                                Severity = Error
                            }
            | ValueNone ->
                match tryResolveClass linkTarget with
                | ValueNone -> ()
                | ValueSome(clsName, args) ->
                    let pending = root.PendingDotAccess
                    root.PendingDotAccess <- []

                    match ctx.ClassTypes.TryGetValue clsName with
                    | true, info ->
                        let subst = mkNamedTypeSubst info.TypeParams args

                        for (memberName, useKey, resultTv) in pending do
                            match info.Members |> Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic) with
                            | Some m -> unify ctx useKey (TyVar resultTv) (substituteWith subst m.Type)
                            | None ->
                                ctx.Diagnostics.Add
                                    {
                                        Key = useKey
                                        Message = sprintf "Type '%s' has no instance member '%s'" clsName memberName
                                        Severity = Error
                                    }
                    | false, _ ->
                        for (_, useKey, _) in pending do
                            ctx.Diagnostics.Add
                                {
                                    Key = useKey
                                    Message = sprintf "Unknown class type '%s'" clsName
                                    Severity = Error
                                }

    /// Built-in primitive support table. `ValueSome true` is a definitive
    /// "yes, this constraint holds on `name`"; `ValueSome false` is a
    /// definitive "no, violation"; `ValueNone` means "not in the table,
    /// fall through to structural / deferred handling". The set covers
    /// the primitives `MockBuiltins` mints — extend as new ground types
    /// reach the analyser.
    and private primitiveSupports (kind: SemanticConstraintKind) (name: string) : bool voption =
        let isValueType = Set.contains name primitiveValueTypes
        let isString = name = "string"

        match kind with
        | SemanticConstraintKind.Equality
        | SemanticConstraintKind.Comparison ->
            if isValueType || isString then
                ValueSome true
            else
                ValueNone
        | SemanticConstraintKind.Struct ->
            if isValueType then ValueSome true
            elif isString then ValueSome false
            else ValueNone
        | SemanticConstraintKind.ReferenceType ->
            if isString then ValueSome true
            elif isValueType then ValueSome false
            else ValueNone
        | SemanticConstraintKind.Nullness ->
            if isString then ValueSome true
            elif isValueType then ValueSome false
            else ValueNone
        | SemanticConstraintKind.NotNull ->
            if isValueType then ValueSome true
            elif isString then ValueSome false
            else ValueNone

    /// Folded outcome of evaluating a list of `checkConstraint` results.
    /// `Violated` is sticky (once any element fails, the whole compound
    /// fails); `Defer` propagates only when no element has failed but at
    /// least one is still pending.
    and private reduceOutcome (check: SemType -> ConstraintOutcome) (items: SemType list) : ConstraintOutcome =
        let mutable result = Satisfied

        for item in items do
            match result, check item with
            | Violated, _ -> ()
            | _, Violated -> result <- Violated
            | Defer, _
            | _, Defer -> result <- Defer
            | Satisfied, Satisfied -> ()

        result

    /// Decide whether `c` is satisfied by `t`. The receiver shape is
    /// resolved through TyVar links one step at a time; nested compounds
    /// (TyTuple / TyRecord / TyUnion) recurse compositionally. Free
    /// TyVars return `Defer` so the next `Link` assignment re-fires the
    /// check via `drainConstraints`.
    and private checkConstraint (ctx: PassContext) (c: SemanticConstraint) (t: SemType) : ConstraintOutcome =
        match c.Kind, resolveStep t with
        | _, TyVar _ -> Defer
        | k, TyConst name ->
            match primitiveSupports k name with
            | ValueSome true -> Satisfied
            | ValueSome false -> Violated
            | ValueNone -> Defer
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyFun _ ->
            // Function types support neither structural equality nor
            // comparison in F#.
            Violated
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyTuple items ->
            reduceOutcome (checkConstraint ctx c) items
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyRecord(name, args) ->
            match ctx.RecordTypes.TryGetValue name with
            | true, info ->
                let subst = mkNamedTypeSubst info.TypeParams args

                info.Fields
                |> Array.map (fun f -> substituteWith subst f.Type)
                |> Array.toList
                |> reduceOutcome (checkConstraint ctx c)
            | false, _ -> Defer
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyUnion(name, args) ->
            match ctx.UnionTypes.TryGetValue name with
            | true, info ->
                let subst = mkNamedTypeSubst info.TypeParams args

                [
                    for case in info.Cases do
                        for field in case.Fields -> substituteWith subst field
                ]
                |> reduceOutcome (checkConstraint ctx c)
            | false, _ -> Defer
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyClass _ ->
            // Per docs/classes-plan.md §Open questions: F# classes are
            // reference-equal by default; structural equality / comparison
            // for classes requires the attribute walker. Defer in v1.
            Defer
        | SemanticConstraintKind.Struct, (TyTuple _ | TyFun _ | TyRecord _ | TyUnion _ | TyClass _) ->
            // v1: tuples, functions, and reference records / unions /
            // classes are all reference types. `[<Struct>]`-attributed
            // records / unions / structs ship with the attribute walker.
            Violated
        | SemanticConstraintKind.ReferenceType, (TyTuple _ | TyFun _ | TyRecord _ | TyUnion _ | TyClass _) -> Satisfied
        | SemanticConstraintKind.Nullness, _ ->
            // Nullness analysis is a separate track — defer until it
            // lands. Treating as `Defer` (not `Violated`) keeps existing
            // code that doesn't annotate nullability noise-free.
            Defer
        | SemanticConstraintKind.NotNull, _ -> Defer

    /// On-unified callback for type-parameter constraints. Walks
    /// `root.Constraints`, evaluates each against the new Link target,
    /// and emits diagnostics for any that fail. Constraints that resolve
    /// (Satisfied) are dropped; ones that defer remain on the root and
    /// re-fire next time `Link` changes (which, after the first set,
    /// only happens during union-find collapse). For compound `Defer`
    /// outcomes (the target is a TyRecord/TyUnion/TyTuple with free
    /// arg TyVars), copy the constraint onto each still-free arg so the
    /// next Link on any of them re-evaluates the rule compositionally.
    and private drainConstraints (ctx: PassContext) (key: NodeKey) (root: TypeVar) (linkTarget: SemType) : unit =
        if List.isEmpty root.Constraints then
            ()
        else
            let cs = root.Constraints
            root.Constraints <- []
            let mutable remaining = []

            for c in cs do
                match checkConstraint ctx c linkTarget with
                | Satisfied -> ()
                | Violated ->
                    ctx.Diagnostics.Add
                        {
                            Key = key
                            Message =
                                sprintf
                                    "The type '%A' does not support the '%s' constraint"
                                    (zonk linkTarget)
                                    (constraintKindName c.Kind)
                            Severity = Error
                        }
                | Defer ->
                    remaining <- c :: remaining
                    propagateToFreeArgs ctx c linkTarget

            root.Constraints <- List.rev remaining

    /// Attach `c` to any free-TyVar arg reachable inside `linkTarget`.
    /// Used by `drainConstraints` when a compound shape (TyRecord /
    /// TyUnion / TyTuple) is partially resolved: the parent constraint
    /// is satisfied iff every component supports it, so a still-free
    /// component carries the same constraint forward.
    and private propagateToFreeArgs (ctx: PassContext) (c: SemanticConstraint) (t: SemType) : unit =
        let rec walk t =
            match resolveStep t with
            | TyVar tv ->
                let root = UnionFind.find tv

                if not (root.Constraints |> List.exists (fun e -> e.Kind = c.Kind)) then
                    root.Constraints <- c :: root.Constraints
            | TyConst _ -> ()
            | TyFun(a, r) ->
                walk a
                walk r
            | TyTuple xs -> List.iter walk xs
            | TyRecord(_, args) -> List.iter walk args
            | TyUnion(_, args) -> List.iter walk args
            | TyClass(_, args) -> List.iter walk args

        walk t

    let private enterLevel (ctx: PassContext) : unit =
        ctx.CurrentLevel <- ctx.CurrentLevel + 1

    let private exitLevel (ctx: PassContext) : unit =
        ctx.CurrentLevel <- ctx.CurrentLevel - 1

    /// Allocate a fresh, unkeyed TypeVar at the current let-depth. Used for
    /// intermediate "result" TyVars in apps, ifs, matches — anything not
    /// directly tied to a CST node's NodeKey.
    let private freshTyVar (ctx: PassContext) : TypeVar =
        let tv = TypeVar()
        tv.Level <- ctx.CurrentLevel
        tv

    /// Allocate a fresh TypeVar for `key`, stamped at the current level, and
    /// store it in ctx.TypeVar. Overwrites any prior entry — callers that
    /// need "get or allocate" (e.g. for forward-referenced let-rec siblings)
    /// must go through `tvOf`.
    let private freshTv (ctx: PassContext) (key: NodeKey) : TypeVar =
        let tv = TypeVar()
        tv.Level <- ctx.CurrentLevel
        ctx.TypeVar.Set(key, tv)
        tv

    /// Look up the TypeVar previously stored for a node. Fresh-allocates if
    /// missing — happens for binding-site patterns that haven't been visited
    /// yet by inferPat, including forward references inside `let rec` groups.
    let private tvOf (ctx: PassContext) (key: NodeKey) : TypeVar =
        match ctx.TypeVar.TryGetValue key with
        | ValueSome tv -> tv
        | ValueNone -> freshTv ctx key

    /// Mint fresh TyVars at the current level for every quantifier of
    /// `scheme`, then walk `scheme.Body` rewriting each quantified TyVar to
    /// its fresh counterpart. Mirrors `ExternalSymbol.Instantiate` for the
    /// finite set of `'a`s captured by a user-written `let`. Non-quantified
    /// TyVars are left alone — they're free with respect to the surrounding
    /// scope and must keep their identity. `scheme.Body` is already zonked
    /// by `generalise`, so we don't follow Links here.
    let private instantiate (ctx: PassContext) (scheme: TypeScheme) : SemType =
        let subst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)
        // Build the substitution and remember each fresh TyVar so the
        // scheme's per-quantifier constraints can be re-stamped onto it.
        let freshOf = Dictionary<TypeVar, TypeVar>(HashIdentity.Reference)

        for q in scheme.Quantified do
            let qRoot = UnionFind.find q
            let fresh = TypeVar()
            fresh.Level <- ctx.CurrentLevel
            subst.[qRoot] <- TyVar fresh
            freshOf.[qRoot] <- fresh

        // Re-stamp constraints onto the freshly minted instance TyVars.
        // A use site that pins the fresh TyVar will then re-evaluate
        // satisfaction against its own substitution; the original
        // quantified TyVars stay constraint-bearing for the next call.
        for (qTv, c) in scheme.Constraints do
            let qRoot = UnionFind.find qTv

            match freshOf.TryGetValue qRoot with
            | true, fresh ->
                if not (fresh.Constraints |> List.exists (fun e -> e.Kind = c.Kind)) then
                    fresh.Constraints <- c :: fresh.Constraints
            | false, _ -> ()

        substituteWith subst scheme.Body

    /// Walk `zonkedTy` and collect every union-find root whose Level strictly
    /// exceeds `outerLevel` — those are the TyVars to quantify. Dedupes by
    /// reference identity (a single TyVar can appear in multiple positions).
    /// Quantified TyVars stay live in the union-find graph; the scheme just
    /// captures their identities so `instantiate` can swap them per use.
    ///
    /// A TyVar with a concrete `Link` is not free — it has been pinned to a
    /// specific target. We skip those even though they survive zonking when
    /// they carry `Units` (measure-bearing TyVars). Treating them as ground
    /// matches v1 "no measure polymorphism" — each `let f (x : float<m>) …`
    /// has the measure baked in.
    /// True if `t` contains a TyVar whose root carries a deferred
    /// `PendingDotAccess` constraint. Such a binding cannot be safely
    /// generalised in v1 — quantifying a TyVar with pending dot accesses
    /// would freeze the constraint into the scheme, and a use site that
    /// pins the receiver would only resolve a fresh instantiation, leaving
    /// the original (still-quantified) constraint dangling. Keeping the
    /// binding monomorphic lets the first use site unify directly with the
    /// pre-instantiation TyVar, which drains the constraint normally.
    let rec private hasPendingDotAccess (t: SemType) : bool =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            if not (List.isEmpty root.PendingDotAccess) then
                true
            else
                match root.Link with
                | ValueSome target -> hasPendingDotAccess target
                | ValueNone -> false
        | TyConst _ -> false
        | TyFun(a, r) -> hasPendingDotAccess a || hasPendingDotAccess r
        | TyTuple xs -> List.exists hasPendingDotAccess xs
        | TyRecord(_, args) -> List.exists hasPendingDotAccess args
        | TyUnion(_, args) -> List.exists hasPendingDotAccess args
        | TyClass(_, args) -> List.exists hasPendingDotAccess args

    let private generalise (zonkedTy: SemType) (outerLevel: int) : TypeScheme =
        let quantified = ResizeArray<TypeVar>()
        let seen = HashSet<TypeVar>(HashIdentity.Reference)

        let rec walk (t: SemType) : unit =
            match t with
            | TyVar tv ->
                let root = UnionFind.find tv

                if root.Level > outerLevel && root.Link.IsNone && seen.Add(root) then
                    quantified.Add(root)
            | TyConst _ -> ()
            | TyFun(a, r) ->
                walk a
                walk r
            | TyTuple xs -> List.iter walk xs
            | TyRecord(_, args) -> List.iter walk args
            | TyUnion(_, args) -> List.iter walk args
            | TyClass(_, args) -> List.iter walk args

        walk zonkedTy

        // Collect constraints from each quantified TyVar's union-find
        // root. `instantiate` swaps these onto the fresh substitutions
        // per use site so satisfaction is re-evaluated independently.
        let constraints =
            [
                for tv in quantified do
                    for c in tv.Constraints -> tv, c
            ]

        TypeScheme(List.ofSeq quantified, zonkedTy, constraints)

    /// Single-name `let` generalises unless the binding is `mutable`.
    /// Mutable bindings stay monomorphic: every use of the name unifies
    /// against the binding's own TyVar (no instantiation), so a free TyVar
    /// in a mutable binding's type can be pinned later by any use or
    /// assignment — but the binding is never made polymorphic at the
    /// scheme level, which would re-introduce the classic value-
    /// restriction soundness hole. Compound destructuring heads and
    /// bindings whose head is something other than `Pat.NamedSimple`
    /// don't get schemes either — they bind values, not function
    /// abstractions, and the scheme table is keyed by a single NodeKey.
    let private shouldGeneralise (b: Binding<SyntaxToken>) : bool =
        if b.mutableToken.IsSome then
            false
        else
            match b.headPat with
            | Pat.NamedSimple _ -> true
            | _ -> false

    /// SemType of the carrier of a single literal token (`int`, `float`, …).
    /// Pulled out of `inferConst` so the measured-literal arm can stamp this
    /// onto a TyVar's `Link` while the measure rides on `Units`.
    let private literalCarrier (t: SyntaxToken) : SemType =
        match t.Token with
        | Token.KWTrue
        | Token.KWFalse -> MockBuiltins.tyBool
        | Token.NumIEEE64
        | Token.NumIEEE64Hex
        | Token.NumIEEE64Octal
        | Token.NumIEEE64Binary -> MockBuiltins.tyFloat
        | Token.NumInt64
        | Token.NumInt64Hex
        | Token.NumInt64Octal
        | Token.NumInt64Binary -> MockBuiltins.tyInt64
        | Token.NumByte
        | Token.NumByteHex
        | Token.NumByteOctal
        | Token.NumByteBinary -> MockBuiltins.tyByte
        | _ -> MockBuiltins.tyInt

    /// Walk a `Measure<SyntaxToken>` CST and produce a canonical
    /// `MeasureTerm`. Multi-segment qualified unit names (`Microsoft.FSharp.SI.kg`)
    /// and measure typars (`'u`) are v2 — they produce an empty term plus a
    /// diagnostic so the rest of inference continues without measure noise.
    let rec private translateMeasure (ctx: PassContext) (diagKey: NodeKey) (m: Measure<SyntaxToken>) : MeasureTerm =
        match m with
        | Measure.One _ -> MeasureTerm.empty
        | Measure.Named li when li.Idents.Length = 1 -> MeasureTerm.ofList [ ctx.NameOf li.Idents.[0], Rational.One ]
        | Measure.Power(inner, _, neg, expTok) ->
            let n = System.Numerics.BigInteger.Parse(ctx.NameOf expTok)
            let signed = if neg.IsSome then -n else n

            MeasureTerm.pow
                (translateMeasure ctx diagKey inner)
                (Rational.create (signed, System.Numerics.BigInteger.One))
        | Measure.Product(l, _, r) -> MeasureTerm.mul (translateMeasure ctx diagKey l) (translateMeasure ctx diagKey r)
        | Measure.Quotient(l, _, r) -> MeasureTerm.div (translateMeasure ctx diagKey l) (translateMeasure ctx diagKey r)
        | Measure.Reciprocal(_, inner) -> MeasureTerm.inv (translateMeasure ctx diagKey inner)
        | Measure.Paren(_, inner, _) -> translateMeasure ctx diagKey inner
        | Measure.Juxtaposition(elems, _) ->
            (MeasureTerm.empty, elems)
            ||> Seq.fold (fun acc m -> MeasureTerm.mul acc (translateMeasure ctx diagKey m))
        | Measure.Anonymous _
        | Measure.Typar _
        | Measure.Named _ ->
            ctx.Diagnostics.Add
                {
                    Key = diagKey
                    Message = "Measure typars / wildcards / qualified unit names not yet supported"
                    Severity = Error
                }

            MeasureTerm.empty

    let private inferConst (ctx: PassContext) (c: Constant<SyntaxToken>) : SemType =
        // Dispatch covers the numeric / boolean tokens that lex into a
        // Constant.Literal. Anything we don't recognise still types as int
        // (matches the parser's most common case) — extend as new literal
        // kinds become reachable.
        match c with
        | Constant.Literal t -> literalCarrier t
        | Constant.MeasuredLiteral(value = t; measure = m) ->
            let carrier = literalCarrier t
            let diagKey = NodeKey.ofToken t NodeKind.ExprConst
            let mt = translateMeasure ctx diagKey m
            let tv = freshTyVar ctx
            tv.Link <- ValueSome carrier
            tv.Units <- ValueSome mt
            TyVar tv

    /// Built-in numeric type names that can carry a measure annotation
    /// (`float<m>`, `int<kg>`, etc.). User-defined `[<Measure>]`-aware types
    /// land when records / DUs do.
    let private isNumericCarrier (name: string) : bool =
        match name with
        | "int"
        | "int64"
        | "byte"
        | "float"
        | "float32"
        | "decimal"
        | "single"
        | "double" -> true
        | _ -> false

    /// Translate a syntactic `Type<SyntaxToken>` into a `SemType`.
    /// Reads `ctx.TyparScope` for `'a` typar resolution; callers open a
    /// fresh scope per signature (binding or type defn) before walking.
    /// Named-type lookups consult `ctx.RecordTypes` / `ctx.UnionTypes` to
    /// produce arg-carrying `TyRecord` / `TyUnion`; bare references back-
    /// fill the arg list with fresh TyVars so unification can pin them.
    let rec private translateType (ctx: PassContext) (t: Type<SyntaxToken>) : SemType =
        match t with
        | Type.ParenType(typ = inner) -> translateType ctx inner
        | Type.VarType(Typar.Named(ident = id))
        | Type.VarType(Typar.Static(ident = id)) ->
            let name = ctx.NameOf id

            match ctx.TyparScope.TryGetValue name with
            | true, tv -> TyVar tv
            | false, _ ->
                if ctx.TyparScopeStrict then
                    // Strict mode (type-defn fill-in): implicit free
                    // typars aren't legal F#. Diagnose, but still mint
                    // and memoise so subsequent occurrences resolve to
                    // the same TyVar and don't cascade.
                    ctx.Diagnostics.Add
                        {
                            Key = NodeKey.ofToken id NodeKind.TypeVarRef
                            Message =
                                sprintf
                                    "Free type parameter %s is not declared in the enclosing type's type-parameter list"
                                    name
                            Severity = Error
                        }

                    let tv = TypeVar()
                    tv.Level <- ctx.CurrentLevel
                    ctx.TyparScope.[name] <- tv
                    TyVar tv
                else
                    // Implicit typar introduction. Mint at the binding's
                    // current level so generalisation at binding-group exit
                    // picks it up; memoise so subsequent occurrences in the
                    // same signature share identity.
                    let tv = TypeVar()
                    tv.Level <- ctx.CurrentLevel
                    ctx.TyparScope.[name] <- tv
                    TyVar tv
        | Type.VarType(Typar.Anon _) ->
            // `_` typar — always fresh, never stored. Distinct per
            // occurrence, same as `Pat.Wildcard`.
            TyVar(freshTyVar ctx)
        | Type.NamedType li when li.Idents.Length = 1 ->
            let name = ctx.NameOf li.Idents.[0]

            match name with
            | "int" -> MockBuiltins.tyInt
            | "bool" -> MockBuiltins.tyBool
            | "unit" -> MockBuiltins.tyUnit
            | "float" -> MockBuiltins.tyFloat
            | "string" -> MockBuiltins.tyString
            | "int64" -> MockBuiltins.tyInt64
            | "byte" -> MockBuiltins.tyByte
            | _ ->
                match ctx.AbbreviationTypes.TryGetValue name with
                | true, info ->
                    // Eager expansion: force the body, then substitute
                    // fresh TyVars for every declared typar (bare reference
                    // to a generic abbreviation works the same way as a
                    // bare reference to a generic record / union).
                    forceFill ctx info
                    let args = [ for _ in info.TypeParams -> TyVar(freshTyVar ctx) ]
                    let diagKey = NodeKey.ofToken li.Idents.[0] NodeKind.TypeNamed
                    expandAbbreviation ctx diagKey info args
                | false, _ ->
                    match ctx.RecordTypes.TryGetValue name with
                    | true, info ->
                        // Bare reference to a (possibly generic) record. For
                        // a generic type, back-fill with fresh TyVars at the
                        // current level — the args are unpinned at the
                        // declaration site and get fixed by the surrounding
                        // unification (e.g. a value annotation `r : Box`
                        // unifies the args with whatever `r`'s usage pins).
                        let args = [ for _ in info.TypeParams -> TyVar(freshTyVar ctx) ]
                        TyRecord(name, args)
                    | false, _ ->
                        match ctx.UnionTypes.TryGetValue name with
                        | true, info ->
                            let args = [ for _ in info.TypeParams -> TyVar(freshTyVar ctx) ]
                            TyUnion(name, args)
                        | false, _ ->
                            match ctx.ClassTypes.TryGetValue name with
                            | true, info ->
                                let args = [ for _ in info.TypeParams -> TyVar(freshTyVar ctx) ]
                                TyClass(name, args)
                            | false, _ -> TyConst name
        | Type.GenericType(longIdent = li; typeArgs = args) when
            li.Idents.Length = 1
            && args.Length = 1
            && isNumericCarrier (ctx.NameOf li.Idents.[0])
            ->
            // `float<m>` / `int<kg>` — recognise the measure-shaped generic
            // and stamp the measure onto a fresh TyVar whose Link carries
            // the carrier. Anything else (real generics) falls through
            // to the named-generic arm below.
            //
            // The parser only tags an arg as `TypeArg.Measure` when the
            // measure grammar is unambiguous; for bare `float<m>` it lands
            // as `TypeArg.Type (Type.NamedType "m")` because the type
            // grammar can't tell unit names apart from type-arg type names.
            // Both shapes resolve here.
            let carrierTok = li.Idents.[0]
            let diagKey = NodeKey.ofToken carrierTok NodeKind.TypeGeneric

            let measureFromTypeArg =
                match args.[0] with
                | TypeArg.Measure m -> ValueSome m
                | TypeArg.Type(Type.NamedType nameLi) ->
                    // Reinterpret a single-segment named type as a measure
                    // atom; multi-segment qualifiers stay a real type.
                    if nameLi.Idents.Length = 1 then
                        ValueSome(Measure.Named nameLi)
                    else
                        ValueNone
                | _ -> ValueNone

            match measureFromTypeArg with
            | ValueSome m ->
                let mt = translateMeasure ctx diagKey m
                let tv = freshTyVar ctx
                tv.Link <- ValueSome(translateType ctx (Type.NamedType li))
                tv.Units <- ValueSome mt
                TyVar tv
            | ValueNone -> TyVar(freshTyVar ctx)
        | Type.GenericType(longIdent = li; typeArgs = args) when li.Idents.Length = 1 ->
            let nameTok = li.Idents.[0]
            let name = ctx.NameOf nameTok
            let diagKey = NodeKey.ofToken nameTok NodeKind.TypeGeneric

            let translatedArgs =
                [
                    for a in args ->
                        match a with
                        | TypeArg.Type t -> translateType ctx t
                        // A measure-shaped arg landing on a non-numeric
                        // carrier shouldn't happen in well-formed code,
                        // but stay total — emit a free TyVar.
                        | TypeArg.Measure _ -> TyVar(freshTyVar ctx)
                ]

            let argCount = List.length translatedArgs

            let diagnoseArity (expected: int) : unit =
                ctx.Diagnostics.Add
                    {
                        Key = diagKey
                        Message = sprintf "Type '%s' expects %d type argument(s) but got %d" name expected argCount
                        Severity = Error
                    }

            match ctx.AbbreviationTypes.TryGetValue name with
            | true, info ->
                forceFill ctx info
                let expected = List.length info.TypeParams

                if expected <> argCount then
                    diagnoseArity expected

                expandAbbreviation ctx diagKey info translatedArgs
            | false, _ ->
                match ctx.RecordTypes.TryGetValue name with
                | true, info ->
                    let expected = List.length info.TypeParams

                    if expected <> argCount then
                        diagnoseArity expected

                    TyRecord(name, translatedArgs)
                | false, _ ->
                    match ctx.UnionTypes.TryGetValue name with
                    | true, info ->
                        let expected = List.length info.TypeParams

                        if expected <> argCount then
                            diagnoseArity expected

                        TyUnion(name, translatedArgs)
                    | false, _ ->
                        match ctx.ClassTypes.TryGetValue name with
                        | true, info ->
                            let expected = List.length info.TypeParams

                            if expected <> argCount then
                                diagnoseArity expected

                            TyClass(name, translatedArgs)
                        | false, _ ->
                            // Unknown name with type args — surface as opaque
                            // TyConst (matches today's behaviour for unrecognised
                            // bare names; the args effectively get ignored).
                            TyConst name
        | Type.FunctionType(fromType = from; toType = into) -> TyFun(translateType ctx from, translateType ctx into)
        | Type.TupleType(types = types) -> TyTuple [ for t in types -> translateType ctx t ]
        | Type.WhenConstrainedType(typ = inner; constraints = cs) ->
            let inner = translateType ctx inner
            translateConstraints ctx cs
            inner
        | _ ->
            // Multi-segment named/generic types and other shapes (array
            // types, anonymous records, etc.) aren't modelled yet. Hand
            // back a free TyVar so unification can pin it via context.
            TyVar(freshTyVar ctx)

    /// Translate a `Constraint<'T>` CST node into a `SemanticConstraint`
    /// and attach it to the constrained typar's TyVar through the
    /// current `ctx.TyparScope`. Unsupported kinds (Coercion, MemberTrait,
    /// etc.) are skipped — they belong to their own resolution phases.
    /// An unknown typar name diagnoses the same way `Type.VarType` does.
    and private translateConstraint (ctx: PassContext) (c: Constraint<SyntaxToken>) : unit =
        let typarTokenOf (t: Typar<SyntaxToken>) : SyntaxToken voption =
            match t with
            | Typar.Named(ident = id)
            | Typar.Static(ident = id) -> ValueSome id
            | Typar.Anon _ -> ValueNone

        let attach (typar: Typar<SyntaxToken>) (kind: SemanticConstraintKind) (declTok: SyntaxToken) : unit =
            match typarTokenOf typar with
            | ValueNone -> ()
            | ValueSome id ->
                let name = ctx.NameOf id

                match ctx.TyparScope.TryGetValue name with
                | true, tv ->
                    let root = UnionFind.find tv

                    let sc =
                        {
                            Kind = kind
                            DeclKey = NodeKey.ofToken declTok NodeKind.TypeVarRef
                        }

                    if not (root.Constraints |> List.exists (fun e -> e.Kind = sc.Kind)) then
                        root.Constraints <- sc :: root.Constraints
                | false, _ ->
                    ctx.Diagnostics.Add
                        {
                            Key = NodeKey.ofToken id NodeKind.TypeVarRef
                            Message =
                                sprintf
                                    "Type parameter '%s' in constraint clause is not declared in the enclosing scope"
                                    name
                            Severity = Error
                        }

        match c with
        | Constraint.Equality(typar = tp; equalityToken = tok) -> attach tp SemanticConstraintKind.Equality tok
        | Constraint.Comparison(typar = tp; comparisonToken = tok) -> attach tp SemanticConstraintKind.Comparison tok
        | Constraint.Struct(typar = tp; structToken = tok) -> attach tp SemanticConstraintKind.Struct tok
        | Constraint.ReferenceType(typar = tp; structToken = tok) -> attach tp SemanticConstraintKind.ReferenceType tok
        | Constraint.Nullness(typar = tp; nullToken = tok) -> attach tp SemanticConstraintKind.Nullness tok
        | Constraint.NotNull(typar = tp; nullToken = tok) -> attach tp SemanticConstraintKind.NotNull tok
        | Constraint.Coercion _
        | Constraint.MemberTrait _
        | Constraint.DefaultConstructor _
        | Constraint.Enum _
        | Constraint.Unmanaged _
        | Constraint.Delegate _
        | Constraint.Default _ ->
            // v1 skips these — each has its own resolution phase
            // (SRTPs / IWSAMs / attribute pass). Silent skip rather than
            // a diagnostic, matching how `Measure.Anonymous` and friends
            // surface today.
            ()

    /// Translate every `Constraint` in a `TyparConstraints` block under
    /// the current `ctx.TyparScope`. The scope must already contain the
    /// constrained typars — callers (binding-level, type-defn fill-in,
    /// inline `WhenConstrainedType`) seed it first.
    and private translateConstraints (ctx: PassContext) (tcs: TyparConstraints<SyntaxToken>) : unit =
        let (TyparConstraints(constraints = cs)) = tcs

        for c in cs do
            translateConstraint ctx c

    /// Force the body of an abbreviation, translating its RHS under a
    /// typar scope seeded from its `TypeParams`. Idempotent — already-
    /// `Filled` entries short-circuit. Re-entry through a recursive
    /// abbreviation reference detects the cycle (`InProgress`), emits a
    /// diagnostic, and freezes `Status` to `Filled` without setting
    /// `Body`. The outer call notices `Status` was flipped mid-walk and
    /// skips assigning `Body`, leaving `ValueNone` so the expansion arm
    /// substitutes a fresh TyVar per use site instead of sharing a
    /// stale one.
    and private forceFill (ctx: PassContext) (info: AbbreviationInfo) : unit =
        match info.Status with
        | AbbreviationStatus.Filled -> ()
        | AbbreviationStatus.InProgress ->
            ctx.Diagnostics.Add
                {
                    Key = info.DeclKey
                    Message = sprintf "Type abbreviation '%s' is cyclic" info.Name
                    Severity = Error
                }

            info.Status <- AbbreviationStatus.Filled
        | AbbreviationStatus.NotFilled ->
            info.Status <- AbbreviationStatus.InProgress
            let savedScope = ctx.TyparScope
            let savedStrict = ctx.TyparScopeStrict
            let scope = Dictionary<string, TypeVar>(System.StringComparer.Ordinal)

            for (n, tv) in info.TypeParams do
                if not (scope.ContainsKey n) then
                    scope.[n] <- tv

            ctx.TyparScope <- scope
            ctx.TyparScopeStrict <- true

            try
                match info.TyparConstraints with
                | ValueSome cs -> translateConstraints ctx cs
                | ValueNone -> ()

                let body = translateType ctx info.RhsCst

                if info.Status = AbbreviationStatus.InProgress then
                    info.Body <- ValueSome body
            finally
                ctx.TyparScope <- savedScope
                ctx.TyparScopeStrict <- savedStrict
                info.Status <- AbbreviationStatus.Filled

    /// Expand an abbreviation reference: substitute the user-supplied (or
    /// fresh) args for the declared `TypeParams` in the stored body.
    /// Returns a fresh TyVar if `Body = ValueNone` (cycle detected, or
    /// fill-in not yet run) — unification stays best-effort rather than
    /// cascading.
    ///
    /// Constraints on the prototype typars are evaluated against the
    /// supplied args here: unlike records / unions, an abbreviation has
    /// no fresh-instance step that would let `drainConstraints` fire
    /// on its own. A Violated outcome diagnoses immediately; a Defer
    /// outcome propagates the constraint to any free TyVar inside the
    /// supplied arg so a later unification re-fires the check.
    and private expandAbbreviation
        (ctx: PassContext)
        (diagKey: NodeKey)
        (info: AbbreviationInfo)
        (args: SemType list)
        : SemType =
        let n = min (List.length info.TypeParams) (List.length args)

        for i = 0 to n - 1 do
            let (_, protoTv) = info.TypeParams.[i]
            let arg = args.[i]
            let protoRoot = UnionFind.find protoTv

            for c in protoRoot.Constraints do
                match checkConstraint ctx c arg with
                | Satisfied -> ()
                | Violated ->
                    ctx.Diagnostics.Add
                        {
                            Key = diagKey
                            Message =
                                sprintf
                                    "The type '%A' does not support the '%s' constraint"
                                    (zonk arg)
                                    (constraintKindName c.Kind)
                            Severity = Error
                        }
                | Defer -> propagateToFreeArgs ctx c arg

        match info.Body with
        | ValueSome body ->
            let subst = mkNamedTypeSubst info.TypeParams args
            substituteWith subst body
        | ValueNone -> TyVar(freshTyVar ctx)

    /// Measure carried on `t`'s union-find root, if any. Reads `Units`
    /// straight off the root — does NOT use `resolveStep`, since that
    /// would follow a measured TyVar through its `Link` to the bare
    /// carrier and drop the measure. Returns ValueNone for plain
    /// `TyConst` (dimensionless), function types, tuples, and free
    /// variables.
    let private unitsOf (t: SemType) : MeasureTerm voption =
        match t with
        | TyVar tv -> (UnionFind.find tv).Units
        | _ -> ValueNone

    /// Underlying numeric carrier of a (possibly measure-wrapped) type.
    /// For a `TyVar` whose Link points at `TyConst "float"`, returns
    /// `TyConst "float"`. For a plain TyConst, returns itself. For a free
    /// variable (no Link), returns the variable so a later unification can
    /// pin it.
    let private carrierOf (t: SemType) : SemType =
        match resolveStep t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome link -> link
            | ValueNone -> TyVar root
        | other -> other

    /// Allocate a fresh TyVar at the current level pre-stamped with a
    /// carrier link and (optionally) a measure. The dispatcher uses this
    /// for the result type of a measured arithmetic operation.
    let private freshTyVarWith (ctx: PassContext) (carrier: SemType) (units: MeasureTerm voption) : TypeVar =
        let tv = freshTyVar ctx
        tv.Link <- ValueSome carrier
        tv.Units <- units
        tv

    /// Compiled names for comparison operators that return `bool` regardless
    /// of operand measure (provided the measures match).
    let private isComparisonOp (name: string) : bool =
        match name with
        | "op_Equality"
        | "op_Inequality"
        | "op_LessThan"
        | "op_GreaterThan"
        | "op_LessThanOrEqual"
        | "op_GreaterThanOrEqual" -> true
        | _ -> false

    /// Measure-aware arithmetic dispatcher. Fires before the provider
    /// lookup in `inferInfix` so measured `+`, `-`, `*`, `/`, and comparison
    /// operators get measure-correct result types and surface a dedicated
    /// "Measure mismatch" diagnostic rather than a generic carrier-type
    /// mismatch. Returns `None` for the all-dimensionless case (or for
    /// operators we don't dispatch); the caller falls through to the
    /// existing provider path.
    let private tryMeasuredArith
        (ctx: PassContext)
        (key: NodeKey)
        (name: string)
        (leftTy: SemType)
        (rightTy: SemType)
        : SemType option =
        let leftUnits = unitsOf leftTy
        let rightUnits = unitsOf rightTy

        match leftUnits, rightUnits with
        | ValueNone, ValueNone -> None
        | _ ->
            let carrier = carrierOf leftTy
            // Carriers must agree even between measured operands (no
            // `float<m> + int<m>`). Surface that as a normal type mismatch.
            unify ctx key carrier (carrierOf rightTy)

            match name, leftUnits, rightUnits with
            | ("op_Addition" | "op_Subtraction"), ValueSome m1, ValueSome m2 when m1.Equals m2 ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome m1)))
            | ("op_Addition" | "op_Subtraction"), ValueSome m1, ValueSome m2 ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: <%O> vs <%O>" m1 m2
                        Severity = Error
                    }

                Some(TyVar(freshTyVarWith ctx carrier (ValueSome m1)))
            | ("op_Addition" | "op_Subtraction"), ValueSome m, ValueNone
            | ("op_Addition" | "op_Subtraction"), ValueNone, ValueSome m ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: dimensionless vs <%O>" m
                        Severity = Error
                    }

                Some(TyVar(freshTyVarWith ctx carrier (ValueSome m)))
            | "op_Multiply", ValueSome m1, ValueSome m2 ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome(MeasureTerm.mul m1 m2))))
            | "op_Multiply", ValueSome m, ValueNone
            | "op_Multiply", ValueNone, ValueSome m -> Some(TyVar(freshTyVarWith ctx carrier (ValueSome m)))
            | "op_Division", ValueSome m1, ValueSome m2 ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome(MeasureTerm.div m1 m2))))
            | "op_Division", ValueSome m, ValueNone -> Some(TyVar(freshTyVarWith ctx carrier (ValueSome m)))
            | "op_Division", ValueNone, ValueSome m ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome(MeasureTerm.inv m))))
            | name, ValueSome m1, ValueSome m2 when isComparisonOp name && m1.Equals m2 -> Some MockBuiltins.tyBool
            | name, ValueSome m1, ValueSome m2 when isComparisonOp name ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: <%O> vs <%O>" m1 m2
                        Severity = Error
                    }

                Some MockBuiltins.tyBool
            | name, ValueSome m, ValueNone
            | name, ValueNone, ValueSome m when isComparisonOp name ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: dimensionless vs <%O>" m
                        Severity = Error
                    }

                Some MockBuiltins.tyBool
            | _ -> None

    /// Field-initializer / field-pattern long-ident inspection. v1 only
    /// supports single-segment (`X`) and two-segment qualified (`R.X`)
    /// forms. Multi-segment qualifiers (`A.B.X`) fall through as ValueNone
    /// for the qualifier and the last segment for the field name.
    let private fieldNameAndQualifier (ctx: PassContext) (li: LongIdent<SyntaxToken>) : string voption * string =
        let idents = li.Idents
        let last = ctx.NameOf idents.[idents.Length - 1]

        if idents.Length = 1 then
            ValueNone, last
        elif idents.Length = 2 then
            ValueSome(ctx.NameOf idents.[0]), last
        else
            ValueNone, last

    /// Mint a fresh instantiation of a named type. Returns the
    /// arg-carrying SemType (`TyRecord("Box", [fresh_a])`) together
    /// with the substitution that maps each prototype `TypeVar` in
    /// `typeParams` onto its fresh stand-in — callers walk declared
    /// field / case-arg types through this subst so every reference
    /// to `'a` lines up with the value in `args`.
    let private freshNamedInstance
        (ctx: PassContext)
        (typeParams: (string * TypeVar) list)
        : SemType list * Dictionary<TypeVar, SemType> =
        let subst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)

        let args =
            [
                for (_, tp) in typeParams ->
                    let fresh = TypeVar()
                    fresh.Level <- ctx.CurrentLevel
                    let protoRoot = UnionFind.find tp
                    // Copy prototype constraints onto the fresh
                    // instance so every use site re-evaluates
                    // satisfaction independently (a `Set<int>` and a
                    // `Set<int -> int>` each see their own copy of the
                    // `'a : comparison` rule).
                    fresh.Constraints <- protoRoot.Constraints
                    let asTy = TyVar fresh
                    subst.[protoRoot] <- asTy
                    asTy
            ]

        args, subst

    /// Class-name-as-function: produces a function value whose argument
    /// shape matches the primary constructor and whose result is the
    /// constructed `TyClass`. Used by `inferIdent` to type bare
    /// `Point(3, 4)` calls (no `new` keyword) through the existing
    /// function-application machinery.
    ///
    /// Returns `ValueNone` if `name` isn't in `ctx.ClassTypes`.
    let private tryClassCtorAsFunction (ctx: PassContext) (name: string) : SemType voption =
        match ctx.ClassTypes.TryGetValue name with
        | true, info ->
            let args, subst = freshNamedInstance ctx info.TypeParams
            let receiverTy = TyClass(info.Name, args)

            let paramTys =
                info.CtorParams
                |> Array.map (fun p -> substituteWith subst p.Type)
                |> Array.toList

            let arg =
                match paramTys with
                | [] -> MockBuiltins.tyUnit
                | [ t ] -> t
                | many -> TyTuple many

            ValueSome(TyFun(arg, receiverTy))
        | false, _ -> ValueNone

    let private classCtorAsFunction (ctx: PassContext) (name: string) : SemType =
        match tryClassCtorAsFunction ctx name with
        | ValueSome t -> t
        | ValueNone -> TyVar(freshTyVar ctx)

    /// Function-shaped type for a DU ctor reference. Nullary cases type as
    /// the union itself (no argument); single-field cases as
    /// `field -> TyUnion`; multi-field cases bundle the fields into a
    /// tuple — F# DUs take a tuple as their single argument. The
    /// receiver union's typars are instantiated with fresh TyVars at
    /// the current level so two independent uses of `Some` don't share
    /// a `'a`.
    let private ctorType (ctx: PassContext) (info: UnionCaseInfo) : SemType =
        let unionInfo = ctx.UnionTypes.[info.UnionName]
        let args, subst = freshNamedInstance ctx unionInfo.TypeParams
        let unionTy = TyUnion(info.UnionName, args)

        let walkedFields = info.Fields |> Array.map (substituteWith subst)

        match walkedFields.Length with
        | 0 -> unionTy
        | 1 -> TyFun(walkedFields.[0], unionTy)
        | _ -> TyFun(TyTuple(List.ofArray walkedFields), unionTy)

    /// Resolve a bare ctor name to its UnionCaseInfo. ValueNone with
    /// `count = 0` means "no such ctor"; `count >= 2` means ambiguous —
    /// the caller emits the appropriate diagnostic.
    let private resolveCtorName (ctx: PassContext) (name: string) : UnionCaseInfo voption * int =
        match ctx.CtorIndex.TryGetValue name with
        | false, _ -> ValueNone, 0
        | true, [ info ] -> ValueSome info, 1
        | true, infos -> ValueNone, List.length infos

    /// Resolve a qualified ctor reference `Type.Case` against the union
    /// registry. ValueSome on a match; ValueNone when the type isn't a
    /// known union, or doesn't declare the named case.
    let private resolveQualifiedCtor (ctx: PassContext) (typeName: string) (caseName: string) : UnionCaseInfo voption =
        match ctx.UnionTypes.TryGetValue typeName with
        | false, _ -> ValueNone
        | true, info ->
            match info.Cases |> Array.tryFind (fun c -> c.Name = caseName) with
            | Some c -> ValueSome c
            | None -> ValueNone

    /// Try to find the unique record type whose declared field set equals
    /// `names` (order-insensitive, duplicates rejected). Returns ValueNone
    /// when zero matches or multiple match — the caller emits the
    /// appropriate diagnostic.
    let private findUniqueRecordByFieldSet (ctx: PassContext) (names: string list) : RecordTypeInfo voption * int =
        // Returns (info, candidateCount). candidateCount disambiguates the
        // "no match" vs "ambiguous" diagnostic paths.
        match names with
        | [] -> ValueNone, 0
        | first :: _ ->
            match ctx.FieldIndex.TryGetValue first with
            | false, _ -> ValueNone, 0
            | true, candidates ->
                let nameSet = Set.ofList names

                let matches =
                    candidates
                    |> List.filter (fun info ->
                        let declared = info.Fields |> Array.map (fun f -> f.Name) |> Set.ofArray
                        declared = nameSet
                    )

                match matches with
                | [ info ] -> ValueSome info, 1
                | [] -> ValueNone, 0
                | many -> ValueNone, List.length many

    /// Strip a `Pat.EnclosedBlock` and a single-element tuple wrapper
    /// from a pattern. `Circle(r)` parses as `Circle (EnclosedBlock r)`;
    /// `Rectangle(w, h)` as `Circle (EnclosedBlock (Tuple [w; h]))`;
    /// `Rectangle w h` (curried syntax) as multiple separate args. v1
    /// supports tuple-argument form and a bare single arg — both are
    /// what the F# DU ctor application convention emits.
    let private unwrapCtorArgPattern (p: Pat<SyntaxToken>) : Pat<SyntaxToken> list =
        match p with
        | Pat.EnclosedBlock(pat = Pat.Tuple(patterns = pats)) -> List.ofSeq pats
        | Pat.EnclosedBlock(pat = inner) -> [ inner ]
        | Pat.Tuple(patterns = pats) -> List.ofSeq pats
        | _ -> [ p ]

    let rec private inferPat (ctx: PassContext) (p: Pat<SyntaxToken>) : SemType =
        // Each pattern node gets its own TypeVar keyed on its NodeKey. For
        // compound patterns (Tuple, EnclosedBlock, As) the outer TypeVar is
        // linked to the underlying shape so a single lookup against any
        // pattern node returns the right type.
        let key = CstKeys.ofPat p

        match p with
        | Pat.NamedSimple t when
            let n = ctx.NameOf t
            n.Length > 0 && System.Char.IsUpper n.[0] && ctx.CtorIndex.ContainsKey n
            ->
            // Uppercase-leading bare ident in pattern position that
            // matches a known ctor — reinterpret as a nullary ctor
            // pattern. Multi-candidate ctors with the same name require a
            // qualifier; diagnose ambiguity and best-effort to the first.
            let n = ctx.NameOf t
            let info, count = resolveCtorName ctx n

            match info with
            | ValueSome i when i.Fields.Length = 0 ->
                let unionInfo = ctx.UnionTypes.[i.UnionName]
                let args, _ = freshNamedInstance ctx unionInfo.TypeParams
                let ty = TyUnion(i.UnionName, args)
                let nodeTv = freshTv ctx key
                nodeTv.Link <- ValueSome ty
                ty
            | ValueSome i ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message =
                            sprintf
                                "Constructor '%s' takes %d argument(s) but is used nullary in pattern position"
                                n
                                i.Fields.Length
                        Severity = Error
                    }

                let unionInfo = ctx.UnionTypes.[i.UnionName]
                let args, _ = freshNamedInstance ctx unionInfo.TypeParams
                let ty = TyUnion(i.UnionName, args)
                let nodeTv = freshTv ctx key
                nodeTv.Link <- ValueSome ty
                ty
            | ValueNone when count >= 2 ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message =
                            sprintf "Ambiguous constructor '%s'; declared in %d union types — add a qualifier" n count
                        Severity = Error
                    }

                TyVar(freshTv ctx key)
            | ValueNone -> TyVar(freshTv ctx key)
        | Pat.NamedSimple _ ->
            // Use tvOf so a let-rec sibling whose TyVar was already lazy-minted
            // by a forward reference (or pre-allocated by inferBindingGroup)
            // is reused, not overwritten.
            TyVar(tvOf ctx key)
        | Pat.Named(longIdent = li; argumentPats = args) when
            li.Idents.Length >= 1
            && (let last = ctx.NameOf li.Idents.[li.Idents.Length - 1]
                last.Length > 0 && System.Char.IsUpper last.[0])
            && (li.Idents.Length = 1 && ctx.CtorIndex.ContainsKey(ctx.NameOf li.Idents.[0])
                || li.Idents.Length = 2
                   && ctx.UnionTypes.ContainsKey(ctx.NameOf li.Idents.[0])
                   && (let info = ctx.UnionTypes.[ctx.NameOf li.Idents.[0]]
                       let caseName = ctx.NameOf li.Idents.[1]
                       info.Cases |> Array.exists (fun c -> c.Name = caseName)))
            ->
            // Ctor pattern: `Circle r`, `Rectangle(w, h)`, `Result1.Ok x`.
            let info =
                if li.Idents.Length = 1 then
                    let name = ctx.NameOf li.Idents.[0]

                    match resolveCtorName ctx name with
                    | ValueSome i, _ -> ValueSome i
                    | ValueNone, count when count >= 2 ->
                        ctx.Diagnostics.Add
                            {
                                Key = key
                                Message =
                                    sprintf
                                        "Ambiguous constructor '%s'; declared in %d union types — add a qualifier"
                                        name
                                        count
                                Severity = Error
                            }

                        ValueNone
                    | _ -> ValueNone
                else
                    let typeName = ctx.NameOf li.Idents.[0]
                    let caseName = ctx.NameOf li.Idents.[1]
                    resolveQualifiedCtor ctx typeName caseName

            match info with
            | ValueNone ->
                for sub in args do
                    inferPat ctx sub |> ignore

                TyVar(freshTv ctx key)
            | ValueSome i ->
                // F# DU application takes a tuple as its single argument;
                // the parser wraps multi-arg ctor patterns in
                // `EnclosedBlock(Tuple [...])`. `unwrapCtorArgPattern`
                // flattens that to the field list.
                let subPats =
                    if args.Length = 1 then
                        unwrapCtorArgPattern args.[0]
                    else
                        List.ofSeq args

                if subPats.Length <> i.Fields.Length then
                    ctx.Diagnostics.Add
                        {
                            Key = key
                            Message =
                                sprintf
                                    "Constructor '%s' expects %d argument(s) but got %d"
                                    i.Name
                                    i.Fields.Length
                                    subPats.Length
                            Severity = Error
                        }

                let unionInfo = ctx.UnionTypes.[i.UnionName]
                let args, subst = freshNamedInstance ctx unionInfo.TypeParams
                let m = min subPats.Length i.Fields.Length

                for j = 0 to m - 1 do
                    let sub = subPats.[j]
                    let subTy = inferPat ctx sub
                    unify ctx (CstKeys.ofPat sub) subTy (substituteWith subst i.Fields.[j])

                // Walk any extra sub-patterns so binders still register.
                for j = m to subPats.Length - 1 do
                    inferPat ctx subPats.[j] |> ignore

                let ty = TyUnion(i.UnionName, args)
                let nodeTv = freshTv ctx key
                nodeTv.Link <- ValueSome ty
                ty
        | Pat.Wildcard _ -> TyVar(freshTv ctx key)
        | Pat.EnclosedBlock(pat = inner) ->
            let innerTy = inferPat ctx inner
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome innerTy
            innerTy
        | Pat.Tuple(patterns = pats) ->
            let elemTys = [ for p in pats -> inferPat ctx p ]
            let tupleTy = TyTuple elemTys
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome tupleTy
            tupleTy
        | Pat.Const c ->
            let constTy = inferConst ctx c
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome constTy
            constTy
        | Pat.As(pat = inner) ->
            let innerTy = inferPat ctx inner
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome innerTy
            innerTy
        | Pat.Typed(pat = inner; typ = t) ->
            let innerTy = inferPat ctx inner
            let annTy = translateType ctx t
            unify ctx key innerTy annTy
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome annTy
            annTy
        | Pat.EmptyBlock _ ->
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome MockBuiltins.tyUnit
            MockBuiltins.tyUnit
        | Pat.Or(left = leftPat; right = rightPat) ->
            // F# requires both sides to bind the same set of names with
            // matching types. Validation will check the name set; here we
            // unify the patterns' overall types so the scrutinee constraint
            // is consistent.
            let leftTy = inferPat ctx leftPat
            let rightTy = inferPat ctx rightPat
            unify ctx key leftTy rightTy
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome leftTy
            leftTy
        | Pat.Record(fieldPats = fieldPats) ->
            let pairs =
                [
                    for FieldPat(longIdent = li; pat = sub) in fieldPats ->
                        let q, n = fieldNameAndQualifier ctx li
                        q, n, sub
                ]

            let qualifier =
                pairs
                |> List.tryPick (fun (q, _, _) ->
                    match q with
                    | ValueSome q -> Some q
                    | _ -> None
                )

            let names = pairs |> List.map (fun (_, n, _) -> n)

            let candidate =
                match qualifier with
                | Some typeName ->
                    match ctx.RecordTypes.TryGetValue typeName with
                    | true, info -> ValueSome info
                    | false, _ ->
                        ctx.Diagnostics.Add
                            {
                                Key = key
                                Message = sprintf "Unknown record type qualifier: %s" typeName
                                Severity = Error
                            }

                        ValueNone
                | None ->
                    let cand, count = findUniqueRecordByFieldSet ctx names

                    match cand with
                    | ValueSome _ -> cand
                    | ValueNone ->
                        if count = 0 then
                            ctx.Diagnostics.Add
                                {
                                    Key = key
                                    Message =
                                        sprintf "No record type matches the field set: %s" (String.concat ", " names)
                                    Severity = Error
                                }
                        else
                            ctx.Diagnostics.Add
                                {
                                    Key = key
                                    Message =
                                        sprintf
                                            "Field set is ambiguous (%d candidate record types); add a qualifier or annotation"
                                            count
                                    Severity = Error
                                }

                        ValueNone

            match candidate with
            | ValueNone ->
                // Still walk sub-patterns so binders are inferred — they
                // attach as free TyVars without unification noise.
                for _, _, sub in pairs do
                    inferPat ctx sub |> ignore

                let nodeTv = freshTv ctx key
                TyVar nodeTv
            | ValueSome info ->
                let args, subst = freshNamedInstance ctx info.TypeParams

                for _, fieldName, sub in pairs do
                    let subTy = inferPat ctx sub

                    match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                    | Some field -> unify ctx (CstKeys.ofPat sub) subTy (substituteWith subst field.Type)
                    | None ->
                        ctx.Diagnostics.Add
                            {
                                Key = CstKeys.ofPat sub
                                Message = sprintf "Type '%s' has no field '%s'" info.Name fieldName
                                Severity = Error
                            }

                let recTy = TyRecord(info.Name, args)
                let nodeTv = freshTv ctx key
                nodeTv.Link <- ValueSome recTy
                recTy
        | _ ->
            // TODO: Named (DU ctor) / Cons patterns — they need
            // provider lookups or recursive shape unification.
            TyVar(freshTv ctx key)

    let rec private infer (ctx: PassContext) (e: Expr<SyntaxToken>) : SemType =
        let key = CstKeys.ofExpr e
        let nodeTv = freshTv ctx key

        let inferredTy =
            match e with
            | Expr.Const c -> inferConst ctx c
            | Expr.Ident _ -> inferIdent ctx e key
            | Expr.LongIdentOrOp _ -> inferIdent ctx e key
            | Expr.App(fn, args) -> inferApp ctx key fn args
            | Expr.HighPrecedenceApp(funcExpr = fn; argExpr = arg) -> inferHighPrecApp ctx key fn arg
            | Expr.InfixApp(left, _, right) -> inferInfix ctx key left right
            | Expr.PrefixApp(_, operand) -> inferPrefix ctx key operand
            | Expr.Fun(argumentPats = argPats; expr = body) -> inferFun ctx argPats body
            | Expr.LetOrUse(bindings = bindings; body = body) -> inferLet ctx key bindings body
            | Expr.EnclosedBlock(expr = inner) -> infer ctx inner
            | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
                inferIfThenElse ctx key cond thenE elifs elseB
            | Expr.Tuple(exprs = items) -> inferTuple ctx items
            | Expr.Sequential(exprs = items) -> inferSequential ctx key items
            | Expr.TypeAnnotation(expr = inner; typ = t) -> inferTypeAnnotation ctx key inner t
            | Expr.EmptyBlock _ -> MockBuiltins.tyUnit
            | Expr.While(condition = cond; body = body) -> inferWhile ctx key cond body
            | Expr.ForTo(ident = ident; startExpr = startE; endExpr = endE; body = body) ->
                inferForTo ctx key ident startE endE body
            | Expr.ForIn(pat = pat; enumerableExpr = src; body = body) -> inferForIn ctx key pat src body
            | Expr.String(parts = parts) -> inferString ctx key parts
            | Expr.Match(matchExpr = scrutinee; rules = Rules(rules = rules)) -> inferMatch ctx key scrutinee rules
            | Expr.Function(rules = Rules(rules = rules)) -> inferFunction ctx key rules
            | Expr.TryWith(expr = body; rules = Rules(rules = rules)) -> inferTryWith ctx key body rules
            | Expr.TryFinally(tryExpr = body; finallyExpr = finallyE) -> inferTryFinally ctx key body finallyE
            | Expr.Assignment(leftExpr = left; rightExpr = right) -> inferAssignment ctx key left right
            | Expr.Range(fromExpr = a; toExpr = b) -> inferRange ctx key a ValueNone b
            | Expr.SteppedRange(fromExpr = a; stepExpr = s; toExpr = b) -> inferRange ctx key a (ValueSome s) b
            | Expr.Null _ ->
                // `null` lacks a constraint in the tiny subset (no
                // reference-type bound yet). Hand back a free TypeVar so
                // surrounding context can pin it.
                TyVar(freshTyVar ctx)
            | Expr.Record(fieldInitializers = inits) -> inferRecord ctx key inits
            | Expr.RecordClone(expr = src; fieldInitializers = inits) -> inferRecordClone ctx key src inits
            | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                inferFieldAccess ctx key r li.Idents.[0]
            | Expr.New(typ = t; expr = argExpr) -> inferNew ctx key t argExpr
            | _ ->
                // TODO: other expression kinds.
                TyVar(freshTyVar ctx)

        nodeTv.Link <- ValueSome inferredTy
        inferredTy

    and private inferIdent (ctx: PassContext) (e: Expr<SyntaxToken>) (key: NodeKey) : SemType =
        // First: a multi-segment LongIdent whose head segment resolved as
        // a local binding is a record-field access chain (`r.X`, `r.X.Y`),
        // not a qualified name. The parser doesn't emit `Expr.DotLookup`
        // for these — they ride inside a single `Expr.LongIdentOrOp`.
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length > 1
            && ctx.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
            ->
            inferLongIdentFieldChain ctx key li
        // Qualified static member: `Math.Pi`, `Box.Empty`. The class
        // name lives in `ctx.ClassTypes`; the second segment must be
        // a member whose `IsStatic = true`. The class's typars are
        // instantiated fresh per use site so two independent
        // `Box.Empty ()` calls don't share a `'a`.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length = 2
            && not (ctx.Binding.ContainsKey key)
            && (
                match ctx.ClassTypes.TryGetValue(ctx.NameOf li.Idents.[0]) with
                | true, info ->
                    let n = ctx.NameOf li.Idents.[1]
                    info.Members |> Array.exists (fun m -> m.IsStatic && m.Name = n)
                | false, _ -> false
            )
            ->
            let className = ctx.NameOf li.Idents.[0]
            let memberName = ctx.NameOf li.Idents.[1]
            let info = ctx.ClassTypes.[className]

            let m = info.Members |> Array.find (fun m -> m.IsStatic && m.Name = memberName)

            // Instantiate the class's typars fresh per use site so
            // generic statics (`Box.Empty<'a>`) don't share variables
            // across uses.
            let _, subst = freshNamedInstance ctx info.TypeParams
            substituteWith subst m.Type
        // Qualified ctor reference: `Result2.Ok` resolves via the union
        // registry (bypasses the CtorIndex ambiguity check).
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length = 2
            && not (ctx.Binding.ContainsKey key)
            && ctx.UnionTypes.ContainsKey(ctx.NameOf li.Idents.[0])
            ->
            let typeName = ctx.NameOf li.Idents.[0]
            let caseName = ctx.NameOf li.Idents.[1]

            match resolveQualifiedCtor ctx typeName caseName with
            | ValueSome info -> ctorType ctx info
            | ValueNone ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Union '%s' has no case '%s'" typeName caseName
                        Severity = Error
                    }

                TyVar(freshTyVar ctx)
        | _ ->
            match ctx.Binding.TryGetValue key with
            | ValueSome rb ->
                // Local binding — the BindingSite is the headPat NodeKey. If the
                // binding has been generalised already, instantiate the scheme so
                // independent use-sites get independent variables (mirrors the
                // external-symbol path). Otherwise fall back to the monomorphic
                // TyVar minted by inferPat — this includes uses inside a sibling's
                // RHS within the same `let rec` group, which is exactly what
                // forbids polymorphic recursion.
                match ctx.Scheme.TryGetValue rb.BindingSite with
                | ValueSome scheme -> instantiate ctx scheme
                | ValueNone -> TyVar(tvOf ctx rb.BindingSite)
            | ValueNone ->
                // No local binding. Try the provider first — provider hits
                // beat ctor-name resolution when both exist (consistent with
                // F# shadowing: a let-bound `Ok` would have a Binding entry
                // and never reach here; but a provider symbol is genuinely
                // a different namespace). For bare single-segment idents
                // that aren't in the provider, check the ctor registry.
                let name = qualifiedNameOf ctx e

                match ctx.Provider.TryLookup name with
                | ValueSome sym -> sym.Instantiate ctx.CurrentLevel
                | ValueNone ->
                    let singleSegName =
                        match e with
                        | Expr.Ident t -> ValueSome(ctx.NameOf t)
                        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                            ValueSome(ctx.NameOf li.Idents.[0])
                        | _ -> ValueNone

                    match singleSegName with
                    | ValueSome n ->
                        let info, count = resolveCtorName ctx n

                        match info with
                        | ValueSome i -> ctorType ctx i
                        | ValueNone when count >= 2 ->
                            ctx.Diagnostics.Add
                                {
                                    Key = key
                                    Message =
                                        sprintf
                                            "Ambiguous constructor '%s'; declared in %d union types — add a qualifier or annotation"
                                            n
                                            count
                                    Severity = Error
                                }

                            TyVar(freshTyVar ctx)
                        | ValueNone ->
                            // Class-name-as-function: `Point(3, 4)` parses
                            // as `Expr.App (Expr.Ident "Point", ...)`. The
                            // class registry holds the ctor signature; mint
                            // a fresh instance and return the ctor as a
                            // function value so `inferApp` types the call
                            // through the normal function arm.
                            classCtorAsFunction ctx n
                    | ValueNone -> TyVar(freshTyVar ctx)

    /// Source-level rendering of an ident/qualified-name expression. For
    /// single-segment idents this is just the token text; for multi-segment
    /// `LongIdent.LongIdent` it joins segments with `.` so the provider can
    /// look up dotted names like `Math.PI` directly.
    and private qualifiedNameOf (ctx: PassContext) (e: Expr<SyntaxToken>) : string =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> li.Idents |> Seq.map ctx.NameOf |> String.concat "."
        | _ -> ctx.NameOf(CstKeys.firstTokenOfExpr e)

    and private inferApp
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : SemType =
        let mutable currTy = infer ctx fn

        for a in args do
            let argTy = infer ctx a
            let resultTy = TyVar(freshTyVar ctx)
            unify ctx key currTy (TyFun(argTy, resultTy))
            currTy <- resultTy

        currTy

    and private inferHighPrecApp
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (arg: Expr<SyntaxToken>)
        : SemType =
        // `f(x)` — high-precedence one-arg application. Same shape as
        // `Expr.App fn [|arg|]`, just a separate CST case for the parser.
        let fnTy = infer ctx fn
        let argTy = infer ctx arg
        let resultTy = TyVar(freshTyVar ctx)
        unify ctx key fnTy (TyFun(argTy, resultTy))
        resultTy

    and private inferRange
        (ctx: PassContext)
        (key: NodeKey)
        (fromE: Expr<SyntaxToken>)
        (stepE: Expr<SyntaxToken> voption)
        (toE: Expr<SyntaxToken>)
        : SemType =
        // `a..b` / `a..step..b` — endpoints (and step) constrained to int
        // in the tiny subset; result is the `seq<int>` placeholder. Real F#
        // is generic over any type with the `..` operator overload.
        let fromTy = infer ctx fromE
        unify ctx key fromTy MockBuiltins.tyInt

        match stepE with
        | ValueSome s ->
            let stepTy = infer ctx s
            unify ctx key stepTy MockBuiltins.tyInt
        | ValueNone -> ()

        let toTy = infer ctx toE
        unify ctx key toTy MockBuiltins.tyInt
        MockBuiltins.tySeqInt

    and private inferInfix
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        : SemType =
        let leftTy = infer ctx left
        let rightTy = infer ctx right

        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            match tryMeasuredArith ctx key name leftTy rightTy with
            | Some resultTy -> resultTy
            | None ->
                match ctx.Provider.TryLookup name with
                | ValueSome sym ->
                    let resultTy = TyVar(freshTyVar ctx)
                    unify ctx key (sym.Instantiate ctx.CurrentLevel) (TyFun(leftTy, TyFun(rightTy, resultTy)))
                    resultTy
                | ValueNone ->
                    ctx.Diagnostics.Add
                        {
                            Key = key
                            Message = sprintf "Unknown operator symbol: %s" name
                            Severity = Error
                        }

                    TyVar(freshTyVar ctx)
        | ValueNone ->
            // Desugar didn't recognise the operator token; leave the result
            // as a free TypeVar (the unknown operator is a deficiency in
            // Desugar's lookup table, not a user error here).
            TyVar(freshTyVar ctx)

    and private inferPrefix (ctx: PassContext) (key: NodeKey) (operand: Expr<SyntaxToken>) : SemType =
        let operandTy = infer ctx operand

        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            match ctx.Provider.TryLookup name with
            | ValueSome sym ->
                let resultTy = TyVar(freshTyVar ctx)
                unify ctx key (sym.Instantiate ctx.CurrentLevel) (TyFun(operandTy, resultTy))
                resultTy
            | ValueNone ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Unknown prefix operator: %s" name
                        Severity = Error
                    }

                TyVar(freshTyVar ctx)
        | ValueNone -> TyVar(freshTyVar ctx)

    and private inferIfThenElse
        (ctx: PassContext)
        (key: NodeKey)
        (cond: Expr<SyntaxToken>)
        (thenE: Expr<SyntaxToken>)
        (elifs: ImmutableArray<ElifBranch<SyntaxToken>>)
        (elseB: ElseBranch<SyntaxToken> voption)
        : SemType =
        let condTy = infer ctx cond
        unify ctx key condTy MockBuiltins.tyBool

        let thenTy = infer ctx thenE

        for elif_ in elifs do
            let elifCond, elifExpr =
                match elif_ with
                | ElifBranch.Elif(condition = c; expr = e)
                | ElifBranch.ElseIf(condition = c; expr = e) -> c, e

            let elifCondTy = infer ctx elifCond
            unify ctx key elifCondTy MockBuiltins.tyBool
            let elifTy = infer ctx elifExpr
            unify ctx key thenTy elifTy

        match elseB with
        | ValueSome(ElseBranch(expr = elseExpr)) ->
            let elseTy = infer ctx elseExpr
            unify ctx key thenTy elseTy
            thenTy
        | ValueNone ->
            // `if c then e` (no else) requires e : unit. Tiny subset
            // doesn't have unit yet — surface as a diagnostic.
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = "if-then without else not yet supported"
                    Severity = Error
                }

            thenTy

    and private inferFun
        (ctx: PassContext)
        (argPats: ImmutableArray<Pat<SyntaxToken>>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let argTypes = [ for p in argPats -> inferPat ctx p ]
        let bodyTy = infer ctx body
        List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

    and private inferTuple (ctx: PassContext) (items: ImmutableArray<Expr<SyntaxToken>>) : SemType =
        TyTuple [ for e in items -> infer ctx e ]

    and private inferSequential (ctx: PassContext) (key: NodeKey) (items: ImmutableArray<Expr<SyntaxToken>>) : SemType =
        // `e1; e2; …; en` — all but the last must be unit, result is the
        // last's type. A `Sequential` with fewer than two items shouldn't
        // come from the parser, but if it does, fall through harmlessly.
        if items.Length = 0 then
            MockBuiltins.tyUnit
        else
            for i = 0 to items.Length - 2 do
                let ty = infer ctx items.[i]
                unify ctx key ty MockBuiltins.tyUnit

            infer ctx items.[items.Length - 1]

    and private inferWhile
        (ctx: PassContext)
        (key: NodeKey)
        (cond: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let condTy = infer ctx cond
        unify ctx key condTy MockBuiltins.tyBool
        let bodyTy = infer ctx body
        unify ctx key bodyTy MockBuiltins.tyUnit
        MockBuiltins.tyUnit

    and private inferForTo
        (ctx: PassContext)
        (key: NodeKey)
        (ident: SyntaxToken)
        (startE: Expr<SyntaxToken>)
        (endE: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let startTy = infer ctx startE
        unify ctx key startTy MockBuiltins.tyInt
        let endTy = infer ctx endE
        unify ctx key endTy MockBuiltins.tyInt
        // Bind the loop variable as int via a TypeVar keyed on its NodeKey.
        let varKey = CstKeys.ofForToVar ident
        let varTv = freshTv ctx varKey
        varTv.Link <- ValueSome MockBuiltins.tyInt
        let bodyTy = infer ctx body
        unify ctx key bodyTy MockBuiltins.tyUnit
        MockBuiltins.tyUnit

    and private inferForIn
        (ctx: PassContext)
        (key: NodeKey)
        (pat: Pat<SyntaxToken>)
        (src: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        // Special-cased: when the source is an int range we know the
        // element type and can bind the pattern to int. For everything
        // else there's no `seq<T>` machinery yet, so the pattern stays
        // unconstrained and we emit an Info to flag the gap.
        let srcTy = infer ctx src
        let patTy = inferPat ctx pat

        let isRangeSource =
            match src with
            | Expr.Range _
            | Expr.SteppedRange _ -> true
            | Expr.EnclosedBlock(expr = Expr.Range _)
            | Expr.EnclosedBlock(expr = Expr.SteppedRange _) -> true
            | _ -> false

        if isRangeSource then
            unify ctx key srcTy MockBuiltins.tySeqInt
            unify ctx key patTy MockBuiltins.tyInt
        else
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = "for-in: enumerable / element-type checking not yet implemented"
                    Severity = Info
                }

        let bodyTy = infer ctx body
        unify ctx key bodyTy MockBuiltins.tyUnit
        MockBuiltins.tyUnit

    and private inferRules
        (ctx: PassContext)
        (key: NodeKey)
        (scrutineeTy: SemType)
        (resultTy: SemType)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : unit =
        for r in rules do
            match r with
            | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                let patTy = inferPat ctx pat
                unify ctx key patTy scrutineeTy

                match guard with
                | ValueSome(PatternGuard(expr = g)) ->
                    let gTy = infer ctx g
                    unify ctx key gTy MockBuiltins.tyBool
                | ValueNone -> ()

                let bodyTy = infer ctx body
                unify ctx key bodyTy resultTy
            | _ -> ()

    and private inferMatch
        (ctx: PassContext)
        (key: NodeKey)
        (scrutinee: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType =
        let scrutineeTy = infer ctx scrutinee
        let resultTy = TyVar(freshTyVar ctx)
        inferRules ctx key scrutineeTy resultTy rules
        resultTy

    and private inferFunction (ctx: PassContext) (key: NodeKey) (rules: ImmutableArray<Rule<SyntaxToken>>) : SemType =
        // `function p1 -> e1 | p2 -> e2` ~ `fun x -> match x with p1 -> e1 | p2 -> e2`.
        // The synthesised parameter's TypeVar IS the scrutinee's TypeVar —
        // every arm's pattern unifies with it.
        let paramTy = TyVar(freshTyVar ctx)
        let resultTy = TyVar(freshTyVar ctx)
        inferRules ctx key paramTy resultTy rules
        TyFun(paramTy, resultTy)

    and private inferTryWith
        (ctx: PassContext)
        (key: NodeKey)
        (body: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType =
        // `try body with | pat -> arm`: body and every arm share the result
        // type. The patterns are matched against an exception value — until
        // a real `exn` type is modelled, leave them as fresh TypeVars and
        // only unify the result side. Arm patterns are still inferred so
        // any names they bind have a stable TypeVar.
        let resultTy = infer ctx body
        let exnTy = TyVar(freshTyVar ctx)
        inferRules ctx key exnTy resultTy rules
        resultTy

    and private inferTryFinally
        (ctx: PassContext)
        (key: NodeKey)
        (body: Expr<SyntaxToken>)
        (finallyE: Expr<SyntaxToken>)
        : SemType =
        // `try body finally cleanup` — body's type is the result; cleanup
        // must be unit.
        let resultTy = infer ctx body
        let finallyTy = infer ctx finallyE
        unify ctx key finallyTy MockBuiltins.tyUnit
        resultTy

    and private inferAssignment
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        : SemType =
        // `lhs <- rhs` — the assignment expression has type unit. The LHS
        // and RHS must agree in type. (Mutability of the LHS binding is a
        // Validation concern; here we only typecheck.)
        let leftTy = infer ctx left
        let rightTy = infer ctx right
        unify ctx key leftTy rightTy
        MockBuiltins.tyUnit

    and private inferRecord
        (ctx: PassContext)
        (key: NodeKey)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        : SemType =
        let pairs =
            [
                for FieldInitializer(longIdent = li; expr = e) in inits ->
                    let q, n = fieldNameAndQualifier ctx li
                    q, n, e
            ]

        let qualifier =
            pairs
            |> List.tryPick (fun (q, _, _) ->
                match q with
                | ValueSome q -> Some q
                | _ -> None
            )

        let names = pairs |> List.map (fun (_, n, _) -> n)

        let candidate =
            match qualifier with
            | Some typeName ->
                match ctx.RecordTypes.TryGetValue typeName with
                | true, info -> ValueSome info
                | false, _ ->
                    ctx.Diagnostics.Add
                        {
                            Key = key
                            Message = sprintf "Unknown record type qualifier: %s" typeName
                            Severity = Error
                        }

                    ValueNone
            | None ->
                let cand, count = findUniqueRecordByFieldSet ctx names

                match cand with
                | ValueSome _ -> cand
                | ValueNone ->
                    if count = 0 then
                        ctx.Diagnostics.Add
                            {
                                Key = key
                                Message = sprintf "No record type matches the field set: %s" (String.concat ", " names)
                                Severity = Error
                            }
                    else
                        ctx.Diagnostics.Add
                            {
                                Key = key
                                Message =
                                    sprintf
                                        "Field set is ambiguous (%d candidate record types); add a qualifier or annotation"
                                        count
                                Severity = Error
                            }

                    ValueNone

        match candidate with
        | ValueNone ->
            for _, _, e in pairs do
                infer ctx e |> ignore

            TyVar(freshTyVar ctx)
        | ValueSome info ->
            // Instantiate the record's typars with fresh TyVars at the
            // current level so independent literals get independent vars.
            // Each field initialiser unifies against the field's declared
            // type *under this substitution* — a `'a` field types as the
            // fresh TyVar, which pins to the initialiser's type.
            let args, subst = freshNamedInstance ctx info.TypeParams

            for _, fieldName, e in pairs do
                let eTy = infer ctx e

                match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                | Some field -> unify ctx (CstKeys.ofExpr e) eTy (substituteWith subst field.Type)
                | None ->
                    ctx.Diagnostics.Add
                        {
                            Key = CstKeys.ofExpr e
                            Message = sprintf "Type '%s' has no field '%s'" info.Name fieldName
                            Severity = Error
                        }

            TyRecord(info.Name, args)

    and private inferRecordClone
        (ctx: PassContext)
        (key: NodeKey)
        (src: Expr<SyntaxToken>)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        : SemType =
        let srcTy = infer ctx src

        match resolveStep srcTy with
        | TyRecord(recName, srcArgs) ->
            match ctx.RecordTypes.TryGetValue recName with
            | true, info ->
                // Clone preserves the source record's arg list — the
                // override RHSes unify against the substituted field type
                // (`'a` → the source's already-pinned arg).
                let subst = mkNamedTypeSubst info.TypeParams srcArgs

                for FieldInitializer(longIdent = li; expr = e) in inits do
                    let _, fieldName = fieldNameAndQualifier ctx li
                    let eTy = infer ctx e

                    match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                    | Some field -> unify ctx (CstKeys.ofExpr e) eTy (substituteWith subst field.Type)
                    | None ->
                        ctx.Diagnostics.Add
                            {
                                Key = CstKeys.ofExpr e
                                Message = sprintf "Type '%s' has no field '%s'" recName fieldName
                                Severity = Error
                            }

                TyRecord(recName, srcArgs)
            | false, _ ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Unknown record type '%s'" recName
                        Severity = Error
                    }

                for FieldInitializer(expr = e) in inits do
                    infer ctx e |> ignore

                TyRecord(recName, srcArgs)
        | _ ->
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = "Record clone requires the source expression to be a record"
                    Severity = Error
                }

            for FieldInitializer(expr = e) in inits do
                infer ctx e |> ignore

            TyVar(freshTyVar ctx)

    /// One step of dot-access resolution: given the receiver's resolved
    /// type and the access expression's diagnostic NodeKey, produce the
    /// access's type. Deferred when the receiver is a free TyVar. For a
    /// generic receiver `(b : Box<int>).Value`, the declared field /
    /// member type `'a` is substituted against the receiver's arg list
    /// before being returned — so `Value` types as `int`, not as a free
    /// typar. Both record-field and class-member access route through
    /// this entry point; the shape of the receiver discriminates.
    and private resolveFieldStep (ctx: PassContext) (diagKey: NodeKey) (rTy: SemType) (memberName: string) : SemType =
        match resolveStep rTy with
        | TyRecord(recName, args) ->
            match ctx.RecordTypes.TryGetValue recName with
            | true, info ->
                match info.Fields |> Array.tryFind (fun f -> f.Name = memberName) with
                | Some field ->
                    let subst = mkNamedTypeSubst info.TypeParams args
                    substituteWith subst field.Type
                | None ->
                    ctx.Diagnostics.Add
                        {
                            Key = diagKey
                            Message = sprintf "Type '%s' has no field '%s'" recName memberName
                            Severity = Error
                        }

                    TyVar(freshTyVar ctx)
            | false, _ ->
                ctx.Diagnostics.Add
                    {
                        Key = diagKey
                        Message = sprintf "Unknown record type '%s'" recName
                        Severity = Error
                    }

                TyVar(freshTyVar ctx)
        | TyClass(clsName, args) ->
            match ctx.ClassTypes.TryGetValue clsName with
            | true, info ->
                match info.Members |> Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic) with
                | Some m ->
                    let subst = mkNamedTypeSubst info.TypeParams args
                    substituteWith subst m.Type
                | None ->
                    // Distinguish "no such member" from "the member is
                    // static — access via class name, not an instance".
                    let isStaticHit =
                        info.Members |> Array.exists (fun m -> m.Name = memberName && m.IsStatic)

                    let msg =
                        if isStaticHit then
                            sprintf
                                "Member '%s' on type '%s' is static; access it via '%s.%s'"
                                memberName
                                clsName
                                clsName
                                memberName
                        else
                            sprintf "Type '%s' has no instance member '%s'" clsName memberName

                    ctx.Diagnostics.Add
                        {
                            Key = diagKey
                            Message = msg
                            Severity = Error
                        }

                    TyVar(freshTyVar ctx)
            | false, _ ->
                ctx.Diagnostics.Add
                    {
                        Key = diagKey
                        Message = sprintf "Unknown class type '%s'" clsName
                        Severity = Error
                    }

                TyVar(freshTyVar ctx)
        | TyVar tv ->
            let root = UnionFind.find tv
            let resultTv = freshTyVar ctx
            root.PendingDotAccess <- (memberName, diagKey, resultTv) :: root.PendingDotAccess
            TyVar resultTv
        | _ ->
            ctx.Diagnostics.Add
                {
                    Key = diagKey
                    Message = sprintf "Cannot read member '%s' from non-record non-class type" memberName
                    Severity = Error
                }

            TyVar(freshTyVar ctx)

    and private inferFieldAccess
        (ctx: PassContext)
        (key: NodeKey)
        (receiver: Expr<SyntaxToken>)
        (fieldTok: SyntaxToken)
        : SemType =
        let fieldName = ctx.NameOf fieldTok
        let rTy = infer ctx receiver
        resolveFieldStep ctx key rTy fieldName

    /// `new T(args)` — translate `T` (resolving to a `TyClass`), build
    /// the primary-constructor's expected argument shape, unify the
    /// supplied argument against it, and return the constructed
    /// `TyClass`. Mirrors a single application against the value
    /// returned by `classCtorAsFunction` — kept inline so a bare
    /// `Expr.New` doesn't need to fabricate an `Expr.App` first.
    and private inferNew
        (ctx: PassContext)
        (key: NodeKey)
        (t: Type<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType =
        let receiverTy = translateType ctx t

        match resolveStep receiverTy with
        | TyClass(name, args) ->
            match ctx.ClassTypes.TryGetValue name with
            | true, info ->
                let subst = mkNamedTypeSubst info.TypeParams args

                let paramTys =
                    info.CtorParams
                    |> Array.map (fun p -> substituteWith subst p.Type)
                    |> Array.toList

                let expected =
                    match paramTys with
                    | [] -> MockBuiltins.tyUnit
                    | [ t ] -> t
                    | many -> TyTuple many

                let argTy = infer ctx argExpr
                unify ctx (CstKeys.ofExpr argExpr) argTy expected
                receiverTy
            | false, _ ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Unknown class type '%s'" name
                        Severity = Error
                    }

                infer ctx argExpr |> ignore
                TyVar(freshTyVar ctx)
        | _ ->
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = "'new' requires a class type"
                    Severity = Error
                }

            infer ctx argExpr |> ignore
            TyVar(freshTyVar ctx)

    /// `r.X.Y…` parsed as a single multi-segment `Expr.LongIdentOrOp`.
    /// The head segment was resolved by NameResolution as a local binding
    /// — type it through `ctx.Binding`/`ctx.Scheme` (same path as
    /// `inferIdent` for a single-segment ident), then walk the remaining
    /// segments as a field-access chain.
    and private inferLongIdentFieldChain (ctx: PassContext) (key: NodeKey) (li: LongIdent<SyntaxToken>) : SemType =
        let head = li.Idents.[0]
        let headKey = NodeKey.ofToken head NodeKind.ExprIdent

        let headTy =
            match ctx.Binding.TryGetValue headKey with
            | ValueSome rb ->
                match ctx.Scheme.TryGetValue rb.BindingSite with
                | ValueSome scheme -> instantiate ctx scheme
                | ValueNone -> TyVar(tvOf ctx rb.BindingSite)
            | ValueNone -> TyVar(freshTyVar ctx)

        let mutable currTy = headTy

        for i = 1 to li.Idents.Length - 1 do
            let seg = li.Idents.[i]
            let segName = ctx.NameOf seg
            // Diagnose against the LongIdent's overall key — there's no
            // separate sub-expression NodeKey for an intermediate segment.
            currTy <- resolveFieldStep ctx key currTy segName

        currTy

    and private inferString
        (ctx: PassContext)
        (_key: NodeKey)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        : SemType =
        // Non-interpolated strings type as `string`. Interpolated strings
        // also type as string (the holes are typed via printf-format
        // checking, which we don't model yet — recurse into the hole exprs
        // so their types still get computed, but don't constrain them).
        for part in parts do
            match part with
            | StringPart.Expr(expr = e) -> infer ctx e |> ignore
            | _ -> ()

        MockBuiltins.tyString

    and private inferTypeAnnotation
        (ctx: PassContext)
        (key: NodeKey)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let innerTy = infer ctx inner
        let annTy = translateType ctx t
        unify ctx key innerTy annTy
        annTy

    and private inferLet
        (ctx: PassContext)
        (key: NodeKey)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        (body: Expr<SyntaxToken> voption)
        : SemType =
        inferBindingGroup ctx bindings

        match body with
        | ValueSome bodyExpr -> infer ctx bodyExpr
        | ValueNone ->
            // `Expr.LetOrUse(body = ValueNone)` is `use fixed` (module-level
            // lets are ModuleElems, not Expr.LetOrUse). Tiny subset doesn't
            // support pinning — surface loudly so Freeze doesn't see a
            // best-effort type for an unsupported construct.
            failwith "Unification: Expr.LetOrUse with no body (UseFixed) not supported"

    and private inferBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        // One typar scope per binding signature. Explicit `<'a>` typars
        // seed it before any pattern / body walk; implicit `'a` mentions
        // in annotations later in the signature pick up the same TyVar.
        let savedScope = ctx.TyparScope
        ctx.TyparScope <- Dictionary<string, TypeVar>(System.StringComparer.Ordinal)

        match b.typarDefns with
        | ValueSome(TyparDefns(defns = ds; constraints = bindingConstraints)) ->
            for TyparDefn(typar = t) in ds do
                match t with
                | Typar.Named(ident = id)
                | Typar.Static(ident = id) ->
                    let n = ctx.NameOf id

                    if not (ctx.TyparScope.ContainsKey n) then
                        let tv = TypeVar()
                        tv.Level <- ctx.CurrentLevel
                        ctx.TyparScope.[n] <- tv
                | Typar.Anon _ -> ()

            match bindingConstraints with
            | ValueSome cs -> translateConstraints ctx cs
            | ValueNone -> ()
        | ValueNone -> ()

        try
            let patTy = inferPat ctx b.headPat

            let rhsTy =
                if b.argumentPats.IsEmpty then
                    let bodyTy = infer ctx b.expr

                    match b.returnType with
                    | ValueSome(ReturnType(typ = t)) ->
                        // `let v : T = expr` — translate T under the
                        // binding's typar scope and unify with the RHS.
                        let annTy = translateType ctx t
                        unify ctx (CstKeys.ofBinding b) bodyTy annTy
                        annTy
                    | ValueNone -> bodyTy
                else
                    // `let f x y = body` is `let f = fun x y -> body`.
                    let argTypes = [ for p in b.argumentPats -> inferPat ctx p ]
                    let bodyTy = infer ctx b.expr

                    let bodyTy =
                        match b.returnType with
                        | ValueSome(ReturnType(typ = t)) ->
                            // `let f x : T = body` — body's type unifies
                            // with the declared return type.
                            let annTy = translateType ctx t
                            unify ctx (CstKeys.ofBinding b) bodyTy annTy
                            annTy
                        | ValueNone -> bodyTy

                    List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

            unify ctx (CstKeys.ofBinding b) patTy rhsTy
        finally
            ctx.TyparScope <- savedScope

    /// Type a `let` / `let rec` group with Rémy-level discipline:
    ///   1. Snapshot the outer level and push one level for the group.
    ///   2. Pre-allocate single-name sibling headPat TyVars so forward
    ///      references from inside one RHS (or any nested let within) find
    ///      the sibling's TyVar at this group's level rather than lazy-mint
    ///      at a deeper one — which would let a nested let generalise a
    ///      var that actually belongs to an un-typed outer sibling.
    ///   3. Type every binding's RHS at the pushed level (sibling lookups
    ///      stay monomorphic — no scheme is written until step 5).
    ///   4. Pop back to the outer level.
    ///   5. Generalise each `shouldGeneralise` binding against the outer
    ///      level and write its scheme to `ctx.Scheme`.
    and private inferBindingGroup (ctx: PassContext) (bindings: ImmutableArray<Binding<SyntaxToken>>) : unit =
        let outerLevel = ctx.CurrentLevel
        enterLevel ctx

        for b in bindings do
            match b.headPat with
            | Pat.NamedSimple _ -> tvOf ctx (CstKeys.ofPat b.headPat) |> ignore
            | _ -> ()

        for b in bindings do
            inferBinding ctx b

        exitLevel ctx

        for b in bindings do
            if shouldGeneralise b then
                let key = CstKeys.ofPat b.headPat
                let headTv = tvOf ctx key
                let zonked = zonk (TyVar headTv)

                if not (hasPendingDotAccess zonked) then
                    let scheme = generalise zonked outerLevel
                    ctx.Scheme.Set(key, scheme)

    let private walkModuleElem (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            inferBindingGroup ctx bindings
        | ModuleElem.Expression e -> infer ctx e |> ignore
        | _ -> ()

    /// Rebuild the typar scope of a type definition from the registry
    /// entry's `TypeParams`. Each `(name, tv)` pair is keyed under the
    /// name so a field type containing `'name` resolves to `tv` —
    /// linking the placeholder field TyVar through the same root the
    /// registry already holds.
    let private scopeOfTypeParams (typeParams: (string * TypeVar) list) : Dictionary<string, TypeVar> =
        let d = Dictionary<string, TypeVar>(System.StringComparer.Ordinal)

        for (n, tv) in typeParams do
            if not (d.ContainsKey n) then
                d.[n] <- tv

        d

    /// After NameResolution stamps placeholder TyVars for every record
    /// field, walk the file's `TypeDefn.Record`s again and Link each
    /// placeholder to the real translated CST type. Done as a pre-pass so
    /// a record's field type can reference another record declared
    /// elsewhere in the same file — at this point every record name is
    /// already in `ctx.RecordTypes`, so `translateType`'s lookup succeeds.
    let private fillRecordFieldTypes (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match td with
                | TypeDefn.Record(typeName = TypeName(ident = nameLi); fields = fields) when nameLi.Idents.Length = 1 ->
                    let name = ctx.NameOf nameLi.Idents.[0]

                    match ctx.RecordTypes.TryGetValue name with
                    | true, info ->
                        let savedScope = ctx.TyparScope
                        let savedStrict = ctx.TyparScopeStrict
                        ctx.TyparScope <- scopeOfTypeParams info.TypeParams
                        ctx.TyparScopeStrict <- true

                        try
                            match info.TyparConstraints with
                            | ValueSome cs -> translateConstraints ctx cs
                            | ValueNone -> ()

                            let n = min info.Fields.Length fields.Length

                            for i = 0 to n - 1 do
                                let (RecordField(ident = id; typ = t)) = fields.[i]
                                let translated = translateType ctx t

                                match info.Fields.[i].Type with
                                | TyVar tv ->
                                    let root = UnionFind.find tv
                                    root.Link <- ValueSome translated
                                | _ -> ()

                                ignore id
                        finally
                            ctx.TyparScope <- savedScope
                            ctx.TyparScopeStrict <- savedStrict
                    | false, _ -> ()
                | _ -> ()
        | _ -> ()

    /// After NameResolution stamps placeholder TyVars for every union case
    /// field, walk the file's `TypeDefn.Union`s again and Link each
    /// placeholder to the real translated CST type. Same shape as
    /// `fillRecordFieldTypes` — runs after every record/union is in the
    /// registry so a case's field type can name another DU declared
    /// elsewhere in the same file.
    let private fillUnionFieldTypes (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match td with
                | TypeDefn.Union(typeName = TypeName(ident = nameLi); cases = cases) when nameLi.Idents.Length = 1 ->
                    let name = ctx.NameOf nameLi.Idents.[0]

                    match ctx.UnionTypes.TryGetValue name with
                    | true, info ->
                        let savedScope = ctx.TyparScope
                        let savedStrict = ctx.TyparScopeStrict
                        ctx.TyparScope <- scopeOfTypeParams info.TypeParams
                        ctx.TyparScopeStrict <- true

                        try
                            match info.TyparConstraints with
                            | ValueSome cs -> translateConstraints ctx cs
                            | ValueNone -> ()

                            // Walk the case list in declaration order, skipping
                            // any GADT cases (they don't have a registry entry
                            // — `inspectCaseData` returns ValueNone). The
                            // registry's `Cases` array tracks declaration order
                            // among successfully-registered cases, so we align
                            // by walking the CST and the array in tandem.
                            let mutable infoIdx = 0

                            for UnionTypeCase(data = data) in cases do
                                let isRegistered =
                                    match data with
                                    | UnionTypeCaseData.GadtNary _
                                    | UnionTypeCaseData.GadtNullary _ -> false
                                    | _ -> true

                                if isRegistered && infoIdx < info.Cases.Length then
                                    let caseInfo = info.Cases.[infoIdx]
                                    infoIdx <- infoIdx + 1

                                    let fields =
                                        match data with
                                        | UnionTypeCaseData.Nary(fields = fs) -> fs
                                        | _ ->
                                            System.Collections.Immutable.ImmutableArray<UnionTypeField<SyntaxToken>>
                                                .Empty

                                    let n = min caseInfo.Fields.Length fields.Length

                                    for i = 0 to n - 1 do
                                        let t =
                                            match fields.[i] with
                                            | UnionTypeField.Unnamed(typ = t) -> t
                                            | UnionTypeField.Named(typ = t) -> t

                                        let translated = translateType ctx t

                                        match caseInfo.Fields.[i] with
                                        | TyVar tv ->
                                            let root = UnionFind.find tv
                                            root.Link <- ValueSome translated
                                        | _ -> ()
                        finally
                            ctx.TyparScope <- savedScope
                            ctx.TyparScopeStrict <- savedStrict
                    | false, _ -> ()
                | _ -> ()
        | _ -> ()

    /// Translate the primary constructor argument types under the class's
    /// typar scope and Link each placeholder TyVar to its declared type.
    /// `Pat.Typed` carries an annotation; un-annotated arguments leave
    /// the placeholder as a free TyVar so a use site can pin it via
    /// argument-type unification in `inferNew` / `inferApp`.
    let private fillClassCtorParamTypes
        (ctx: PassContext)
        (info: ClassTypeInfo)
        (pcOpt: PrimaryConstrArgs<SyntaxToken> voption)
        : unit =
        match pcOpt with
        | ValueNone -> ()
        | ValueSome(PrimaryConstrArgs(pat = ValueNone)) -> ()
        | ValueSome(PrimaryConstrArgs(pat = ValueSome p)) ->
            let idx = ref 0

            let rec walk (p: Pat<SyntaxToken>) =
                match p with
                | Pat.NamedSimple _ -> incr idx
                | Pat.Typed(pat = inner; typ = t) ->
                    let i = !idx
                    incr idx

                    if i < info.CtorParams.Length then
                        let translated = translateType ctx t

                        match info.CtorParams.[i].Type with
                        | TyVar tv ->
                            let root = UnionFind.find tv
                            root.Link <- ValueSome translated
                        | _ -> ()

                    ignore inner
                | Pat.EnclosedBlock(pat = inner) -> walk inner
                | Pat.Tuple(patterns = pats) ->
                    for sub in pats do
                        walk sub
                | _ -> ()

            walk p

    /// Walk every class member body under a typar scope seeded from
    /// `info.TypeParams` and a binding scope that supplies `this` and
    /// each ctor param. The placeholder member TyVars stored by
    /// NameResolution are pre-populated into `ctx.TypeVar` keyed on the
    /// member's headPat NodeKey so `inferBinding`'s `tvOf` reuses them
    /// — letting `inferBinding`'s final `unify patTy rhsTy` link the
    /// placeholder directly to the inferred member type. AutoProperty
    /// has no `Binding`, so its placeholder is linked manually.
    let private fillClassMembers (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        let common (td: TypeDefn<SyntaxToken>) =
            match td with
            | TypeDefn.Class(typeName = TypeName(ident = nameLi); primaryConstr = pc; body = body)
            | TypeDefn.Anon(typeName = TypeName(ident = nameLi); primaryConstr = pc; body = body) when
                nameLi.Idents.Length = 1
                ->
                ValueSome(ctx.NameOf nameLi.Idents.[0], pc, body)
            | _ -> ValueNone

        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match common td with
                | ValueSome(name, pc, body) ->

                    match ctx.ClassTypes.TryGetValue name with
                    | true, info ->
                        let savedScope = ctx.TyparScope
                        let savedStrict = ctx.TyparScopeStrict
                        ctx.TyparScope <- scopeOfTypeParams info.TypeParams
                        ctx.TyparScopeStrict <- true

                        try
                            // Translate ctor-param type annotations now,
                            // under the class's typar scope (so `'a`
                            // resolves to the registry's prototype typar).
                            fillClassCtorParamTypes ctx info pc

                            // Seed `ctx.TypeVar` so `inferIdent` lookups
                            // against the param/`this` binding sites
                            // return these TyVars. ctor params: pull
                            // out the underlying TyVar from each
                            // placeholder's `info.CtorParams[i].Type`.
                            for p in info.CtorParams do
                                match p.Type with
                                | TyVar tv -> ctx.TypeVar.Set(p.DeclKey, tv)
                                | _ -> ()

                            // `this` TyVar: a fresh TyVar at the
                            // current level pre-linked to `TyClass`
                            // over the class's prototype typars. The
                            // prototype-typar instance lets generic
                            // class member bodies that mention `'a`
                            // share identity with the registry typars.
                            let thisTv = TypeVar()
                            thisTv.Level <- ctx.CurrentLevel

                            let selfArgs = [ for (_, ptv) in info.TypeParams -> TyVar ptv ]

                            thisTv.Link <- ValueSome(TyClass(info.Name, selfArgs))
                            ctx.TypeVar.Set(info.ThisKey, thisTv)

                            // Type each member's body. Static members
                            // are typed under the same typar scope but
                            // their body never sees `this` / ctor
                            // params (NameResolution gives them an
                            // empty binding scope). Placeholder TyVars
                            // for static members live in the same
                            // `info.Members` array — IsStatic
                            // discriminates downstream lookups.
                            for el in body.elements do
                                match el with
                                | TypeDefnElement.Member(MemberDefn.Member(defn = d)) ->
                                    match d with
                                    | MethodOrPropDefn.Method(defn = b)
                                    | MethodOrPropDefn.Property(defn = b) ->
                                        let mNameOpt =
                                            let rec walkP (p: Pat<SyntaxToken>) =
                                                match p with
                                                | Pat.NamedSimple id -> ValueSome id
                                                | Pat.EnclosedBlock(pat = inner)
                                                | Pat.Typed(pat = inner) -> walkP inner
                                                | _ -> ValueNone

                                            walkP b.headPat

                                        match mNameOpt with
                                        | ValueSome mTok ->
                                            let mKey = NodeKey.ofToken mTok NodeKind.PatIdent

                                            // Pre-seed ctx.TypeVar with the
                                            // placeholder TyVar so inferPat's
                                            // tvOf reuses it.
                                            let mInfoOpt = info.Members |> Array.tryFind (fun m -> m.DeclKey = mKey)

                                            match mInfoOpt with
                                            | Some mInfo ->
                                                match mInfo.Type with
                                                | TyVar tv -> ctx.TypeVar.Set(mKey, tv)
                                                | _ -> ()
                                            | None -> ()

                                            let outerLevel = ctx.CurrentLevel
                                            enterLevel ctx

                                            try
                                                inferBinding ctx b
                                            finally
                                                exitLevel ctx
                                        | ValueNone -> ()
                                    | MethodOrPropDefn.AutoProperty(ident = id; expr = e; returnType = rt) ->
                                        let outerLevel = ctx.CurrentLevel
                                        enterLevel ctx

                                        try
                                            let bodyTy = infer ctx e

                                            let resultTy =
                                                match rt with
                                                | ValueSome(ReturnType(typ = t)) ->
                                                    let t' = translateType ctx t
                                                    unify ctx (CstKeys.ofExpr e) bodyTy t'
                                                    t'
                                                | ValueNone -> bodyTy

                                            let mKey = NodeKey.ofToken id NodeKind.PatIdent

                                            match info.Members |> Array.tryFind (fun m -> m.DeclKey = mKey) with
                                            | Some mInfo ->
                                                match mInfo.Type with
                                                | TyVar tv ->
                                                    let root = UnionFind.find tv
                                                    root.Link <- ValueSome resultTy
                                                | _ -> ()
                                            | None -> ()
                                        finally
                                            exitLevel ctx
                                    | _ -> ()
                                | _ -> ()
                        finally
                            ctx.TyparScope <- savedScope
                            ctx.TyparScopeStrict <- savedStrict
                    | false, _ -> ()
                | ValueNone -> ()
        | _ -> ()

    /// Force every abbreviation body in source order. Each call into
    /// `forceFill` recurses through `translateType` for any abbreviation
    /// reference it encounters, so dependencies fill themselves DFS-style
    /// regardless of declaration order. Already-`Filled` entries are
    /// no-ops; cycles are diagnosed once. Runs before record / union
    /// field fill so a record field or DU case-arg referencing an
    /// abbreviation by name sees the expanded type.
    let private fillAbbreviationBodies (ctx: PassContext) (elems: ModuleElems<SyntaxToken>) : unit =
        for m in elems do
            match m with
            | ModuleElem.Type defs ->
                for td in defs do
                    match td with
                    | TypeDefn.Abbrev(typeName = TypeName(ident = nameLi)) when nameLi.Idents.Length = 1 ->
                        let name = ctx.NameOf nameLi.Idents.[0]

                        match ctx.AbbreviationTypes.TryGetValue name with
                        | true, info -> forceFill ctx info
                        | false, _ -> ()
                    | _ -> ()
            | _ -> ()

    let private walkElems (ctx: PassContext) (elems: ModuleElems<SyntaxToken>) =
        fillAbbreviationBodies ctx elems

        for m in elems do
            fillRecordFieldTypes ctx m

        for m in elems do
            fillUnionFieldTypes ctx m

        for m in elems do
            fillClassMembers ctx m

        for m in elems do
            walkModuleElem ctx m

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        match file with
        | ImplementationFile.AnonymousModule elems -> walkElems ctx elems
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) -> walkElems ctx elems
        | ImplementationFile.Namespaces _ -> ()
