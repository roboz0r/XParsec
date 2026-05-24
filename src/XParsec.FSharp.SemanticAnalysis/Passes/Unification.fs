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
// Algorithm J + Rémy's levels.
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

    /// One level deep — call recursively for full resolution. Stops at a
    /// measure-bearing root so the measure stays attached: `unify` and
    /// `unitsOf` need the TyVar wrapper to see Units, and following Link
    /// straight through to the bare carrier would drop them.
    let private resolveStep (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome t' when root.Units.IsNone -> t'
            | _ -> TyVar root
        | _ -> t

    /// Fully resolve a SemType: walk all TyVar chains AND recurse into
    /// compound shapes. A measure-bearing TyVar (`Units` set on its root)
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

    /// Collapses any pair whose `Kind` already appears on the target: two
    /// constraints with the same `Kind` discharge to the same predicate, so
    /// keeping both would fire the diagnostic twice for one rule.
    let private mergeConstraints (target: TypeVar) (additions: SemanticConstraint list) : unit =
        let mutable acc = target.Constraints

        for c in additions do
            if not (acc |> List.exists (fun existing -> existing.Kind = c.Kind)) then
                acc <- c :: acc

        target.Constraints <- acc

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

            if not (List.isEmpty source.Defaults) then
                target.Defaults <- target.Defaults @ source.Defaults
                source.Defaults <- []
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

    /// Two non-equal measures emit a diagnostic; one of them is kept on the
    /// survivor so further unifications against it stay coherent.
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
    /// target `SemType`, recursing into compound shapes. Other TyVars are
    /// returned unchanged (followed through union-find but not their
    /// `Link`s — that's `zonk`'s job). Public so Freeze can reuse the same
    /// substitution when reading field types off a generic receiver.
    let rec substituteWith (subst: Dictionary<TypeVar, SemType>) (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match subst.TryGetValue root with
            | true, target -> target
            | false, _ ->
                // Field / case-arg types stored on the registry are
                // *placeholder* TyVars whose root is never in `subst` (keys
                // are the declared type's `TypeParams`). Follow `Link` so a
                // placeholder targeting `TyVar typarRoot` resolves to
                // whatever `subst[typarRoot]` says. Stop at measure-bearing
                // roots (same rule `zonk` uses): a measured TyVar's `Link`
                // carries the bare carrier, and following through would drop
                // the `Units` on the root.
                match root.Link with
                | ValueSome target when root.Units.IsNone -> substituteWith subst target
                | _ -> TyVar root
        | TyConst _ -> t
        | TyFun(a, r) -> TyFun(substituteWith subst a, substituteWith subst r)
        | TyTuple xs -> TyTuple [ for x in xs -> substituteWith subst x ]
        | TyRecord(n, args) -> TyRecord(n, [ for a in args -> substituteWith subst a ])
        | TyUnion(n, args) -> TyUnion(n, [ for a in args -> substituteWith subst a ])
        | TyClass(n, args) -> TyClass(n, [ for a in args -> substituteWith subst a ])

    /// Empty when the lengths don't match — the caller has already (or
    /// should) emit an arity diagnostic, and an empty subst keeps the field
    /// types unsubstituted rather than silently mismatching. Public so
    /// Freeze can rebuild the same substitution when projecting fields off a
    /// generic receiver in a field-chain.
    let mkNamedTypeSubst (typeParams: (string * TypeVar) list) (args: SemType list) : Dictionary<TypeVar, SemType> =
        let subst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)

        if List.length typeParams = List.length args then
            List.iter2 (fun (_, tp) arg -> subst.[UnionFind.find tp] <- arg) typeParams args

        subst

    /// Walk a `SemType` through TyVar Links to surface a `TyRecord _`. The
    /// arg list rides along so `drainPendingDotAccess` can substitute the
    /// record's typars when resolving deferred field accesses.
    let rec private tryResolveRecord (t: SemType) : (string * SemType list) voption =
        match t with
        | TyRecord(n, args) -> ValueSome(n, args)
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome target -> tryResolveRecord target
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Mirror of `tryResolveRecord` for `TyClass`.
    let rec private tryResolveClass (t: SemType) : (string * SemType list) voption =
        match t with
        | TyClass(n, args) -> ValueSome(n, args)
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome target -> tryResolveClass target
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Mirror of `tryResolveClass` for `TyUnion` (P3d.3 augmentation members).
    let rec private tryResolveUnion (t: SemType) : (string * SemType list) voption =
        match t with
        | TyUnion(n, args) -> ValueSome(n, args)
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome target -> tryResolveUnion target
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// `Defer` is the "I don't know yet" answer: the target is still free
    /// (or compound-with-free-args) and a future unification might pin it.
    /// `drainConstraints` keeps deferred constraints on the TyVar so they
    /// re-fire on the next `Link` change.
    type private ConstraintOutcome =
        | Satisfied
        | Violated
        | Defer

    /// `string` is excluded and handled separately since it's a reference type.
    let private primitiveValueTypes =
        Set.ofList [ "int"; "int64"; "byte"; "bool"; "float"; "float32"; "char"; "unit" ]

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
            // If both sides carried links, unify them so the carriers agree.
            match linkA, linkB with
            | ValueNone, ValueNone -> ()
            | ValueSome _, ValueNone ->
                newRoot.Link <- linkA

                match linkA with
                | ValueSome t ->
                    drainPendingDotAccess ctx newRoot t
                    drainConstraints ctx key newRoot t
                    drainSrtpBounds ctx key newRoot t
                | ValueNone -> ()
            | ValueNone, ValueSome _ ->
                newRoot.Link <- linkB

                match linkB with
                | ValueSome t ->
                    drainPendingDotAccess ctx newRoot t
                    drainConstraints ctx key newRoot t
                    drainSrtpBounds ctx key newRoot t
                | ValueNone -> ()
            | ValueSome a, ValueSome b ->
                newRoot.Link <- linkA
                unify ctx key a b

                match linkA with
                | ValueSome t ->
                    drainPendingDotAccess ctx newRoot t
                    drainConstraints ctx key newRoot t
                    drainSrtpBounds ctx key newRoot t
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
                drainSrtpBounds ctx key root other
        | _ ->
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = sprintf "Type mismatch: %A vs %A" (zonk a) (zonk b)
                    Severity = Error
                }

    /// When a TyVar's Link resolves to a `TyRecord`/`TyClass`/`TyUnion`,
    /// resolve any dot-access constraints parked on it. When `T` is generic,
    /// the receiver's arg list substitutes for the type's declared typars so
    /// `(b : Box<int>).Value` resolves to `int`, not `Box`'s prototype `'a`.
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
                | ValueNone ->
                    // Union augmentation members (P3d.3).
                    match tryResolveUnion linkTarget with
                    | ValueNone -> ()
                    | ValueSome(unionName, args) ->
                        let pending = root.PendingDotAccess
                        root.PendingDotAccess <- []

                        match ctx.UnionTypes.TryGetValue unionName with
                        | true, info ->
                            let subst = mkNamedTypeSubst info.TypeParams args

                            for (memberName, useKey, resultTv) in pending do
                                match
                                    info.Members |> Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic)
                                with
                                | Some m -> unify ctx useKey (TyVar resultTv) (substituteWith subst m.Type)
                                | None ->
                                    ctx.Diagnostics.Add
                                        {
                                            Key = useKey
                                            Message =
                                                sprintf "Type '%s' has no instance member '%s'" unionName memberName
                                            Severity = Error
                                        }
                        | false, _ ->
                            for (_, useKey, _) in pending do
                                ctx.Diagnostics.Add
                                    {
                                        Key = useKey
                                        Message = sprintf "Unknown union type '%s'" unionName
                                        Severity = Error
                                    }

    /// `ValueSome true` = constraint holds; `ValueSome false` = violation;
    /// `ValueNone` = not in the table, fall through to structural / deferred
    /// handling.
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

    /// Free TyVars return `Defer` so the next `Link` assignment re-fires the
    /// check via `drainConstraints`; nested compounds recurse compositionally.
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

    /// On-unified callback for type-parameter constraints. Satisfied
    /// constraints are dropped; deferred ones remain on the root and re-fire
    /// next time `Link` changes (which, after the first set, only happens
    /// during union-find collapse). For compound `Defer` outcomes, copy the
    /// constraint onto each still-free arg so the next Link on any of them
    /// re-evaluates the rule compositionally.
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

    /// When a compound shape is partially resolved, the parent constraint is
    /// satisfied iff every component supports it, so a still-free component
    /// carries the same constraint forward.
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

    /// SRTP arithmetic dispatch on numeric primitives. For `op_Addition`
    /// etc. on `int` the candidate "static member" type is `int * int ->
    /// int`; we synthesise it here so the unifier doesn't need to know
    /// which provider declared the primitive.
    and private numericPrimitives =
        Set.ofList
            [
                "int"
                "int8"
                "int16"
                "int32"
                "int64"
                "uint"
                "uint8"
                "uint16"
                "uint32"
                "uint64"
                "byte"
                "sbyte"
                "nativeint"
                "unativeint"
                "float"
                "float32"
                "double"
                "single"
                "decimal"
            ]

    and private arithmeticBinaryOps =
        Set.ofList [ "op_Addition"; "op_Subtraction"; "op_Multiply"; "op_Division"; "op_Modulus" ]

    and private comparisonBinaryOps =
        Set.ofList
            [
                "op_LessThan"
                "op_GreaterThan"
                "op_LessThanOrEqual"
                "op_GreaterThanOrEqual"
                "op_Equality"
                "op_Inequality"
            ]

    and private tryPrimitiveTraitCandidate (memberName: string) (primName: string) (argCount: int) : SemType voption =
        if not (Set.contains primName numericPrimitives) then
            ValueNone
        elif argCount = 2 && Set.contains memberName arithmeticBinaryOps then
            let t = TyConst primName
            ValueSome(TyFun(TyTuple [ t; t ], t))
        elif argCount = 1 && memberName = "op_UnaryNegation" then
            let t = TyConst primName
            ValueSome(TyFun(t, t))
        elif argCount = 2 && Set.contains memberName comparisonBinaryOps then
            let t = TyConst primName
            ValueSome(TyFun(TyTuple [ t; t ], TyConst "bool"))
        else
            ValueNone

    /// Build the expected trait signature in tupled or curried form,
    /// picking whichever matches the candidate's shape. F# accepts both
    /// `static member (+)(a, b)` (tupled) and `static member (+) a b`
    /// (curried) as satisfying a trait declared `^T * ^T -> ^T`.
    and private unifySrtpAgainst
        (ctx: PassContext)
        (key: NodeKey)
        (candidate: SemType)
        (bound: MemberSignature)
        : unit =
        let tupled =
            match bound.ArgTypes with
            | [] -> bound.ReturnType
            | [ a ] -> TyFun(a, bound.ReturnType)
            | args -> TyFun(TyTuple args, bound.ReturnType)

        match resolveStep candidate, bound.ArgTypes with
        | TyFun(TyTuple _, _), _ -> unify ctx key candidate tupled
        | _, _ :: _ :: _ ->
            let curried = List.foldBack (fun a r -> TyFun(a, r)) bound.ArgTypes bound.ReturnType

            unify ctx key candidate curried
        | _ -> unify ctx key candidate tupled

    /// On-unified callback for SRTP member-trait bounds. The `Resolved` flag
    /// on the shared `MemberSignature` instance (all participating typars
    /// hold the same record by reference) dedupes dispatch when multiple
    /// participating typars resolve in sequence — whichever links first runs
    /// the drain; the others see the flag set and skip. Bounds that can't
    /// dispatch yet (target is still a free TyVar) remain on the root.
    ///
    /// Diagnostics use `key` — the user's call site, threaded through from
    /// the caller — so "Type X has no static member Y" points there rather
    /// than at the prelude's `(+)` declaration.
    and private drainSrtpBounds (ctx: PassContext) (key: NodeKey) (root: TypeVar) (linkTarget: SemType) : unit =
        if List.isEmpty root.SrtpBounds then
            ()
        else
            let bounds = root.SrtpBounds
            root.SrtpBounds <- []
            let mutable remaining = []

            for b in bounds do
                if b.Resolved then
                    ()
                else
                    match resolveStep linkTarget with
                    | TyConst primName ->
                        match tryPrimitiveTraitCandidate b.MemberName primName b.ArgTypes.Length with
                        | ValueSome candTy ->
                            b.Resolved <- true
                            unifySrtpAgainst ctx key candTy b
                        | ValueNone ->
                            ctx.Diagnostics.Add
                                {
                                    Key = key
                                    Message =
                                        sprintf "Type '%s' has no built-in static member '%s'" primName b.MemberName
                                    Severity = Error
                                }

                            b.Resolved <- true
                    | TyClass(className, classArgs) ->
                        match ctx.ClassTypes.TryGetValue className with
                        | true, info ->
                            match info.Members |> Array.tryFind (fun m -> m.IsStatic && m.Name = b.MemberName) with
                            | Some m ->
                                let subst = mkNamedTypeSubst info.TypeParams classArgs
                                let candTy = substituteWith subst m.Type
                                b.Resolved <- true
                                unifySrtpAgainst ctx key candTy b
                            | None ->
                                ctx.Diagnostics.Add
                                    {
                                        Key = key
                                        Message = sprintf "Type '%s' has no static member '%s'" className b.MemberName
                                        Severity = Error
                                    }

                                b.Resolved <- true
                        | false, _ ->
                            // Unknown class — keep the bound so a later
                            // pass might still be able to dispatch.
                            remaining <- b :: remaining
                    | _ ->
                        // Target not yet a concrete type-bearing shape — defer.
                        remaining <- b :: remaining

            root.SrtpBounds <- List.rev remaining

    let private enterLevel (ctx: PassContext) : unit =
        ctx.CurrentLevel <- ctx.CurrentLevel + 1

    let private exitLevel (ctx: PassContext) : unit =
        ctx.CurrentLevel <- ctx.CurrentLevel - 1

    /// Fresh unkeyed TypeVar — for intermediate "result" TyVars not tied to
    /// a CST node's NodeKey.
    let private freshTyVar (ctx: PassContext) : TypeVar =
        let tv = TypeVar()
        tv.Level <- ctx.CurrentLevel
        tv

    /// Overwrites any prior entry — callers that need "get or allocate"
    /// (e.g. forward-referenced let-rec siblings) must go through `tvOf`.
    let private freshTv (ctx: PassContext) (key: NodeKey) : TypeVar =
        let tv = TypeVar()
        tv.Level <- ctx.CurrentLevel
        ctx.TypeVar.Set(key, tv)
        tv

    /// Get-or-allocate: fresh-allocates if missing — happens for binding-site
    /// patterns not yet visited by inferPat, including forward references
    /// inside `let rec` groups.
    let private tvOf (ctx: PassContext) (key: NodeKey) : TypeVar =
        match ctx.TypeVar.TryGetValue key with
        | ValueSome tv -> tv
        | ValueNone -> freshTv ctx key

    /// Mint fresh TyVars per quantifier, then rewrite `scheme.Body`.
    /// Non-quantified TyVars are left alone — they're free w.r.t. the
    /// surrounding scope and must keep their identity. `scheme.Body` is
    /// already zonked by `generalise`, so we don't follow Links here.
    let private instantiate (ctx: PassContext) (scheme: TypeScheme) : SemType =
        let subst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)
        // freshOf remembers each fresh TyVar so per-quantifier constraints
        // can be re-stamped onto it below.
        let freshOf = Dictionary<TypeVar, TypeVar>(HashIdentity.Reference)

        for q in scheme.Quantified do
            let qRoot = UnionFind.find q
            let fresh = TypeVar()
            fresh.Level <- ctx.CurrentLevel
            subst.[qRoot] <- TyVar fresh
            freshOf.[qRoot] <- fresh

        // Re-stamp constraints onto the fresh instance TyVars so each use
        // site re-evaluates satisfaction against its own substitution; the
        // original quantified TyVars stay constraint-bearing for the next call.
        for (qTv, c) in scheme.Constraints do
            let qRoot = UnionFind.find qTv

            match freshOf.TryGetValue qRoot with
            | true, fresh ->
                if not (fresh.Constraints |> List.exists (fun e -> e.Kind = c.Kind)) then
                    fresh.Constraints <- c :: fresh.Constraints
            | false, _ -> ()

        substituteWith subst scheme.Body

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

    /// Apply `Defaults` entries on free TyVars whose level exceeds
    /// `outerLevel`. A default fires when its target resolves to a concrete
    /// shape. Iterates to fixpoint — a chained default like
    /// `default ^T3 : ^T1 ; default ^T1 : int` needs two passes.
    ///
    /// Defaults walked here are *consumed*: once a fire happens (or once
    /// all candidates fail), the `Defaults` list is cleared so subsequent
    /// passes don't re-walk dead targets. A TyVar generalised at a use-site
    /// instantiation is re-stamped with fresh defaults on the next call to
    /// its `Instantiate` closure.
    let private applyDefaults (zonkedTy: SemType) (outerLevel: int) : unit =
        let visited = HashSet<TypeVar>(HashIdentity.Reference)

        let rec collect (t: SemType) : ResizeArray<TypeVar> =
            let acc = ResizeArray<TypeVar>()

            let rec go (t: SemType) =
                match t with
                | TyVar tv ->
                    let root = UnionFind.find tv

                    if visited.Add root then
                        if root.Level > outerLevel && root.Link.IsNone && not (List.isEmpty root.Defaults) then
                            acc.Add root

                        match root.Link with
                        | ValueSome target -> go target
                        | ValueNone -> ()
                | TyConst _ -> ()
                | TyFun(a, r) ->
                    go a
                    go r
                | TyTuple xs -> List.iter go xs
                | TyRecord(_, args) -> List.iter go args
                | TyUnion(_, args) -> List.iter go args
                | TyClass(_, args) -> List.iter go args

            go t
            acc

        let candidates = collect zonkedTy

        // ValueNone if every TyVar in the chain is still free.
        let rec resolveTarget (t: SemType) : SemType voption =
            match t with
            | TyVar tv ->
                let root = UnionFind.find tv

                match root.Link with
                | ValueSome target -> resolveTarget target
                | ValueNone -> ValueNone
            | _ -> ValueSome t

        let tryDefault (tv: TypeVar) : bool =
            let mutable fired = false
            let defaults = tv.Defaults

            for target in defaults do
                if not fired then
                    match resolveTarget target with
                    | ValueSome concrete when not (occursAndAdjust tv concrete) ->
                        // Occurs guard: a chain like `default ^T3 : ^T1`
                        // with a structural target (`^T1 list`) could build
                        // a `concrete` transitively containing tv; linking
                        // through would create an infinite type. Skip on
                        // occurs — the default is unsatisfiable.
                        tv.Link <- ValueSome concrete
                        fired <- true
                    | _ -> ()

            // Clear regardless — discharged, or not worth chasing further.
            tv.Defaults <- []
            fired

        // Iterate to fixpoint: each pass may unblock chained defaults.
        let mutable changed = true

        while changed do
            changed <- false

            for tv in candidates do
                if tv.Link.IsNone && not (List.isEmpty tv.Defaults) then
                    if tryDefault tv then
                        changed <- true

    let private generalise (zonkedTy: SemType) (outerLevel: int) : TypeScheme =
        // Apply defaults before quantifying: a default that resolves links
        // its source TyVar, which the quantifier walk then skips. Without
        // this, `let x = 1 + 2` would generalise as `∀'a. 'a` instead of
        // `int` (the unbound `^T3` from external-symbol Instantiate).
        applyDefaults zonkedTy outerLevel

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

        // `instantiate` swaps these onto fresh substitutions per use site
        // so satisfaction is re-evaluated independently.
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
        | Token.CharLiteral -> MockBuiltins.tyChar
        | Token.NumDecimal
        | Token.NumDecimalHex
        | Token.NumDecimalOctal
        | Token.NumDecimalBinary -> MockBuiltins.tyDecimal
        | _ -> MockBuiltins.tyInt

    /// Multi-segment qualified unit names (`Microsoft.FSharp.SI.kg`) and
    /// measure typars (`'u`) are v2 — they produce an empty term plus a
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
        // Unrecognised tokens still type as int (the parser's commonest case)
        // — extend as new literal kinds become reachable.
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
    /// (`float<m>`, `int<kg>`). User-defined `[<Measure>]`-aware types land
    /// when records / DUs do.
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

    /// Reads `ctx.TyparScope` for `'a` typar resolution; callers open a
    /// fresh scope per signature (binding or type defn) before walking.
    /// Bare references to generic named types back-fill the arg list with
    /// fresh TyVars so unification can pin them.
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
                    // Strict (type-defn fill-in): implicit free typars aren't
                    // legal F#. Diagnose, but still mint and memoise so later
                    // occurrences share the TyVar and don't cascade.
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
                    // Implicit typar: mint at the binding's current level so
                    // generalisation at binding-group exit picks it up;
                    // memoise so later occurrences share identity.
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
            | _ when ctx.IntrinsicReprTypes.ContainsKey name ->
                // Primitive binding (`type int = (# "System.Int32" #)`): a
                // nominal intrinsic, NOT a transparent abbreviation. Resolve to
                // `TyConst name`; the representation string is consumed later by
                // the codegen `encodeType` rekey. See docs/self-host-rung1-plan.md.
                TyConst name
            | _ ->
                match ctx.AbbreviationTypes.TryGetValue name with
                | true, info ->
                    // Eager expansion: force the body, then substitute fresh
                    // TyVars for every declared typar.
                    forceFill ctx info
                    let args = [ for _ in info.TypeParams -> TyVar(freshTyVar ctx) ]
                    let diagKey = NodeKey.ofToken li.Idents.[0] NodeKind.TypeNamed
                    expandAbbreviation ctx diagKey info args
                | false, _ ->
                    match ctx.RecordTypes.TryGetValue name with
                    | true, info ->
                        // Back-fill generic args with fresh TyVars at the
                        // current level — unpinned at the declaration site,
                        // fixed by surrounding unification (e.g. `r : Box`
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
            // `float<m>` / `int<kg>` — stamp the measure onto a fresh TyVar
            // whose Link carries the carrier.
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

            resolveNamedGeneric ctx diagKey name translatedArgs
        | Type.SuffixedType(baseType = baseTy; longIdent = li) when li.Idents.Length = 1 ->
            // Postfix generic syntax: `'T list` ≡ `list<'T>`. Multi-arg
            // postfix forms (`(int, string) Map`) parse the base as a tuple
            // and fall to the single-arg arity diagnostic — out of scope for v1.
            let nameTok = li.Idents.[0]
            let name = ctx.NameOf nameTok
            let diagKey = NodeKey.ofToken nameTok NodeKind.TypeGeneric
            resolveNamedGeneric ctx diagKey name [ translateType ctx baseTy ]
        | Type.FunctionType(fromType = from; toType = into) -> TyFun(translateType ctx from, translateType ctx into)
        | Type.TupleType(types = types) -> TyTuple [ for t in types -> translateType ctx t ]
        | Type.WhenConstrainedType(typ = inner; constraints = cs) ->
            let inner = translateType ctx inner
            translateConstraints ctx cs
            inner
        | _ ->
            // Multi-segment named/generic types and other shapes (arrays,
            // anonymous records, etc.) aren't modelled yet. Hand back a free
            // TyVar so unification can pin it via context.
            TyVar(freshTyVar ctx)

    /// Resolve a single-segment generic type reference against the type
    /// registries, in the same precedence the bare-name arm uses: intrinsic
    /// binding → transparent abbreviation → record → union → class → opaque
    /// `TyConst`. An arity mismatch diagnoses but still produces a
    /// best-effort shape.
    and private resolveNamedGeneric
        (ctx: PassContext)
        (diagKey: NodeKey)
        (name: string)
        (translatedArgs: SemType list)
        : SemType =
        let argCount = List.length translatedArgs

        let diagnoseArity (expected: int) : unit =
            ctx.Diagnostics.Add
                {
                    Key = diagKey
                    Message = sprintf "Type '%s' expects %d type argument(s) but got %d" name expected argCount
                    Severity = Error
                }

        let checkArity (expected: int) : unit =
            if expected <> argCount then
                diagnoseArity expected

        if ctx.IntrinsicReprTypes.ContainsKey name then
            // Generic primitive binding: nominal, not transparent.
            TyConst name
        else
            match ctx.AbbreviationTypes.TryGetValue name with
            | true, info ->
                forceFill ctx info
                checkArity (List.length info.TypeParams)
                expandAbbreviation ctx diagKey info translatedArgs
            | false, _ ->
                match ctx.RecordTypes.TryGetValue name with
                | true, info ->
                    checkArity (List.length info.TypeParams)
                    TyRecord(name, translatedArgs)
                | false, _ ->
                    match ctx.UnionTypes.TryGetValue name with
                    | true, info ->
                        checkArity (List.length info.TypeParams)
                        TyUnion(name, translatedArgs)
                    | false, _ ->
                        match ctx.ClassTypes.TryGetValue name with
                        | true, info ->
                            checkArity (List.length info.TypeParams)
                            TyClass(name, translatedArgs)
                        | false, _ ->
                            // Unknown name with type args — opaque TyConst,
                            // args ignored (matches the bare-name arm).
                            TyConst name

    /// Attach to the constrained typar's TyVar through the current
    /// `ctx.TyparScope`. Unsupported kinds (Coercion, MemberTrait, etc.) are
    /// skipped — they belong to their own resolution phases.
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
            // v1 skips these — each has its own resolution phase (SRTPs /
            // IWSAMs / attribute pass). Silent skip, not a diagnostic.
            ()

    /// The scope must already contain the constrained typars — callers
    /// (binding-level, type-defn fill-in, inline `WhenConstrainedType`)
    /// seed it first.
    and private translateConstraints (ctx: PassContext) (tcs: TyparConstraints<SyntaxToken>) : unit =
        let (TyparConstraints(constraints = cs)) = tcs

        for c in cs do
            translateConstraint ctx c

    /// Idempotent — already-`Filled` entries short-circuit. Re-entry through
    /// a recursive abbreviation reference detects the cycle (`InProgress`),
    /// emits a diagnostic, and freezes `Status` to `Filled` without setting
    /// `Body`. The outer call notices `Status` flipped mid-walk and skips
    /// assigning `Body`, leaving `ValueNone` so the expansion arm
    /// substitutes a fresh TyVar per use site instead of a stale one.
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

    /// Returns a fresh TyVar if `Body = ValueNone` (cycle detected, or
    /// fill-in not yet run) — best-effort rather than cascading.
    ///
    /// Constraints on the prototype typars are evaluated against the supplied
    /// args here: unlike records / unions, an abbreviation has no
    /// fresh-instance step that would let `drainConstraints` fire on its own.
    /// A Defer outcome propagates the constraint to any free TyVar inside the
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

    /// Reads `Units` straight off the root — does NOT use `resolveStep`,
    /// which would follow a measured TyVar through its `Link` to the bare
    /// carrier and drop the measure.
    let private unitsOf (t: SemType) : MeasureTerm voption =
        match t with
        | TyVar tv -> (UnionFind.find tv).Units
        | _ -> ValueNone

    /// Underlying numeric carrier of a (possibly measure-wrapped) type. A
    /// free variable (no Link) is returned as-is so a later unification can
    /// pin it.
    let private carrierOf (t: SemType) : SemType =
        match resolveStep t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome link -> link
            | ValueNone -> TyVar root
        | other -> other

    /// Fresh TyVar pre-stamped with a carrier link and (optionally) a measure.
    let private freshTyVarWith (ctx: PassContext) (carrier: SemType) (units: MeasureTerm voption) : TypeVar =
        let tv = freshTyVar ctx
        tv.Link <- ValueSome carrier
        tv.Units <- units
        tv

    let private isComparisonOp (name: string) : bool =
        match name with
        | "op_Equality"
        | "op_Inequality"
        | "op_LessThan"
        | "op_GreaterThan"
        | "op_LessThanOrEqual"
        | "op_GreaterThanOrEqual" -> true
        | _ -> false

    /// Fires before the provider lookup in `inferInfix` so measured
    /// arithmetic / comparison operators get measure-correct result types and
    /// a dedicated "Measure mismatch" diagnostic rather than a generic
    /// carrier-type mismatch. Returns `None` for the all-dimensionless case
    /// (or operators we don't dispatch); the caller falls through to the
    /// provider path.
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

    /// v1 only supports single-segment (`X`) and two-segment qualified
    /// (`R.X`) forms. Multi-segment qualifiers (`A.B.X`) fall through as
    /// ValueNone for the qualifier and the last segment for the field name.
    let private fieldNameAndQualifier (ctx: PassContext) (li: LongIdent<SyntaxToken>) : string voption * string =
        let idents = li.Idents
        let last = ctx.NameOf idents.[idents.Length - 1]

        if idents.Length = 1 then
            ValueNone, last
        elif idents.Length = 2 then
            ValueSome(ctx.NameOf idents.[0]), last
        else
            ValueNone, last

    /// Returns the fresh args together with the substitution mapping each
    /// prototype `TypeVar` onto its fresh stand-in — callers walk declared
    /// field / case-arg types through this subst so every reference to `'a`
    /// lines up with the value in `args`.
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
                    // Copy prototype constraints onto the fresh instance so
                    // every use site re-evaluates satisfaction independently
                    // (a `Set<int>` and a `Set<int -> int>` each get their own
                    // copy of `'a : comparison`).
                    fresh.Constraints <- protoRoot.Constraints
                    let asTy = TyVar fresh
                    subst.[protoRoot] <- asTy
                    asTy
            ]

        args, subst

    /// Function value whose argument shape matches the primary constructor
    /// and whose result is the constructed `TyClass`. Routes bare
    /// `Point(3, 4)` calls (no `new`) through the function-application
    /// machinery. `ValueNone` if `name` isn't in `ctx.ClassTypes`.
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

    /// Function-shaped type for a DU ctor reference. Multi-field cases bundle
    /// the fields into a tuple — F# DUs take a tuple as their single argument.
    /// The receiver union's typars are instantiated fresh so two independent
    /// uses of `Some` don't share a `'a`.
    let private ctorType (ctx: PassContext) (info: UnionCaseInfo) : SemType =
        let unionInfo = ctx.UnionTypes.[info.UnionName]
        let args, subst = freshNamedInstance ctx unionInfo.TypeParams
        let unionTy = TyUnion(info.UnionName, args)

        let walkedFields = info.Fields |> Array.map (substituteWith subst)

        match walkedFields.Length with
        | 0 -> unionTy
        | 1 -> TyFun(walkedFields.[0], unionTy)
        | _ -> TyFun(TyTuple(List.ofArray walkedFields), unionTy)

    /// ValueNone with `count = 0` means "no such ctor"; `count >= 2` means
    /// ambiguous — the caller emits the appropriate diagnostic.
    let private resolveCtorName (ctx: PassContext) (name: string) : UnionCaseInfo voption * int =
        match ctx.CtorIndex.TryGetValue name with
        | false, _ -> ValueNone, 0
        | true, [ info ] -> ValueSome info, 1
        | true, infos -> ValueNone, List.length infos

    /// Resolve a qualified ctor reference `Type.Case` against the union
    /// registry.
    let private resolveQualifiedCtor (ctx: PassContext) (typeName: string) (caseName: string) : UnionCaseInfo voption =
        match ctx.UnionTypes.TryGetValue typeName with
        | false, _ -> ValueNone
        | true, info ->
            match info.Cases |> Array.tryFind (fun c -> c.Name = caseName) with
            | Some c -> ValueSome c
            | None -> ValueNone

    /// Unique record type whose declared field set equals `names`
    /// (order-insensitive). Returns (info, candidateCount); candidateCount
    /// disambiguates the "no match" vs "ambiguous" diagnostic paths.
    let private findUniqueRecordByFieldSet (ctx: PassContext) (names: string list) : RecordTypeInfo voption * int =
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

    /// `Circle(r)` parses as `Circle (EnclosedBlock r)`; `Rectangle(w, h)`
    /// as `Circle (EnclosedBlock (Tuple [w; h]))`. v1 supports the
    /// tuple-argument form and a bare single arg — both are what the F# DU
    /// ctor application convention emits.
    let private unwrapCtorArgPattern (p: Pat<SyntaxToken>) : Pat<SyntaxToken> list =
        match p with
        | Pat.EnclosedBlock(pat = Pat.Tuple(patterns = pats)) -> List.ofSeq pats
        | Pat.EnclosedBlock(pat = inner) -> [ inner ]
        | Pat.Tuple(patterns = pats) -> List.ofSeq pats
        | _ -> [ p ]

    let rec private inferPat (ctx: PassContext) (p: Pat<SyntaxToken>) : SemType =
        // Each pattern node gets its own TypeVar keyed on its NodeKey; for
        // compound patterns the outer TypeVar is linked to the underlying
        // shape so a lookup against any pattern node returns the right type.
        let key = CstKeys.ofPat p

        match p with
        | Pat.NamedSimple t when
            let n = ctx.NameOf t
            n.Length > 0 && System.Char.IsUpper n.[0] && ctx.CtorIndex.ContainsKey n
            ->
            // Uppercase-leading bare ident matching a known ctor —
            // reinterpret as a nullary ctor pattern. Multi-candidate names
            // require a qualifier; diagnose ambiguity, best-effort otherwise.
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
                // The parser wraps multi-arg ctor patterns in
                // `EnclosedBlock(Tuple [...])`; flatten to the field list.
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
            // Validation checks the name set; here we only unify the
            // patterns' overall types for scrutinee consistency.
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
                // Walk sub-patterns so binders register as free TyVars.
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

    /// Reuses the lexer's canonical placeholder parser
    /// (`Lexing.parseFormatSpecifierView`) so no second copy of the format
    /// grammar lives here. `ValueNone` when the string carries interpolation
    /// holes or lexer-error parts (not a simple format literal), so the
    /// printf special-case falls through to standard inference.
    let private formatSpecifiers (ctx: PassContext) (e: Expr<SyntaxToken>) : FormatType list voption =
        match e with
        | Expr.String(parts = parts) ->
            let acc = ResizeArray<FormatType>()
            let mutable ok = true

            for part in parts do
                match part with
                | StringPart.Text _
                | StringPart.EscapeSequence _
                | StringPart.EscapePercent _
                | StringPart.VerbatimEscapeQuote _ -> ()
                | StringPart.FormatSpecifier t ->
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf t) with
                    | ValueSome placeholder -> acc.Add placeholder.Type
                    | ValueNone -> ok <- false
                | StringPart.Expr _
                | StringPart.OrphanFormatSpecifier _
                | StringPart.InvalidText _ -> ok <- false

            if ok then ValueSome(List.ofSeq acc) else ValueNone
        | _ -> ValueNone

    /// Whether every specifier is one the happy path lowers inline
    /// (`PrintfSpec.tryHoleFormat`); a `false` keeps the FSharp.Core cold
    /// path. `%%` escapes are lowerable (P2): Freeze collapses `%%`→`%` in the
    /// literal segment. Only interpolation holes (`Expr`), orphan specifiers
    /// and lexer-error parts force the cold path.
    let private lowerablePlaceholders (ctx: PassContext) (e: Expr<SyntaxToken>) : bool =
        match e with
        | Expr.String(parts = parts) ->
            let mutable ok = true

            for part in parts do
                match part with
                | StringPart.FormatSpecifier t ->
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf t) with
                    | ValueSome p ->
                        match PrintfSpec.tryHoleFormat p with
                        | ValueSome _ -> ()
                        | ValueNone -> ok <- false
                    | ValueNone -> ok <- false
                // A `%%` escape arrives as raw `Text` "%%" — still lowerable.
                | StringPart.Text _
                | StringPart.EscapeSequence _
                | StringPart.VerbatimEscapeQuote _
                | StringPart.EscapePercent _ -> ()
                | StringPart.Expr _
                | StringPart.OrphanFormatSpecifier _
                | StringPart.InvalidText _ -> ok <- false

            ok
        | _ -> false

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
            | Expr.EnclosedBlock(lParen = ParenKind.List _; expr = inner; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRBracket "]"
                inferListLikeLiteral ctx key inner false
            | Expr.EnclosedBlock(lParen = ParenKind.Array _; expr = inner; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRArrayBracket "|]"
                inferListLikeLiteral ctx key inner true
            | Expr.EnclosedBlock(expr = inner) -> infer ctx inner
            | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
                inferIfThenElse ctx key cond thenE elifs elseB
            | Expr.Tuple(exprs = items) -> inferTuple ctx items
            | Expr.Sequential(exprs = items) -> inferSequential ctx key items
            | Expr.TypeAnnotation(expr = inner; typ = t) -> inferTypeAnnotation ctx key inner t
            | Expr.EmptyBlock(lParen = ParenKind.List _; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRBracket "]"
                emptyListLikeLiteral ctx key false
            | Expr.EmptyBlock(lParen = ParenKind.Array _; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRArrayBracket "|]"
                emptyListLikeLiteral ctx key true
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
                // No reference-type bound yet — free TypeVar so surrounding
                // context can pin it.
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
        // A multi-segment LongIdent whose head is a local binding is a
        // record-field access chain (`r.X.Y`), not a qualified name — the
        // parser rides these inside a single `Expr.LongIdentOrOp` rather
        // than emitting `Expr.DotLookup`.
        match e with
        // `(+)` used as a value: resolve the operator's compiled name through
        // the provider, instantiating its scheme like any external symbol.
        // Freeze projects this to `External("op_Addition", …)`.
        | Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(opName = OpName.SymbolicOp op))) ->
            match Desugar.symbolicOpCompiledName op.Token with
            | ValueSome name ->
                match ctx.Provider.TryLookup name with
                | ValueSome sym -> sym.Instantiate ctx.CurrentLevel
                | ValueNone ->
                    ctx.Diagnostics.Add
                        {
                            Key = key
                            Message = sprintf "Operator '%s' is not available from the symbol provider" name
                            Severity = Error
                        }

                    TyVar(freshTyVar ctx)
            | ValueNone -> TyVar(freshTyVar ctx)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length > 1
            && ctx.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
            ->
            inferLongIdentFieldChain ctx key li
        // Qualified static member: `Math.Pi`, `Box.Empty`. Typars are
        // instantiated fresh per use site so two `Box.Empty ()` calls don't
        // share a `'a`.
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

            let _, subst = freshNamedInstance ctx info.TypeParams
            substituteWith subst m.Type
        // Qualified static member on a *union*: `Lst.Empty` (P3d.3). Checked
        // before the ctor arm so a static member shadows the not-a-case
        // diagnostic; a name that is a case (not a static member) fails this
        // guard and falls through to the ctor arm.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length = 2
            && not (ctx.Binding.ContainsKey key)
            && (
                match ctx.UnionTypes.TryGetValue(ctx.NameOf li.Idents.[0]) with
                | true, info ->
                    let n = ctx.NameOf li.Idents.[1]
                    info.Members |> Array.exists (fun m -> m.IsStatic && m.Name = n)
                | false, _ -> false
            )
            ->
            let unionName = ctx.NameOf li.Idents.[0]
            let memberName = ctx.NameOf li.Idents.[1]
            let info = ctx.UnionTypes.[unionName]

            let m = info.Members |> Array.find (fun m -> m.IsStatic && m.Name = memberName)

            let _, subst = freshNamedInstance ctx info.TypeParams
            substituteWith subst m.Type
        // Qualified ctor reference `Result2.Ok` — via the union registry,
        // bypassing the CtorIndex ambiguity check.
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
                // Already-generalised binding: instantiate its scheme for
                // independent use-sites. Otherwise the monomorphic TyVar from
                // inferPat — including uses inside a sibling's RHS in the same
                // `let rec` group, which is what forbids polymorphic recursion.
                match ctx.Scheme.TryGetValue rb.BindingSite with
                | ValueSome scheme -> instantiate ctx scheme
                | ValueNone -> TyVar(tvOf ctx rb.BindingSite)
            | ValueNone ->
                // Provider first — provider hits beat ctor-name resolution
                // when both exist (a let-bound `Ok` would have a Binding entry
                // and never reach here). Bare single-segment idents absent
                // from the provider fall to the ctor registry.
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
                            // Class-name-as-function: `Point(3, 4)` parses as
                            // `Expr.App (Expr.Ident "Point", ...)`. Return the
                            // ctor as a function value so `inferApp` types the
                            // call through the normal function arm.
                            classCtorAsFunction ctx n
                    | ValueNone -> TyVar(freshTyVar ctx)

    /// Joins multi-segment names with `.` so the provider can look up dotted
    /// names like `Math.PI` directly.
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
        match tryInferPrintfApp ctx key fn args with
        | ValueSome ty -> ty
        | ValueNone ->
            let mutable currTy = infer ctx fn

            for a in args do
                let argTy = infer ctx a
                let resultTy = TyVar(freshTyVar ctx)
                unify ctx key currTy (TyFun(argTy, resultTy))
                currTy <- resultTy

            currTy

    /// Printf-family typing rule (front-end-gaps-plan §B). When `fn` is a
    /// recognised printf entry point (not shadowed by a local binding) with a
    /// plain-literal format argument, the format spec — not the literal's
    /// apparent `string` type — drives the call's curried result type. The
    /// format argument types as `PrintfFormat<printer, …>`. Non-literal
    /// format strings return `ValueNone` and fall through to standard inference.
    and private tryInferPrintfApp
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : SemType voption =
        let fnKey = CstKeys.ofExpr fn

        // A local binding shadowing a printf name is an ordinary function —
        // don't apply the special rule.
        if ctx.Binding.ContainsKey fnKey then
            ValueNone
        else
            match fn with
            | Expr.Ident _
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) ->
                match PrintfSpec.tryFamily (qualifiedNameOf ctx fn) with
                | ValueNone -> ValueNone
                | ValueSome fam ->
                    let idx = fam.FormatArgIndex

                    if args.Length <= idx then
                        // Format argument not supplied (e.g. partially-applied
                        // `fprintf writer`); defer to standard inference.
                        ValueNone
                    else
                        match formatSpecifiers ctx args.[idx] with
                        | ValueNone -> ValueNone
                        | ValueSome specs ->
                            let fresh () = TyVar(freshTyVar ctx)

                            match PrintfSpec.appliedTypeOf fresh specs fam with
                            // A specifier we don't type in v1 (`%a` / `%t`);
                            // defer to standard inference.
                            | ValueNone -> ValueNone
                            | ValueSome(fnTy, fmtTy, _) ->
                                // Stamp the function node so Freeze threads the
                                // curried result type through the App chain.
                                (freshTv ctx fnKey).Link <- ValueSome fnTy

                                let mutable currTy = fnTy

                                for i in 0 .. args.Length - 1 do
                                    let a = args.[i]

                                    let argTy =
                                        if i = idx then
                                            // The format literal types as the
                                            // PrintfFormat — not as `string`.
                                            (freshTv ctx (CstKeys.ofExpr a)).Link <- ValueSome fmtTy
                                            fmtTy
                                        else
                                            infer ctx a

                                    let resultTy = TyVar(freshTyVar ctx)
                                    unify ctx key currTy (TyFun(argTy, resultTy))
                                    currTy <- resultTy

                                // P1 happy-path lowering marker: fully-applied
                                // literal call, a StdOut/StdErr/StringResult
                                // sink, and every specifier in `tryHoleFormat`
                                // → Freeze mints a `TExpr.Format`. Otherwise the
                                // FSharp.Core path stands (additive — `%A`,
                                // partial application, etc. unaffected).
                                match PrintfSpec.sinkOf (qualifiedNameOf ctx fn) with
                                | ValueSome sink when
                                    idx = 0
                                    && args.Length = specs.Length + 1
                                    && lowerablePlaceholders ctx args.[idx]
                                    ->
                                    ctx.PrintfApp.Set(key, sink)
                                | _ -> ()

                                ValueSome currTy
            | _ -> ValueNone

    and private inferHighPrecApp
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (arg: Expr<SyntaxToken>)
        : SemType =
        // `f(x)` — same shape as `Expr.App fn [|arg|]`, a separate CST case.
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
        // Tiny subset: endpoints (and step) constrained to int, result the
        // `seq<int>` placeholder. Real F# is generic over the `..` overload.
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
        | ValueSome _
        | ValueNone ->
            // Desugar didn't recognise the operator (non-OpName can't happen
            // for an InfixApp key) — leave the result free.
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
        | ValueSome _
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
            // `if c then e` (no else) requires e : unit — not yet supported.
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
        // All but the last must be unit; result is the last's type.
        if items.Length = 0 then
            MockBuiltins.tyUnit
        else
            for i = 0 to items.Length - 2 do
                let ty = infer ctx items.[i]
                unify ctx key ty MockBuiltins.tyUnit

            infer ctx items.[items.Length - 1]

    /// `pEnclosed` virtual-inserts the expected close token (with a
    /// parser-side diagnostic) when the source token is missing or
    /// mismatched. That parser diagnostic isn't visible to semantic-analysis
    /// consumers, so surface the breakage on `ctx.Diagnostics` too —
    /// otherwise the malformed literal types successfully and Freeze emits a
    /// well-shaped TAST as if the source were correct.
    and private checkLiteralClose
        (ctx: PassContext)
        (key: NodeKey)
        (rTok: SyntaxToken)
        (expected: Token)
        (display: string)
        : unit =
        match rTok.Index with
        | TokenIndex.Virtual ->
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = sprintf "Mismatched or missing closing delimiter: expected '%s'" display
                    Severity = Error
                }
        | TokenIndex.Regular _ when rTok.Token <> expected ->
            // Defensive: pEnclosed only emits a real rParen when the peeked
            // token matched, so this can't trigger today — guards against a
            // future parser change letting a mismatched close-token through.
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = sprintf "Mismatched closing delimiter: expected '%s'" display
                    Severity = Error
                }
        | TokenIndex.Regular _ -> ()

    /// The list type a `[…]` literal carries. A program-declared `'T list`
    /// abbreviation (the self-host list shape — `List.fs`'s
    /// `and 'T list = List<'T>`) retargets list literals to its RHS union;
    /// absent it, FSharp.Core's `Microsoft.FSharp.Collections.list` nominal
    /// is the default. Additive — a normal program never declares a `list`
    /// abbreviation, so the shape is byte-identical to before.
    and private listLiteralTy (ctx: PassContext) (key: NodeKey) (elemTy: SemType) : SemType =
        match ctx.AbbreviationTypes.TryGetValue "list" with
        | true, info ->
            forceFill ctx info
            expandAbbreviation ctx key info [ elemTy ]
        | false, _ -> TyRecord("Microsoft.FSharp.Collections.list", [ elemTy ])

    and private inferListLikeLiteral
        (ctx: PassContext)
        (key: NodeKey)
        (body: Expr<SyntaxToken>)
        (isArray: bool)
        : SemType =
        let elemTy = TyVar(freshTyVar ctx)

        let items =
            match body with
            | Expr.Sequential(exprs = items) -> items
            | single -> ImmutableArray.Create(single)

        for i = 0 to items.Length - 1 do
            let itemTy = infer ctx items.[i]
            unify ctx key itemTy elemTy

        if isArray then
            TyRecord("Microsoft.FSharp.Core.[]", [ elemTy ])
        else
            listLiteralTy ctx key elemTy

    /// Element type stays free so context can pin it (`let xs : int list = []`).
    and private emptyListLikeLiteral (ctx: PassContext) (key: NodeKey) (isArray: bool) : SemType =
        let elemTy = TyVar(freshTyVar ctx)

        if isArray then
            TyRecord("Microsoft.FSharp.Core.[]", [ elemTy ])
        else
            listLiteralTy ctx key elemTy

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
        // Int-range source: element type is int. No `seq<T>` machinery yet
        // for anything else — pattern stays unconstrained, Info flags the gap.
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
        // `function … ` ~ `fun x -> match x with …`. The synthesised
        // parameter's TypeVar IS the scrutinee's — every arm's pattern
        // unifies with it.
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
        // Until a real `exn` type lands, pin the scrutinee to placeholder
        // `TyConst "exn"`. A fresh TyVar would let wildcard / variable arm
        // patterns carry an unresolved TyVar into the TAST, which
        // `ResolvedTypes` correctly flags.
        let resultTy = infer ctx body
        let exnTy = TyConst "exn"
        inferRules ctx key exnTy resultTy rules
        resultTy

    and private inferTryFinally
        (ctx: PassContext)
        (key: NodeKey)
        (body: Expr<SyntaxToken>)
        (finallyE: Expr<SyntaxToken>)
        : SemType =
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
        // Mutability of the LHS is a Validation concern; here we only typecheck.
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
            // Fresh typars per literal so independent literals get independent
            // vars; each initialiser unifies against the field type *under this
            // substitution*, pinning a `'a` field to the initialiser's type.
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
                // Clone preserves the source's arg list — overrides unify
                // against the substituted field type (`'a` → source's arg).
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

    /// One step of dot-access resolution. Deferred when the receiver is a
    /// free TyVar. For a generic receiver `(b : Box<int>).Value`, the
    /// declared field / member type `'a` is substituted against the
    /// receiver's arg list so `Value` types as `int`, not a free typar.
    /// Record-field, class- and union-member access all route through here;
    /// the receiver's shape discriminates.
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
                    // Distinguish "no such member" from "member is static —
                    // access via class name, not an instance".
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
        | TyUnion(unionName, args) ->
            // Union instance member access (P3d.3) — mirrors the `TyClass`
            // arm against the union's augmentation members.
            match ctx.UnionTypes.TryGetValue unionName with
            | true, info ->
                match info.Members |> Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic) with
                | Some m ->
                    let subst = mkNamedTypeSubst info.TypeParams args
                    substituteWith subst m.Type
                | None ->
                    let isStaticHit =
                        info.Members |> Array.exists (fun m -> m.Name = memberName && m.IsStatic)

                    let msg =
                        if isStaticHit then
                            sprintf
                                "Member '%s' on type '%s' is static; access it via '%s.%s'"
                                memberName
                                unionName
                                unionName
                                memberName
                        else
                            sprintf "Type '%s' has no instance member '%s'" unionName memberName

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
                        Message = sprintf "Unknown union type '%s'" unionName
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

    /// `new T(args)`. Mirrors a single application against the value returned
    /// by `classCtorAsFunction` — kept inline so a bare `Expr.New` doesn't
    /// need to fabricate an `Expr.App` first.
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

    /// `r.X.Y…` parsed as a single multi-segment `Expr.LongIdentOrOp`. The
    /// head segment was resolved by NameResolution as a local binding — type
    /// it through `ctx.Binding`/`ctx.Scheme`, then walk the remaining
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
        // Interpolated strings are `string` too — Freeze lowers them to a
        // `TExpr.Format` (D9). Recurse into every hole expr so its type is
        // computed (Freeze reads it back to emit `AppendFormatted<T>`); a
        // `%d{x}` specifier additionally constrains the hole.
        for part in parts do
            match part with
            | StringPart.Expr(formatSpecifier = fs; expr = e) ->
                let holeTy = infer ctx e

                match fs with
                | ValueSome ft ->
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf ft) with
                    | ValueSome p ->
                        match PrintfSpec.argType (fun () -> TyVar(freshTyVar ctx)) p.Type with
                        | ValueSome t -> unify ctx (CstKeys.ofExpr e) holeTy t
                        | ValueNone -> ()
                    | ValueNone -> ()
                | ValueNone -> ()
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
            // `body = ValueNone` is `use fixed` — pinning unsupported; fail
            // loudly rather than hand Freeze a best-effort type.
            failwith "Unification: Expr.LetOrUse with no body (UseFixed) not supported"

    and private inferBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        // One typar scope per binding signature: explicit `<'a>` typars seed
        // it first so later implicit `'a` mentions share the same TyVar.
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
                            let annTy = translateType ctx t
                            unify ctx (CstKeys.ofBinding b) bodyTy annTy
                            annTy
                        | ValueNone -> bodyTy

                    List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

            unify ctx (CstKeys.ofBinding b) patTy rhsTy
        finally
            ctx.TyparScope <- savedScope

    /// Type a `let` / `let rec` group with Rémy-level discipline. Key
    /// subtlety: pre-allocate single-name sibling headPat TyVars (step 2) so
    /// forward references from inside one RHS (or a nested let) find the
    /// sibling's TyVar at this group's level rather than lazy-minting at a
    /// deeper one — which would let a nested let generalise a var that
    /// actually belongs to an un-typed outer sibling. RHSes type at the
    /// pushed level (sibling lookups stay monomorphic — no scheme written
    /// yet); generalisation happens against the outer level after popping.
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

    /// Rebuild a type definition's typar scope from the registry entry's
    /// `TypeParams`, so a field type containing `'name` resolves to the same
    /// root the registry already holds.
    let private scopeOfTypeParams (typeParams: (string * TypeVar) list) : Dictionary<string, TypeVar> =
        let d = Dictionary<string, TypeVar>(System.StringComparer.Ordinal)

        for (n, tv) in typeParams do
            if not (d.ContainsKey n) then
                d.[n] <- tv

        d

    /// Link each placeholder field TyVar (stamped by NameResolution) to its
    /// real translated CST type. Done as a pre-pass so a record's field type
    /// can reference another record declared elsewhere in the same file —
    /// every record name is already in `ctx.RecordTypes` by now.
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

    /// Same shape as `fillRecordFieldTypes` for union case fields — runs
    /// after every record/union is in the registry so a case's field type can
    /// name another DU declared elsewhere in the same file.
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

                            // A case is registered iff its head names a case —
                            // mirror `NameResolution.unionCaseName`'s accept
                            // set so this CST walk stays index-aligned with the
                            // registry's `Cases` array (named cases only).
                            let headNames (head: IdentOrOp<SyntaxToken>) =
                                match head with
                                | IdentOrOp.Ident _
                                | IdentOrOp.ParenOp(opName = OpName.NilOp _)
                                | IdentOrOp.ParenOp(opName = OpName.SymbolicOp _) -> true
                                | _ -> false

                            let caseHeadFields data =
                                match data with
                                | UnionTypeCaseData.Nullary(name = h) -> struct (headNames h, [])
                                | UnionTypeCaseData.GadtNullary(name = h) -> struct (headNames h, [])
                                | UnionTypeCaseData.Nary(name = h; fields = fs) ->
                                    let tys =
                                        [
                                            for f in fs ->
                                                match f with
                                                | UnionTypeField.Unnamed(typ = t) -> t
                                                | UnionTypeField.Named(typ = t) -> t
                                        ]

                                    struct (headNames h, tys)
                                | UnionTypeCaseData.GadtNary(
                                    name = h; sign = UncurriedSig(args = ArgsSpec(args = specs))) ->
                                    let tys = [ for ArgSpec(typ = t) in specs -> t ]
                                    struct (headNames h, tys)

                            let mutable infoIdx = 0

                            for UnionTypeCase(data = data) in cases do
                                let struct (isRegistered, fieldTypes) = caseHeadFields data

                                if isRegistered && infoIdx < info.Cases.Length then
                                    let caseInfo = info.Cases.[infoIdx]
                                    infoIdx <- infoIdx + 1

                                    let fieldTypes = List.toArray fieldTypes
                                    let n = min caseInfo.Fields.Length fieldTypes.Length

                                    for i = 0 to n - 1 do
                                        let translated = translateType ctx fieldTypes.[i]

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

    /// Link each ctor-param placeholder TyVar to its declared type.
    /// Un-annotated arguments leave the placeholder free so a use site can
    /// pin it via argument-type unification in `inferNew` / `inferApp`.
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

    /// Fold a curried member signature into a `TyFun` chain (a multi-arg
    /// group `a * b` is a tuple parameter), under the caller's typar scope.
    /// Used to fill abstract member signatures, which have no body to infer.
    let private curriedSigToSemType (ctx: PassContext) (CurriedSig(args = args; returnType = ret)) : SemType =
        let groupTy (ArgsSpec(args = specs)) =
            match List.ofSeq specs with
            | [ ArgSpec(typ = t) ] -> translateType ctx t
            | many -> TyTuple [ for ArgSpec(typ = t) in many -> translateType ctx t ]

        let retTy = translateType ctx ret
        List.foldBack (fun struct (g, _arrow) acc -> TyFun(groupTy g, acc)) (List.ofSeq args) retTy

    /// Walk every class member body under a typar scope seeded from
    /// `info.TypeParams` plus a binding scope supplying `this` and each ctor
    /// param. Placeholder member TyVars are pre-populated into `ctx.TypeVar`
    /// so `inferBinding`'s `tvOf` reuses them and its final
    /// `unify patTy rhsTy` links the placeholder to the inferred member type.
    /// AutoProperty has no `Binding`, so its placeholder is linked manually.
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
                            // Under the class's typar scope so `'a` resolves
                            // to the registry's prototype typar.
                            fillClassCtorParamTypes ctx info pc

                            // Seed `ctx.TypeVar` so `inferIdent` lookups
                            // against the param binding sites return these.
                            for p in info.CtorParams do
                                match p.Type with
                                | TyVar tv -> ctx.TypeVar.Set(p.DeclKey, tv)
                                | _ -> ()

                            // `this`: fresh TyVar pre-linked to `TyClass` over
                            // the class's prototype typars, so a generic member
                            // body mentioning `'a` shares identity with them.
                            let thisTv = TypeVar()
                            thisTv.Level <- ctx.CurrentLevel

                            let selfArgs = [ for (_, ptv) in info.TypeParams -> TyVar ptv ]

                            thisTv.Link <- ValueSome(TyClass(info.Name, selfArgs))
                            ctx.TypeVar.Set(info.ThisKey, thisTv)

                            // Static member bodies never see `this` / ctor
                            // params (NameResolution gives them an empty
                            // binding scope); IsStatic discriminates downstream.
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
                                    | MethodOrPropDefn.AbstractSignature(MemberSig.MethodOrPropSig(
                                        ident = idOrOp; sign = csig)) ->
                                        // No body to infer — translate the
                                        // signature directly and link the placeholder.
                                        let mTokOpt =
                                            match idOrOp with
                                            | IdentOrOp.Ident t -> ValueSome t
                                            | IdentOrOp.ParenOp(opName = OpName.SymbolicOp op) -> ValueSome op
                                            | _ -> ValueNone

                                        match mTokOpt with
                                        | ValueSome mTok ->
                                            let mKey = NodeKey.ofToken mTok NodeKind.PatIdent

                                            match info.Members |> Array.tryFind (fun mm -> mm.DeclKey = mKey) with
                                            | Some mInfo ->
                                                match mInfo.Type with
                                                | TyVar tv ->
                                                    let root = UnionFind.find tv

                                                    // Extend the scope with the method's own
                                                    // `<'C, …>` typars so they resolve to their
                                                    // prototype TyVars (not diagnosed as free).
                                                    let savedMScope = ctx.TyparScope

                                                    if not (List.isEmpty mInfo.MethodTypeParams) then
                                                        let extended =
                                                            Dictionary<string, TypeVar>(
                                                                savedMScope,
                                                                System.StringComparer.Ordinal
                                                            )

                                                        for (n, ptv) in mInfo.MethodTypeParams do
                                                            extended.[n] <- ptv

                                                        ctx.TyparScope <- extended

                                                    try
                                                        root.Link <- ValueSome(curriedSigToSemType ctx csig)
                                                    finally
                                                        ctx.TyparScope <- savedMScope
                                                | _ -> ()
                                            | None -> ()
                                        | ValueNone -> ()
                                    | _ -> ()
                                | _ -> ()
                        finally
                            ctx.TyparScope <- savedScope
                            ctx.TyparScopeStrict <- savedStrict
                    | false, _ -> ()
                | ValueNone -> ()
        | _ -> ()

    /// Union augmentation member bodies (P3d.3). Mirrors `fillClassMembers`
    /// but reads `extensions.elements` and binds `this` to a `TyUnion` over
    /// the union's prototype typars (a v1 union has no primary-ctor params).
    let private fillUnionMembers (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match td with
                | TypeDefn.Union(
                    typeName = TypeName(ident = nameLi); extensions = ValueSome(TypeExtensionElements(elements = elems))) when
                    nameLi.Idents.Length = 1
                    ->
                    let name = ctx.NameOf nameLi.Idents.[0]

                    match ctx.UnionTypes.TryGetValue name with
                    | true, info when not (Array.isEmpty info.Members) ->
                        let savedScope = ctx.TyparScope
                        let savedStrict = ctx.TyparScopeStrict
                        ctx.TyparScope <- scopeOfTypeParams info.TypeParams
                        ctx.TyparScopeStrict <- true

                        try
                            // `this` pre-linked to `TyUnion` over the union's
                            // prototype typars (shared identity for generic bodies).
                            let thisTv = TypeVar()
                            thisTv.Level <- ctx.CurrentLevel
                            let selfArgs = [ for (_, ptv) in info.TypeParams -> TyVar ptv ]
                            thisTv.Link <- ValueSome(TyUnion(info.Name, selfArgs))
                            ctx.TypeVar.Set(info.ThisKey, thisTv)

                            for el in elems do
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

                                            match info.Members |> Array.tryFind (fun mm -> mm.DeclKey = mKey) with
                                            | Some mInfo ->
                                                match mInfo.Type with
                                                | TyVar tv -> ctx.TypeVar.Set(mKey, tv)
                                                | _ -> ()
                                            | None -> ()

                                            enterLevel ctx

                                            try
                                                inferBinding ctx b
                                            finally
                                                exitLevel ctx
                                        | ValueNone -> ()
                                    | MethodOrPropDefn.AutoProperty(ident = id; expr = e; returnType = rt) ->
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

                                            match info.Members |> Array.tryFind (fun mm -> mm.DeclKey = mKey) with
                                            | Some mInfo ->
                                                match mInfo.Type with
                                                | TyVar tv -> (UnionFind.find tv).Link <- ValueSome resultTy
                                                | _ -> ()
                                            | None -> ()
                                        finally
                                            exitLevel ctx
                                    | _ -> ()
                                | _ -> ()
                        finally
                            ctx.TyparScope <- savedScope
                            ctx.TyparScopeStrict <- savedStrict
                    | _ -> ()
                | _ -> ()
        | _ -> ()

    /// `forceFill` recurses through `translateType`, so dependencies fill
    /// DFS-style regardless of declaration order. Runs before record / union
    /// field fill so a field or case-arg referencing an abbreviation by name
    /// sees the expanded type.
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
            fillUnionMembers ctx m

        for m in elems do
            walkModuleElem ctx m

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        walkElems ctx (CstWalk.implFileElems file)
