namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

module UnificationEngine =

    /// One level deep — call recursively for full resolution. Stops at a
    /// measure-bearing root so the measure stays attached: `unify` and
    /// `unitsOf` need the TyVar wrapper to see Units, and following Link
    /// straight through to the bare carrier would drop them.
    let resolveStep (t: SemType) : SemType =
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
        | TyTuple items -> TyTuple(EqArray.map zonk items)
        | TyRecord(n, args) -> TyRecord(n, EqArray.map zonk args)
        | TyUnion(n, args) -> TyUnion(n, EqArray.map zonk args)
        | TyClass(n, args) -> TyClass(n, EqArray.map zonk args)

    /// Decompose a (zonked) tupled-argument type into its element types: a
    /// .NET-style call passes one argument that is a tuple / unit / single
    /// value. The inverse of `tupleOrSingle`; used by call-site overload
    /// resolution (`String.Concat(…)`, external ctors).
    let argElemsOf (argTy: SemType) : SemType list =
        match zonk argTy with
        | TyTuple xs -> EqArray.toList xs
        | TyConst "unit" -> []
        | single -> [ single ]

    /// The single SemType a parameter list presents as a function argument:
    /// `unit` for none, the bare type for one, a tuple for many. Inverse of
    /// `argElemsOf`.
    let tupleOrSingle (paramTys: SemType list) : SemType =
        match paramTys with
        | [] -> BuiltinTypes.tyUnit
        | [ t ] -> t
        | many -> TyTuple(EqArray.ofList many)

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
    let rec occursAndAdjust (target: TypeVar) (t: SemType) : bool =
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
        | TyTuple items -> EqArray.exists (occursAndAdjust target) items
        | TyRecord(_, args) -> EqArray.exists (occursAndAdjust target) args
        | TyUnion(_, args) -> EqArray.exists (occursAndAdjust target) args
        | TyClass(_, args) -> EqArray.exists (occursAndAdjust target) args

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
                    Code = ""
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
        | TyTuple xs -> TyTuple(EqArray.map (substituteWith subst) xs)
        | TyRecord(n, args) -> TyRecord(n, EqArray.map (substituteWith subst) args)
        | TyUnion(n, args) -> TyUnion(n, EqArray.map (substituteWith subst) args)
        | TyClass(n, args) -> TyClass(n, EqArray.map (substituteWith subst) args)

    /// Empty when the lengths don't match — the caller has already (or
    /// should) emit an arity diagnostic, and an empty subst keeps the field
    /// types unsubstituted rather than silently mismatching. Public so
    /// Freeze can rebuild the same substitution when projecting fields off a
    /// generic receiver in a field-chain.
    let mkNamedTypeSubst
        (typeParams: EqArray<string * TypeVar>)
        (args: EqArray<SemType>)
        : Dictionary<TypeVar, SemType> =
        let subst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)

        if typeParams.Length = args.Length then
            let mutable i = 0

            for (_, tp) in typeParams do
                subst.[UnionFind.find tp] <- args.[i]
                i <- i + 1

        subst

    /// One-shot field / member instantiation: build the typar→arg subst from
    /// the declaring type's `TypeParams` and the receiver's `args`, then
    /// substitute it through `ty`. Hot single-substitution sites (record
    /// field read, class- and union-member access, SRTP static-member
    /// dispatch, abbreviation expansion) route through this helper. Sites
    /// that reuse the same subst across a loop / Array.map keep the explicit
    /// `mkNamedTypeSubst` + `substituteWith` pair so the dictionary is only
    /// built once.
    let instantiateMember (typeParams: EqArray<string * TypeVar>, args: EqArray<SemType>) (ty: SemType) : SemType =
        substituteWith (mkNamedTypeSubst typeParams args) ty

    /// Walk a `SemType` through TyVar Links to surface a `TyRecord _`. The
    /// arg list rides along so `drainPendingDotAccess` can substitute the
    /// record's typars when resolving deferred field accesses.
    let rec private tryResolveRecord (t: SemType) : (string * EqArray<SemType>) voption =
        match t with
        | TyRecord(n, args) -> ValueSome(n, args)
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome target -> tryResolveRecord target
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Mirror of `tryResolveRecord` for `TyClass`.
    let rec private tryResolveClass (t: SemType) : (string * EqArray<SemType>) voption =
        match t with
        | TyClass(n, args) -> ValueSome(n, args)
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome target -> tryResolveClass target
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Mirror of `tryResolveClass` for `TyUnion` (P3d.3 augmentation members).
    let rec private tryResolveUnion (t: SemType) : (string * EqArray<SemType>) voption =
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
    type ConstraintOutcome =
        | Satisfied
        | Violated
        | Defer

    /// `string` is excluded and handled separately since it's a reference type.
    let private primitiveValueTypes =
        Set.ofList [ "int"; "int64"; "byte"; "bool"; "float"; "float32"; "char"; "unit" ]

    let constraintKindName (k: SemanticConstraintKind) : string =
        match k with
        | SemanticConstraintKind.Equality -> "equality"
        | SemanticConstraintKind.Comparison -> "comparison"
        | SemanticConstraintKind.Struct -> "struct"
        | SemanticConstraintKind.ReferenceType -> "not struct"
        | SemanticConstraintKind.Nullness -> "null"
        | SemanticConstraintKind.NotNull -> "not null"

    let rec unify (ctx: PassContext) (key: NodeKey) (a: SemType) (b: SemType) =
        let a = resolveStep a
        let b = resolveStep b

        match a, b with
        | TyConst n1, TyConst n2 when n1 = n2 -> ()
        | TyRecord(n1, a1), TyRecord(n2, a2) when n1 = n2 && a1.Length = a2.Length ->
            for i in 0 .. a1.Length - 1 do
                unify ctx key a1.[i] a2.[i]
        | TyUnion(n1, a1), TyUnion(n2, a2) when n1 = n2 && a1.Length = a2.Length ->
            for i in 0 .. a1.Length - 1 do
                unify ctx key a1.[i] a2.[i]
        | TyClass(n1, a1), TyClass(n2, a2) when n1 = n2 && a1.Length = a2.Length ->
            for i in 0 .. a1.Length - 1 do
                unify ctx key a1.[i] a2.[i]
        | TyFun(a1, r1), TyFun(a2, r2) ->
            unify ctx key a1 a2
            unify ctx key r1 r2
        | TyTuple xs, TyTuple ys when xs.Length = ys.Length ->
            for i in 0 .. xs.Length - 1 do
                unify ctx key xs.[i] ys.[i]
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
                        Code = ""
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
                            Code = ""
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
                    Code = ""
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

                match ctx.Types.Record.TryGetValue recName with
                | true, info ->
                    let subst = mkNamedTypeSubst info.TypeParams args

                    for d in pending do
                        match info.Fields |> Array.tryFind (fun f -> f.Name = d.MemberName) with
                        | Some field -> unify ctx d.UseKey (TyVar d.ResultTv) (substituteWith subst field.Type)
                        | None ->
                            ctx.Diagnostics.Add
                                {
                                    Key = d.UseKey
                                    Message = sprintf "Type '%s' has no field '%s'" recName d.MemberName
                                    Code = ""
                                    Severity = Error
                                }
                | false, _ ->
                    for d in pending do
                        ctx.Diagnostics.Add
                            {
                                Key = d.UseKey
                                Message = sprintf "Unknown record type '%s'" recName
                                Code = ""
                                Severity = Error
                            }
            | ValueNone ->
                match tryResolveClass linkTarget with
                | ValueSome(clsName, args) ->
                    let pending = root.PendingDotAccess
                    root.PendingDotAccess <- []

                    match ctx.Types.Class.TryGetValue clsName with
                    | true, info ->
                        let subst = mkNamedTypeSubst info.TypeParams args

                        for d in pending do
                            match info.Members |> Array.tryFind (fun m -> m.Name = d.MemberName && not m.IsStatic) with
                            | Some m -> unify ctx d.UseKey (TyVar d.ResultTv) (substituteWith subst m.Type)
                            | None ->
                                ctx.Diagnostics.Add
                                    {
                                        Key = d.UseKey
                                        Message = sprintf "Type '%s' has no instance member '%s'" clsName d.MemberName
                                        Code = ""
                                        Severity = Error
                                    }
                    | false, _ ->
                        for d in pending do
                            ctx.Diagnostics.Add
                                {
                                    Key = d.UseKey
                                    Message = sprintf "Unknown class type '%s'" clsName
                                    Code = ""
                                    Severity = Error
                                }
                | ValueNone ->
                    // Union augmentation members (P3d.3).
                    match tryResolveUnion linkTarget with
                    | ValueNone -> ()
                    | ValueSome(unionName, args) ->
                        let pending = root.PendingDotAccess
                        root.PendingDotAccess <- []

                        match ctx.Types.Union.TryGetValue unionName with
                        | true, info ->
                            let subst = mkNamedTypeSubst info.TypeParams args

                            for d in pending do
                                match
                                    info.Members |> Array.tryFind (fun m -> m.Name = d.MemberName && not m.IsStatic)
                                with
                                | Some m -> unify ctx d.UseKey (TyVar d.ResultTv) (substituteWith subst m.Type)
                                | None ->
                                    ctx.Diagnostics.Add
                                        {
                                            Key = d.UseKey
                                            Message =
                                                sprintf "Type '%s' has no instance member '%s'" unionName d.MemberName
                                            Code = ""
                                            Severity = Error
                                        }
                        | false, _ ->
                            for d in pending do
                                ctx.Diagnostics.Add
                                    {
                                        Key = d.UseKey
                                        Message = sprintf "Unknown union type '%s'" unionName
                                        Code = ""
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
    and private reduceOutcome (check: SemType -> ConstraintOutcome) (items: EqArray<SemType>) : ConstraintOutcome =
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
    and checkConstraint (ctx: PassContext) (c: SemanticConstraint) (t: SemType) : ConstraintOutcome =
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
            match ctx.Types.Record.TryGetValue name with
            | true, info ->
                // C-Attr verdict overrides the field-walk. Equality: a
                // `[<NoEquality>]` record at a `=` / `<>` use site is a
                // diagnostic; a `[<ReferenceEquality>]` record satisfies the
                // equality predicate via BCL `Object.Equals`. Comparison
                // (brainstorm-comparison §9) is opt-in, so an unannotated
                // record is `NoComparison` ⇒ ordering use site rejected;
                // `[<StructuralComparison>]` falls through to the field-walk.
                match c.Kind, info.EqualitySupport, info.ComparisonSupport with
                | SemanticConstraintKind.Equality, EqualityVerdict.NoEquality, _ -> Violated
                | SemanticConstraintKind.Equality, EqualityVerdict.Reference, _ -> Satisfied
                | SemanticConstraintKind.Comparison, _, ComparisonVerdict.NoComparison -> Violated
                | _ ->
                    let subst = mkNamedTypeSubst info.TypeParams args

                    info.Fields
                    |> Array.map (fun f -> substituteWith subst f.Type)
                    |> EqArray.ofArray
                    |> reduceOutcome (checkConstraint ctx c)
            | false, _ -> Defer
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyUnion(name, args) ->
            match ctx.Types.Union.TryGetValue name with
            | true, info ->
                match c.Kind, info.EqualitySupport, info.ComparisonSupport with
                | SemanticConstraintKind.Equality, EqualityVerdict.NoEquality, _ -> Violated
                | SemanticConstraintKind.Equality, EqualityVerdict.Reference, _ -> Satisfied
                | SemanticConstraintKind.Comparison, _, ComparisonVerdict.NoComparison -> Violated
                | _ ->
                    let subst = mkNamedTypeSubst info.TypeParams args

                    let fields = ResizeArray<SemType>()

                    for case in info.Cases do
                        for field in case.Fields do
                            fields.Add(substituteWith subst field)

                    fields |> EqArray.ofResizeArray |> reduceOutcome (checkConstraint ctx c)
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
                            Code = ""
                            Severity = Error
                        }
                | Defer ->
                    remaining <- c :: remaining
                    propagateToFreeArgs ctx c linkTarget

            root.Constraints <- List.rev remaining

    /// When a compound shape is partially resolved, the parent constraint is
    /// satisfied iff every component supports it, so a still-free component
    /// carries the same constraint forward.
    and propagateToFreeArgs (ctx: PassContext) (c: SemanticConstraint) (t: SemType) : unit =
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
            | TyTuple xs ->
                for x in xs do
                    walk x
            | TyRecord(_, args) ->
                for a in args do
                    walk a
            | TyUnion(_, args) ->
                for a in args do
                    walk a
            | TyClass(_, args) ->
                for a in args do
                    walk a

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

    // Bitwise AND/OR/XOR have the same `^T * ^T -> ^T` primitive shape as
    // arithmetic; the shift operators differ — their second operand is `int32`,
    // not `^T` (`op_LeftShift`/`op_RightShift`: `^T * int32 -> ^T`).
    and private bitwiseBinaryOps =
        Set.ofList [ "op_BitwiseAnd"; "op_BitwiseOr"; "op_ExclusiveOr" ]

    and private shiftOps = Set.ofList [ "op_LeftShift"; "op_RightShift" ]

    // Unary `~-` / `~+` / `~~~` — one primitive operand, `^T -> ^T`.
    and private unaryPrimitiveOps =
        Set.ofList [ "op_UnaryNegation"; "op_UnaryPlus"; "op_LogicalNot" ]

    and private equalityBinaryOps = Set.ofList [ "op_Equality"; "op_Inequality" ]

    and private orderingBinaryOps =
        Set.ofList
            [
                "op_LessThan"
                "op_GreaterThan"
                "op_LessThanOrEqual"
                "op_GreaterThanOrEqual"
            ]

    // Split per operators-plan.md O4: equality stays in Vesper.Core, ordering in
    // Vesper.Comparison. Both families synthesise the same primitive trait shape
    // (`prim*prim → bool`), so `tryPrimitiveTraitCandidate` checks the union; the
    // split is what lets the decline-fallthrough diverge by family once the .fsi
    // contracts become the live provider (today they resolve identically).
    and private comparisonBinaryOps = Set.union equalityBinaryOps orderingBinaryOps

    and private tryPrimitiveTraitCandidate (memberName: string) (primName: string) (argCount: int) : SemType voption =
        if not (Set.contains primName numericPrimitives) then
            ValueNone
        elif
            argCount = 2
            && (Set.contains memberName arithmeticBinaryOps
                || Set.contains memberName bitwiseBinaryOps)
        then
            let t = TyConst primName
            ValueSome(TyFun(TyTuple(EqArray.ofList [ t; t ]), t))
        elif argCount = 2 && Set.contains memberName shiftOps then
            // `value: ^T -> shift: int32 -> ^T` — the shift amount is always int32.
            let t = TyConst primName
            ValueSome(TyFun(TyTuple(EqArray.ofList [ t; TyConst "int" ]), t))
        elif argCount = 1 && Set.contains memberName unaryPrimitiveOps then
            let t = TyConst primName
            ValueSome(TyFun(t, t))
        elif argCount = 2 && Set.contains memberName comparisonBinaryOps then
            let t = TyConst primName
            ValueSome(TyFun(TyTuple(EqArray.ofList [ t; t ]), TyConst "bool"))
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
        let argTys = bound.ArgTypes

        let tupled =
            match argTys.Length with
            | 0 -> bound.ReturnType
            | 1 -> TyFun(argTys.[0], bound.ReturnType)
            | _ -> TyFun(TyTuple argTys, bound.ReturnType)

        match resolveStep candidate with
        | TyFun(TyTuple _, _) -> unify ctx key candidate tupled
        | _ when argTys.Length >= 2 ->
            let curried = EqArray.foldBack (fun a r -> TyFun(a, r)) argTys bound.ReturnType

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
                                    Code = ""
                                    Severity = Error
                                }

                            b.Resolved <- true
                    | TyClass(className, classArgs) ->
                        match ctx.Types.Class.TryGetValue className with
                        | true, info ->
                            match info.Members |> Array.tryFind (fun m -> m.IsStatic && m.Name = b.MemberName) with
                            | Some m ->
                                let candTy = instantiateMember (info.TypeParams, classArgs) m.Type
                                b.Resolved <- true
                                unifySrtpAgainst ctx key candTy b
                            | None ->
                                ctx.Diagnostics.Add
                                    {
                                        Key = key
                                        Message = sprintf "Type '%s' has no static member '%s'" className b.MemberName
                                        Code = ""
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
