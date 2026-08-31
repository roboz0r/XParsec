namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis
open EmitTypes

/// The synthesised equality, hashing and comparison bodies of a union, a record, and a
/// hierarchy union's case type. Each entry point takes a `StructuralWalk` naming the values
/// to visit, an `IStructuralHandles` for the BCL members it calls, and `isVt`, the declaring
/// type's `[<Struct>]`.
///
/// `%A` is the sibling of this module: `EmitStructuralFormat`.
module internal EmitStructural =

    let private intTy = FTConst(RuntimeNames.intKey, EqArray.empty)

    /// What separates two values of a type before their fields are reached.
    [<RequireQualifiedAccess>]
    type Discriminant =
        /// A record, or a single-case union: one shape, so the field walk stands alone.
        | None
        /// A flat union's `_tag`: compared ahead of the fields, and seeds the hash.
        | TagField of EntityHandle
        /// A hierarchy union's case type, whose case the dispatch reaching the body already
        /// settled. It seeds the hash, so two cases of equal payload hash apart, and
        /// contributes nothing to the equality or comparison walk.
        | CaseTag of tag: int

    /// The values one synthesised structural body visits, and the self shape its
    /// `object`-typed entry point casts to.
    type StructuralWalk =
        {
            /// The declaring type's own token — the `isinst` target.
            SelfType: EntityHandle
            /// `FTUnion` / `FTRecord` / `FTClass` at the declaring type's own typars: the
            /// type of the cast `other` local, and the parameter type of the typed entry
            /// points.
            SelfTy: FrozenType
            /// `(field handle, declared type)` in visit order. A flat union crosses every
            /// case's fields, which agrees with a per-case walk because an inactive case's
            /// fields hold their default. The caller mints these as `Def` tokens or as
            /// `MemberRef`s on the type's own `TypeSpec` (`Box\`1<!0>::Value`).
            Fields: (EntityHandle * FrozenType) list
            Discriminant: Discriminant
        }

    /// Cast the `object` arg (`ldarg.1`) to `Self` and return its load, branching to
    /// `failLabel` on a non-`Self` arg (`null` included). On a value type (`isVt`)
    /// `isinst` yields a BOXED reference, `unbox.any`-ed into a value-typed local.
    let private castObjArgOrBranch
        (isVt: bool)
        (selfType: EntityHandle)
        (selfTy: FrozenType)
        (b: IlBuilder)
        (failLabel: int)
        : IlBuilder -> unit =
        let other = b.Local selfTy

        if isVt then
            let boxed = b.Local(FTConst(RuntimeNames.objKey, EqArray.empty))
            b.Add(ILInstr.Ldarg 1)
            b.Add(ILInstr.Isinst selfType)
            b.Add(ILInstr.Stloc boxed)
            b.Add(ILInstr.Ldloc boxed)
            b.Add(ILInstr.Brfalse failLabel)
            b.Add(ILInstr.Ldloc boxed)
            b.Add(ILInstr.UnboxAny selfType)
            b.Add(ILInstr.Stloc other)
        else
            b.Add(ILInstr.Ldarg 1)
            b.Add(ILInstr.Isinst selfType)
            b.Add(ILInstr.Stloc other)
            b.Add(ILInstr.Ldloc other)
            b.Add(ILInstr.Brfalse failLabel)

        fun b -> b.Add(ILInstr.Ldloc other)

    /// `override bool Equals(object obj)`: cast-or-false, then the walk; any mismatch
    /// returns `false`.
    let private buildStructuralEqualsObj
        (isVt: bool)
        (selfType: EntityHandle)
        (selfTy: FrozenType)
        (walk: IlBuilder -> (IlBuilder -> unit) -> int -> unit)
        : ILBody =
        let b = IlBuilder()
        let falseLabel = b.Label()
        let loadOther = castObjArgOrBranch isVt selfType selfTy b falseLabel

        walk b loadOther falseLabel

        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
        b.Add(ILInstr.Mark falseLabel)
        b.Add(ILInstr.LdcI4 0)
        b.Add ILInstr.Ret
        b.Body

    /// `bool Equals(Self other)` — the typed `IEquatable<Self>::Equals`, the boxing-free
    /// path `EqualityComparer<Self>.Default` takes, so a nested DU / record field recurses
    /// here. A reference type null-guards `other`; a value type takes it by value.
    let private buildStructuralEqualsTyped
        (isVt: bool)
        (walk: IlBuilder -> (IlBuilder -> unit) -> int -> unit)
        : ILBody =
        let b = IlBuilder()
        let falseLabel = b.Label()

        if not isVt then
            b.Add(ILInstr.Ldarg 1)
            b.Add(ILInstr.Brfalse falseLabel)

        walk b (fun b -> b.Add(ILInstr.Ldarg 1)) falseLabel

        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
        b.Add(ILInstr.Mark falseLabel)
        b.Add(ILInstr.LdcI4 0)
        b.Add ILInstr.Ret
        b.Body

    /// `int CompareTo(Self other)` — the typed `IComparable<Self>::CompareTo`. A
    /// `null` `other` sorts before any non-null value (BCL convention), returning `1`;
    /// otherwise the walk's first non-zero result, else `0`.
    let private buildStructuralCompareTo
        (isVt: bool)
        (walk: IlBuilder -> (IlBuilder -> unit) -> int -> int -> unit)
        : ILBody =
        let b = IlBuilder()
        let c = b.Local intTy
        let returnLabel = b.Label()

        let nullLabel =
            if isVt then
                ValueNone
            else
                let l = b.Label()
                b.Add(ILInstr.Ldarg 1)
                b.Add(ILInstr.Brfalse l)
                ValueSome l

        walk b (fun b -> b.Add(ILInstr.Ldarg 1)) c returnLabel

        b.Add(ILInstr.LdcI4 0)
        b.Add ILInstr.Ret
        b.Add(ILInstr.Mark returnLabel)
        b.Add(ILInstr.Ldloc c)
        b.Add ILInstr.Ret

        match nullLabel with
        | ValueSome l ->
            b.Add(ILInstr.Mark l)
            b.Add(ILInstr.LdcI4 1)
            b.Add ILInstr.Ret
        | ValueNone -> ()

        b.Body

    /// How an `object`-typed override reaches its typed counterpart: `Direct` where that is
    /// a concrete method, `Virtual` where a hierarchy union's base declares it abstract and
    /// the case types override it.
    [<RequireQualifiedAccess>]
    type TypedEntry =
        | Direct of EntityHandle
        | Virtual of EntityHandle

    let private callTypedEntry (b: IlBuilder) (entry: TypedEntry) : unit =
        match entry with
        | TypedEntry.Direct h -> b.Add(ILInstr.Call(h, 2, 1))
        | TypedEntry.Virtual h -> b.Add(ILInstr.Callvirt(h, 2, 1))

    /// `int CompareTo(object obj)` — the non-generic `IComparable::CompareTo`. `null`
    /// sorts first (returns `1`), a non-`Self` arg throws `ArgumentException`, otherwise
    /// `typedEntry` reaches the typed `CompareTo(Self)`.
    ///
    /// A hierarchy union's base declares no field walk of its own, so this takes the self
    /// shape rather than a `StructuralWalk`.
    let buildCompareToObj
        (h: IStructuralHandles)
        (isVt: bool)
        (selfType: EntityHandle)
        (selfTy: FrozenType)
        (typedEntry: TypedEntry)
        : ILBody =
        let b = IlBuilder()
        let nullLabel = b.Label()
        let throwLabel = b.Label()

        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Brfalse nullLabel)

        let loadOther = castObjArgOrBranch isVt selfType selfTy b throwLabel

        b.Add(ILInstr.Ldarg 0)
        loadOther b
        callTypedEntry b typedEntry
        b.Add ILInstr.Ret

        b.Add(ILInstr.Mark throwLabel)
        b.Add(ILInstr.Ldstr(h.UserString "Object type mismatch"))
        b.Add(ILInstr.Newobj(h.ArgumentExceptionCtor, 1))
        b.Add ILInstr.Throw
        b.Add(ILInstr.Mark nullLabel)
        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
        b.Body

    // ---- Equality --------------------------------------------------------------------

    /// The walk both equality entry points share: a `TagField` discriminant must match,
    /// then each field via `EqualityComparer<F>.Default` (total equality, so a `float`
    /// field gets `NaN = NaN` here). Any mismatch branches to `falseLabel`.
    let private fieldEquality
        (h: IStructuralHandles)
        (w: StructuralWalk)
        (b: IlBuilder)
        (loadOther: IlBuilder -> unit)
        (falseLabel: int)
        : unit =
        match w.Discriminant with
        | Discriminant.TagField tagField ->
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld tagField)
            loadOther b
            b.Add(ILInstr.Ldfld tagField)
            b.Add(ILInstr.BneUn falseLabel)
        | Discriminant.None
        | Discriminant.CaseTag _ -> ()

        for (fieldHandle, fieldTy) in w.Fields do
            b.Add(ILInstr.Call(h.EqualityComparerDefault fieldTy, 0, 1))
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            loadOther b
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Callvirt(h.EqualityComparerEquals fieldTy, 3, 1))
            b.Add(ILInstr.Brfalse falseLabel)

    /// `override bool Equals(object obj)`: cast-or-false, then the walk.
    let buildEqualsObj (h: IStructuralHandles) (isVt: bool) (w: StructuralWalk) : ILBody =
        buildStructuralEqualsObj isVt w.SelfType w.SelfTy (fieldEquality h w)

    /// `bool Equals(Self other)` — the typed `IEquatable<Self>::Equals` over the walk.
    let buildEqualsTyped (h: IStructuralHandles) (isVt: bool) (w: StructuralWalk) : ILBody =
        buildStructuralEqualsTyped isVt (fieldEquality h w)

    /// `override int GetHashCode()`: a `System.HashCode` seeded by the discriminant, every
    /// field added through `HashCode.Add<T>`, then `ToHashCode()`. Equal values hash equal,
    /// because the seed distinguishes cases and the walk covers every field the equality
    /// body reads.
    let buildGetHashCode (h: IStructuralHandles) (w: StructuralWalk) : ILBody =
        let b = IlBuilder()
        let hc = b.Local h.HashCodeType

        let seedPush =
            match w.Discriminant with
            | Discriminant.None -> []
            | Discriminant.CaseTag tag -> [ ILInstr.LdcI4 tag ]
            | Discriminant.TagField tagField -> [ ILInstr.Ldarg 0; ILInstr.Ldfld tagField ]

        match seedPush with
        | [] -> ()
        | pushes ->
            b.Add(ILInstr.Ldloca hc)

            for p in pushes do
                b.Add p

            b.Add(ILInstr.Call(h.HashCodeAdd intTy, 2, 0))

        for (fieldHandle, fieldTy) in w.Fields do
            b.Add(ILInstr.Ldloca hc)
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Call(h.HashCodeAdd fieldTy, 2, 0))

        b.Add(ILInstr.Ldloca hc)
        b.Add(ILInstr.Call(h.HashCodeToHashCode, 1, 1))
        b.Add ILInstr.Ret
        b.Body

    // ---- Comparison ------------------------------------------------------------------

    /// The lexicographic walk the typed `CompareTo` takes: a `TagField` discriminant
    /// compared first via `sub` (case indices are small, so it cannot overflow), then each
    /// field via `Comparer<F>.Default.Compare`. The first non-zero result lands in `cLocal`
    /// and `brtrue`-s to `returnLabel`; on fall-through every comparison returned 0.
    let private fieldComparison
        (h: IStructuralHandles)
        (w: StructuralWalk)
        (b: IlBuilder)
        (loadOther: IlBuilder -> unit)
        (cLocal: int)
        (returnLabel: int)
        : unit =
        match w.Discriminant with
        | Discriminant.TagField tagField ->
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld tagField)
            loadOther b
            b.Add(ILInstr.Ldfld tagField)
            b.Add(ILInstr.Bin ILOpCode.Sub)
            b.Add(ILInstr.Stloc cLocal)
            b.Add(ILInstr.Ldloc cLocal)
            b.Add(ILInstr.Brtrue returnLabel)
        | Discriminant.None
        | Discriminant.CaseTag _ -> ()

        for (fieldHandle, fieldTy) in w.Fields do
            b.Add(ILInstr.Call(h.ComparerDefault fieldTy, 0, 1))
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            loadOther b
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Callvirt(h.ComparerCompare fieldTy, 3, 1))
            b.Add(ILInstr.Stloc cLocal)
            b.Add(ILInstr.Ldloc cLocal)
            b.Add(ILInstr.Brtrue returnLabel)

    /// `int CompareTo(Self other)` — the typed `IComparable<Self>::CompareTo` over the
    /// lexicographic walk.
    let buildCompareTo (h: IStructuralHandles) (isVt: bool) (w: StructuralWalk) : ILBody =
        buildStructuralCompareTo isVt (fieldComparison h w)

    // ---- A hierarchy union's own entry points ----------------------------------------

    /// `override bool Equals(U other)` on a case type: `isinst` this case and hand over to
    /// the typed `Equals(<Case>)`, whose null guard answers a null argument and another
    /// case alike. `call`, not `callvirt`: a case type is sealed and the typed entry
    /// declares no slot.
    let buildUnionCaseEqualsUnion (caseType: EntityHandle) (equalsCase: EntityHandle) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Isinst caseType)
        b.Add(ILInstr.Call(equalsCase, 2, 1))
        b.Add ILInstr.Ret
        b.Body

    /// `override bool Equals(object obj)` on a hierarchy union's base: cast and hand over
    /// to the abstract `Equals(U)` slot the case types implement. A non-`U` argument
    /// `isinst`s to `null`, which that override answers `false`.
    let buildUnionBaseEqualsObj (selfType: EntityHandle) (equalsUnion: EntityHandle) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Isinst selfType)
        b.Add(ILInstr.Callvirt(equalsUnion, 2, 1))
        b.Add ILInstr.Ret
        b.Body

    /// `override int CompareTo(U other)` on a hierarchy union's case type: an `other` of
    /// this case hands over to the typed `CompareTo(<Case>)`; a `null` one sorts after
    /// (returns `1`, the BCL convention); any other case yields the ordinal difference.
    /// This is the one structural body that reads a discriminant off `other`, because
    /// `other` is the value the dispatch did not settle.
    let buildUnionCaseCompareToUnion
        (caseType: EntityHandle)
        (caseTy: FrozenType)
        (compareToCase: EntityHandle)
        (tag: int)
        (tagField: EntityHandle)
        : ILBody =
        let b = IlBuilder()
        let sameCaseLabel = b.Label()
        let nullLabel = b.Label()
        let other = b.Local caseTy

        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Isinst caseType)
        b.Add(ILInstr.Stloc other)
        b.Add(ILInstr.Ldloc other)
        b.Add(ILInstr.Brtrue sameCaseLabel)

        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Brfalse nullLabel)

        // Case indices are small, so the difference cannot overflow.
        b.Add(ILInstr.LdcI4 tag)
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Ldfld tagField)
        b.Add(ILInstr.Bin ILOpCode.Sub)
        b.Add ILInstr.Ret

        b.Add(ILInstr.Mark sameCaseLabel)
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldloc other)
        b.Add(ILInstr.Call(compareToCase, 2, 1))
        b.Add ILInstr.Ret

        b.Add(ILInstr.Mark nullLabel)
        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
        b.Body
