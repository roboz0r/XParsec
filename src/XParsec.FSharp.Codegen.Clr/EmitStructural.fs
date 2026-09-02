namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis
open EmitTypes

/// The synthesised equality, hashing and comparison bodies of a union, a record, and a
/// hierarchy union's case type. The `%A` bodies are in `EmitStructuralFormat`.
module internal EmitStructural =

    let private intTy = FTConst(RuntimeNames.intKey, EqArray.empty)

    /// One value a synthesised structural body visits.
    type StructuralField =
        {
            /// A `Def` token, or a `MemberRef` on the type's own `TypeSpec` where the type
            /// is generic (`Box\`1<!0>::Value`).
            Handle: EntityHandle
            /// The declared type, which the comparer, the hasher and `%A`'s `box` are
            /// instantiated at.
            Ty: FrozenType
            /// The `castclass` target a slot storing `object` is read through.
            Cast: EntityHandle voption
        }

    /// The values one synthesised structural body visits, and the order it reaches them in.
    [<RequireQualifiedAccess>]
    type StructuralWalk =
        /// One straight-line walk: a record, a single-case union, or a hierarchy union's
        /// case type. `seed` enters the hash alone, so two cases of equal payload hash
        /// apart.
        | Flat of seed: int voption * fields: StructuralField list
        /// A flat union's `_tag`, compared and hashed ahead of the fields, then selecting
        /// the case's own walk. `cases` is in tag order.
        | Tagged of tagField: EntityHandle * cases: StructuralField list list

    /// `ldfld` the field off the value already pushed, adding the `castclass` an erased
    /// slot needs.
    let loadField (b: IlBuilder) (f: StructuralField) : unit =
        b.Add(ILInstr.Ldfld f.Handle)

        match f.Cast with
        | ValueSome token -> b.Add(ILInstr.Castclass token)
        | ValueNone -> ()

    /// Run `body` over the fields of whichever case `_tag` identifies, off `this`. A case
    /// carrying no field is reached by the fall-through, so an all-nullary union emits
    /// nothing here. `body` leaves the stack as it found it.
    let private perCaseDispatch
        (b: IlBuilder)
        (tagField: EntityHandle)
        (cases: StructuralField list list)
        (body: StructuralField list -> unit)
        : unit =
        match cases |> List.indexed |> List.filter (fun (_, fs) -> not (List.isEmpty fs)) with
        | [] -> ()
        | occupied ->
            let doneLabel = b.Label()
            let labelled = [ for (tag, fs) in occupied -> tag, fs, b.Label() ]

            for (tag, _, label) in labelled do
                b.Add(ILInstr.Ldarg 0)
                b.Add(ILInstr.Ldfld tagField)
                b.Add(ILInstr.LdcI4 tag)
                b.Add(ILInstr.Beq label)

            b.Add(ILInstr.Br doneLabel)

            // The last case falls through to `doneLabel`.
            let rec bodies (rest: (int * StructuralField list * int) list) =
                match rest with
                | [] -> ()
                | (_, fs, label) :: later ->
                    b.Add(ILInstr.Mark label)
                    body fs

                    if not (List.isEmpty later) then
                        b.Add(ILInstr.Br doneLabel)

                    bodies later

            bodies labelled
            b.Add(ILInstr.Mark doneLabel)

    /// Run `onTag` on a `Tagged` walk's `_tag`, then `fields` over the fields the walk
    /// reaches.
    let private walk
        (b: IlBuilder)
        (w: StructuralWalk)
        (onTag: EntityHandle -> unit)
        (fields: StructuralField list -> unit)
        : unit =
        match w with
        | StructuralWalk.Flat(_, fs) -> fields fs
        | StructuralWalk.Tagged(tagField, cases) ->
            onTag tagField
            perCaseDispatch b tagField cases fields

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

    /// The walk both equality entry points share: a `Tagged` walk's tags must match, then
    /// the active case's fields via `EqualityComparer<F>.Default` (total equality, so a
    /// `float` field gets `NaN = NaN` here). Any mismatch branches to `falseLabel`.
    let private fieldEquality
        (h: IStructuralHandles)
        (w: StructuralWalk)
        (b: IlBuilder)
        (loadOther: IlBuilder -> unit)
        (falseLabel: int)
        : unit =
        let fields (fs: StructuralField list) =
            for f in fs do
                b.Add(ILInstr.Call(h.EqualityComparerDefault f.Ty, 0, 1))
                b.Add(ILInstr.Ldarg 0)
                loadField b f
                loadOther b
                loadField b f
                b.Add(ILInstr.Callvirt(h.EqualityComparerEquals f.Ty, 3, 1))
                b.Add(ILInstr.Brfalse falseLabel)

        let tagsMatch (tagField: EntityHandle) =
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld tagField)
            loadOther b
            b.Add(ILInstr.Ldfld tagField)
            b.Add(ILInstr.BneUn falseLabel)

        walk b w tagsMatch fields

    /// `override bool Equals(object obj)`: cast-or-false, then the walk. `selfType` is
    /// the `isinst` target (the declaring type's own token); `selfTy` types the cast
    /// `other` local.
    let buildEqualsObj
        (h: IStructuralHandles)
        (isVt: bool)
        (selfType: EntityHandle)
        (selfTy: FrozenType)
        (w: StructuralWalk)
        : ILBody =
        buildStructuralEqualsObj isVt selfType selfTy (fieldEquality h w)

    /// `bool Equals(Self other)` — the typed `IEquatable<Self>::Equals` over the walk.
    let buildEqualsTyped (h: IStructuralHandles) (isVt: bool) (w: StructuralWalk) : ILBody =
        buildStructuralEqualsTyped isVt (fieldEquality h w)

    /// `override int GetHashCode()`: a `System.HashCode` seeded by the case, the active
    /// case's fields added through `HashCode.Add<T>`, then `ToHashCode()`.
    let buildGetHashCode (h: IStructuralHandles) (w: StructuralWalk) : ILBody =
        let b = IlBuilder()
        let hc = b.Local h.HashCodeType

        let seed (pushes: ILInstr list) =
            b.Add(ILInstr.Ldloca hc)

            for p in pushes do
                b.Add p

            b.Add(ILInstr.Call(h.HashCodeAdd intTy, 2, 0))

        let fields (fs: StructuralField list) =
            for f in fs do
                b.Add(ILInstr.Ldloca hc)
                b.Add(ILInstr.Ldarg 0)
                loadField b f
                b.Add(ILInstr.Call(h.HashCodeAdd f.Ty, 2, 0))

        match w with
        | StructuralWalk.Flat(ValueSome tag, _) -> seed [ ILInstr.LdcI4 tag ]
        | StructuralWalk.Flat(ValueNone, _)
        | StructuralWalk.Tagged _ -> ()

        walk b w (fun tagField -> seed [ ILInstr.Ldarg 0; ILInstr.Ldfld tagField ]) fields

        b.Add(ILInstr.Ldloca hc)
        b.Add(ILInstr.Call(h.HashCodeToHashCode, 1, 1))
        b.Add ILInstr.Ret
        b.Body

    // ---- Comparison ------------------------------------------------------------------

    /// The lexicographic walk the typed `CompareTo` takes: a `Tagged` walk orders by tag
    /// first via `sub` (case indices are small, so the difference cannot overflow), then the
    /// active case's fields via `Comparer<F>.Default.Compare`; a non-zero exits `returnLabel`.
    let private fieldComparison
        (h: IStructuralHandles)
        (w: StructuralWalk)
        (b: IlBuilder)
        (loadOther: IlBuilder -> unit)
        (cLocal: int)
        (returnLabel: int)
        : unit =
        let fields (fs: StructuralField list) =
            for f in fs do
                b.Add(ILInstr.Call(h.ComparerDefault f.Ty, 0, 1))
                b.Add(ILInstr.Ldarg 0)
                loadField b f
                loadOther b
                loadField b f
                b.Add(ILInstr.Callvirt(h.ComparerCompare f.Ty, 3, 1))
                b.Add(ILInstr.Stloc cLocal)
                b.Add(ILInstr.Ldloc cLocal)
                b.Add(ILInstr.Brtrue returnLabel)

        let tagsOrder (tagField: EntityHandle) =
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld tagField)
            loadOther b
            b.Add(ILInstr.Ldfld tagField)
            b.Add(ILInstr.Bin ILOpCode.Sub)
            b.Add(ILInstr.Stloc cLocal)
            b.Add(ILInstr.Ldloc cLocal)
            b.Add(ILInstr.Brtrue returnLabel)

        walk b w tagsOrder fields

    /// `int CompareTo(Self other)` — the typed `IComparable<Self>::CompareTo` over the
    /// lexicographic walk.
    let buildCompareTo (h: IStructuralHandles) (isVt: bool) (w: StructuralWalk) : ILBody =
        buildStructuralCompareTo isVt (fieldComparison h w)

    // ---- A hierarchy union's own entry points ----------------------------------------

    /// `override bool Equals(U other)` on a case type: `isinst` this case and `call` the
    /// typed `Equals(<Case>)`, whose null guard answers a null argument and another case
    /// alike.
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

    /// How `CompareTo(U)` obtains the ordinal of a non-null `other` of another case: the
    /// one value the dispatch that reached the body did not settle.
    [<RequireQualifiedAccess>]
    type OtherOrdinal =
        /// `Tagged`: load `other`'s `_tag`.
        | TagField of EntityHandle
        /// `TypeTested`: the REMAINING cases' `(type token, tag)` in tag order. The last
        /// entry is the fall-through, so the chain over a union of n cases costs n − 2
        /// tests here on top of the body's own-case test.
        | TypeTests of (EntityHandle * int) list

    /// `override int CompareTo(U other)` on a hierarchy union's case type: an `other` of
    /// this case hands over to the typed `CompareTo(<Case>)`; a `null` one sorts after
    /// (returns `1`, the BCL convention); any other case yields the ordinal difference.
    let buildUnionCaseCompareToUnion
        (caseType: EntityHandle)
        (caseTy: FrozenType)
        (compareToCase: EntityHandle)
        (tag: int)
        (otherOrdinal: OtherOrdinal)
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

        // Case indices are small, so no difference below can overflow.
        (match otherOrdinal with
         | OtherOrdinal.TagField tagField ->
             b.Add(ILInstr.LdcI4 tag)
             b.Add(ILInstr.Ldarg 1)
             b.Add(ILInstr.Ldfld tagField)
             b.Add(ILInstr.Bin ILOpCode.Sub)
             b.Add ILInstr.Ret
         | OtherOrdinal.TypeTests others ->
             let rec emit rest =
                 match rest with
                 | [] -> failwith "Emit: a hierarchy union declares at least two cases"
                 | [ (_, lastTag) ] ->
                     b.Add(ILInstr.LdcI4(tag - lastTag))
                     b.Add ILInstr.Ret
                 | (token, otherTag) :: later ->
                     let skip = b.Label()
                     b.Add(ILInstr.Ldarg 1)
                     b.Add(ILInstr.Isinst token)
                     b.Add(ILInstr.Brfalse skip)
                     b.Add(ILInstr.LdcI4(tag - otherTag))
                     b.Add ILInstr.Ret
                     b.Add(ILInstr.Mark skip)
                     emit later

             emit others)

        b.Add(ILInstr.Mark sameCaseLabel)
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldloc other)
        b.Add(ILInstr.Call(compareToCase, 2, 1))
        b.Add ILInstr.Ret

        b.Add(ILInstr.Mark nullLabel)
        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
        b.Body
