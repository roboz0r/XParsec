namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open Vesper
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open EmitTypes

/// The synthesised equality, hashing and comparison bodies of a union, a record, and a
/// hierarchy union's case type. The `%A` bodies are in `EmitStructuralFormat`.
module internal EmitStructural =


    /// How a structural body compares one field's two values.
    [<RequireQualifiedAccess>]
    type FieldCompare =
        /// A signed integral, or an enum over one: `ceq`, and `cgt` / `clt` for order.
        | Signed
        /// `bool`, `char`, an unsigned integral, or an enum over one: `ceq`, and
        /// `cgt.un` / `clt.un` for order.
        | Unsigned
        /// `string`: ordinal `String.Equals` and `String.CompareOrdinal`.
        | String
        /// Everything else: `EqualityComparer<T>.Default` and `Comparer<T>.Default`.
        | Comparer

    [<RequireQualifiedAccess>]
    module FieldCompare =

        /// How a field typed at the argless intrinsic `key` compares. Every primitive outside
        /// the integral, `bool`, `char` and `string` set (`float`, `obj`, ...) is `Comparer`.
        let ofPrimitiveKey (key: TypeKey) : FieldCompare =
            match RuntimeNames.intKindOfKey key with
            | ValueSome k when IntKind.isSigned k -> FieldCompare.Signed
            | ValueSome _ -> FieldCompare.Unsigned
            | ValueNone ->
                if key = RuntimeNames.boolKey || key = RuntimeNames.charKey then
                    FieldCompare.Unsigned
                elif key = RuntimeNames.stringKey then
                    FieldCompare.String
                else
                    FieldCompare.Comparer

    /// One value a synthesised structural body visits.
    type StructuralField =
        {
            /// The non-empty `ldfld` chain from the value to the field: `Def` tokens, or
            /// `MemberRef`s on the type's own `TypeSpec` where the type is generic
            /// (`Box\`1<!0>::Value`).
            Path: Block<EntityHandle>
            /// The declared type, which the comparer, the hasher and `%A`'s `box` are
            /// instantiated at.
            Ty: FrozenType
            /// The `castclass` target a slot storing `object` is read through.
            Cast: EntityHandle voption
            Compare: FieldCompare
        }

    /// The values one synthesised structural body visits, and the order it reaches them in.
    [<RequireQualifiedAccess>]
    type StructuralWalk =
        /// One straight-line walk: a record, a single-case union, or a hierarchy union's
        /// case type. `seed` enters the hash alone, so two cases of equal payload hash
        /// apart.
        | Flat of seed: int voption * fields: StructuralField list
        /// A flat union's `_tag`, compared and hashed ahead of the fields, then selecting
        /// the case's own walk. `cases` is in tag order. Every body visits the active case's
        /// fields one at a time: padding inside a case data struct is not preserved across copies.
        | Tagged of tagField: EntityHandle * cases: StructuralField list list

    /// `ldfld` the field's chain off the value already pushed; an erased slot's value is
    /// left as `object`. One chain serves `this`, a by-value `other` and a scrutinee alike,
    /// because `ldfld` accepts an object reference, a managed pointer and a value type.
    let loadFieldPath (b: IlBuilder) (f: StructuralField) : unit =
        for h in f.Path do
            b.Add(ILInstr.Ldfld h)

    /// `loadFieldPath`, then the `castclass` an erased slot needs to yield the declared type.
    let loadField (b: IlBuilder) (f: StructuralField) : unit =
        loadFieldPath b f

        match f.Cast with
        | ValueSome token -> b.Add(ILInstr.Castclass token)
        | ValueNone -> ()

    /// Whether control reaches the point after an emitted body.
    [<RequireQualifiedAccess>]
    type WalkExit =
        /// The body leaves the stack as it found it and falls through.
        | Joins
        /// The body ends in `ret`.
        | Returned

    /// `switch` on `_tag` off `this` over `arms`, which are ascending by tag. A tag without
    /// an arm, and every joining arm, reaches the point after the dispatch. An empty
    /// `arms` emits nothing.
    let switchOnTag (b: IlBuilder) (tagField: EntityHandle) (arms: (int * (unit -> WalkExit)) list) : unit =
        match arms with
        | [] -> ()
        | _ ->
            let doneLabel = b.Label()
            let labelled = [ for (tag, arm) in arms -> tag, arm, b.Label() ]
            let lastTag, _, _ = List.last labelled
            let targets = Array.create (lastTag + 1) doneLabel

            for (tag, _, label) in labelled do
                targets.[tag] <- label

            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld tagField)
            b.Add(ILInstr.Switch(List.ofArray targets))
            b.Add(ILInstr.Br doneLabel)

            // A joining last arm falls through to `doneLabel`.
            let rec bodies (rest: (int * (unit -> WalkExit) * int) list) =
                match rest with
                | [] -> ()
                | (_, arm, label) :: later ->
                    b.Add(ILInstr.Mark label)

                    match arm (), later with
                    | WalkExit.Joins, _ :: _ -> b.Add(ILInstr.Br doneLabel)
                    | WalkExit.Joins, []
                    | WalkExit.Returned, _ -> ()

                    bodies later

            bodies labelled
            b.Add(ILInstr.Mark doneLabel)

    /// Run `onTag` on a `Tagged` walk's `_tag`, then `fields` over the fields the walk
    /// reaches: once for a `Flat` walk, per payload-bearing case for a `Tagged` walk. A
    /// `Tagged` walk always joins, a nullary case reaching the join directly.
    let private walk
        (b: IlBuilder)
        (w: StructuralWalk)
        (onTag: EntityHandle -> unit)
        (fields: StructuralField list -> WalkExit)
        : WalkExit =
        match w with
        | StructuralWalk.Flat(_, fs) -> fields fs
        | StructuralWalk.Tagged(tagField, cases) ->
            onTag tagField

            switchOnTag
                b
                tagField
                [
                    for (tag, fs) in List.indexed cases do
                        if not (List.isEmpty fs) then
                            tag, (fun () -> fields fs)
                ]

            WalkExit.Joins

    /// Cast the `object` arg (`ldarg.1`) to `Self` and return its load, branching to
    /// `failLabel` on a non-`Self` arg (`null` included). A value type (`isVt`) tests with
    /// `isinst` and then `unbox.any`s the arg in place, with no local; a reference type
    /// keeps the `isinst` result in a `Self` local.
    let private castObjArgOrBranch
        (isVt: bool)
        (selfType: EntityHandle)
        (selfTy: FrozenType)
        (b: IlBuilder)
        (failLabel: int)
        : IlBuilder -> unit =
        if isVt then
            b.Add(ILInstr.Ldarg 1)
            b.Add(ILInstr.Isinst selfType)
            b.Add(ILInstr.Brfalse failLabel)

            fun b ->
                b.Add(ILInstr.Ldarg 1)
                b.Add(ILInstr.UnboxAny selfType)
        else
            let other = b.Local selfTy
            b.Add(ILInstr.Ldarg 1)
            b.Add(ILInstr.Isinst selfType)
            b.Add(ILInstr.Stloc other)
            b.Add(ILInstr.Ldloc other)
            b.Add(ILInstr.Brfalse failLabel)

            fun b -> b.Add(ILInstr.Ldloc other)

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

    /// `return a && b && c` over a `Tagged` walk's tags, then the active case's fields each
    /// by its `FieldCompare`: a mismatch branches to `falseLabel`, the last comparison is
    /// returned directly. A `Comparer` field is total, so a `float` field has `NaN = NaN`.
    let private fieldEquality
        (h: IStructuralHandles)
        (w: StructuralWalk)
        (b: IlBuilder)
        (loadOther: IlBuilder -> unit)
        (falseLabel: int)
        : WalkExit =
        let loadBoth (f: StructuralField) =
            b.Add(ILInstr.Ldarg 0)
            loadField b f
            loadOther b
            loadField b f

        let compare (f: StructuralField) =
            match f.Compare with
            | FieldCompare.Signed
            | FieldCompare.Unsigned ->
                loadBoth f
                b.Add(ILInstr.Bin ILOpCode.Ceq)
            | FieldCompare.String ->
                loadBoth f
                b.Add(ILInstr.Call(h.StringEquals, 2, 1))
            | FieldCompare.Comparer ->
                b.Add(ILInstr.Call(h.EqualityComparerDefault f.Ty, 0, 1))
                loadBoth f
                b.Add(ILInstr.Callvirt(h.EqualityComparerEquals f.Ty, 3, 1))

        let rec fields (fs: StructuralField list) : WalkExit =
            match fs with
            | [] -> WalkExit.Joins
            | [ f ] ->
                compare f
                b.Add ILInstr.Ret
                WalkExit.Returned
            | f :: rest ->
                compare f
                b.Add(ILInstr.Brfalse falseLabel)
                fields rest

        let tagsMatch (tagField: EntityHandle) =
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld tagField)
            loadOther b
            b.Add(ILInstr.Ldfld tagField)
            b.Add(ILInstr.BneUn falseLabel)

        walk b w tagsMatch fields

    /// `override bool Equals(object obj)`: cast-or-false, then `typedEntry` reaches the
    /// typed `Equals(Self)`. `selfType` is the `isinst` target (the declaring type's own
    /// token); `selfTy` types the cast `other` local.
    let buildEqualsObj (isVt: bool) (selfType: EntityHandle) (selfTy: FrozenType) (typedEntry: TypedEntry) : ILBody =
        let b = IlBuilder()
        let falseLabel = b.Label()
        let loadOther = castObjArgOrBranch isVt selfType selfTy b falseLabel

        b.Add(ILInstr.Ldarg 0)
        loadOther b
        callTypedEntry b typedEntry
        b.Add ILInstr.Ret

        b.Add(ILInstr.Mark falseLabel)
        b.Add(ILInstr.LdcI4 0)
        b.Add ILInstr.Ret
        b.Body

    /// `bool Equals(Self other)` — the typed `IEquatable<Self>::Equals`, the boxing-free
    /// path `EqualityComparer<Self>.Default` takes, so a nested DU / record field recurses
    /// here. A reference type null-guards `other`; a value type takes it by value.
    let buildEqualsTyped (h: IStructuralHandles) (isVt: bool) (w: StructuralWalk) : ILBody =
        let b = IlBuilder()
        let falseLabel = b.Label()

        if not isVt then
            b.Add(ILInstr.Ldarg 1)
            b.Add(ILInstr.Brfalse falseLabel)

        match fieldEquality h w b (fun b -> b.Add(ILInstr.Ldarg 1)) falseLabel with
        | WalkExit.Joins ->
            b.Add(ILInstr.LdcI4 1)
            b.Add ILInstr.Ret
        | WalkExit.Returned -> ()

        b.Add(ILInstr.Mark falseLabel)
        b.Add(ILInstr.LdcI4 0)
        b.Add ILInstr.Ret
        b.Body

    /// `override int GetHashCode()`: a `System.HashCode` seeded by the case, the active
    /// case's fields added through `HashCode.Add<T>`, then `ToHashCode()`.
    let buildGetHashCode (h: IStructuralHandles) (w: StructuralWalk) : ILBody =
        let b = IlBuilder()
        let hc = b.Local h.HashCodeType

        let seed (pushes: ILInstr list) =
            b.Add(ILInstr.Ldloca hc)

            for p in pushes do
                b.Add p

            b.Add(ILInstr.Call(h.HashCodeAdd RuntimeNames.intTy, 2, 0))

        let fields (fs: StructuralField list) : WalkExit =
            for f in fs do
                b.Add(ILInstr.Ldloca hc)
                b.Add(ILInstr.Ldarg 0)
                loadField b f
                b.Add(ILInstr.Call(h.HashCodeAdd f.Ty, 2, 0))

            WalkExit.Joins

        match w with
        | StructuralWalk.Flat(ValueSome tag, _) -> seed [ ILInstr.LdcI4 tag ]
        | StructuralWalk.Flat(ValueNone, _)
        | StructuralWalk.Tagged _ -> ()

        walk b w (fun tagField -> seed [ ILInstr.Ldarg 0; ILInstr.Ldfld tagField ]) fields
        |> ignore

        b.Add(ILInstr.Ldloca hc)
        b.Add(ILInstr.Call(h.HashCodeToHashCode, 1, 1))
        b.Add ILInstr.Ret
        b.Body

    // ---- Comparison ------------------------------------------------------------------

    /// The lexicographic walk: a `Tagged` walk orders by tag first via `sub` (case indices
    /// are small, so the difference cannot overflow), then the active case's fields each by
    /// its `FieldCompare`; a non-zero exits `returnLabel`, the last result is returned directly.
    let private fieldComparison
        (h: IStructuralHandles)
        (w: StructuralWalk)
        (b: IlBuilder)
        (loadOther: IlBuilder -> unit)
        (returnLabel: int)
        : WalkExit =
        /// Exit to `returnLabel` with the result on the stack when it is non-zero, else
        /// discard it.
        let exitIfNonZero () =
            b.Add ILInstr.Dup
            b.Add(ILInstr.Brtrue returnLabel)
            b.Add ILInstr.Pop

        let loadBoth (f: StructuralField) =
            b.Add(ILInstr.Ldarg 0)
            loadField b f
            loadOther b
            loadField b f

        /// `(a > b) - (a < b)`: `-1`, `0` or `1` without a branch.
        let sign (f: StructuralField) (gt: ILOpCode) (lt: ILOpCode) =
            loadBoth f
            b.Add(ILInstr.Bin gt)
            loadBoth f
            b.Add(ILInstr.Bin lt)
            b.Add(ILInstr.Bin ILOpCode.Sub)

        let compare (f: StructuralField) =
            match f.Compare with
            | FieldCompare.Signed -> sign f ILOpCode.Cgt ILOpCode.Clt
            | FieldCompare.Unsigned -> sign f ILOpCode.Cgt_un ILOpCode.Clt_un
            | FieldCompare.String ->
                loadBoth f
                b.Add(ILInstr.Call(h.StringCompareOrdinal, 2, 1))
            | FieldCompare.Comparer ->
                b.Add(ILInstr.Call(h.ComparerDefault f.Ty, 0, 1))
                loadBoth f
                b.Add(ILInstr.Callvirt(h.ComparerCompare f.Ty, 3, 1))

        let rec fields (fs: StructuralField list) : WalkExit =
            match fs with
            | [] -> WalkExit.Joins
            | [ f ] ->
                compare f
                b.Add ILInstr.Ret
                WalkExit.Returned
            | f :: rest ->
                compare f
                exitIfNonZero ()
                fields rest

        let tagsOrder (tagField: EntityHandle) =
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld tagField)
            loadOther b
            b.Add(ILInstr.Ldfld tagField)
            b.Add(ILInstr.Bin ILOpCode.Sub)
            exitIfNonZero ()

        walk b w tagsOrder fields

    /// `int CompareTo(Self other)` — the typed `IComparable<Self>::CompareTo`. A `null`
    /// `other` sorts before any non-null value (BCL convention), returning `1`; otherwise
    /// the walk's first non-zero result, else `0`.
    let buildCompareTo (h: IStructuralHandles) (isVt: bool) (w: StructuralWalk) : ILBody =
        let b = IlBuilder()
        // Reached with the non-zero result on the stack.
        let returnLabel = b.Label()

        let nullLabel =
            if isVt then
                ValueNone
            else
                let l = b.Label()
                b.Add(ILInstr.Ldarg 1)
                b.Add(ILInstr.Brfalse l)
                ValueSome l

        match fieldComparison h w b (fun b -> b.Add(ILInstr.Ldarg 1)) returnLabel with
        | WalkExit.Joins ->
            b.Add(ILInstr.LdcI4 0)
            b.Add ILInstr.Ret
        | WalkExit.Returned -> ()

        b.Add(ILInstr.Mark returnLabel)
        b.Add ILInstr.Ret

        match nullLabel with
        | ValueSome l ->
            b.Add(ILInstr.Mark l)
            b.Add(ILInstr.LdcI4 1)
            b.Add ILInstr.Ret
        | ValueNone -> ()

        b.Body

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
