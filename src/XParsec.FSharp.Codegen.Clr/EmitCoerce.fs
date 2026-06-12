namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower
open EmitDispatch
open EmitPattern

/// The implicit value→`obj` upcast policy, shared by every call / construction /
/// record-cons emit site. The front end accepts `'T`/value → `obj` *without
/// grounding* the typar (Engine's `obj` rule — `tryCoerceUpcast` /
/// `unifyAppliedSig`); the box that upcast implies is materialised here, in one
/// place, rather than re-inlined at each site. `isValueType` (EmitPattern) and
/// `typeOfExpr` (EmitLower) force this module below those two — hence its own
/// file rather than a home in the call/ctor/member modules that consume it.
module EmitCoerce =

    /// `obj` as a *parameter* slot: the user-facing `obj` (`FTConst "obj"`) or a
    /// BCL `object` arriving as an external class (`FTClass System.Object`, a
    /// metadata `object` param). `ClrEncoder` encodes both to
    /// `ELEMENT_TYPE_OBJECT`, so both are the universal-supertype slot. The two
    /// nominal forms of the one `obj ≡ System.Object` identity declared in
    /// `prim-types-object.fs`, recognised through `RuntimeNames` so this never
    /// re-spells the name (the abbreviation) or the key (the intrinsic).
    let isObjParamTy (ty: FrozenType) : bool =
        match ty with
        | FTConst(n, _) when n = RuntimeNames.objAbbrevName -> true
        | FTClass(key, _) -> RuntimeNames.isSystemObjectKey key
        | _ -> false

    /// `isObjParamTy` for an external member's *rendered* parameter signature: an
    /// `obj` parameter renders to `System.Object` (what the open-typar render
    /// `openTyparSig` produces) or the user-facing `obj`. The same universal-
    /// supertype slot, reached by a string compare because an external member's
    /// param model is the key's rendered `argSig`, not a typed `FrozenType` — so it
    /// matches the `RuntimeNames` *string* forms of the `prim-types-object.fs`
    /// identity (the intrinsic's qualified name and the abbreviation).
    let isObjParamSig (paramSig: string) : bool =
        paramSig = RuntimeNames.systemObjectQualifiedName
        || paramSig = RuntimeNames.objAbbrevName

    /// Materialise the implicit value→`obj` upcast for an argument **already
    /// pushed onto the stack**. When the argument flows into an `obj` parameter
    /// (`paramIsObj`) and its static type is a value type / generic typar, emit
    /// `box <argTy>`; a reference-typed argument is already usable as `obj` (no
    /// box) and `box` on a typar is a JIT no-op for a reference instantiation.
    let boxArgIntoObjParam (env: EmitEnv) (b: IlBuilder) (paramIsObj: bool) (argTy: FrozenType) : unit =
        if paramIsObj then
            let needsBox =
                match argTy with
                | FTTypar _ -> true
                | _ -> isValueType env argTy

            if needsBox then
                b.Add(ILInstr.Box(env.Provider.TypeToken argTy))

    /// Push each argument via `recur`, then box any position `isObjSlot` reports as
    /// an `obj` parameter. The single loop behind every call / ctor emit site; the
    /// param model varies (typed `FrozenType` list for project-local members,
    /// rendered `argSig` strings for external members, none for the provider path),
    /// so each site supplies its own predicate via `objSlotsOf` / `objSlotsOfSig` /
    /// `noObjSlots`.
    let emitArgsBoxed
        (recur: Recur)
        (env: EmitEnv)
        (b: IlBuilder)
        (args: EqArray<Frozen.TExpr>)
        (isObjSlot: int -> bool)
        : unit =
        for i in 0 .. args.Length - 1 do
            recur env b args.[i]
            boxArgIntoObjParam env b (isObjSlot i) (typeOfExpr args.[i])

    /// An `isObjSlot` predicate from a project-local member's declared parameter
    /// types. A position past the end of `paramTys` is not an obj slot — so a
    /// caller with no (or a shorter) param model pushes those arguments raw.
    let objSlotsOf (paramTys: FrozenType list) : int -> bool =
        let ptys = List.toArray paramTys
        fun i -> i < ptys.Length && isObjParamTy ptys.[i]

    /// An `isObjSlot` predicate from an external member's rendered `argSig`.
    let objSlotsOfSig (argSig: EqArray<string>) : int -> bool =
        fun i -> i < argSig.Length && isObjParamSig argSig.[i]

    /// The no-obj-slot predicate: the external/provider construction path has no
    /// local param model, so it pushes every argument raw (the provider's recipe
    /// boxes if needed). The explicit, named stand-in for the old `pushArgs []`.
    let noObjSlots: int -> bool = fun _ -> false
