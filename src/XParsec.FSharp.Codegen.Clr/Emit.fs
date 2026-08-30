namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitPattern
open EmitExpr

/// The method and body builders codegen calls. Re-exports the helper modules' public
/// surface so callers keep using `Emit.*`.
module Emit =

    type Closure = EmitTypes.Closure
    type EmittedCase = EmitTypes.EmittedCase
    type EmittedMember = EmitTypes.EmittedMember
    type EmittedUnion = EmitTypes.EmittedUnion
    type EmittedRecord = EmitTypes.EmittedRecord
    type EmittedClass = EmitTypes.EmittedClass
    type EmittedInterface = EmitTypes.EmittedInterface
    type EmittedEnum = EmitTypes.EmittedEnum
    type EmittedEnumRepr = EmitTypes.EmittedEnumRepr
    type ModuleClassKey = EmitTypes.ModuleClassKey
    type StaticFn = EmitTypes.StaticFn
    type ModuleValue = EmitTypes.ModuleValue
    type StaticMethodRef = EmitTypes.StaticMethodRef
    type EmitContext = EmitTypes.EmitContext
    type PreambleStep = EmitTypes.PreambleStep
    type CtorChain = EmitTypes.CtorChain
    type ClosureNamer = EmitClosures.ClosureNamer

    let closureIsCached = EmitTypes.closureIsCached

    type Emission = EmitClosures.Emission

    let lower = EmitLower.lower
    let emissions = EmitClosures.emissions
    let collectModuleValues = EmitClosures.collectModuleValues
    let collectGenericModuleValues = EmitClosures.collectGenericModuleValues
    let collectProgramValues = EmitClosures.collectProgramValues
    let validateModuleValueInits = EmitClosures.validateModuleValueInits
    let staticEligible = EmitClosures.staticEligible
    let bridgeStaticFnEscapes = EmitClosures.bridgeStaticFnEscapes
    let collectStaticFns = EmitClosures.collectStaticFns
    let staticFnTypars = EmitClosures.staticFnTypars
    let discoverClosures = EmitClosures.discoverClosures

    /// Build the `Main` body from the lowered decls: each top-level `let` binds a
    /// `Main` local, each effectful expression is emitted in source order, then
    /// `ldc.i4.0; ret`.
    let buildMain (ctx: EmitContext) (decls: TastAccessor.DeclId list) : ILBody =
        let b = IlBuilder()
        let env = EmitEnv.ofContext ctx (Dictionary())

        for d in decls do
            match TastAccessor.declKind d with
            | DeclShape.Expression -> buildStatement env b (TastAccessor.declExpression d)
            | DeclShape.Type -> ()
            | DeclShape.Let ->
                let dl = TastAccessor.declLet d

                match TastAccessor.patBoundVar dl.Pattern with
                // A function emitted as a static method has no Main local.
                | ValueSome boundVar when ctx.StaticMethods.ContainsKey boundVar -> ()
                // A top-level value that follows a top-level `do`: its `public static`
                // field is written here, in source order, rather than in the Program `.cctor`,
                // which runs before `Main`.
                | ValueSome boundVar when ctx.MainInitValues.ContainsKey boundVar ->
                    buildExpr env b dl.Value
                    b.Add(ILInstr.Stsfld ctx.MainInitValues.[boundVar])
                // A module-level value is a `public static` field its module class's `.cctor`
                // initialises; a reference `ldsfld`s it, so it needs no Main local.
                | ValueSome boundVar when ctx.ModuleValues.ContainsKey boundVar -> ()
                | ValueSome boundVar ->
                    let slot = b.Local dl.Ty
                    env.Slots.[boundVar] <- slot
                    buildExpr env b dl.Value
                    b.Add(ILInstr.Stloc slot)
                // A destructuring top-level `let (a, b) = tupleExpr`: evaluate the value
                // once into a Main local, then `bindPattern` pulls each bound variable into
                // its own slot.
                | ValueNone ->
                    let slot = b.Local(EmitLower.typeOfExpr dl.Value)
                    buildExpr env b dl.Value
                    b.Add(ILInstr.Stloc slot)
                    bindPattern env b slot dl.Pattern

        b.Add(ILInstr.LdcI4 0)
        b.Add ILInstr.Ret
        b.Body

    /// Build a closure's `Invoke` body: evaluate its (lowered) body under a
    /// resolver mapping the parameter to `ldarg.1` and each capture to its
    /// field, leaving the result on the stack, then `ret`.
    let buildClosureInvoke
        (ctx: EmitContext)
        (closure: Closure)
        (captureFields: Dictionary<BoundVarId, EntityHandle>)
        : ILBody =
        let b = IlBuilder()
        let args = Dictionary<BoundVarId, int>()
        args.[closure.ParamKey] <- 1 // `this` is 0; the single applied parameter is 1

        // A flat closure's extra (peeled inner-`Lambda`) parameters: extra param `i`,
        // 0-based, is `ldarg.(2+i)`.
        closure.ExtraParams |> List.iteri (fun i (pk, _, _) -> args.[pk] <- 2 + i)

        let env = EmitEnv.create ctx closure.SelfKey captureFields args

        // A destructuring tuple parameter (`fun (a, b) -> …`): `ldarg.1` holds the
        // `ValueTuple`n`; spill it to a local and `bindPattern` the leaves out of it.
        match TastAccessor.patKind closure.ParamPat with
        | PatShape.Tuple ->
            let slot = b.Local closure.ParamTy
            b.Add(ILInstr.Ldarg 1)
            b.Add(ILInstr.Stloc slot)
            bindPattern env b slot closure.ParamPat
        | _ -> ()

        buildExpr env b closure.Body
        b.Add ILInstr.Ret
        b.Body

    /// Build a static-method function's body: bind each flattened parameter to its
    /// `ldarg` index (no `this`, so the first parameter is `ldarg.0`), evaluate the
    /// body onto the stack, then `ret`.
    let buildStaticMethod (ctx: EmitContext) (fn: StaticFn) : ILBody =
        let b = IlBuilder()
        let args = Dictionary<BoundVarId, int>()
        let flatParams = fn.Params.Flat
        flatParams |> List.iteri (fun i p -> args.[p.Slot] <- i)
        let env = EmitEnv.ofContext ctx args

        // A destructuring tuple parameter: its `ldarg.i` holds the `ValueTuple`n`;
        // spill it to a local and `bindPattern` the leaves out of it.
        flatParams
        |> List.iteri (fun i p ->
            match p.Pat with
            | Some pat ->
                let slot = b.Local p.Ty
                b.Add(ILInstr.Ldarg i)
                b.Add(ILInstr.Stloc slot)
                bindPattern env b slot pat
            | None -> ()
        )

        buildExpr env b fn.Body

        // Every Vesper expression yields a value, so a `void` body leaves the
        // `unit`-as-`ValueTuple` on the stack. Pop it before `ret`. A body that
        // terminates (`raise`) already left depth 0.
        if fn.ReturnsVoid then
            match b.Depth with
            | 0 -> ()
            | 1 -> b.Add ILInstr.Pop
            | n -> failwithf "void-returning static function body left %d values on the stack (expected 0 or 1)" n

        b.Add ILInstr.Ret
        b.Body

    /// Build a nominal member's body: an instance member's `this` is `ldarg.0` and its
    /// parameters `ldarg.1…`, a static member's parameters start at `ldarg.0`. Member
    /// bodies synthesise no closures, so an empty capture map is passed. `selfValueTy`
    /// is the declaring type when it is a VALUE type, `ValueNone` otherwise.
    let buildMember
        (ctx: EmitContext)
        (selfValueTy: FrozenType voption)
        (thisKey: BoundVarKeyG<BoundVarId> voption)
        (baseKey: BoundVarKeyG<BoundVarId> voption)
        (prms: EqArray<BoundVarKeyG<BoundVarId> * FrozenType>)
        (voidReturn: bool)
        (body: TastAccessor.ExprId)
        : ILBody =
        let b = IlBuilder()
        // Keyed by raw identity: a body loads a parameter through a `Var` that refers to it
        // that way, so each definition site widens as it takes its `ldarg` index.
        let args = Dictionary<BoundVarId, int>()

        let baseIdx =
            match thisKey with
            | ValueSome k ->
                args.[BoundVarKey.identity k] <- 0 // `this`
                1
            | ValueNone -> 0

        // `base` loads the same `ldarg.0` as `this`; the `CallVia.Base` discriminator on
        // the member access, not the object-arg load, makes the dispatch non-virtual.
        match baseKey with
        | ValueSome k -> args.[BoundVarKey.identity k] <- 0
        | ValueNone -> ()

        prms
        |> EqArray.iteri (fun i (k, _) -> args.[BoundVarKey.identity k] <- baseIdx + i)
        // `this` as `SelfKey` too, so the struct `this`-pointer path recognises a self-call:
        // `ldarg.0` is already the byref `this` and must be loaded directly, because spilling
        // it to a value temp copies the struct and a mutating self-call would not persist.
        let env =
            { EmitEnv.create ctx (ValueOption.map BoundVarKey.identity thisKey) (Dictionary()) args with
                SelfValueType = selfValueTy
            }

        buildExpr env b body

        // A `void` method must `ret` empty-stacked, so pop the residual
        // `unit`-as-`ValueTuple`. A body that terminates (`raise`) already left depth 0,
        // and a `Pop` there would be unreachable, which the IL balance check rejects.
        if voidReturn then
            match b.Depth with
            | 0 -> ()
            | 1 -> b.Add ILInstr.Pop
            | n -> failwithf "void-returning member body left %d values on the stack (expected 0 or 1)" n

        b.Add ILInstr.Ret
        b.Body

    /// Build a secondary constructor body: run the `let`-preamble into locals, then
    /// chain the primary `.ctor` (`ldarg.0; <primaryArgs>; call instance void
    /// Self::.ctor`). No base-ctor call, because the primary performs it.
    let buildSecondaryCtor
        (ctx: EmitContext)
        (prms: EqArray<BoundVarKeyG<BoundVarId> * FrozenType>)
        (lets: TastAccessor.CtorLet list)
        (primaryCtor: EntityHandle)
        (primaryArgs: TastAccessor.ExprId list)
        : ILBody =
        let b = IlBuilder()
        let args = Dictionary<BoundVarId, int>()
        prms |> EqArray.iteri (fun i (k, _) -> args.[BoundVarKey.identity k] <- 1 + i)
        let env = EmitEnv.ofContext ctx args

        for l in lets do
            let slot = b.Local l.Type
            env.Slots.[BoundVarKey.identity l.BoundVar] <- slot
            buildExpr env b l.Init
            b.Add(ILInstr.Stloc slot)

        b.Add(ILInstr.Ldarg 0)

        for a in primaryArgs do
            buildExpr env b a

        b.Add(ILInstr.Call(primaryCtor, List.length primaryArgs + 1, 0))
        b.Add ILInstr.Ret
        b.Body

    /// Build the explicit field-init secondary ctor (`new(args) = { f = e; … }`): run
    /// the `let`-preamble, then `ldarg.0; <init>; stfld field` per initialiser, in
    /// source order. NO primary chain, so unlisted fields stay zero-initialised.
    let buildSecondaryCtorFieldInit
        (ctx: EmitContext)
        (prms: EqArray<BoundVarKeyG<BoundVarId> * FrozenType>)
        (lets: TastAccessor.CtorLet list)
        (fieldInits: (EntityHandle * TastAccessor.ExprId) list)
        : ILBody =
        let b = IlBuilder()
        let args = Dictionary<BoundVarId, int>()
        prms |> EqArray.iteri (fun i (k, _) -> args.[BoundVarKey.identity k] <- 1 + i)
        let env = EmitEnv.ofContext ctx args

        for l in lets do
            let slot = b.Local l.Type
            env.Slots.[BoundVarKey.identity l.BoundVar] <- slot
            buildExpr env b l.Init
            b.Add(ILInstr.Stloc slot)

        for (field, init) in fieldInits do
            b.Add(ILInstr.Ldarg 0)
            buildExpr env b init
            b.Add(ILInstr.Stfld field)

        b.Add ILInstr.Ret
        b.Body

    /// Run a preamble `do` body for effect: every Vesper expression yields a value, so
    /// pop the `unit` before the next step. A terminating body (`raise`) left depth 0.
    let private buildForEffect (env: EmitEnv) (b: IlBuilder) (body: TastAccessor.ExprId) : unit =
        buildExpr env b body

        match b.Depth with
        | 0 -> ()
        | 1 -> b.Add ILInstr.Pop
        | n -> failwithf "class-preamble `do` body left %d values on the stack (expected 0 or 1)" n

    /// Build a class primary `.ctor`: chain the base ctor (a value type chains none),
    /// store each ctor param into its backing field, then run the instance preamble.
    /// Base args read params as `ldarg.1…`; `this` is unusable until the chain returns.
    let buildClassPrimaryCtor
        (ctx: EmitContext)
        (chain: CtorChain)
        (thisKey: BoundVarKeyG<BoundVarId>)
        (ctorParams: (BoundVarKeyG<BoundVarId> * FrozenType) list)
        (fields: EntityHandle list)
        (preamble: PreambleStep list)
        : ILBody =
        let b = IlBuilder()
        let args = Dictionary<BoundVarId, int>()
        args.[BoundVarKey.identity thisKey] <- 0

        ctorParams
        |> List.iteri (fun i (k, _) -> args.[BoundVarKey.identity k] <- 1 + i)
        // `this` as `SelfKey`: on a value type `ldarg.0` is the byref `this`, so a
        // self-call must load it directly rather than spill a copy.
        let env =
            EmitEnv.create ctx (ValueSome(BoundVarKey.identity thisKey)) (Dictionary()) args

        match chain with
        | CtorChain.None -> ()
        | CtorChain.Base(baseCtor, baseArgs) ->
            b.Add(ILInstr.Ldarg 0)

            for a in baseArgs do
                buildExpr env b a

            b.Add(ILInstr.Call(baseCtor, List.length baseArgs + 1, 0))

        fields
        |> List.iteri (fun i field ->
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldarg(i + 1))
            b.Add(ILInstr.Stfld field)
        )

        for step in preamble do
            match step with
            | PreambleStep.Store(field, init) ->
                b.Add(ILInstr.Ldarg 0)
                buildExpr env b init
                b.Add(ILInstr.Stfld field)
            | PreambleStep.Run body -> buildForEffect env b body

        b.Add ILInstr.Ret
        b.Body

    /// Build a `.cctor` body from a static preamble, in declaration order: a `let`
    /// initialiser `stsfld`ed into its backing field, a `static do` body run for
    /// effect. A `.cctor` is parameterless, so the env carries no args.
    let buildStaticCctor (ctx: EmitContext) (steps: PreambleStep list) : ILBody =
        let b = IlBuilder()
        let env = EmitEnv.ofContext ctx (Dictionary())

        for step in steps do
            match step with
            | PreambleStep.Store(field, init) ->
                buildExpr env b init
                b.Add(ILInstr.Stsfld field)
            | PreambleStep.Run body -> buildForEffect env b body

        b.Add ILInstr.Ret
        b.Body

    /// Build a closure's `.ctor` body: chain to the `FSharpFunc\`2` base ctor,
    /// then store each capture argument into its field.
    let buildClosureCtor (baseCtor: EntityHandle) (fields: EntityHandle list) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Call(baseCtor, 1, 0))

        fields
        |> List.iteri (fun i field ->
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldarg(i + 1))
            b.Add(ILInstr.Stfld field)
        )

        b.Add ILInstr.Ret
        b.Body

    /// A non-capturing, monomorphic closure's `.cctor`: `newobj` the closure once into
    /// the singleton `instance` field, so a stateless lambda allocates once rather than
    /// per construction site.
    let buildCachedClosureCctor (ctorHandle: EntityHandle) (cachedField: EntityHandle) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Newobj(ctorHandle, 0))
        b.Add(ILInstr.Stsfld cachedField)
        b.Add ILInstr.Ret
        b.Body

    /// A value-type (`[<Struct>]`) primary constructor: store each ctor param into its
    /// backing field and return. NO chained base `.ctor`, because `System.ValueType` has
    /// none accessible. `ldarg 0` is the managed pointer `newobj` passes (`&temp`).
    let buildStructCtor (fields: EntityHandle list) : ILBody =
        let b = IlBuilder()

        fields
        |> List.iteri (fun i field ->
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldarg(i + 1))
            b.Add(ILInstr.Stfld field)
        )

        b.Add ILInstr.Ret
        b.Body

    /// Build a string/mixed enum's `.cctor`: per case, push its literal (`pushLit` —
    /// `ldstr`, or `ldc;box` for a mixed int), `newobj` the wrapper's single-arg
    /// `.ctor`, `stsfld` the singleton into the case's `static initonly` field.
    let buildStructEnumCctor (ctorHandle: EntityHandle) (cases: (EntityHandle * ILInstr list) list) : ILBody =
        let b = IlBuilder()

        for (caseField, pushLit) in cases do
            pushLit |> List.iter b.Add
            b.Add(ILInstr.Newobj(ctorHandle, 1))
            b.Add(ILInstr.Stsfld caseField)

        b.Add ILInstr.Ret
        b.Body

    /// Build a union case's static factory: `newobj` via the union's parameterless
    /// ctor, stamp the discriminant `tag`, store each parameter into its field, return.
    /// `fieldHandles` are in declaration order = the factory's `ldarg.i` order.
    let buildUnionFactory
        (unionCtor: EntityHandle)
        (tag: int)
        (tagField: EntityHandle)
        (fieldHandles: EntityHandle list)
        : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Newobj(unionCtor, 0))
        b.Add ILInstr.Dup
        b.Add(ILInstr.LdcI4 tag)
        b.Add(ILInstr.Stfld tagField)

        fieldHandles
        |> List.iteri (fun i field ->
            b.Add ILInstr.Dup
            b.Add(ILInstr.Ldarg i)
            b.Add(ILInstr.Stfld field)
        )

        b.Add ILInstr.Ret
        b.Body

    /// One argument slot of a struct union's flat `.ctor`, as a case factory pushes it:
    /// the factory's own parameter for the constructed case's fields, a zeroed default
    /// (`ldloca; initobj; ldloc`) for every other case's.
    [<RequireQualifiedAccess>]
    type StructUnionCtorArg =
        | Param of index: int
        | Default of ty: FrozenType * token: EntityHandle

    /// Build a `[<Struct>]` union case's static factory: push the discriminant `tag` and
    /// each flat `.ctor` argument (`args` are in field declaration order), `newobj` the
    /// union's flat `.ctor`, return the value.
    let buildStructUnionFactory (ctorRef: EntityHandle) (tag: int) (args: StructUnionCtorArg list) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.LdcI4 tag)

        for a in args do
            match a with
            | StructUnionCtorArg.Param i -> b.Add(ILInstr.Ldarg i)
            | StructUnionCtorArg.Default(ty, token) ->
                let slot = b.Local ty
                b.Add(ILInstr.Ldloca slot)
                b.Add(ILInstr.Initobj token)
                b.Add(ILInstr.Ldloc slot)

        b.Add(ILInstr.Newobj(ctorRef, args.Length + 1))
        b.Add ILInstr.Ret
        b.Body

    // The synthesised equality / comparison bodies, shared between unions and records.
    // Each entry point takes the field walk (tag-then-fields for a union, fields for a
    // record) and `isVt`, the declaring type's `[<Struct>]`.

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
        let c = b.Local(FTConst(RuntimeNames.intKey, EqArray.empty))
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

    /// `int CompareTo(object obj)` — the non-generic `IComparable::CompareTo`. `null`
    /// sorts first (returns `1`), a non-`Self` arg throws `ArgumentException`,
    /// otherwise delegate to the typed `CompareTo(Self)`.
    let private buildStructuralCompareToObj
        (isVt: bool)
        (selfType: EntityHandle)
        (selfTy: FrozenType)
        (mismatchMessage: UserStringHandle)
        (argumentExceptionCtor: EntityHandle)
        (typedCompareTo: EntityHandle)
        : ILBody =
        let b = IlBuilder()
        let nullLabel = b.Label()
        let throwLabel = b.Label()

        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Brfalse nullLabel)

        let loadOther = castObjArgOrBranch isVt selfType selfTy b throwLabel

        b.Add(ILInstr.Ldarg 0)
        loadOther b
        b.Add(ILInstr.Call(typedCompareTo, 2, 1))
        b.Add ILInstr.Ret

        b.Add(ILInstr.Mark throwLabel)
        b.Add(ILInstr.Ldstr mismatchMessage)
        b.Add(ILInstr.Newobj(argumentExceptionCtor, 1))
        b.Add ILInstr.Throw
        b.Add(ILInstr.Mark nullLabel)
        b.Add(ILInstr.LdcI4 1)
        b.Add ILInstr.Ret
        b.Body

    /// The resolved handles a union's synthesised `Equals` / `GetHashCode` bodies need.
    /// A case factory sets only its own case's payload fields and a DU is immutable, so
    /// once the tags match, walking EVERY field equals a per-case walk and no tag switch
    /// is needed.
    type UnionEqualitySupport =
        {
            /// The union's own `TypeDefinition` — the `isinst` target.
            SelfType: EntityHandle
            /// `FTUnion(key, args)` — the type of the cast `other` local.
            SelfTy: FrozenType
            TagField: EntityHandle
            /// `(field handle, field type)` across every case, declaration order.
            Fields: (EntityHandle * FrozenType) list
            /// `int` — the tag's type, for `HashCode.Add<int>`.
            IntType: FrozenType
            /// `EqualityComparer<T>.Default` getter for a field type.
            ComparerDefault: FrozenType -> EntityHandle
            /// `EqualityComparer<T>::Equals(T, T) : bool` for a field type.
            ComparerEquals: FrozenType -> EntityHandle
            /// The `System.HashCode` value-type local.
            HashCodeLocal: FrozenType
            /// `HashCode::Add<T>(T)` for a field/tag type.
            HashCodeAdd: FrozenType -> EntityHandle
            /// `HashCode::ToHashCode() : int`.
            HashCodeToHashCode: EntityHandle
        }

    /// The tag-then-field walk shared by both equality entry points: tags must match,
    /// then each field via `EqualityComparer<F>.Default` (total equality, so a `float`
    /// field gets `NaN = NaN` here). Any mismatch branches to `falseLabel`.
    let private buildTagAndFieldEquality
        (s: UnionEqualitySupport)
        (b: IlBuilder)
        (loadOther: IlBuilder -> unit)
        (falseLabel: int)
        : unit =
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldfld s.TagField)
        loadOther b
        b.Add(ILInstr.Ldfld s.TagField)
        b.Add(ILInstr.BneUn falseLabel)

        for (fieldHandle, fieldTy) in s.Fields do
            b.Add(ILInstr.Call(s.ComparerDefault fieldTy, 0, 1))
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            loadOther b
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Callvirt(s.ComparerEquals fieldTy, 3, 1))
            b.Add(ILInstr.Brfalse falseLabel)

    /// `override bool Equals(object obj)` for a union: cast-or-false, then the shared
    /// tag/field walk.
    let buildUnionEquals (isVt: bool) (s: UnionEqualitySupport) : ILBody =
        buildStructuralEqualsObj isVt s.SelfType s.SelfTy (buildTagAndFieldEquality s)

    /// `bool Equals(Self other)` — the typed `IEquatable<Self>::Equals` over the
    /// shared tag/field walk.
    let buildUnionEqualsTyped (isVt: bool) (s: UnionEqualitySupport) : ILBody =
        buildStructuralEqualsTyped isVt (buildTagAndFieldEquality s)

    /// `override int GetHashCode()` for a union: a `System.HashCode` seeded with the
    /// `_tag`, every field added through it, then `ToHashCode()`. Equal values hash
    /// equal because the tag distinguishes cases and inactive-case fields are default.
    let buildUnionGetHashCode (s: UnionEqualitySupport) : ILBody =
        let b = IlBuilder()
        let hc = b.Local s.HashCodeLocal

        b.Add(ILInstr.Ldloca hc)
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldfld s.TagField)
        b.Add(ILInstr.Call(s.HashCodeAdd s.IntType, 2, 0))

        for (fieldHandle, fieldTy) in s.Fields do
            b.Add(ILInstr.Ldloca hc)
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Call(s.HashCodeAdd fieldTy, 2, 0))

        b.Add(ILInstr.Ldloca hc)
        b.Add(ILInstr.Call(s.HashCodeToHashCode, 1, 1))
        b.Add ILInstr.Ret
        b.Body

    /// Build a record's `.ctor` body: chain `Object::.ctor()`, then store each ctor
    /// argument into the matching field.
    let buildRecordCtor (baseCtor: EntityHandle) (fields: EntityHandle list) : ILBody = buildClosureCtor baseCtor fields

    /// The record-shaped analogue of `UnionEqualitySupport`: no `_tag` to compare or
    /// seed. Fields are in declaration order; the caller mints their handles as `Def`
    /// tokens or `MemberRef`s on the type's own `TypeSpec` (`Box\`1<!0>::Value`).
    type RecordEqualitySupport =
        {
            /// The record's own `TypeDefinition` — the `isinst` target.
            SelfType: EntityHandle
            /// `FTRecord(key, args)` — the type of the cast `other` local.
            SelfTy: FrozenType
            /// `(field handle, field type)` in declaration order.
            Fields: (EntityHandle * FrozenType) list
            ComparerDefault: FrozenType -> EntityHandle
            ComparerEquals: FrozenType -> EntityHandle
            HashCodeLocal: FrozenType
            HashCodeAdd: FrozenType -> EntityHandle
            HashCodeToHashCode: EntityHandle
        }

    /// The field walk shared by both record equality entry points, the union's minus
    /// the leading tag compare. Any field mismatch branches to `falseLabel`.
    let private buildRecordFieldEquality
        (s: RecordEqualitySupport)
        (b: IlBuilder)
        (loadOther: IlBuilder -> unit)
        (falseLabel: int)
        : unit =
        for (fieldHandle, fieldTy) in s.Fields do
            b.Add(ILInstr.Call(s.ComparerDefault fieldTy, 0, 1))
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            loadOther b
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Callvirt(s.ComparerEquals fieldTy, 3, 1))
            b.Add(ILInstr.Brfalse falseLabel)

    /// `override bool Equals(object obj)` for a record: cast-or-false, then the shared
    /// field walk.
    let buildRecordEquals (isVt: bool) (s: RecordEqualitySupport) : ILBody =
        buildStructuralEqualsObj isVt s.SelfType s.SelfTy (buildRecordFieldEquality s)

    /// `bool Equals(Self other)` — the typed `IEquatable<Self>::Equals` over the
    /// shared field walk.
    let buildRecordEqualsTyped (isVt: bool) (s: RecordEqualitySupport) : ILBody =
        buildStructuralEqualsTyped isVt (buildRecordFieldEquality s)

    /// `override int GetHashCode()` for a record: every field added through
    /// `HashCode.Add<T>`, then `ToHashCode()`. No tag seed, because a record has one shape.
    let buildRecordGetHashCode (s: RecordEqualitySupport) : ILBody =
        let b = IlBuilder()
        let hc = b.Local s.HashCodeLocal

        for (fieldHandle, fieldTy) in s.Fields do
            b.Add(ILInstr.Ldloca hc)
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Call(s.HashCodeAdd fieldTy, 2, 0))

        b.Add(ILInstr.Ldloca hc)
        b.Add(ILInstr.Call(s.HashCodeToHashCode, 1, 1))
        b.Add ILInstr.Ret
        b.Body

    /// Comparison support for a union: tags compared first via `sub` (case indices are
    /// small, so it cannot overflow), then each field via `Comparer<F>.Default.Compare`,
    /// returning the first non-zero result.
    type UnionComparisonSupport =
        {
            /// The union's own `TypeDefinition` — the `isinst` target the
            /// `CompareTo(object)` boxing entry uses to cast and type-check.
            SelfType: EntityHandle
            /// `FTUnion(key, args)` — the type of the cast `other` local and the
            /// param type of the typed `CompareTo(Self)`.
            SelfTy: FrozenType
            TagField: EntityHandle
            /// `(field handle, field type)` across every case, declaration order.
            Fields: (EntityHandle * FrozenType) list
            /// `Comparer<T>.Default` getter for a field type.
            ComparerDefault: FrozenType -> EntityHandle
            /// `Comparer<T>::Compare(T, T) : int32` for a field type.
            ComparerCompare: FrozenType -> EntityHandle
            /// `System.ArgumentException::.ctor(string)` — the
            /// `CompareTo(object)` body throws this on a non-`Self` arg.
            ArgumentExceptionCtor: EntityHandle
            /// The `"Object type mismatch"` literal `CompareTo(object)` throws with.
            /// Minted by the caller, because the builder owns no metadata context.
            MismatchMessage: UserStringHandle
        }

    /// The tag-then-field lex comparison shared by both `CompareTo` entry points. The
    /// first non-zero result is left in `cLocal` and `brtrue`-ed to `returnLabel`; on
    /// fall-through every comparison returned 0.
    let private buildTagAndFieldComparison
        (s: UnionComparisonSupport)
        (b: IlBuilder)
        (loadOther: IlBuilder -> unit)
        (cLocal: int)
        (returnLabel: int)
        : unit =
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldfld s.TagField)
        loadOther b
        b.Add(ILInstr.Ldfld s.TagField)
        b.Add(ILInstr.Bin ILOpCode.Sub)
        b.Add(ILInstr.Stloc cLocal)
        b.Add(ILInstr.Ldloc cLocal)
        b.Add(ILInstr.Brtrue returnLabel)

        for (fieldHandle, fieldTy) in s.Fields do
            b.Add(ILInstr.Call(s.ComparerDefault fieldTy, 0, 1))
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            loadOther b
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Callvirt(s.ComparerCompare fieldTy, 3, 1))
            b.Add(ILInstr.Stloc cLocal)
            b.Add(ILInstr.Ldloc cLocal)
            b.Add(ILInstr.Brtrue returnLabel)

    /// `int CompareTo(Self other)` — the typed `IComparable<Self>::CompareTo` over the
    /// shared tag/field lex walk.
    let buildUnionCompareTo (isVt: bool) (s: UnionComparisonSupport) : ILBody =
        buildStructuralCompareTo isVt (buildTagAndFieldComparison s)

    /// `int CompareTo(object obj)` — the non-generic `IComparable::CompareTo`,
    /// delegating to the typed `CompareTo(Self)`.
    let buildUnionCompareToObj (isVt: bool) (s: UnionComparisonSupport) (typedCompareTo: EntityHandle) : ILBody =
        buildStructuralCompareToObj isVt s.SelfType s.SelfTy s.MismatchMessage s.ArgumentExceptionCtor typedCompareTo

    /// Mirror of `UnionComparisonSupport` for a record: the same shape minus the tag.
    type RecordComparisonSupport =
        {
            SelfType: EntityHandle
            SelfTy: FrozenType
            /// `(field handle, field type)` in declaration order.
            Fields: (EntityHandle * FrozenType) list
            ComparerDefault: FrozenType -> EntityHandle
            ComparerCompare: FrozenType -> EntityHandle
            ArgumentExceptionCtor: EntityHandle
            MismatchMessage: UserStringHandle
        }

    /// The field-by-field lex comparison shared by both record `CompareTo` entry points.
    /// The first non-zero result lands in `cLocal` and branches to `returnLabel`.
    let private buildRecordFieldComparison
        (s: RecordComparisonSupport)
        (b: IlBuilder)
        (loadOther: IlBuilder -> unit)
        (cLocal: int)
        (returnLabel: int)
        : unit =
        for (fieldHandle, fieldTy) in s.Fields do
            b.Add(ILInstr.Call(s.ComparerDefault fieldTy, 0, 1))
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldfld fieldHandle)
            loadOther b
            b.Add(ILInstr.Ldfld fieldHandle)
            b.Add(ILInstr.Callvirt(s.ComparerCompare fieldTy, 3, 1))
            b.Add(ILInstr.Stloc cLocal)
            b.Add(ILInstr.Ldloc cLocal)
            b.Add(ILInstr.Brtrue returnLabel)

    /// `int CompareTo(Self other)` — the typed `IComparable<Self>::CompareTo` over the
    /// shared field lex walk.
    let buildRecordCompareTo (isVt: bool) (s: RecordComparisonSupport) : ILBody =
        buildStructuralCompareTo isVt (buildRecordFieldComparison s)

    /// `int CompareTo(object obj)` — the non-generic `IComparable::CompareTo`,
    /// delegating to the typed `CompareTo(Self)`.
    let buildRecordCompareToObj (isVt: bool) (s: RecordComparisonSupport) (typedCompareTo: EntityHandle) : ILBody =
        buildStructuralCompareToObj isVt s.SelfType s.SelfTy s.MismatchMessage s.ArgumentExceptionCtor typedCompareTo

    // Each co-slot shim forwards through a `call`, not a `callvirt`, so the exact method
    // binds. The object arg is `ldarg.0`: an object reference for a class, a managed
    // pointer for a struct enumerator, which `call` on its own instance method takes.

    /// `IEnumerator IEnumerable.GetEnumerator()` — forwards to the capability's
    /// `GetEnumerator`, whose `IEnumerator`1<T>` return already IS an `IEnumerator`.
    let buildEnumerableGetEnumeratorCoSlot (capabilityGetEnumerator: EntityHandle) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Call(capabilityGetEnumerator, 1, 1))
        b.Add ILInstr.Ret
        b.Body

    /// `object IEnumerator.get_Current()` — forwards to the capability's `Current` and
    /// boxes its `'T` (`elemType`; the declaring typar `!0` for a generic enumerator).
    let buildEnumeratorCurrentCoSlot (capabilityCurrent: EntityHandle) (elemType: EntityHandle) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Call(capabilityCurrent, 1, 1))
        b.Add(ILInstr.Box elemType)
        b.Add ILInstr.Ret
        b.Body

    /// `void IEnumerator.Reset()` — no capability member to forward to: the pull protocol
    /// has no rewind, so it throws, as non-resettable BCL enumerators do.
    let buildEnumeratorResetCoSlot (notSupportedExceptionCtor: EntityHandle) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Newobj(notSupportedExceptionCtor, 0))
        b.Add ILInstr.Throw
        b.Body
