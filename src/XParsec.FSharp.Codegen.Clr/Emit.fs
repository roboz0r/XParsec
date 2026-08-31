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

    /// `this.<field_i> = arg_(i+1)`, fields in declaration order = the ctor's parameter
    /// order. Every constructor body here ends this way.
    let private storeCtorArgs (b: IlBuilder) (fields: EntityHandle list) : unit =
        fields
        |> List.iteri (fun i field ->
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldarg(i + 1))
            b.Add(ILInstr.Stfld field)
        )

    /// Build a `.ctor` body that chains `baseCtor` with `baseArgs` pushed ahead of the
    /// call, then stores each of its own arguments into the matching field. A closure's
    /// captures, a record's fields and a hierarchy union case's payload all take this
    /// shape; `baseArgs` is the union case's `ldc.i4 <tag>` where the base declares a
    /// `_tag`, and empty everywhere else.
    let buildChainedCtor (baseCtor: EntityHandle) (baseArgs: ILInstr list) (fields: EntityHandle list) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Ldarg 0)
        baseArgs |> List.iter b.Add
        b.Add(ILInstr.Call(baseCtor, List.length baseArgs + 1, 0))
        storeCtorArgs b fields
        b.Add ILInstr.Ret
        b.Body

    /// Build a `.cctor` that fills each `static initonly` field once, in the given order.
    /// Each entry's instruction list leaves the instance to cache on the stack; a cached
    /// closure, a string/mixed enum case and a union's nullary case all take this shape.
    let buildCachedFieldCctor (entries: (ILInstr list * EntityHandle) list) : ILBody =
        let b = IlBuilder()

        for (push, cachedField) in entries do
            push |> List.iter b.Add
            b.Add(ILInstr.Stsfld cachedField)

        b.Add ILInstr.Ret
        b.Body

    /// A value-type (`[<Struct>]`) primary constructor: store each ctor param into its
    /// backing field and return. NO chained base `.ctor`, because `System.ValueType` has
    /// none accessible. `ldarg 0` is the managed pointer `newobj` passes (`&temp`).
    let buildStructCtor (fields: EntityHandle list) : ILBody =
        let b = IlBuilder()
        storeCtorArgs b fields
        b.Add ILInstr.Ret
        b.Body

    /// Build a static factory that forwards its parameters whole: `newobj` `ctor` over
    /// them in declaration order. A hierarchy case's `.ctor` takes exactly its factory's
    /// parameters, as does a single-case union's flat `.ctor`.
    let buildUnionCaseFactory (ctor: EntityHandle) (arity: int) : ILBody =
        let b = IlBuilder()

        for i in 0 .. arity - 1 do
            b.Add(ILInstr.Ldarg i)

        b.Add(ILInstr.Newobj(ctor, arity))
        b.Add ILInstr.Ret
        b.Body

    /// Build an instance getter over one field: `ldarg.0; ldfld; ret`.
    let buildFieldGetter (field: EntityHandle) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldfld field)
        b.Add ILInstr.Ret
        b.Body

    /// Build a NULLARY case's static factory: the singleton the union's `.cctor`
    /// constructed, so construction allocates nothing.
    let buildUnionSingletonFactory (singletonField: EntityHandle) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Ldsfld singletonField)
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
