namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Vesper
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitPattern
open EmitExpr

/// The method and body builders codegen calls. Re-exports the helper modules' public
/// surface so callers keep using `Emit.*`.
module Emit =

    type Closure = EmitTypes.Closure
    type Capture = EmitTypes.Capture
    type CaptureFill = EmitTypes.CaptureFill
    type EmittedClosure = EmitTypes.EmittedClosure
    type ClosureToken = EmitTypes.ClosureToken
    let closureTokenWith = EmitTypes.closureTokenWith
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
    type LiftedLocal = EmitTypes.LiftedLocal
    type LiftedLocalRef = EmitTypes.LiftedLocalRef
    type Discovered = EmitClosures.Discovered
    type EmitContext = EmitTypes.EmitContext
    type StaticPreambleStep = EmitTypes.StaticPreambleStep
    type InstancePreambleStep = EmitTypes.InstancePreambleStep
    type CtorParam = EmitTypes.CtorParam
    type CtorParamStorage = EmitTypes.CtorParamStorage
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
    let bridgeStaticFnEscapes = EmitBridges.bridgeStaticFnEscapes
    let bridgeLiftedLocalEscapes = EmitBridges.bridgeLiftedLocalEscapes
    let collectStaticFns = EmitClosures.collectStaticFns
    let discoverClosures = EmitClosures.discoverClosures

    /// Build the `Main` body from the lowered decls: each top-level `let` binds a
    /// `Main` local, each effectful expression is emitted in source order, then
    /// `ldc.i4.0; ret`.
    let buildMain (ctx: EmitContext) (decls: Block<TastAccessor.DeclId>) : ILBody =
        let b = IlBuilder()
        let env = EmitEnv.ofContext ctx (Dictionary())

        for d in decls do
            match TastAccessor.declKind d with
            | DeclShape.Expression -> buildStatement env b (TastAccessor.declExpression d)
            | DeclShape.Type -> ()
            | DeclShape.LetGroup -> failwithf "Emit: a `let rec … and …` group survived `TastLower.lower` unsplit: %A" d
            | DeclShape.Let ->
                let dl = (TastAccessor.declLet d).Binding

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
        closure.ExtraParams |> Block.iteri (fun i ep -> args.[ep.Key] <- 2 + i)

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

    /// Build a static method's body: bind `leading`, then each flattened parameter, to its
    /// `ldarg` index (no `this`, so the first is `ldarg.0`), evaluate the body onto the
    /// stack, then `ret`.
    let private buildFlatStaticBody
        (ctx: EmitContext)
        (leading: Block<BoundVarId>)
        (flatParams: StaticParam list)
        (returnsVoid: bool)
        (body: TastAccessor.ExprId)
        : ILBody =
        let b = IlBuilder()
        let args = Dictionary<BoundVarId, int>()
        leading |> Block.iteri (fun i k -> args.[k] <- i)
        let offset = leading.Length
        flatParams |> List.iteri (fun i p -> args.[p.Slot] <- offset + i)
        let env = EmitEnv.ofContext ctx args

        // A destructuring tuple parameter: its `ldarg.i` holds the `ValueTuple`n`;
        // spill it to a local and `bindPattern` the leaves out of it.
        flatParams
        |> List.iteri (fun i p ->
            match p.Pat with
            | Some pat ->
                let slot = b.Local p.Ty
                b.Add(ILInstr.Ldarg(offset + i))
                b.Add(ILInstr.Stloc slot)
                bindPattern env b slot pat
            | None -> ()
        )

        buildExprAt (ExprPos.ofReturnsVoid returnsVoid) env b body
        b.Add ILInstr.Ret
        b.Body

    /// Build a static-method function's body over its flattened parameters alone.
    let buildStaticMethod (ctx: EmitContext) (fn: StaticFn) : ILBody =
        buildFlatStaticBody ctx Block.empty fn.Params.Flat fn.ReturnsVoid fn.Body

    /// Build a lifted local's body: its captures lead the flattened parameters.
    let buildLiftedLocal (ctx: EmitContext) (ll: LiftedLocal) : ILBody =
        buildFlatStaticBody ctx (Block.map fst ll.Captures) ll.Fn.Params.Flat ll.Fn.ReturnsVoid ll.Fn.Body

    /// Build a nominal member's body: an instance member's `this` is `ldarg.0` and its
    /// parameters `ldarg.1…`, a static member's parameters start at `ldarg.0`.
    /// `selfValueTy` is the declaring type when it is a VALUE type, `ValueNone` otherwise.
    let buildMember
        (ctx: EmitContext)
        (selfValueTy: FrozenType voption)
        (thisKey: BoundVarKeyG<BoundVarId> voption)
        (baseKey: BoundVarKeyG<BoundVarId> voption)
        (prms: Block<BoundVarKeyG<BoundVarId> * FrozenType>)
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
        |> Block.iteri (fun i (k, _) -> args.[BoundVarKey.identity k] <- baseIdx + i)
        // `this` as `SelfKey` too, so the struct `this`-pointer path recognises a self-call:
        // `ldarg.0` is already the byref `this` and must be loaded directly, because spilling
        // it to a value temp copies the struct and a mutating self-call would not persist.
        let env =
            { EmitEnv.create ctx (ValueOption.map BoundVarKey.identity thisKey) (Dictionary()) args with
                SelfValueType = selfValueTy
            }

        buildExprAt (ExprPos.ofReturnsVoid voidReturn) env b body
        b.Add ILInstr.Ret
        b.Body

    /// Build a secondary constructor body: run the `let`-preamble into locals, then
    /// chain the primary `.ctor` (`ldarg.0; <primaryArgs>; call instance void
    /// Self::.ctor`). No base-ctor call, because the primary performs it.
    let buildSecondaryCtor
        (ctx: EmitContext)
        (prms: Block<BoundVarKeyG<BoundVarId> * FrozenType>)
        (lets: Block<TastAccessor.CtorLet>)
        (primaryCtor: EntityHandle)
        (primaryArgs: Block<TastAccessor.ExprId>)
        : ILBody =
        let b = IlBuilder()
        let args = Dictionary<BoundVarId, int>()
        prms |> Block.iteri (fun i (k, _) -> args.[BoundVarKey.identity k] <- 1 + i)
        let env = EmitEnv.ofContext ctx args

        for l in lets do
            let slot = b.Local l.Type
            env.Slots.[BoundVarKey.identity l.BoundVar] <- slot
            buildExpr env b l.Init
            b.Add(ILInstr.Stloc slot)

        b.Add(ILInstr.Ldarg 0)

        for a in primaryArgs do
            buildExpr env b a

        b.Add(ILInstr.Call(primaryCtor, primaryArgs.Length + 1, 0))
        b.Add ILInstr.Ret
        b.Body

    /// Build the explicit field-init secondary ctor (`new(args) = { f = e; … }`): run
    /// the `let`-preamble, then `ldarg.0; <init>; stfld field` per initialiser, in
    /// source order. NO primary chain, so unlisted fields stay zero-initialised.
    let buildSecondaryCtorFieldInit
        (ctx: EmitContext)
        (prms: Block<BoundVarKeyG<BoundVarId> * FrozenType>)
        (lets: Block<TastAccessor.CtorLet>)
        (fieldInits: Block<EntityHandle * TastAccessor.ExprId>)
        : ILBody =
        let b = IlBuilder()
        let args = Dictionary<BoundVarId, int>()
        prms |> Block.iteri (fun i (k, _) -> args.[BoundVarKey.identity k] <- 1 + i)
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

    /// Build a class primary `.ctor`: chain the base ctor (a value type chains none),
    /// `stfld` each field-backed ctor param, then run the instance preamble. Base args
    /// read params as `ldarg.1…`; `this` is unusable until the chain returns.
    let buildClassPrimaryCtor
        (ctx: EmitContext)
        (chain: CtorChain)
        (thisKey: BoundVarKeyG<BoundVarId>)
        (baseArgParams: Block<BoundVarKeyG<BoundVarId> * FrozenType>)
        (ctorParams: Block<CtorParam>)
        (preamble: Block<InstancePreambleStep>)
        : ILBody =
        let b = IlBuilder()
        let args = Dictionary<BoundVarId, int>()
        args.[BoundVarKey.identity thisKey] <- 0

        baseArgParams
        |> Block.iteri (fun i (k, _) -> args.[BoundVarKey.identity k] <- 1 + i)
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

            b.Add(ILInstr.Call(baseCtor, baseArgs.Length + 1, 0))

        ctorParams
        |> Block.iteri (fun i p ->
            let arg = 1 + i

            match p.Storage with
            | CtorParamStorage.Field field ->
                b.Add(ILInstr.Ldarg 0)
                b.Add(ILInstr.Ldarg arg)
                b.Add(ILInstr.Stfld field)
            | CtorParamStorage.CtorLocal -> env.CtorLocals.[p.Name] <- CtorLocal.Arg arg
        )

        for step in preamble do
            match step with
            | InstancePreambleStep.StoreField(field, init) ->
                b.Add(ILInstr.Ldarg 0)
                buildExpr env b init
                b.Add(ILInstr.Stfld field)
            | InstancePreambleStep.Local(name, ty, init) ->
                let slot = b.Local ty
                buildExpr env b init
                b.Add(ILInstr.Stloc slot)
                env.CtorLocals.[name] <- CtorLocal.Slot slot
            | InstancePreambleStep.Run body -> buildStatement env b body

        b.Add ILInstr.Ret
        b.Body

    /// Build a `.cctor` body from a static preamble, in declaration order: a `let`
    /// initialiser `stsfld`ed into its backing field, a `static do` body run for
    /// effect. A `.cctor` is parameterless, so the env carries no args.
    let buildStaticCctor (ctx: EmitContext) (steps: Block<StaticPreambleStep>) : ILBody =
        let b = IlBuilder()
        let env = EmitEnv.ofContext ctx (Dictionary())

        for step in steps do
            match step with
            | StaticPreambleStep.Store(field, init) ->
                buildExpr env b init
                b.Add(ILInstr.Stsfld field)
            | StaticPreambleStep.Run body -> buildStatement env b body

        b.Add ILInstr.Ret
        b.Body

    /// `this.<field_i> = arg_(i+1)`, fields in declaration order = the ctor's parameter
    /// order. Every constructor body here ends this way.
    let private storeCtorArgs (b: IlBuilder) (fields: Block<EntityHandle>) : unit =
        fields
        |> Block.iteri (fun i field ->
            b.Add(ILInstr.Ldarg 0)
            b.Add(ILInstr.Ldarg(i + 1))
            b.Add(ILInstr.Stfld field)
        )

    /// Build a `.ctor` body that chains `baseCtor` with `baseArgs` pushed ahead of the
    /// call, then stores each of its own arguments into the matching field.
    let buildChainedCtor (baseCtor: EntityHandle) (baseArgs: Block<ILInstr>) (fields: Block<EntityHandle>) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Ldarg 0)
        baseArgs |> Block.iter b.Add
        b.Add(ILInstr.Call(baseCtor, baseArgs.Length + 1, 0))
        storeCtorArgs b fields
        b.Add ILInstr.Ret
        b.Body

    /// Build a `.cctor` that fills each `static initonly` field once, in the given order.
    /// Each entry's instructions leave the instance to cache on the stack; a cached
    /// closure, a string/mixed enum case and a union's nullary case all take this shape.
    let buildCachedFieldCctor (entries: Block<Block<ILInstr> * EntityHandle>) : ILBody =
        let b = IlBuilder()

        for (push, cachedField) in entries do
            push |> Block.iter b.Add
            b.Add(ILInstr.Stsfld cachedField)

        b.Add ILInstr.Ret
        b.Body

    /// A value-type (`[<Struct>]`) primary constructor: store each ctor param into its
    /// backing field and return. NO chained base `.ctor`, because `System.ValueType` has
    /// none accessible. `ldarg 0` is the managed pointer `newobj` passes (`&temp`).
    let buildStructCtor (fields: Block<EntityHandle>) : ILBody =
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

    /// Build an instance setter over one field: `ldarg.0; ldarg.1; stfld; ret`.
    let buildFieldSetter (field: EntityHandle) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldarg 1)
        b.Add(ILInstr.Stfld field)
        b.Add ILInstr.Ret
        b.Body

    /// Build an instance getter over a field chain: `ldarg.0`, `ldfld` each field of the
    /// non-empty `path` in turn, then `castclass` to `castTo` where the chain ends on a slot
    /// stored erased to `object`.
    let buildFieldPathGetter (path: Block<EntityHandle>) (castTo: EntityHandle voption) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Ldarg 0)

        for h in path do
            b.Add(ILInstr.Ldfld h)

        match castTo with
        | ValueSome token -> b.Add(ILInstr.Castclass token)
        | ValueNone -> ()

        b.Add ILInstr.Ret
        b.Body

    /// Build a struct union's `GetPayload_<Case>`: copy `_payload` off `this` into a fresh
    /// `Payload_<Case>` view.
    let buildUnionCaseViewGetter (payloadField: EntityHandle) (viewCtor: EntityHandle) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Ldarg 0)
        b.Add(ILInstr.Ldfld payloadField)
        b.Add(ILInstr.Newobj(viewCtor, 1))
        b.Add ILInstr.Ret
        b.Body

    /// Build a NULLARY case's static factory: the singleton the union's `.cctor`
    /// constructed, so construction allocates nothing.
    let buildUnionSingletonFactory (singletonField: EntityHandle) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.Ldsfld singletonField)
        b.Add ILInstr.Ret
        b.Body

    /// Build a nullary `[<Struct>]` union case's static factory where the union declares no
    /// payload: push the discriminant `tag` and `newobj` the `.ctor(tag)`.
    let buildStructUnionTagFactory (ctorRef: EntityHandle) (tag: int) : ILBody =
        let b = IlBuilder()
        b.Add(ILInstr.LdcI4 tag)
        b.Add(ILInstr.Newobj(ctorRef, 1))
        b.Add ILInstr.Ret
        b.Body

    /// One store a struct union case's factory makes into the `Payload` it builds: the
    /// factory parameter `Arg`, written by `ldflda` through each struct-typed field of `Via`
    /// from the `Payload` value and `stfld` into `Field`.
    type PayloadStore =
        {
            Arg: int
            Via: Block<EntityHandle>
            Field: EntityHandle
        }

    /// Build a `[<Struct>]` union case's static factory: zero a `Payload` local with
    /// `initobj`, write each of this case's parameters into its placement, then push the
    /// discriminant `tag` and the payload and `newobj` the union's `.ctor(tag, payload)`.
    let buildStructUnionPayloadFactory
        (ctorRef: EntityHandle)
        (tag: int)
        (payloadTy: FrozenType)
        (payloadToken: EntityHandle)
        (stores: Block<PayloadStore>)
        : ILBody =
        let b = IlBuilder()
        let payload = b.Local payloadTy
        b.Add(ILInstr.Ldloca payload)
        b.Add(ILInstr.Initobj payloadToken)

        for s in stores do
            b.Add(ILInstr.Ldloca payload)

            for via in s.Via do
                b.Add(ILInstr.Ldflda via)

            b.Add(ILInstr.Ldarg s.Arg)
            b.Add(ILInstr.Stfld s.Field)

        b.Add(ILInstr.LdcI4 tag)
        b.Add(ILInstr.Ldloc payload)
        b.Add(ILInstr.Newobj(ctorRef, 2))
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
