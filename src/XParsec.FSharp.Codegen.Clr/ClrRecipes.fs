namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// Call / constructor / format recipes (`printfn`, function `Invoke`, list cons/nil, `List.fold`, the
/// `Vesper.Formatter` write-through handler) plus the synthesised structural equality / comparison
/// member refs (`EqualityComparer`1` / `Comparer`1` / `HashCode` / `IEquatable`1` / `IComparable`1`).
type internal ClrRecipes(env: ClrEnv, enc: ClrEncoder) =
    let ctx = env.Ctx
    let symbols = env.Symbols
    let markFSharpCoreDep c = env.MarkFSharpCoreDep c

    let recoverOpenTypars declTyparArity methodTyparArity openT instT =
        enc.RecoverOpenTypars(declTyparArity, methodTyparArity, openT, instT)

    let encodeType te t = enc.EncodeType(te, t)

    /// `p1 -> … -> pN -> ret` peels to `([p1; …; pN], ret)`.
    let uncurryFrozen (t: FrozenType) : FrozenType list * FrozenType = TastLower.peelFunDomains -1 t

    let encodeListOf te inner = enc.EncodeListOf(te, inner)
    let methodSpec handle args = enc.MethodSpec(handle, args)
    let eTextWriter = env.ETextWriter
    let eStringBuilder = env.EStringBuilder
    let eFun2 () = env.EFun2()
    let eVesperList1 = env.EVesperList1
    let eListModule = env.EListModule
    let eFormatter = env.EFormatter
    let eConsole = env.EConsole
    let eEqualityComparer1 = env.EEqualityComparer1
    let eHashCode = env.EHashCode
    let eEquatable1 = env.EEquatable1
    let eComparer1 = env.EComparer1
    let eComparable1 = env.EComparable1

    /// `Vesper.Fun`2<a,b>::Invoke(!0) : !1` as a `MemberRef` token — applying a function value.
    /// `Fun` is an interface, so the dispatch stays `callvirt`.
    let funInvokeRef (a: FrozenType) (b: FrozenType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eFun2 (), 2, false)
        encodeType (g.AddArgument()) a
        encodeType (g.AddArgument()) b
        let typeSpec = ctx.TypeSpec tsB

        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().GenericTypeParameter(1)),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().GenericTypeParameter(0))
            )

        toEntity (ctx.MemberRef(toEntity typeSpec, "Invoke", msig))

    let emitInvoke (funcTy: FrozenType) : CallRecipe =
        match funcTy with
        | FTFun(a, b) ->
            let invokeRef = funInvokeRef a b

            {
                Emit =
                    fun il ->
                        il.Encoder.OpCode ILOpCode.Callvirt
                        il.Encoder.Token invokeRef
                Arity = CallArity.Flat 2
                Pushes = 1
            }
        | other -> failwithf "ClrProvider: cannot invoke non-function type: %A" other

    /// `FSharpList`1<elem>` as a member-ref parent `TypeSpec`.
    let listTypeSpec (elem: FrozenType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        encodeListOf te (fun arg -> encodeType arg elem)
        toEntity (ctx.TypeSpec tsB)

    let emitListCons (elem: FrozenType) : CallRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Collections.FSharpList`1.Cons"
        let typeSpec = listTypeSpec elem
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                2,
                (fun (ret: ReturnTypeEncoder) -> encodeListOf (ret.Type()) (fun a -> a.GenericTypeParameter(0))),
                (fun (pars: ParametersEncoder) ->
                    pars.AddParameter().Type().GenericTypeParameter(0)
                    encodeListOf (pars.AddParameter().Type()) (fun a -> a.GenericTypeParameter(0))
                )
            )

        let consRef = toEntity (ctx.MemberRef(typeSpec, "Cons", msig))

        {
            Emit = fun il -> il.Encoder.Call consRef
            Arity = CallArity.Flat 2
            Pushes = 1
        }

    let emitListNil (elem: FrozenType) : CallRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Collections.FSharpList`1.get_Empty"
        let typeSpec = listTypeSpec elem
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                0,
                (fun (ret: ReturnTypeEncoder) -> encodeListOf (ret.Type()) (fun a -> a.GenericTypeParameter(0))),
                (fun (_: ParametersEncoder) -> ())
            )

        let emptyRef = toEntity (ctx.MemberRef(typeSpec, "get_Empty", msig))

        {
            Emit = fun il -> il.Encoder.Call emptyRef
            Arity = CallArity.Flat 0
            Pushes = 1
        }

    /// `Vesper.Collections.List`1<elem>` as a member-ref parent `TypeSpec`.
    let vesperListTypeSpec (elem: FrozenType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eVesperList1.Value, 1, false)
        encodeType (g.AddArgument()) elem
        toEntity (ctx.TypeSpec tsB)

    let encodeVesperListOfTypar (te: SignatureTypeEncoder) : unit =
        let g = te.GenericInstantiation(eVesperList1.Value, 1, false)
        g.AddArgument().GenericTypeParameter(0)

    let emitVesperListCons (elem: FrozenType) : CallRecipe =
        let typeSpec = vesperListTypeSpec elem
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                2,
                (fun (ret: ReturnTypeEncoder) -> encodeVesperListOfTypar (ret.Type())),
                (fun (pars: ParametersEncoder) ->
                    pars.AddParameter().Type().GenericTypeParameter(0)
                    encodeVesperListOfTypar (pars.AddParameter().Type())
                )
            )

        let consRef = toEntity (ctx.MemberRef(typeSpec, "Cons", msig))

        {
            Emit = fun il -> il.Encoder.Call consRef
            Arity = CallArity.Flat 2
            Pushes = 1
        }

    let emitVesperListEmpty (elem: FrozenType) : CallRecipe =
        let typeSpec = vesperListTypeSpec elem
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(0, (fun (ret: ReturnTypeEncoder) -> encodeVesperListOfTypar (ret.Type())), (fun _ -> ()))

        let emptyRef = toEntity (ctx.MemberRef(typeSpec, "Empty", msig))

        {
            Emit = fun il -> il.Encoder.Call emptyRef
            Arity = CallArity.Flat 0
            Pushes = 1
        }

    /// The `_tag : int32` discriminator field on the referenced cons-list
    /// `Vesper.Collections.List`1<elem>`, read by a cross-package `match` on `[]` / `::`.
    /// Its contract keeps op-form names (`op_Nil` / `op_ColonColon`), unknown to the generic path.
    let emitVesperListTagField (elem: FrozenType) : EntityHandle =
        let typeSpec = vesperListTypeSpec elem
        let s = BlobBuilder()
        encodeType (BlobEncoder(s).FieldSignature()) (FTConst(RuntimeNames.intKey, EqArray.empty))
        toEntity (ctx.MemberRef(typeSpec, "_tag", s))

    /// One `Cons_<fieldIndex>` payload field on the referenced cons-list (`Cons_0` = head
    /// `'T`, `Cons_1` = tail `List<'T>`), instantiated at `elem`. The blob encodes the
    /// field's *open* (declaring-typar) type, matching the emitted field definition.
    let emitVesperListConsField (elem: FrozenType) (fieldIndex: int) : EntityHandle =
        let typeSpec = vesperListTypeSpec elem
        let s = BlobBuilder()
        let fte = BlobEncoder(s).FieldSignature()

        match fieldIndex with
        | 0 -> fte.GenericTypeParameter(0) // Cons_0 : 'T
        | _ -> encodeVesperListOfTypar fte // Cons_1 : List<'T>

        toEntity (ctx.MemberRef(typeSpec, sprintf "Cons_%d" fieldIndex, s))

    /// `Vesper.Fun`2<a,b>` as a `TypeSpec` — the interface a synthesised closure *implements*.
    let funInterfaceSpec (a: FrozenType) (b: FrozenType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eFun2 (), 2, false)
        encodeType (g.AddArgument()) a
        encodeType (g.AddArgument()) b
        toEntity (ctx.TypeSpec tsB)

    /// `Vesper.Fun`(len)<tys…>` as a `TypeSpec` — the FLAT interface a flat value-struct
    /// closure of param-arity `len-1` implements. `tys` is the flat params followed by the
    /// result, so `len` picks the entity (`3`⇒`Fun`3`, `4`⇒`Fun`4`, `5`⇒`Fun`5`).
    let flatFunInterfaceSpecN (tys: FrozenType list) : EntityHandle =
        let len = List.length tys
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(env.FlatFunEntity len, len, false)

        for ty in tys do
            encodeType (g.AddArgument()) ty

        toEntity (ctx.TypeSpec tsB)

    /// `List.fold folder state xs` over the *Vesper* list — a `call` to `fold` compiled into
    /// `Vesper.List.dll`. Folder, state, list are already on the stack (ArgCount = 3); the call
    /// leaves the `'State` result. No FSharp.Core dep.
    let emitFold (fnTy: FrozenType) : CallRecipe =
        let elemTy, stateTy =
            match fnTy with
            | FTFun(FTFun(state, FTFun(t, _)), _) -> t, state
            | other -> failwithf "ClrProvider: List.fold has unexpected type %A" other

        // `fold`'s two method typars are self-describing `FTTypar(Method, i)` nodes (`'State` ⇒
        // `!!0`, `'T` ⇒ `!!1`), which `encodeType` maps to `!!i`, so no ambient typar window is needed.
        let sT = FTTypar(TyparAxis.Method, 0)
        let eT = FTTypar(TyparAxis.Method, 1)
        let folderT = FTFun(sT, FTFun(eT, sT))
        let listT = FTUnion(RuntimeNames.vesperListKey, EqArray.singleton eT)

        let foldSig =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(genericParameterCount = 2, isInstanceMethod = false)
                .Parameters(
                    3,
                    (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) sT),
                    (fun (pars: ParametersEncoder) ->
                        encodeType (pars.AddParameter().Type()) folderT
                        encodeType (pars.AddParameter().Type()) sT
                        encodeType (pars.AddParameter().Type()) listT
                    )
                )

            s

        let foldRef = ctx.MemberRef(eListModule.Value, "fold", foldSig)

        let inst = BlobBuilder()
        let specEnc = BlobEncoder(inst).MethodSpecificationSignature(2)
        encodeType (specEnc.AddArgument()) stateTy
        encodeType (specEnc.AddArgument()) elemTy
        let foldSpec = toEntity (ctx.MethodSpec(toEntity foldRef, inst))

        {
            Emit = fun il -> il.Encoder.Call foldSpec
            Arity = CallArity.Flat 3
            Pushes = 1
        }

    /// The seq-interface witness for a referenced-package nominal type: pick the shape's
    /// `FrozenInterfaces` template matching `ifaceKey` and instantiate it at this object arg
    /// (`FTTypar(Declaring, i) := args.[i]`). Direct-declared interfaces only.
    let tryExternalInterfaceWitness (objArgTy: FrozenType) (ifaceKey: TypeKey) : EqArray<FrozenType> voption =
        match objArgTy with
        | FTClass(rKey, rArgs)
        | FTUnion(rKey, rArgs)
        | FTRecord(rKey, rArgs) ->
            match env.LookupTypeByKey(SymbolKey.Type rKey) with
            | ValueSome(ExternalTypeShape.Class shape) ->
                pickInterfaceWitness ifaceKey (rArgs.AsSpan().ToArray()) shape.FrozenInterfaces
            | _ -> ValueNone
        | _ -> ValueNone

    /// A `call` to a static method `<ns>::<name>` compiled into a referenced package by our own
    /// backend; `fnTy` is the *use-site* curried type. `binding` is taken WHOLE rather than rebuilt
    /// from `(module, name)`: a top-level `let` is held by a namespace, which a pair cannot spell.
    let emitExternalCall (binding: BindingKey) (fnTy: FrozenType) : CallRecipe voption =
        let name = binding.Name
        let valueKey = SymbolKey.Binding binding
        let compiledFullName = SymbolKeyOps.qualifiedName valueKey

        match symbols.TryLookupOpenSignature valueKey with
        | ValueNone -> ValueNone
        | ValueSome openSig ->
            // The open curried signature *template*: its method typars are already
            // `FTTypar(Method, i)` and its nominal type constructors kind-correct (`'T option` ⇒ `FTUnion`),
            // so it encodes and recovers against the producer's emitted signature unchanged.
            let methodTyparArity = openSig.MethodTyparArity

            // Peel exactly `n` top-level `->` groups, one per SOURCE argument group. Unlike
            // `uncurryFrozen`, which peels every `->`, this stops at the source arity, so a
            // function-typed RESULT stays whole.
            let peelN n t = TastLower.peelFunDomains n t

            // A `unit` source result is emitted genuine CLR `void`, so encoding a
            // `System.ValueTuple` return here would miss the void method (`MissingMethodException`).
            let isUnitReturn t =
                match t with
                | FTUnit -> true
                | _ -> false

            // The flat parameter vector and the void decision come from the SOURCE `ValRepr`
            // groups (tuple-flatten, lone-`unit`-erase); the TYPES come from peeling the open
            // template. Without a `ValRepr`, fall back to the all-curried shape.
            let flatParamTys, openRetTy, returnsVoid, recipeGroups =
                match openSig.ValRepr with
                | ValueSome vr ->
                    let groups = vr.Groups
                    let n = List.length groups
                    let groupParamTys, retTy = peelN n openSig.Signature

                    if List.length groupParamTys <> n then
                        failwithf
                            "emitExternalCall: contract for %s declares %d source groups but its template has only %d"
                            compiledFullName
                            n
                            (List.length groupParamTys)

                    TastLower.flattenGroupShape groups groupParamTys, retTy, isUnitReturn retTy, ValueSome groups
                | ValueNone ->
                    let ps, r = uncurryFrozen openSig.Signature
                    ps, r, isUnitReturn r, ValueNone

            // The open method-ref signature: parameters + return encoded with the method typars as
            // `!!i`, as the producer's static-method emit uses.
            let msig =
                let s = BlobBuilder()

                BlobEncoder(s)
                    .MethodSignature(genericParameterCount = methodTyparArity, isInstanceMethod = false)
                    .Parameters(
                        List.length flatParamTys,
                        (fun (ret: ReturnTypeEncoder) ->
                            if returnsVoid then
                                ret.Void()
                            else
                                encodeType (ret.Type()) openRetTy
                        ),
                        (fun (pars: ParametersEncoder) ->
                            for p in flatParamTys do
                                encodeType (pars.AddParameter().Type()) p
                        )
                    )

                s

            // A function homed in this compilation's OWN assembly resolves to its local `MethodDef`;
            // on a miss, mint an external `MemberRef` scoped by `openSig.Origin` (the key alone
            // carries no assembly). Only a module gives that ref a declaring type, hence `ValueNone`.
            let callBaseOpt =
                match env.LocalModuleFns.TryGetValue valueKey with
                | true, defHandle -> ValueSome defHandle
                | _ ->
                    match binding.Decl with
                    | ModuleContainer.InNamespace _ -> ValueNone
                    | ModuleContainer.InModule declModule ->
                        let parent = env.ExternalModuleRef(openSig.Origin, declModule)
                        ValueSome(toEntity (ctx.MemberRef(parent, name, msig)))

            let callHandleOf (callBase: EntityHandle) =
                if methodTyparArity = 0 then
                    callBase
                elif List.isEmpty openSig.Constraints then
                    // Match the open template's `FTTypar(Method, i)` against the call's concrete
                    // type, recovering each method arg by index. No constraint typars here, so
                    // every method typar is signature-reachable.
                    let _, methodArgs = recoverOpenTypars 0 methodTyparArity openSig.Signature fnTy

                    methodSpec callBase methodArgs
                else
                    // The template carries a phantom constraint typar (`fold`'s enumerator `'E` in
                    // `'S :> IStructSeq<'T,'E>`) that appears in no param or result, so
                    // `recoverOpenTypars` would fail loud: recover partially, then solve it.
                    let instArr =
                        TastLower.matchInstantiationPartial methodTyparArity [ openSig.Signature ] [ fnTy ]

                    TastLower.solvePhantomTypars
                        methodTyparArity
                        openSig.Constraints
                        tryExternalInterfaceWitness
                        instArr

                    let methodArgs =
                        [
                            for i in 0 .. methodTyparArity - 1 ->
                                match instArr.[i] with
                                | ValueSome t -> t
                                | ValueNone ->
                                    failwithf
                                        "emitExternalCall: could not infer instantiation for method type parameter %d of %s (phantom-typar solve found no witness)"
                                        i
                                        compiledFullName
                        ]

                    methodSpec callBase methodArgs

            // `Grouped` carries the SOURCE grouping (the walker consumes `groups.Length`
            // arguments, flattening each) AND the FLAT pop count. They diverge for a
            // non-`GSimple` group: a tupled group is N flat from ONE argument, a lone `()` zero.
            let arity =
                match recipeGroups with
                | ValueSome groups -> CallArity.Grouped(groups, List.length flatParamTys)
                | ValueNone -> CallArity.Flat(List.length flatParamTys)

            callBaseOpt
            |> ValueOption.map (fun callBase ->
                let callHandle = callHandleOf callBase

                {
                    Emit = fun il -> il.Encoder.Call callHandle
                    Arity = arity
                    Pushes = if returnsVoid then 0 else 1
                }
            )

    /// Member refs + the `AppendFormatted<T>` factory for lowering a `TExpr.Format` to the
    /// `Vesper.Formatter` write-through handler. All members hang off the non-generic
    /// `Formatter` value type, so the parent is a plain `TypeRef`.
    let buildFormatHandles () : FormatHandles =
        let ctorWriter =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    3,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        pars.AddParameter().Type().Int32()
                        pars.AddParameter().Type().Int32()
                        pars.AddParameter().Type().Type(eTextWriter.Value, false)
                    )
                )

            toEntity (ctx.MemberRef(eFormatter.Value, ".ctor", s))

        let ctorBuilder =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    3,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        pars.AddParameter().Type().Int32()
                        pars.AddParameter().Type().Int32()
                        pars.AddParameter().Type().Type(eStringBuilder.Value, false)
                    )
                )

            toEntity (ctx.MemberRef(eFormatter.Value, ".ctor", s))

        let ctorString =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    2,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        pars.AddParameter().Type().Int32()
                        pars.AddParameter().Type().Int32()
                    )
                )

            toEntity (ctx.MemberRef(eFormatter.Value, ".ctor", s))

        let appendLiteral =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    1,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().String())
                )

            toEntity (ctx.MemberRef(eFormatter.Value, "AppendLiteral", s))

        let flush =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

            toEntity (ctx.MemberRef(eFormatter.Value, "Flush", s))

        let toStringAndClear =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    0,
                    (fun (ret: ReturnTypeEncoder) -> ret.Type().String()),
                    (fun (_: ParametersEncoder) -> ())
                )

            toEntity (ctx.MemberRef(eFormatter.Value, "ToStringAndClear", s))

        // (value, alignment); the walker always pushes the alignment (0 ⇒ none).
        let appendMember (name: string) (param0: SignatureTypeEncoder -> unit) : EntityHandle =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    2,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        param0 (pars.AddParameter().Type())
                        pars.AddParameter().Type().Int32()
                    )
                )

            toEntity (ctx.MemberRef(eFormatter.Value, name, s))

        let appendBool = appendMember "AppendBool" (fun te -> te.Boolean())
        let appendOctal = appendMember "AppendOctal" (fun te -> te.Int32())
        let appendUnsigned = appendMember "AppendUnsigned" (fun te -> te.UInt32())
        // `%08o` / `%05u`: same `(value, int32 width)` shape as the space-pad members.
        let appendZeroPaddedOctal =
            appendMember "AppendZeroPaddedOctal" (fun te -> te.Int32())

        let appendZeroPaddedUnsigned =
            appendMember "AppendZeroPaddedUnsigned" (fun te -> te.UInt32())

        // `instance void AppendZeroPaddedFloat(float64, string, int32)` — `%0w.pf`
        // (value, "F<prec>" body, field width).
        let appendZeroPaddedFloat =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    3,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        pars.AddParameter().Type().Double()
                        pars.AddParameter().Type().String()
                        pars.AddParameter().Type().Int32()
                    )
                )

            toEntity (ctx.MemberRef(eFormatter.Value, "AppendZeroPaddedFloat", s))

        // `instance void AppendRightZeroPaddedFloat(float64, string, int32)` — `%-0w.pf`
        // (value, "F<prec>" body, field width).
        let appendRightZeroPaddedFloat =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    3,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        pars.AddParameter().Type().Double()
                        pars.AddParameter().Type().String()
                        pars.AddParameter().Type().Int32()
                    )
                )

            toEntity (ctx.MemberRef(eFormatter.Value, "AppendRightZeroPaddedFloat", s))

        // `instance void AppendForcedSignZeroPaddedFloat(float64, string, int32, bool)` —
        // `%+0w.pf`/`% 0w.pf` (value, "F<prec>" body, field width, space flag).
        let appendForcedSignZeroPaddedFloat =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    4,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        pars.AddParameter().Type().Double()
                        pars.AddParameter().Type().String()
                        pars.AddParameter().Type().Int32()
                        pars.AddParameter().Type().Boolean()
                    )
                )

            toEntity (ctx.MemberRef(eFormatter.Value, "AppendForcedSignZeroPaddedFloat", s))

        // `instance void AppendDynamicPrecisionFloat(float64, char, int32, int32)` —
        // `%.*f`/`%*.*f`/`%.*e`/`%.*g` (value, type letter, runtime precision, field width).
        let appendDynamicPrecisionFloat =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    4,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        pars.AddParameter().Type().Double()
                        pars.AddParameter().Type().Char()
                        pars.AddParameter().Type().Int32()
                        pars.AddParameter().Type().Int32()
                    )
                )

            toEntity (ctx.MemberRef(eFormatter.Value, "AppendDynamicPrecisionFloat", s))

        // `instance void AppendDynamicPrecisionSignedFloat(float64, char, int32, int32, bool)` —
        // `%+.*f`/`% .*f`/`%+*.*f` (value, 'f', runtime precision, field width, space flag).
        let appendDynamicPrecisionSignedFloat =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    5,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        pars.AddParameter().Type().Double()
                        pars.AddParameter().Type().Char()
                        pars.AddParameter().Type().Int32()
                        pars.AddParameter().Type().Int32()
                        pars.AddParameter().Type().Boolean()
                    )
                )

            toEntity (ctx.MemberRef(eFormatter.Value, "AppendDynamicPrecisionSignedFloat", s))

        let consoleGetter (name: string) : EntityHandle =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = false)
                .Parameters(
                    0,
                    (fun (ret: ReturnTypeEncoder) -> ret.Type().Type(eTextWriter.Value, false)),
                    (fun (_: ParametersEncoder) -> ())
                )

            toEntity (ctx.MemberRef(eConsole.Value, name, s))

        // The overload is selected by which optional params are present (alignment before format,
        // matching the C# declaration), then `<T>` is bound.
        let appendFormatted (ty: FrozenType, hasAlignment: bool, hasFormat: bool) : EntityHandle =
            let paramCount = 1 + (if hasAlignment then 1 else 0) + (if hasFormat then 1 else 0)
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(genericParameterCount = 1, isInstanceMethod = true)
                .Parameters(
                    paramCount,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        pars.AddParameter().Type().GenericMethodTypeParameter(0)

                        if hasAlignment then
                            pars.AddParameter().Type().Int32()

                        if hasFormat then
                            pars.AddParameter().Type().String()
                    )
                )

            let memberRef = ctx.MemberRef(eFormatter.Value, "AppendFormatted", s)
            let inst = BlobBuilder()
            let specEnc = BlobEncoder(inst).MethodSpecificationSignature(1)
            encodeType (specEnc.AddArgument()) ty
            toEntity (ctx.MethodSpec(toEntity memberRef, inst))

        // `instance void AppendStructured<T>(!!0, int32, int32)` — `%A`. A member ref to the
        // open generic method + a `MethodSpec` binding `<T = ty>` per hole; the two `int32`s
        // are the print-width then the print-size budget the walker pushes.
        let appendStructured (ty: FrozenType) : EntityHandle =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(genericParameterCount = 1, isInstanceMethod = true)
                .Parameters(
                    3,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) ->
                        pars.AddParameter().Type().GenericMethodTypeParameter(0)
                        pars.AddParameter().Type().Int32()
                        pars.AddParameter().Type().Int32()
                    )
                )

            let memberRef = ctx.MemberRef(eFormatter.Value, "AppendStructured", s)
            let inst = BlobBuilder()
            let specEnc = BlobEncoder(inst).MethodSpecificationSignature(1)
            encodeType (specEnc.AddArgument()) ty
            toEntity (ctx.MethodSpec(toEntity memberRef, inst))

        // `static int32 M(int32)` — the star-width guard / clamp helpers.
        let staticIntToInt (name: string) : EntityHandle =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = false)
                .Parameters(
                    1,
                    (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                    (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().Int32())
                )

            toEntity (ctx.MemberRef(eFormatter.Value, name, s))

        {
            HandlerLocal = FTConst(ClrSinkKeys.formatter, EqArray.empty)
            CtorWriter = ctorWriter
            CtorBuilder = ctorBuilder
            CtorString = ctorString
            AppendLiteral = appendLiteral
            Flush = flush
            ToStringAndClear = toStringAndClear
            ConsoleOut = consoleGetter "get_Out"
            ConsoleError = consoleGetter "get_Error"
            AppendFormatted = appendFormatted
            AppendBool = appendBool
            AppendOctal = appendOctal
            AppendUnsigned = appendUnsigned
            AppendZeroPaddedOctal = appendZeroPaddedOctal
            AppendZeroPaddedUnsigned = appendZeroPaddedUnsigned
            AppendZeroPaddedFloat = appendZeroPaddedFloat
            AppendRightZeroPaddedFloat = appendRightZeroPaddedFloat
            AppendForcedSignZeroPaddedFloat = appendForcedSignZeroPaddedFloat
            AppendDynamicPrecisionFloat = appendDynamicPrecisionFloat
            AppendDynamicPrecisionSignedFloat = appendDynamicPrecisionSignedFloat
            AppendStructured = appendStructured
            GuardTotalWidth = staticIntToInt "GuardTotalWidth"
            ClampWidth = staticIntToInt "ClampWidth"
            NormalizePrecision = staticIntToInt "NormalizePrecision"
        }

    // Resolved per call: Core's own `TypeDef` when compiling Core, else the `TypeRef`
    // through Core's `AssemblyRef`.
    let eFormatSink () = env.EFormatSink()
    let eStructuralFormattable () = env.EStructuralFormattable()

    /// The `instance void` `Vesper.IFormatSink` member refs a synthesised `Format` body
    /// `callvirt`s around the type's fields. Type-independent, so built once.
    let formatSinkHandles =
        lazy
            (let sinkMember (name: string) (paramCount: int) (param0: SignatureTypeEncoder -> unit) : EntityHandle =
                let s = BlobBuilder()

                BlobEncoder(s)
                    .MethodSignature(isInstanceMethod = true)
                    .Parameters(
                        paramCount,
                        (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                        (fun (pars: ParametersEncoder) ->
                            if paramCount > 0 then
                                param0 (pars.AddParameter().Type())
                        )
                    )

                toEntity (ctx.MemberRef(eFormatSink (), name, s))

             let nullary (name: string) = sinkMember name 0 ignore

             {
                 Text = sinkMember "Text" 1 (fun te -> te.String())
                 Line = nullary "Line"
                 SoftBreak = nullary "SoftBreak"
                 BeginGroup = nullary "BeginGroup"
                 EndGroup = nullary "EndGroup"
                 BeginNest = sinkMember "BeginNest" 1 (fun te -> te.Int32())
                 EndNest = nullary "EndNest"
                 BeginRecord = nullary "BeginRecord"
                 Field = sinkMember "Field" 1 (fun te -> te.String())
                 EndRecord = nullary "EndRecord"
                 BeginCase = sinkMember "BeginCase" 1 (fun te -> te.String())
                 EndCase = nullary "EndCase"
                 Child = sinkMember "Child" 1 (fun te -> te.Object())
             })

    /// `instance void Format(IFormatSink)` — the synthesised `IStructuralFormattable.Format`
    /// member. The param is the bare `Vesper.IFormatSink` `TypeRef`, encoded identically to
    /// the interface slot it binds to, so name + signature match.
    let structuralFormatSignature () : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().Type(eFormatSink (), false))
            )

        s

    let equalityComparerTypeSpec (elem: FrozenType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eEqualityComparer1.Value, 1, false)
        encodeType (g.AddArgument()) elem
        toEntity (ctx.TypeSpec tsB)

    let equalityComparerDefault (elem: FrozenType) : EntityHandle =
        let parent = equalityComparerTypeSpec elem
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                0,
                (fun (ret: ReturnTypeEncoder) ->
                    let g = ret.Type().GenericInstantiation(eEqualityComparer1.Value, 1, false)
                    g.AddArgument().GenericTypeParameter(0)
                ),
                (fun (_: ParametersEncoder) -> ())
            )

        toEntity (ctx.MemberRef(parent, "get_Default", s))

    let equalityComparerEquals (elem: FrozenType) : EntityHandle =
        let parent = equalityComparerTypeSpec elem
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                2,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Boolean()),
                (fun (pars: ParametersEncoder) ->
                    pars.AddParameter().Type().GenericTypeParameter(0)
                    pars.AddParameter().Type().GenericTypeParameter(0)
                )
            )

        toEntity (ctx.MemberRef(parent, "Equals", s))

    /// There is no IL opcode for a structural hash, so unlike `=`/`+` this is a BCL call, not an
    /// `ILIntrinsic`.
    let equalityComparerGetHashCode (elem: FrozenType) : EntityHandle =
        let parent = equalityComparerTypeSpec elem
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().GenericTypeParameter(0))
            )

        toEntity (ctx.MemberRef(parent, "GetHashCode", s))

    let hashCodeAdd (elem: FrozenType) : EntityHandle =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(genericParameterCount = 1, isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().GenericMethodTypeParameter(0))
            )

        let memberRef = ctx.MemberRef(eHashCode.Value, "Add", s)
        let inst = BlobBuilder()
        let specEnc = BlobEncoder(inst).MethodSpecificationSignature(1)
        encodeType (specEnc.AddArgument()) elem
        toEntity (ctx.MethodSpec(toEntity memberRef, inst))

    let equatableInterfaceSpec (selfTy: FrozenType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eEquatable1.Value, 1, false)
        encodeType (g.AddArgument()) selfTy
        toEntity (ctx.TypeSpec tsB)

    let comparerTypeSpec (elem: FrozenType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eComparer1.Value, 1, false)
        encodeType (g.AddArgument()) elem
        toEntity (ctx.TypeSpec tsB)

    let comparerDefault (elem: FrozenType) : EntityHandle =
        let parent = comparerTypeSpec elem
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(
                0,
                (fun (ret: ReturnTypeEncoder) ->
                    let g = ret.Type().GenericInstantiation(eComparer1.Value, 1, false)
                    g.AddArgument().GenericTypeParameter(0)
                ),
                (fun (_: ParametersEncoder) -> ())
            )

        toEntity (ctx.MemberRef(parent, "get_Default", s))

    let comparerCompare (elem: FrozenType) : EntityHandle =
        let parent = comparerTypeSpec elem
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                2,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                (fun (pars: ParametersEncoder) ->
                    pars.AddParameter().Type().GenericTypeParameter(0)
                    pars.AddParameter().Type().GenericTypeParameter(0)
                )
            )

        toEntity (ctx.MemberRef(parent, "Compare", s))

    let comparableInterfaceSpec (selfTy: FrozenType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eComparable1.Value, 1, false)
        encodeType (g.AddArgument()) selfTy
        toEntity (ctx.TypeSpec tsB)

    /// A `TypeSpec` token for an arbitrary `FrozenType`, the type operand of `isinst` /
    /// `castclass` / `box` / `unbox.any` (`:>` / `:?` / `:?>`). A `TypeSpec` is a legal
    /// `TypeDefOrRefOrSpec` operand, so one path serves mono and generic targets alike.
    let typeToken (ty: FrozenType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        encodeType te ty
        toEntity (ctx.TypeSpec tsB)

    member _.EmitInvoke funcTy = emitInvoke funcTy
    member _.EmitListCons elem = emitListCons elem
    member _.EmitListNil elem = emitListNil elem
    member _.EmitVesperListCons elem = emitVesperListCons elem
    member _.EmitVesperListEmpty elem = emitVesperListEmpty elem
    member _.EmitVesperListTagField elem = emitVesperListTagField elem
    member _.EmitVesperListConsField(elem, fieldIndex) = emitVesperListConsField elem fieldIndex
    member _.FunInterfaceSpec(a, b) = funInterfaceSpec a b
    member _.FlatFunInterfaceSpecN(tys) = flatFunInterfaceSpecN tys
    member _.EmitFold fnTy = emitFold fnTy
    member _.EmitExternalCall(binding, fnTy) = emitExternalCall binding fnTy
    member _.BuildFormatHandles() = buildFormatHandles ()
    member _.FormatSinkHandles = formatSinkHandles.Value
    member _.StructuralFormatSignature() = structuralFormatSignature ()
    member _.StructuralFormattableInterface = eStructuralFormattable ()

    member _.EqualityComparerDefault elem = equalityComparerDefault elem
    member _.EqualityComparerEquals elem = equalityComparerEquals elem
    member _.EqualityComparerGetHashCode elem = equalityComparerGetHashCode elem
    member _.HashCodeAdd elem = hashCodeAdd elem
    member _.EquatableInterfaceSpec selfTy = equatableInterfaceSpec selfTy
    member _.ComparerDefault elem = comparerDefault elem
    member _.ComparerCompare elem = comparerCompare elem
    member _.ComparableInterfaceSpec selfTy = comparableInterfaceSpec selfTy
    member _.TypeToken ty = typeToken ty
