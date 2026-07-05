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
    let externalAsmRef asm = env.ExternalAsmRef asm

    let recoverOpenTypars declArity methodArity openT instT =
        enc.RecoverOpenTypars(declArity, methodArity, openT, instT)

    let encodeType te t = enc.EncodeType(te, t)
    let encodeFSharpFunc te t = enc.EncodeFSharpFunc(te, t)

    /// Decurry a `FrozenType` arrow chain into `(params, return)` — the `FrozenType`
    /// analogue of `ClrEnv.decurryTy`, for the open module-function template.
    /// A curried `p1 -> … -> pN -> ret` peels to `([p1; …; pN], ret)`. Peels every
    /// arrow (`TastLower.peelArrowDomains -1`); the `n`-group variant uses `peelN`.
    let decurryFrozen (t: FrozenType) : FrozenType list * FrozenType = TastLower.peelArrowDomains -1 t

    let encodeListOf te inner = enc.EncodeListOf(te, inner)
    let methodSpec handle args = enc.MethodSpec(handle, args)
    let formatterTypeName = env.FormatterTypeName

    let ePrintfFormat4 = env.EPrintfFormat4
    let eUnit = env.EUnit
    let eTextWriter = env.ETextWriter
    let eStringBuilder = env.EStringBuilder
    let ePrintfModule = env.EPrintfModule
    let eFun2 = env.EFun2
    let eFlatFun = env.EFlatFun
    let eFSharpFunc2 = env.EFSharpFunc2
    let eVesperList1 = env.EVesperList1
    let eListModule = env.EListModule
    let eFormatter = env.EFormatter
    let eConsole = env.EConsole
    let eEqualityComparer1 = env.EEqualityComparer1
    let eHashCode = env.EHashCode
    let eEquatable1 = env.EEquatable1
    let eComparer1 = env.EComparer1
    let eComparable1 = env.EComparable1

    /// `PrintfFormat<!!0, TextWriter, Unit, Unit>` — the parameter type of the generic
    /// `PrintFormatLine<T>`, where the printer slot is method type parameter 0.
    let encodeFormatParam (te: SignatureTypeEncoder) : unit =
        markFSharpCoreDep "Microsoft.FSharp.Core.PrintfFormat`4"
        markFSharpCoreDep "Microsoft.FSharp.Core.Unit"
        let g = te.GenericInstantiation(ePrintfFormat4.Value, 4, false)
        g.AddArgument().GenericMethodTypeParameter(0)
        g.AddArgument().Type(eTextWriter.Value, false)
        g.AddArgument().Type(eUnit.Value, false)
        g.AddArgument().Type(eUnit.Value, false)

    /// `printfn` → `call PrintfModule::PrintFormatLine<printer>(format)`. The `printer` is the *result*
    /// of the head's curried type `fnTy = PrintfFormat<…> -> printer`.
    let emitPrintfn (fnTy: FrozenType) : CallRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine"

        let resultTy =
            match fnTy with
            | FTFun(_, printer) -> printer
            | other -> failwithf "ClrProvider: printfn has non-function type %A" other

        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(genericParameterCount = 1, isInstanceMethod = false)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().GenericMethodTypeParameter(0)),
                (fun (pars: ParametersEncoder) -> encodeFormatParam (pars.AddParameter().Type()))
            )

        let memberRef = ctx.MemberRef(ePrintfModule.Value, "PrintFormatLine", msig)

        // The printer is an FSharp.Core `FSharpFunc` (PrintFormatLine builds it), so its arrow encodes
        // to `FSharpFunc`, not `Vesper.Fun` — this cold path is FSharp.Core interop.
        let inst = BlobBuilder()
        let specEnc = BlobEncoder(inst).MethodSpecificationSignature(1)
        encodeFSharpFunc (specEnc.AddArgument()) resultTy
        let spec = toEntity (ctx.MethodSpec(toEntity memberRef, inst))

        {
            Emit = fun il -> il.Encoder.Call spec
            Arity = CallArity.Flat 1
            Pushes = 1
        }

    /// `Vesper.Fun`2<a,b>::Invoke(!0) : !1` as a `MemberRef` token — applying a function value.
    /// `Fun` is an interface, so the dispatch stays `callvirt`.
    let funInvokeRef (a: FrozenType) (b: FrozenType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eFun2.Value, 2, false)
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

    /// `FSharpFunc`2<a,b>::Invoke(a) : b` — applying an FSharp.Core `FSharpFunc`, not a `Vesper.Fun`.
    /// The only such island is the cold printf printer returned by `PrintFormatLine`.
    let emitFSharpFuncInvoke (funcTy: FrozenType) : CallRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Core.FSharpFunc`2.Invoke"

        match funcTy with
        | FTFun(a, b) ->
            let tsB = BlobBuilder()
            let te = BlobEncoder(tsB).TypeSpecificationSignature()
            let g = te.GenericInstantiation(eFSharpFunc2.Value, 2, false)
            encodeFSharpFunc (g.AddArgument()) a
            encodeFSharpFunc (g.AddArgument()) b
            let typeSpec = ctx.TypeSpec tsB

            let msig = BlobBuilder()

            BlobEncoder(msig)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    1,
                    (fun (ret: ReturnTypeEncoder) -> ret.Type().GenericTypeParameter(1)),
                    (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().GenericTypeParameter(0))
                )

            let invokeRef = toEntity (ctx.MemberRef(toEntity typeSpec, "Invoke", msig))

            {
                Emit =
                    fun il ->
                        il.Encoder.OpCode ILOpCode.Callvirt
                        il.Encoder.Token invokeRef
                Arity = CallArity.Flat 2
                Pushes = 1
            }
        | other -> failwithf "ClrProvider: cannot invoke non-function type: %A" other

    /// `new PrintfFormat<tyArgs>(string)`. The first type arg is the printer — an FSharp.Core
    /// `FSharpFunc` (cold path, flows into `PrintFormatLine`), so its arrows encode to `FSharpFunc`.
    let emitPrintfFormatCtor (tyArgs: FrozenType list) : CtorRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Core.PrintfFormat`4 (.ctor)"
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(ePrintfFormat4.Value, List.length tyArgs, false)

        for a in tyArgs do
            encodeFSharpFunc (g.AddArgument()) a

        let typeSpec = ctx.TypeSpec tsB
        let ctorSig = BlobBuilder()

        BlobEncoder(ctorSig)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().String())
            )

        let ctorRef = ctx.MemberRef(toEntity typeSpec, ".ctor", ctorSig)

        {
            Handle = toEntity ctorRef
            ArgCount = 1
        }

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

    /// The `_tag : int32` discriminator field `MemberRef` on the referenced cons-list
    /// `Vesper.Collections.List`1<elem>` — the slot a cross-package `match` against
    /// `[]` / `::` reads. The cons-list keeps op-form case names (`op_Nil` /
    /// `op_ColonColon`) in its extracted contract, so it never resolves through the
    /// generic external-union path; like construction (`emitVesperListCons` /
    /// `…Empty`), the match path special-cases it against the known emitted layout
    /// (`Empty` tag 0, `Cons` tag 1; payload fields `Cons_0` / `Cons_1`).
    let emitVesperListTagField (elem: FrozenType) : EntityHandle =
        let typeSpec = vesperListTypeSpec elem
        let s = BlobBuilder()
        encodeType (BlobEncoder(s).FieldSignature()) (FTConst("int", EqArray.empty))
        toEntity (ctx.MemberRef(typeSpec, "_tag", s))

    /// One `Cons_<fieldIndex>` payload field `MemberRef` on the referenced cons-list
    /// (`Cons_0` = head `'T`, `Cons_1` = tail `List<'T>`), instantiated at `elem`. The
    /// signature blob encodes the field's *open* (declaring-typar) type so it matches
    /// the emitted field definition; the sibling of `emitVesperListTagField`.
    let emitVesperListConsField (elem: FrozenType) (fieldIndex: int) : EntityHandle =
        let typeSpec = vesperListTypeSpec elem
        let s = BlobBuilder()
        let fte = BlobEncoder(s).FieldSignature()

        match fieldIndex with
        | 0 -> fte.GenericTypeParameter(0) // Cons_0 : 'T
        | _ -> encodeVesperListOfTypar fte // Cons_1 : List<'T>

        toEntity (ctx.MemberRef(typeSpec, sprintf "Cons_%d" fieldIndex, s))

    /// `Vesper.Fun`2<a,b>` as a `TypeSpec` — the interface a synthesised closure *implements*.
    /// A closure derives from `System.Object`, not `FSharpFunc`.
    let funInterfaceSpec (a: FrozenType) (b: FrozenType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eFun2.Value, 2, false)
        encodeType (g.AddArgument()) a
        encodeType (g.AddArgument()) b
        toEntity (ctx.TypeSpec tsB)

    /// `Vesper.Fun`(len)<tys…>` as a `TypeSpec` — the FLAT interface a flat
    /// value-struct closure of param-arity `len-1` implements (sibling of the curried
    /// `funInterfaceSpec`). `tys` is the full type-arg list (the flat params followed
    /// by the result), so `len` picks the `Fun`(len)` entity (`3`⇒`Fun`3`,
    /// `4`⇒`Fun`4`, `5`⇒`Fun`5`).
    let flatFunInterfaceSpecN (tys: FrozenType list) : EntityHandle =
        let len = List.length tys
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation((env.FlatFunEntity len).Value, len, false)

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

        // The generic `fold` signature is encoded with two method typars carried as self-describing
        // `FTTypar(Method, i)` nodes (`'State` ⇒ `!!0`, `'T` ⇒ `!!1`): the keystone `encodeType` arm
        // maps them — and the `Fun` / `List` instances over them — to `!!i` straight off the node,
        // exactly as the producer side emits the method's own signature. No ambient typar window.
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

    /// The member-ref parent `TypeRef` for an external module's compiled holder type — `declFullName`
    /// is the holder's fully-qualified compiled name (`Vesper.OptionModule`), `metaNs` its metadata
    /// namespace (the package namespace, `Vesper`). A nested holder (`Outer.Inner`) chains through the
    /// enclosing `TypeRef` with the bare nested name + empty namespace, exactly as `ClrEnv.externalClassRef`
    /// does for a nested class.
    let externalModuleRef (asm: string option) (metaNs: string) (declFullName: string) : EntityHandle =
        let asmRef = externalAsmRef asm
        let simple = SymbolOrigin.StripNamespace metaNs declFullName

        match simple.Split('.') with
        | [| flat |] -> toEntity (ctx.TypeRef(asmRef, metaNs, flat))
        | parts ->
            let mutable scope = toEntity (ctx.TypeRef(asmRef, metaNs, parts.[0]))

            for i in 1 .. parts.Length - 1 do
                scope <- toEntity (ctx.TypeRef(scope, "", parts.[i]))

            scope

    /// The EXTERNAL head of the seq-interface witness
    /// (`FrozenTypeBridge.pickInterfaceWitness` is the shared tail;
    /// `EmitResolve.tryInterfaceWitness` is the project-local head). Given a
    /// referenced-package nominal receiver `FTClass/FTUnion/FTRecord(key, args)` and a
    /// wanted `ifaceKey`, look the type's shape up through the codegen symbol provider
    /// (`ICodegenSymbols.TryLookupType`) and pick the matching
    /// `ExternalClassShape.FrozenInterfaces` template (its args over the declaring
    /// typars), instantiated at THIS receiver (`FTTypar(Declaring, i) := args.[i]`).
    /// `ValueNone` for a non-nominal receiver, an unknown / non-class shape, or no
    /// matching interface. Direct-declared interfaces only (the `.fsi` extractor's
    /// `FrozenInterfaces` is the frozen direct-impl set), matching the project-local
    /// witness's depth. Names compare on `qualifiedName` (arity suffix retained on both
    /// sides — `FrozenInterfaces` from `nominalInterface`, `ifaceKey` from the frozen
    /// constraint target).
    let tryExternalInterfaceWitness (receiver: FrozenType) (ifaceKey: SymbolKey) : EqArray<FrozenType> voption =
        match receiver with
        | FTClass(rKey, rArgs)
        | FTUnion(rKey, rArgs)
        | FTRecord(rKey, rArgs) ->
            match symbols.TryLookupType(SymbolKeyOps.qualifiedName rKey) with
            | ValueSome(ExternalTypeShape.Class shape) ->
                pickInterfaceWitness
                    (SymbolKeyOps.qualifiedName ifaceKey)
                    (rArgs.AsSpan().ToArray())
                    shape.FrozenInterfaces
            | _ -> ValueNone
        | _ -> ValueNone

    /// General external module-function call: a `call` to a static method `<ns>::<name>` compiled into
    /// a referenced package by our own backend, generalised from `emitFold`. `declFullName` is the
    /// declaring module's compiled holder name (the call key's `ns`, e.g. `Vesper.OptionModule`),
    /// `name` the method, `fnTy` the *use-site* curried function type.
    ///
    /// The open method signature is reconstructed by the symbol layer's `Inline.openMethodSignature`
    /// accessor: it instantiates the symbol and hands back a curried monotype whose method-own typars
    /// are already self-describing `FTTypar(Method, i)` nodes,
    /// so codegen never authors a `TyVar`. The keystone `encodeType` arm maps those to `!!i`, matching
    /// the producer's emitted signature; the use-site type arguments are then recovered by structurally
    /// matching that open type against `fnTy` (`recoverOpenTypars`, method axis). A monomorphic method
    /// needs no `MethodSpec`. `ValueNone` ⇒ the symbol is unknown to the provider, or carries no home
    /// assembly (a project-local symbol the provider never sees), in which case the caller falls back to
    /// its hard error.
    let emitExternalCall (declFullName: string) (name: string) (fnTy: FrozenType) : CallRecipe voption =
        let compiledFullName =
            if declFullName = "" then
                name
            else
                declFullName + "." + name

        match symbols.TryLookupOpenSignature compiledFullName with
        | ValueNone -> ValueNone
        | ValueSome openSig ->
            // The symbol's open curried signature *template*, with its method typars already self-
            // describing `FTTypar(Method, i)` (`ICodegenSymbols.TryLookupOpenSignature` instantiates +
            // freezes in the symbol layer, so codegen authors no `TypeVar` and never touches
            // `Instantiate`). Its nominal heads are already kind-correct (`'T option` ⇒
            // `FTUnion`), so they encode + recover against the producer's emitted signature unchanged.
            let methodArity = openSig.MethodArity

            // Peel exactly `n` top-level `->` groups off the open template — one per
            // SOURCE argument group. Unlike `decurryFrozen` (which peels every arrow),
            // `peelArrowDomains n` stops at the source arity, so a function-typed
            // RESULT stays whole.
            let peelN n t = TastLower.peelArrowDomains n t

            // The flat parameter vector + `void`-vs-value decision come from the SOURCE
            // `ValRepr` the symbol carries: its groups drive the tuple-flatten
            // / lone-`unit`-erase (mirroring the producer's `compiledOf`), and the
            // result peeled to exactly that arity decides `void`. The parameter TYPES
            // come from peeling the open template (its method typars are already
            // `FTTypar(Method, i)`, matching the producer's `!!i` slots). Without a
            // captured `ValRepr` (a value, a metadata-layer symbol), fall back to the
            // bare-`decurryFrozen` reconstruction — the curried calling convention,
            // correct for an all-`GSimple` signature. A `unit` source result is emitted
            // genuine CLR `void` by the producer ("void everywhere"), so the
            // member-ref must encode `void` too or a `System.ValueTuple` return misses
            // the void method (`MissingMethodException`) — hence both arms read void
            // from the same `isUnitReturn` of the (exactly-peeled) source result.
            let isUnitReturn t =
                match t with
                | FTConst("unit", _) -> true
                | _ -> false

            let flatParamTys, openRetTy, returnsVoid, recipeGroups =
                match openSig.ValRepr with
                | ValueSome vr ->
                    let groups = vr.Groups
                    let n = List.length groups
                    let groupParamTys, retTy = peelN n openSig.Signature

                    if List.length groupParamTys <> n then
                        // The producer peels the same arity off the same template, so a
                        // well-formed contract always exposes `n` arrows here; a shortfall
                        // is a corrupt contract, not a recoverable shape.
                        failwithf
                            "emitExternalCall: contract for %s declares %d source groups but its template has only %d arrows"
                            compiledFullName
                            n
                            (List.length groupParamTys)

                    TastLower.flattenGroupShape groups groupParamTys, retTy, isUnitReturn retTy, ValueSome groups
                | ValueNone ->
                    let ps, r = decurryFrozen openSig.Signature
                    ps, r, isUnitReturn r, ValueNone

            // The open method-ref signature: parameters + return encoded with the method typars as
            // `!!i`, as the producer's static-method emit uses.
            let msig =
                let s = BlobBuilder()

                BlobEncoder(s)
                    .MethodSignature(genericParameterCount = methodArity, isInstanceMethod = false)
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

            let parent =
                externalModuleRef openSig.Origin.Assembly openSig.Origin.Namespace declFullName

            let memberRef = toEntity (ctx.MemberRef(parent, name, msig))

            let callHandle =
                if methodArity = 0 then
                    memberRef
                elif List.isEmpty openSig.Constraints then
                    // Use-site instantiation: match the open template (its `FTTypar(Method, i)`) against
                    // the call's concrete type, recovering each method arg by its index. No phantom
                    // constraint typars, so every method typar is signature-reachable.
                    let _, methodArgs = recoverOpenTypars 0 methodArity openSig.Signature fnTy

                    methodSpec memberRef methodArgs
                else
                    // The EXTERNAL analogue of the
                    // project-local phantom-typar solve (`EmitCall.buildAppCall`). The open
                    // template carries a phantom constraint typar (`fold`'s enumerator `'E` in
                    // `'S :> IStructSeq<'T,'E>`, in no param/result) that `recoverOpenTypars`
                    // cannot recover — it would fail loud. Recover the signature-reachable slots
                    // partially, then solve the phantom from `openSig.Constraints` via the source's
                    // external seq impl (`tryExternalInterfaceWitness`), exactly as the in-assembly
                    // path solves it from `env.Classes`. The member-ref's `genericParameterCount`
                    // already encodes `methodArity` (= 5 for `fold`), so the minted `MethodSpec`
                    // carries the full method instantiation incl. `'E`.
                    let instArr =
                        TastLower.matchInstantiationPartial methodArity [ openSig.Signature ] [ fnTy ]

                    TastLower.solvePhantomTypars methodArity openSig.Constraints tryExternalInterfaceWitness instArr

                    let methodArgs =
                        [
                            for i in 0 .. methodArity - 1 ->
                                match instArr.[i] with
                                | ValueSome t -> t
                                | ValueNone ->
                                    failwithf
                                        "emitExternalCall: could not infer instantiation for method type parameter %d of %s (phantom-typar solve found no witness)"
                                        i
                                        compiledFullName
                        ]

                    methodSpec memberRef methodArgs

            // `Grouped` carries the SOURCE grouping (the walker consumes
            // `groups.Length` spine elements and flattens each) AND the FLAT pop count
            // `List.length flatParamTys` — what the `call` actually consumes and what
            // drives the IlIr stack model (`Pushes - FlatArgCount`). The two diverge
            // for a non-`GSimple` group (a tupled group is N flat from ONE spine
            // element; a lone `()` is ZERO from one); `Flat` is the all-`GSimple`
            // fallback where they coincide.
            let arity =
                match recipeGroups with
                | ValueSome groups -> CallArity.Grouped(groups, List.length flatParamTys)
                | ValueNone -> CallArity.Flat(List.length flatParamTys)

            ValueSome
                {
                    Emit = fun il -> il.Encoder.Call callHandle
                    Arity = arity
                    // A `void` call leaves nothing; the recipe consumer reifies the
                    // `unit` value (a value-position result still needs one).
                    Pushes = if returnsVoid then 0 else 1
                }

    /// Member refs + the `AppendFormatted<T>` factory for lowering a `TExpr.Format` to the
    /// `Vesper.Formatter` write-through handler. All members hang off the non-generic `Formatter` value
    /// type, so the parent is a plain `TypeRef`; the generic `AppendFormatted` is a member ref to the
    /// open generic method + a `MethodSpec` per hole.
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
        // `%08o` / `%05u` zero-pad members: same `(value, int32 width)` shape as the
        // space-pad ones, so `appendMember` builds them — only the semantics differ.
        let appendZeroPaddedOctal =
            appendMember "AppendZeroPaddedOctal" (fun te -> te.Int32())

        let appendZeroPaddedUnsigned =
            appendMember "AppendZeroPaddedUnsigned" (fun te -> te.UInt32())

        // `instance void AppendZeroPaddedFloat(float64, string, int32)` — `%0w.pf` (value, "F<prec>"
        // body, field width). Distinct arity from `appendMember`, so built here.
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
        // (value, "F<prec>" body, field width). Same shape as `AppendZeroPaddedFloat`.
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

        // `instance void AppendDynamicPrecisionSignedFloat(float64, char, int32, int32, bool)`
        // — `%+.*f`/`% .*f`/`%+*.*f` (value, 'f', runtime precision, field width, space flag).
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

        // `instance void AppendStructured<T>(!!0, int32, int32)` — `%A`. Generic
        // like `appendFormatted`: a member ref to the open generic method + a
        // `MethodSpec` binding `<T = ty>` per hole. The two `int32`s are the
        // print-width budget then the print-size budget the walker pushes.
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
            HandlerLocal = FTConst(formatterTypeName, EqArray.empty)
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
            AppendDynamicPrecisionFloat = appendDynamicPrecisionFloat
            AppendDynamicPrecisionSignedFloat = appendDynamicPrecisionSignedFloat
            AppendStructured = appendStructured
            GuardTotalWidth = staticIntToInt "GuardTotalWidth"
            ClampWidth = staticIntToInt "ClampWidth"
            NormalizePrecision = staticIntToInt "NormalizePrecision"
        }

    let eFormatSink = env.EFormatSink
    let eStructuralFormattable = env.EStructuralFormattable

    /// The `IFormatSink` member refs the synthesised `Format` body calls. Built
    /// once (the handles are type-independent); each is `instance void` on
    /// `Vesper.IFormatSink`. The `Format` body `callvirt`s these around the type's
    /// fields, mirroring the hand-written `Point`/`Opt` impls.
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

                toEntity (ctx.MemberRef(eFormatSink.Value, name, s))

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

    /// `instance void Format(IFormatSink)` — the signature of the synthesised
    /// `IStructuralFormattable.Format` member. The param type is the bare
    /// `Vesper.IFormatSink` `TypeRef`, encoded identically to the interface slot it
    /// binds to (name + signature match, like the typed equality `Equals(Self)`).
    let structuralFormatSignature () : BlobBuilder =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().Type(eFormatSink.Value, false))
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

    /// A `TypeSpec` token for an arbitrary `FrozenType`, for the type operand of
    /// `isinst` / `castclass` / `box` / `unbox.any` (`:>` / `:?` / `:?>`).
    /// `encodeType` maps user types to their `TypeDefinition`,
    /// generic instances to instantiated specs, and externals through the
    /// provider — a `TypeSpec` token is a legal `TypeDefOrRefOrSpec` operand for
    /// all of them, so one path serves mono and generic targets alike.
    let typeToken (ty: FrozenType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        encodeType te ty
        toEntity (ctx.TypeSpec tsB)

    member _.EmitPrintfn fnTy = emitPrintfn fnTy
    member _.EmitInvoke funcTy = emitInvoke funcTy
    member _.EmitFSharpFuncInvoke funcTy = emitFSharpFuncInvoke funcTy
    member _.EmitPrintfFormatCtor tyArgs = emitPrintfFormatCtor tyArgs
    member _.EmitListCons elem = emitListCons elem
    member _.EmitListNil elem = emitListNil elem
    member _.EmitVesperListCons elem = emitVesperListCons elem
    member _.EmitVesperListEmpty elem = emitVesperListEmpty elem
    member _.EmitVesperListTagField elem = emitVesperListTagField elem
    member _.EmitVesperListConsField(elem, fieldIndex) = emitVesperListConsField elem fieldIndex
    member _.FunInterfaceSpec(a, b) = funInterfaceSpec a b
    member _.FlatFunInterfaceSpecN(tys) = flatFunInterfaceSpecN tys
    member _.EmitFold fnTy = emitFold fnTy
    member _.EmitExternalCall(declFullName, name, fnTy) = emitExternalCall declFullName name fnTy
    member _.BuildFormatHandles() = buildFormatHandles ()
    member _.FormatSinkHandles = formatSinkHandles.Value
    member _.StructuralFormatSignature() = structuralFormatSignature ()
    member _.StructuralFormattableInterface = eStructuralFormattable.Value

    member _.EqualityComparerDefault elem = equalityComparerDefault elem
    member _.EqualityComparerEquals elem = equalityComparerEquals elem
    member _.EqualityComparerGetHashCode elem = equalityComparerGetHashCode elem
    member _.HashCodeAdd elem = hashCodeAdd elem
    member _.EquatableInterfaceSpec selfTy = equatableInterfaceSpec selfTy
    member _.ComparerDefault elem = comparerDefault elem
    member _.ComparerCompare elem = comparerCompare elem
    member _.ComparableInterfaceSpec selfTy = comparableInterfaceSpec selfTy
    member _.TypeToken ty = typeToken ty
