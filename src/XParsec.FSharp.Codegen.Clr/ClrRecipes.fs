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
    let zonk t = env.Zonk t
    let decurryTy t = env.DecurryTy t
    let externalAsmRef asm = env.ExternalAsmRef asm

    let recoverOpenTypars declArity methodArity openT instT =
        enc.RecoverOpenTypars(declArity, methodArity, openT, instT)

    let encodeType te t = enc.EncodeType(te, t)
    let encodeFSharpFunc te t = enc.EncodeFSharpFunc(te, t)
    let encodeListOf te inner = enc.EncodeListOf(te, inner)
    let methodSpec handle args = enc.MethodSpec(handle, args)
    let formatterTypeName = env.FormatterTypeName

    let ePrintfFormat4 = env.EPrintfFormat4
    let eUnit = env.EUnit
    let eTextWriter = env.ETextWriter
    let ePrintfModule = env.EPrintfModule
    let eFun2 = env.EFun2
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
    let emitPrintfn (fnTy: SemType) : CallRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine"

        let resultTy =
            match zonk fnTy with
            | TyFun(_, printer) -> printer
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
        // to `FSharpFunc`, not `Vesper.Fun` — this cold path is FSharp.Core interop (R9).
        let inst = BlobBuilder()
        let specEnc = BlobEncoder(inst).MethodSpecificationSignature(1)
        encodeFSharpFunc (specEnc.AddArgument()) resultTy
        let spec = toEntity (ctx.MethodSpec(toEntity memberRef, inst))

        {
            Emit = fun il -> il.Encoder.Call spec
            ArgCount = 1
            Pushes = 1
        }

    /// `Vesper.Fun`2<a,b>::Invoke(!0) : !1` as a `MemberRef` token — applying a function value (R1/D3).
    /// `Fun` is an interface, so the dispatch stays `callvirt`.
    let funInvokeRef (a: SemType) (b: SemType) : EntityHandle =
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

    let emitInvoke (funcTy: SemType) : CallRecipe =
        match funcTy with
        | TyFun(a, b) ->
            let invokeRef = funInvokeRef a b

            {
                Emit =
                    fun il ->
                        il.Encoder.OpCode ILOpCode.Callvirt
                        il.Encoder.Token invokeRef
                ArgCount = 2
                Pushes = 1
            }
        | other -> failwithf "ClrProvider: cannot invoke non-function type: %A" other

    /// `FSharpFunc`2<a,b>::Invoke(a) : b` — applying an FSharp.Core `FSharpFunc`, not a `Vesper.Fun`.
    /// R1 left exactly one such island: the cold printf printer returned by `PrintFormatLine` (R9).
    let emitFSharpFuncInvoke (funcTy: SemType) : CallRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Core.FSharpFunc`2.Invoke"

        match funcTy with
        | TyFun(a, b) ->
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
                ArgCount = 2
                Pushes = 1
            }
        | other -> failwithf "ClrProvider: cannot invoke non-function type: %A" other

    /// `new PrintfFormat<tyArgs>(string)`. The first type arg is the printer — an FSharp.Core
    /// `FSharpFunc` (cold path, flows into `PrintFormatLine`), so its arrows encode to `FSharpFunc`.
    let emitPrintfFormatCtor (tyArgs: SemType list) : CtorRecipe =
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
    let listTypeSpec (elem: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        encodeListOf te (fun arg -> encodeType arg elem)
        toEntity (ctx.TypeSpec tsB)

    let emitListCons (elem: SemType) : CallRecipe =
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
            ArgCount = 2
            Pushes = 1
        }

    let emitListNil (elem: SemType) : CallRecipe =
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
            ArgCount = 0
            Pushes = 1
        }

    /// `Vesper.Collections.List`1<elem>` as a member-ref parent `TypeSpec`.
    let vesperListTypeSpec (elem: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eVesperList1.Value, 1, false)
        encodeType (g.AddArgument()) elem
        toEntity (ctx.TypeSpec tsB)

    let encodeVesperListOfTypar (te: SignatureTypeEncoder) : unit =
        let g = te.GenericInstantiation(eVesperList1.Value, 1, false)
        g.AddArgument().GenericTypeParameter(0)

    let emitVesperListCons (elem: SemType) : CallRecipe =
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
            ArgCount = 2
            Pushes = 1
        }

    let emitVesperListEmpty (elem: SemType) : CallRecipe =
        let typeSpec = vesperListTypeSpec elem
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(0, (fun (ret: ReturnTypeEncoder) -> encodeVesperListOfTypar (ret.Type())), (fun _ -> ()))

        let emptyRef = toEntity (ctx.MemberRef(typeSpec, "Empty", msig))

        {
            Emit = fun il -> il.Encoder.Call emptyRef
            ArgCount = 0
            Pushes = 1
        }

    /// `Vesper.Fun`2<a,b>` as a `TypeSpec` — the interface a synthesised closure *implements* (R1/D3).
    /// A closure derives from `System.Object`, not `FSharpFunc`.
    let funInterfaceSpec (a: SemType) (b: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eFun2.Value, 2, false)
        encodeType (g.AddArgument()) a
        encodeType (g.AddArgument()) b
        toEntity (ctx.TypeSpec tsB)

    /// `List.fold folder state xs` over the *Vesper* list — a `call` to `fold` compiled into
    /// `Vesper.List.dll` (R3). Folder, state, list are already on the stack (ArgCount = 3); the call
    /// leaves the `'State` result. No FSharp.Core dep.
    let emitFold (fnTy: SemType) : CallRecipe =
        let elemTy, stateTy =
            match zonk fnTy with
            | TyFun(TyFun(state, TyFun(t, _)), _) -> t, state
            | other -> failwithf "ClrProvider: List.fold has unexpected type %A" other

        // The generic `fold` signature is encoded with two method typars carried as self-describing
        // `TempTypar(Method, i)` nodes (`'State` ⇒ `!!0`, `'T` ⇒ `!!1`): the keystone `encodeType` arm
        // maps them — and the `Fun` / `List` instances over them — to `!!i` straight off the node,
        // exactly as the producer side emits the method's own signature. No ambient typar window.
        let sT = TempTypar(TyparAxis.Method, 0)
        let eT = TempTypar(TyparAxis.Method, 1)
        let folderT = TyFun(sT, TyFun(eT, sT))
        let listT = TyUnion(RuntimeNames.vesperListKey, EqArray.singleton eT)

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
            ArgCount = 3
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

    /// General external module-function call (vesper-lib-test-plan Gap 2 Layer D): a `call` to a static
    /// method `<ns>::<name>` compiled into a referenced package by our own backend, generalised from
    /// `emitFold`. `declFullName` is the declaring module's compiled holder name (the call key's `ns`,
    /// e.g. `Vesper.OptionModule`), `name` the method, `fnTy` the *use-site* curried function type.
    ///
    /// The open method signature is reconstructed by the symbol layer's `Inline.openMethodSignature`
    /// accessor (the §3A-precursor `instantiate` seam): it instantiates the symbol and hands back a
    /// curried monotype whose method-own typars are already self-describing `TempTypar(Method, i)` nodes,
    /// so codegen never authors a `TyVar`. The keystone `encodeType` arm maps those to `!!i`, matching
    /// the producer's emitted signature; the use-site type arguments are then recovered by structurally
    /// matching that open type against `fnTy` (`recoverOpenTypars`, method axis). A monomorphic method
    /// needs no `MethodSpec`. `ValueNone` ⇒ the symbol is unknown to the provider, or carries no home
    /// assembly (a project-local symbol the provider never sees), in which case the caller falls back to
    /// its hard error.
    let emitExternalCall (declFullName: string) (name: string) (fnTy: SemType) : CallRecipe voption =
        let compiledFullName =
            if declFullName = "" then
                name
            else
                declFullName + "." + name

        match symbols.TryLookup compiledFullName with
        | ValueNone -> ValueNone
        | ValueSome sym ->
            match sym.Origin.Assembly with
            | None -> ValueNone
            | Some _ ->
                // The symbol's open curried monotype, with its method typars already self-describing
                // `TempTypar(Method, i)` (`Inline.openMethodSignature` instantiates + rewrites in the
                // symbol layer, so codegen authors no `TypeVar`). Its nominal heads are already kind-
                // correct (`'T option` ⇒ `TyUnion`) — dependency-aware extraction bakes them so — so they
                // encode + recover against the producer's emitted signature with no reconciliation.
                let openSig = Inline.openMethodSignature sym
                let methodArity = openSig.MethodArity
                let openParamTys, openRetTy = decurryTy openSig.Signature

                // The open method-ref signature: parameters + return encoded with the method typars as
                // `!!i`, as the producer's static-method emit uses.
                let msig =
                    let s = BlobBuilder()

                    BlobEncoder(s)
                        .MethodSignature(genericParameterCount = methodArity, isInstanceMethod = false)
                        .Parameters(
                            List.length openParamTys,
                            (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) openRetTy),
                            (fun (pars: ParametersEncoder) ->
                                for p in openParamTys do
                                    encodeType (pars.AddParameter().Type()) p
                            )
                        )

                    s

                let parent = externalModuleRef sym.Origin.Assembly sym.Origin.Namespace declFullName
                let memberRef = toEntity (ctx.MemberRef(parent, name, msig))

                let callHandle =
                    if methodArity = 0 then
                        memberRef
                    else
                        // Use-site instantiation: match the open monotype (its `TempTypar(Method, i)`)
                        // against the call's concrete type, recovering each method arg by its index.
                        let _, methodArgs = recoverOpenTypars 0 methodArity openSig.Signature (zonk fnTy)
                        methodSpec memberRef methodArgs

                ValueSome
                    {
                        Emit = fun il -> il.Encoder.Call callHandle
                        ArgCount = List.length openParamTys
                        Pushes = 1
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
        let appendFormatted (ty: SemType, hasAlignment: bool, hasFormat: bool) : EntityHandle =
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
            encodeType (specEnc.AddArgument()) (zonk ty)
            toEntity (ctx.MethodSpec(toEntity memberRef, inst))

        {
            HandlerLocal = TyConst(formatterTypeName, EqArray.empty)
            CtorWriter = ctorWriter
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
            AppendZeroPaddedFloat = appendZeroPaddedFloat
        }

    // ---- Structural equality / hashing (C-Eq1) ----

    let equalityComparerTypeSpec (elem: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eEqualityComparer1.Value, 1, false)
        encodeType (g.AddArgument()) (zonk elem)
        toEntity (ctx.TypeSpec tsB)

    let equalityComparerDefault (elem: SemType) : EntityHandle =
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

    let equalityComparerEquals (elem: SemType) : EntityHandle =
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
    let equalityComparerGetHashCode (elem: SemType) : EntityHandle =
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

    let hashCodeAdd (elem: SemType) : EntityHandle =
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
        encodeType (specEnc.AddArgument()) (zonk elem)
        toEntity (ctx.MethodSpec(toEntity memberRef, inst))

    let equatableInterfaceSpec (selfTy: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eEquatable1.Value, 1, false)
        encodeType (g.AddArgument()) (zonk selfTy)
        toEntity (ctx.TypeSpec tsB)

    // ---- Structural comparison (records-plan §B6) ----

    let comparerTypeSpec (elem: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eComparer1.Value, 1, false)
        encodeType (g.AddArgument()) (zonk elem)
        toEntity (ctx.TypeSpec tsB)

    let comparerDefault (elem: SemType) : EntityHandle =
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

    let comparerCompare (elem: SemType) : EntityHandle =
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

    let comparableInterfaceSpec (selfTy: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eComparable1.Value, 1, false)
        encodeType (g.AddArgument()) (zonk selfTy)
        toEntity (ctx.TypeSpec tsB)

    /// A `TypeSpec` token for an arbitrary `SemType`, for the type operand of
    /// `isinst` / `castclass` / `box` / `unbox.any` (inheritance-plan §`:>` /
    /// `:?` / `:?>`). `encodeType` maps user types to their `TypeDefinition`,
    /// generic instances to instantiated specs, and externals through the
    /// provider — a `TypeSpec` token is a legal `TypeDefOrRefOrSpec` operand for
    /// all of them, so one path serves mono and generic targets alike.
    let typeToken (ty: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        encodeType te (zonk ty)
        toEntity (ctx.TypeSpec tsB)

    member _.EmitPrintfn fnTy = emitPrintfn fnTy
    member _.EmitInvoke funcTy = emitInvoke funcTy
    member _.EmitFSharpFuncInvoke funcTy = emitFSharpFuncInvoke funcTy
    member _.EmitPrintfFormatCtor tyArgs = emitPrintfFormatCtor tyArgs
    member _.EmitListCons elem = emitListCons elem
    member _.EmitListNil elem = emitListNil elem
    member _.EmitVesperListCons elem = emitVesperListCons elem
    member _.EmitVesperListEmpty elem = emitVesperListEmpty elem
    member _.FunInterfaceSpec(a, b) = funInterfaceSpec a b
    member _.EmitFold fnTy = emitFold fnTy
    member _.EmitExternalCall(declFullName, name, fnTy) = emitExternalCall declFullName name fnTy
    member _.BuildFormatHandles() = buildFormatHandles ()

    member _.EqualityComparerDefault elem = equalityComparerDefault elem
    member _.EqualityComparerEquals elem = equalityComparerEquals elem
    member _.EqualityComparerGetHashCode elem = equalityComparerGetHashCode elem
    member _.HashCodeAdd elem = hashCodeAdd elem
    member _.EquatableInterfaceSpec selfTy = equatableInterfaceSpec selfTy
    member _.ComparerDefault elem = comparerDefault elem
    member _.ComparerCompare elem = comparerCompare elem
    member _.ComparableInterfaceSpec selfTy = comparableInterfaceSpec selfTy
    member _.TypeToken ty = typeToken ty
