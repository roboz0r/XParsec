namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// The .NET implementation of `ICodegenProvider`. v1 references a real
// `FSharp.Core.dll` (+ the core BCL) at runtime; assembly identities are read
// from the assemblies *loaded in the codegen host*, so the emitted references
// match exactly what an in-process loader will bind against.
//
// Slice 1 covers `printfn "hi"`: the `PrintfFormat\`4` constructor and the
// generic `PrintfModule.PrintFormatLine` call. Coverage grows with the slices.

/// `ICodegenProvider` over the BCL + the loaded `FSharp.Core.dll`.
type ClrProvider(ctx: MetadataContext) =

    // Reference identities from the live assemblies (version-proof).
    let fsharpCore = typeof<Microsoft.FSharp.Core.Unit>.Assembly.GetName()
    let coreLib = typeof<System.Object>.Assembly.GetName()
    // The bootstrap printf runtime (Vesper.Printf.dll) hosting `Formatter`; and
    // the assembly that owns `System.Console` (its own ref assembly, not
    // CoreLib — type-forwarded at runtime). Both read live, like FSharp.Core.
    let vesperPrintf = typeof<Vesper.PrintfRuntime>.Assembly.GetName()
    let consoleAsm = typeof<System.Console>.Assembly.GetName()

    let fsCoreRef = toEntity (ctx.AssemblyRef fsharpCore)
    let coreRef = toEntity (ctx.AssemblyRef coreLib)
    let vesperRef = toEntity (ctx.AssemblyRef vesperPrintf)
    let consoleRef = toEntity (ctx.AssemblyRef consoleAsm)

    // FSharp.Core type references.
    let eUnit = toEntity (ctx.TypeRef(fsCoreRef, "Microsoft.FSharp.Core", "Unit"))

    let ePrintfFormat4 =
        toEntity (ctx.TypeRef(fsCoreRef, "Microsoft.FSharp.Core", "PrintfFormat`4"))

    let ePrintfModule =
        toEntity (ctx.TypeRef(fsCoreRef, "Microsoft.FSharp.Core", "PrintfModule"))

    let eFSharpFunc2 =
        toEntity (ctx.TypeRef(fsCoreRef, "Microsoft.FSharp.Core", "FSharpFunc`2"))

    let eFSharpList1 =
        toEntity (ctx.TypeRef(fsCoreRef, "Microsoft.FSharp.Collections", "FSharpList`1"))

    let eListModule =
        toEntity (ctx.TypeRef(fsCoreRef, "Microsoft.FSharp.Collections", "ListModule"))

    /// The abbreviation name the list-literal freeze hard-codes
    /// ([front-end-gaps-plan](../XParsec.FSharp.SemanticAnalysis/docs/front-end-gaps-plan.md)
    /// §A). Both `encodeType` and `TryEmitUnionCons` key on it; when the
    /// extractor mints `FSharpList` properly this becomes a registered
    /// `TyUnion` and the match keys on that instead — additive.
    let listTypeName = "Microsoft.FSharp.Collections.list"

    // BCL type references.
    let eObject = toEntity (ctx.TypeRef(coreRef, "System", "Object"))
    let eTextWriter = toEntity (ctx.TypeRef(coreRef, "System.IO", "TextWriter"))
    let eConsole = toEntity (ctx.TypeRef(consoleRef, "System", "Console"))
    let eFormatter = toEntity (ctx.TypeRef(vesperRef, "Vesper", "Formatter"))
    let eDecimal = toEntity (ctx.TypeRef(coreRef, "System", "Decimal"))

    /// `instance void System.Decimal::.ctor(int32, int32, int32, bool, uint8)` —
    /// the lo/mid/hi/sign/scale constructor used to materialise a `decimal`
    /// constant from its `Decimal.GetBits` representation.
    let eDecimalCtor =
        let s = BlobBuilder()

        BlobEncoder(s)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                5,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) ->
                    pars.AddParameter().Type().Int32()
                    pars.AddParameter().Type().Int32()
                    pars.AddParameter().Type().Int32()
                    pars.AddParameter().Type().Boolean()
                    pars.AddParameter().Type().Byte()
                )
            )

        toEntity (ctx.MemberRef(eDecimal, ".ctor", s))

    /// The `SemType` standing for the `Vesper.Formatter` ref-struct handler
    /// local. `encodeType` maps it to the value-type signature; the printf
    /// special-form declares a local of this type per `Format` node.
    let formatterTypeName = "Vesper.Formatter"

    /// Each distinct FSharp.Core construct the emission actually references.
    /// Lets a build tell — *positively* — whether the produced PE depends on
    /// `FSharp.Core.dll` at all (empty ⇒ no dependency, so `materialiseApp`
    /// skips copying it) and, when it does, *what* pins the dependency (a
    /// ready-made target list for the eventual FSharp.Core cut, handoff §D3).
    /// Every FSharp.Core reference is minted through this provider — `fsCoreRef`
    /// and its typerefs are private here — so marking each use-site below
    /// captures the whole dependency surface. A `HashSet` so repeats collapse;
    /// the tag names the construct, not just the type, to be useful as a target
    /// list. Populated during emission; read after via `FSharpCoreDependencies`.
    let fsharpCoreDeps = HashSet<string>()
    let markFSharpCoreDep (construct: string) : unit = fsharpCoreDeps.Add construct |> ignore

    /// Resolve a `SemType` to its concrete representative, chasing union-find
    /// links. After `ResolvedTypes` no *free* TyVar survives, so the only job
    /// here is dereferencing linked ones.
    let rec zonk (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome target -> zonk target
            | ValueNone -> t
        | TyFun(a, b) -> TyFun(zonk a, zonk b)
        | TyTuple items -> TyTuple(List.map zonk items)
        | TyRecord(n, args) -> TyRecord(n, List.map zonk args)
        | TyUnion(n, args) -> TyUnion(n, List.map zonk args)
        | TyClass(n, args) -> TyClass(n, List.map zonk args)
        | TyConst _ -> t

    /// `FSharpList\`1<X>` where `X` is encoded by `inner`. The one place that
    /// knows the list type's metadata shape, shared by `encodeType`'s list case
    /// (elem encoded recursively) and the cons/nil recipe signatures (where the
    /// element is the declaring type's generic parameter `!0`).
    let encodeListOf (te: SignatureTypeEncoder) (inner: SignatureTypeEncoder -> unit) : unit =
        markFSharpCoreDep "Microsoft.FSharp.Collections.FSharpList`1"
        let g = te.GenericInstantiation(eFSharpList1, 1, false)
        inner (g.AddArgument())

    /// Encode a (zonked) `SemType` into a metadata signature type slot.
    let rec encodeType (te: SignatureTypeEncoder) (t: SemType) : unit =
        match zonk t with
        | TyConst "int" -> te.Int32()
        | TyConst "int64" -> te.Int64()
        | TyConst "byte" -> te.Byte()
        | TyConst "float" -> te.Double()
        | TyConst "bool" -> te.Boolean()
        | TyConst "char" -> te.Char()
        | TyConst "decimal" -> te.Type(eDecimal, true)
        | TyConst "string" -> te.String()
        | TyConst "unit" ->
            markFSharpCoreDep "Microsoft.FSharp.Core.Unit"
            te.Type(eUnit, false)
        | TyConst "System.IO.TextWriter" -> te.Type(eTextWriter, false)
        | TyConst "Vesper.Formatter" -> te.Type(eFormatter, true)
        | TyFun(a, b) ->
            // `a -> b` is `FSharpFunc\`2<a, b>` at the metadata level.
            markFSharpCoreDep "Microsoft.FSharp.Core.FSharpFunc`2"
            let g = te.GenericInstantiation(eFSharpFunc2, 2, false)
            encodeType (g.AddArgument()) a
            encodeType (g.AddArgument()) b
        | TyClass(name, args) when name = PrintfSpec.printfFormatName ->
            markFSharpCoreDep "Microsoft.FSharp.Core.PrintfFormat`4"
            let g = te.GenericInstantiation(ePrintfFormat4, List.length args, false)

            for a in args do
                encodeType (g.AddArgument()) a
        | TyRecord(name, [ elem ]) when name = listTypeName ->
            // `list<elem>` ≡ `FSharpList\`1<elem>`. Serves every list-typed
            // slot: the `%A` printer's `FSharpFunc` arg, the `PrintfFormat`
            // ctor / `PrintFormatLine` / `Invoke` instantiations, and any
            // list-typed local signature.
            encodeListOf te (fun arg -> encodeType arg elem)
        | other -> failwithf "ClrProvider: cannot encode SemType: %A" other

    let lastSegment (name: string) : string =
        let i = name.LastIndexOf '.'
        if i < 0 then name else name.Substring(i + 1)

    /// `PrintfFormat<!!0, TextWriter, Unit, Unit>` — the parameter type of the
    /// generic `PrintFormatLine<T>`, where the printer slot is method type
    /// parameter 0.
    let encodeFormatParam (te: SignatureTypeEncoder) : unit =
        markFSharpCoreDep "Microsoft.FSharp.Core.PrintfFormat`4"
        markFSharpCoreDep "Microsoft.FSharp.Core.Unit"
        let g = te.GenericInstantiation(ePrintfFormat4, 4, false)
        g.AddArgument().GenericMethodTypeParameter(0)
        g.AddArgument().Type(eTextWriter, false)
        g.AddArgument().Type(eUnit, false)
        g.AddArgument().Type(eUnit, false)

    /// `printfn` → `call PrintfModule::PrintFormatLine<printer>(format)`. The
    /// `printer` (e.g. `int -> unit` for `"%d"`) is the *result* of the head's
    /// curried type `fnTy = PrintfFormat<…> -> printer`. The format object is
    /// already on the stack; the call pops it and pushes the `T` result.
    let emitPrintfn (fnTy: SemType) : CallRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine"

        let resultTy =
            match zonk fnTy with
            | TyFun(_, printer) -> printer
            | other -> failwithf "ClrProvider: printfn has non-function type %A" other

        // Member reference to the generic method definition:
        //   T PrintFormatLine<T>(PrintfFormat<T, TextWriter, Unit, Unit>)
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(genericParameterCount = 1, isInstanceMethod = false)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().GenericMethodTypeParameter(0)),
                (fun (pars: ParametersEncoder) -> encodeFormatParam (pars.AddParameter().Type()))
            )

        let memberRef = ctx.MemberRef(ePrintfModule, "PrintFormatLine", msig)

        // Method specification instantiating T = resultTy.
        let inst = BlobBuilder()
        let specEnc = BlobEncoder(inst).MethodSpecificationSignature(1)
        encodeType (specEnc.AddArgument()) resultTy
        let spec = toEntity (ctx.MethodSpec(toEntity memberRef, inst))

        {
            Emit = fun il -> il.Encoder.Call spec
            ArgCount = 1
            Pushes = 1
        }

    /// `FSharpFunc\`2<a, b>::Invoke(a) : b` for applying a function value of
    /// type `a -> b` to one argument. The member ref is minted against the
    /// instantiated `TypeSpec`, with the signature written in terms of the
    /// parent's generic type parameters (`instance !1 Invoke(!0)`).
    let emitInvoke (funcTy: SemType) : CallRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Core.FSharpFunc`2.Invoke"

        match funcTy with
        | TyFun(a, b) ->
            let tsB = BlobBuilder()
            let te = BlobEncoder(tsB).TypeSpecificationSignature()
            let g = te.GenericInstantiation(eFSharpFunc2, 2, false)
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

            let invokeRef = toEntity (ctx.MemberRef(toEntity typeSpec, "Invoke", msig))

            {
                // `callvirt`: SRM has no helper, so emit the opcode + token.
                Emit =
                    fun il ->
                        il.Encoder.OpCode ILOpCode.Callvirt
                        il.Encoder.Token invokeRef
                // Consumes the receiver func *and* the applied arg; pushes the
                // result. So depth adjusts by 1 - 2 = -1.
                ArgCount = 2
                Pushes = 1
            }
        | other -> failwithf "ClrProvider: cannot invoke non-function type: %A" other

    /// Bare CIL arithmetic intrinsic (`add` / `sub` / `mul`): pops two
    /// operands, pushes one result, references no metadata.
    let arithmetic (opCode: ILOpCode) : CallRecipe =
        {
            Emit = fun il -> il.Encoder.OpCode opCode
            ArgCount = 2
            Pushes = 1
        }

    /// `new PrintfFormat<tyArgs>(string)`.
    let emitPrintfFormatCtor (tyArgs: SemType list) : CtorRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Core.PrintfFormat`4 (.ctor)"
        // TypeSpec for the instantiated generic, used as the member-ref parent.
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(ePrintfFormat4, List.length tyArgs, false)

        for a in tyArgs do
            encodeType (g.AddArgument()) a

        let typeSpec = ctx.TypeSpec tsB

        // Constructor signature: instance void (string).
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

    /// `FSharpList\`1<elem>` as a member-ref parent `TypeSpec` — the declaring
    /// type both list constructors hang off.
    let listTypeSpec (elem: SemType) : EntityHandle =
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        encodeListOf te (fun arg -> encodeType arg elem)
        toEntity (ctx.TypeSpec tsB)

    /// `FSharpList\`1<elem>::Cons(!0, FSharpList\`1<!0>) : FSharpList\`1<!0>` —
    /// the public static cons-cell constructor (what the F# compiler itself
    /// emits for `1 :: rest`). The signature is written in terms of the
    /// declaring type's generic parameter `!0`; the instantiation rides the
    /// parent `TypeSpec`. Head + tail are already on the stack beneath.
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

    /// `FSharpList\`1<elem>::get_Empty() : FSharpList\`1<!0>` — the empty
    /// singleton getter (`[]` / `Nil`; the parameterless `.ctor` is internal,
    /// so this is the only public empty path).
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

    // ---- Closure synthesis support (slice 5) ----

    /// `FSharpFunc\`2<a, b>` as a `TypeSpec` `EntityHandle` — a synthesised
    /// closure's base type. Same encoding as `encodeType`'s `TyFun` case.
    let fsharpFunc2Spec (a: SemType) (b: SemType) : EntityHandle =
        markFSharpCoreDep "Microsoft.FSharp.Core.FSharpFunc`2 (closure base)"
        let tsB = BlobBuilder()
        let te = BlobEncoder(tsB).TypeSpecificationSignature()
        let g = te.GenericInstantiation(eFSharpFunc2, 2, false)
        encodeType (g.AddArgument()) a
        encodeType (g.AddArgument()) b
        toEntity (ctx.TypeSpec tsB)

    /// Member ref to the protected parameterless `FSharpFunc\`2<a,b>::.ctor()`
    /// a closure's own ctor chains to.
    let fsharpFuncCtorRef (a: SemType) (b: SemType) : EntityHandle =
        let parent = fsharpFunc2Spec a b
        let ctorSig = BlobBuilder()

        BlobEncoder(ctorSig)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

        toEntity (ctx.MemberRef(parent, ".ctor", ctorSig))

    /// `instance b Invoke(a)` — the closure's concrete `Invoke` override
    /// signature (the closure type is closed, so no `TypeSpec`-relative typars).
    let invokeSignature (a: SemType) (b: SemType) : BlobBuilder =
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> encodeType (ret.Type()) b),
                (fun (pars: ParametersEncoder) -> encodeType (pars.AddParameter().Type()) a)
            )

        msig

    /// `instance void .ctor(captures…)` — the closure ctor signature, one
    /// concrete parameter per captured value (in field order).
    let closureCtorSignature (captures: SemType list) : BlobBuilder =
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                List.length captures,
                (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                (fun (pars: ParametersEncoder) ->
                    for c in captures do
                        encodeType (pars.AddParameter().Type()) c
                )
            )

        msig

    /// A capture field's signature (`encodeType` of its stored type).
    let fieldSignature (ty: SemType) : BlobBuilder =
        let blob = BlobBuilder()
        let te = BlobEncoder(blob).FieldSignature()
        encodeType te ty
        blob

    /// `List.fold (+) state xs` → `call ListModule::Fold<'T,'State>(folder,
    /// state, list)`. The 2-typar generic method's `'T` / `'State` are read off
    /// the folder parameter of the head's type `fnTy = ('State -> 'T -> 'State)
    /// -> 'State -> 'T list -> 'State`; the member ref is the generic def and a
    /// `MethodSpec` instantiates it. Folder, state, list are already on the
    /// stack beneath; the call pushes the `'State` result.
    let emitFold (fnTy: SemType) : CallRecipe =
        markFSharpCoreDep "Microsoft.FSharp.Collections.ListModule.Fold"

        let elemTy, stateTy =
            match zonk fnTy with
            | TyFun(TyFun(state, TyFun(t, _)), _) -> t, state
            | other -> failwithf "ClrProvider: List.fold has unexpected type %A" other

        // Generic method def signature:
        //   !!1 Fold<'T,'State>(FSharpFunc`2<!!1, FSharpFunc`2<!!0,!!1>>, !!1,
        //                       FSharpList`1<!!0>)   (!!0 = 'T, !!1 = 'State)
        let msig = BlobBuilder()

        BlobEncoder(msig)
            .MethodSignature(genericParameterCount = 2, isInstanceMethod = false)
            .Parameters(
                3,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().GenericMethodTypeParameter(1)),
                (fun (pars: ParametersEncoder) ->
                    let folder = pars.AddParameter().Type()
                    let g = folder.GenericInstantiation(eFSharpFunc2, 2, false)
                    g.AddArgument().GenericMethodTypeParameter(1)
                    let inner = g.AddArgument().GenericInstantiation(eFSharpFunc2, 2, false)
                    inner.AddArgument().GenericMethodTypeParameter(0)
                    inner.AddArgument().GenericMethodTypeParameter(1)
                    pars.AddParameter().Type().GenericMethodTypeParameter(1)
                    encodeListOf (pars.AddParameter().Type()) (fun a -> a.GenericMethodTypeParameter(0))
                )
            )

        let memberRef = ctx.MemberRef(eListModule, "Fold", msig)

        // MethodSpec instantiating <'T = elemTy, 'State = stateTy>.
        let inst = BlobBuilder()
        let specEnc = BlobEncoder(inst).MethodSpecificationSignature(2)
        encodeType (specEnc.AddArgument()) elemTy
        encodeType (specEnc.AddArgument()) stateTy
        let spec = toEntity (ctx.MethodSpec(toEntity memberRef, inst))

        {
            Emit = fun il -> il.Encoder.Call spec
            ArgCount = 3
            Pushes = 1
        }

    /// Encode a method body's locals into a standalone local-variable
    /// signature, reusing `encodeType` for each slot.
    let encodeLocalSignature (locals: SemType list) : StandaloneSignatureHandle =
        let blob = BlobBuilder()
        let enc = BlobEncoder(blob).LocalVariableSignature(List.length locals)

        for t in locals do
            encodeType (enc.AddVariable().Type()) (zonk t)

        ctx.AddStandaloneSignature blob

    /// Member refs + the `AppendFormatted<T>` factory for lowering a
    /// `TExpr.Format` to the `Vesper.Formatter` write-through handler. All four
    /// member kinds hang off the non-generic `Formatter` value type, so the
    /// parent is a plain `TypeRef` (no `TypeSpec`); the generic `AppendFormatted`
    /// is a member ref to the open generic method + a `MethodSpec` per hole.
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
                        pars.AddParameter().Type().Type(eTextWriter, false)
                    )
                )

            toEntity (ctx.MemberRef(eFormatter, ".ctor", s))

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

            toEntity (ctx.MemberRef(eFormatter, ".ctor", s))

        let appendLiteral =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    1,
                    (fun (ret: ReturnTypeEncoder) -> ret.Void()),
                    (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().String())
                )

            toEntity (ctx.MemberRef(eFormatter, "AppendLiteral", s))

        let flush =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(0, (fun (ret: ReturnTypeEncoder) -> ret.Void()), (fun (_: ParametersEncoder) -> ()))

            toEntity (ctx.MemberRef(eFormatter, "Flush", s))

        let toStringAndClear =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = true)
                .Parameters(
                    0,
                    (fun (ret: ReturnTypeEncoder) -> ret.Type().String()),
                    (fun (_: ParametersEncoder) -> ())
                )

            toEntity (ctx.MemberRef(eFormatter, "ToStringAndClear", s))

        // `instance void AppendBool(bool, int32)` / `AppendOctal(int32, int32)`
        // / `AppendUnsigned(uint32, int32)` — the dedicated handler members for
        // `%b` / `%o` / `%u` (no `AppendFormatted<T>(…, fmt)` shape). Each takes
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

            toEntity (ctx.MemberRef(eFormatter, name, s))

        let appendBool = appendMember "AppendBool" (fun te -> te.Boolean())
        let appendOctal = appendMember "AppendOctal" (fun te -> te.Int32())
        let appendUnsigned = appendMember "AppendUnsigned" (fun te -> te.UInt32())

        // `instance void AppendZeroPaddedFloat(float64, string, int32)` — `%0w.pf`
        // (value, "F<prec>" body, field width). Distinct arity from `appendMember`
        // (three params, not (value, alignment)), so its signature is built here.
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

            toEntity (ctx.MemberRef(eFormatter, "AppendZeroPaddedFloat", s))

        let consoleGetter (name: string) : EntityHandle =
            let s = BlobBuilder()

            BlobEncoder(s)
                .MethodSignature(isInstanceMethod = false)
                .Parameters(
                    0,
                    (fun (ret: ReturnTypeEncoder) -> ret.Type().Type(eTextWriter, false)),
                    (fun (_: ParametersEncoder) -> ())
                )

            toEntity (ctx.MemberRef(eConsole, name, s))

        // `AppendFormatted<T>(value [, int alignment] [, string format])` — the
        // overload is selected by which optional params are present (alignment
        // before format, matching the C# declaration), then `<T>` is bound.
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

            let memberRef = ctx.MemberRef(eFormatter, "AppendFormatted", s)
            let inst = BlobBuilder()
            let specEnc = BlobEncoder(inst).MethodSpecificationSignature(1)
            encodeType (specEnc.AddArgument()) (zonk ty)
            toEntity (ctx.MethodSpec(toEntity memberRef, inst))

        {
            HandlerLocal = TyConst formatterTypeName
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

    member _.ObjectType: EntityHandle = eObject

    /// `FSharpFunc\`2<a,b>` `TypeSpec` for a closure's base type.
    member _.ClosureBaseSpec(a: SemType, b: SemType) : EntityHandle = fsharpFunc2Spec a b

    /// Member ref to `FSharpFunc\`2<a,b>::.ctor()` for the closure ctor chain.
    member _.FSharpFuncCtorRef(a: SemType, b: SemType) : EntityHandle = fsharpFuncCtorRef a b

    /// `instance b Invoke(a)` signature for the closure's `Invoke` override.
    member _.InvokeSignature(a: SemType, b: SemType) : BlobBuilder = invokeSignature a b

    /// `instance void .ctor(captures…)` signature for the closure ctor.
    member _.ClosureCtorSignature(captures: SemType list) : BlobBuilder = closureCtorSignature captures

    /// Field signature for a captured value of type `ty`.
    member _.FieldSignature(ty: SemType) : BlobBuilder = fieldSignature ty

    interface ICodegenProvider with
        member _.ObjectType = eObject
        member _.DecimalCtor = eDecimalCtor

        // Sorted for a deterministic, diff-friendly dependency list.
        member _.FSharpCoreDependencies() =
            fsharpCoreDeps |> List.ofSeq |> List.sort

        member _.TryEmitCall(compiledName, fnTy) =
            if compiledName = "List.fold" then
                ValueSome(emitFold (zonk fnTy))
            else
                match lastSegment compiledName with
                | "printfn" -> ValueSome(emitPrintfn (zonk fnTy))
                | "op_Addition" -> ValueSome(arithmetic ILOpCode.Add)
                | "op_Subtraction" -> ValueSome(arithmetic ILOpCode.Sub)
                | "op_Multiply" -> ValueSome(arithmetic ILOpCode.Mul)
                | _ -> ValueNone

        member _.TryEmitCtor(className, tyArgs) =
            if className = PrintfSpec.printfFormatName then
                ValueSome(emitPrintfFormatCtor (List.map zonk tyArgs))
            else
                ValueNone

        member _.TryEmitUnionCons(typeName, caseName, tyArgs) =
            if typeName = listTypeName then
                let elem =
                    match List.map zonk tyArgs with
                    | [ e ] -> e
                    | other -> failwithf "ClrProvider: list type expects one type argument, got %A" other

                match caseName with
                | "Cons" -> ValueSome(emitListCons elem)
                | "Nil" -> ValueSome(emitListNil elem)
                | _ -> ValueNone
            else
                // User-defined DU constructors flow through here too; minting
                // their types + ctors from the provider is a later slice.
                ValueNone

        member _.TryEmitInvoke(funcTy) =
            match zonk funcTy with
            | TyFun _ as ft -> ValueSome(emitInvoke ft)
            | _ -> ValueNone

        member _.FormatHandles() = buildFormatHandles ()

        member _.EncodeLocalSignature(locals) = encodeLocalSignature locals
