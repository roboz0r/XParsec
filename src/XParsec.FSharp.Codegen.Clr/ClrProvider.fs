namespace XParsec.FSharp.Codegen.Clr

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

    let fsCoreRef = toEntity (ctx.AssemblyRef fsharpCore)
    let coreRef = toEntity (ctx.AssemblyRef coreLib)

    // FSharp.Core type references.
    let eUnit = toEntity (ctx.TypeRef(fsCoreRef, "Microsoft.FSharp.Core", "Unit"))

    let ePrintfFormat4 =
        toEntity (ctx.TypeRef(fsCoreRef, "Microsoft.FSharp.Core", "PrintfFormat`4"))

    let ePrintfModule =
        toEntity (ctx.TypeRef(fsCoreRef, "Microsoft.FSharp.Core", "PrintfModule"))

    let eFSharpFunc2 =
        toEntity (ctx.TypeRef(fsCoreRef, "Microsoft.FSharp.Core", "FSharpFunc`2"))

    // BCL type references.
    let eObject = toEntity (ctx.TypeRef(coreRef, "System", "Object"))
    let eTextWriter = toEntity (ctx.TypeRef(coreRef, "System.IO", "TextWriter"))

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

    /// Encode a (zonked) `SemType` into a metadata signature type slot.
    let rec encodeType (te: SignatureTypeEncoder) (t: SemType) : unit =
        match zonk t with
        | TyConst "int" -> te.Int32()
        | TyConst "int64" -> te.Int64()
        | TyConst "byte" -> te.Byte()
        | TyConst "float" -> te.Double()
        | TyConst "bool" -> te.Boolean()
        | TyConst "string" -> te.String()
        | TyConst "unit" -> te.Type(eUnit, false)
        | TyConst "System.IO.TextWriter" -> te.Type(eTextWriter, false)
        | TyFun(a, b) ->
            // `a -> b` is `FSharpFunc\`2<a, b>` at the metadata level.
            let g = te.GenericInstantiation(eFSharpFunc2, 2, false)
            encodeType (g.AddArgument()) a
            encodeType (g.AddArgument()) b
        | TyClass(name, args) when name = PrintfSpec.printfFormatName ->
            let g = te.GenericInstantiation(ePrintfFormat4, List.length args, false)

            for a in args do
                encodeType (g.AddArgument()) a
        | other -> failwithf "ClrProvider: cannot encode SemType: %A" other

    let lastSegment (name: string) : string =
        let i = name.LastIndexOf '.'
        if i < 0 then name else name.Substring(i + 1)

    /// `PrintfFormat<!!0, TextWriter, Unit, Unit>` — the parameter type of the
    /// generic `PrintFormatLine<T>`, where the printer slot is method type
    /// parameter 0.
    let encodeFormatParam (te: SignatureTypeEncoder) : unit =
        let g = te.GenericInstantiation(ePrintfFormat4, 4, false)
        g.AddArgument().GenericMethodTypeParameter(0)
        g.AddArgument().Type(eTextWriter, false)
        g.AddArgument().Type(eUnit, false)
        g.AddArgument().Type(eUnit, false)

    /// `printfn` → `call PrintfModule::PrintFormatLine<resultTy>(format)`.
    /// The format object is already on the stack; the call pops it and pushes
    /// the `T` result (here `Unit`).
    let emitPrintfn (resultTy: SemType) : CallRecipe =
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

    /// Encode a method body's locals into a standalone local-variable
    /// signature, reusing `encodeType` for each slot.
    let encodeLocalSignature (locals: SemType list) : StandaloneSignatureHandle =
        let blob = BlobBuilder()
        let enc = BlobEncoder(blob).LocalVariableSignature(List.length locals)

        for t in locals do
            encodeType (enc.AddVariable().Type()) (zonk t)

        ctx.AddStandaloneSignature blob

    member _.ObjectType: EntityHandle = eObject

    interface ICodegenProvider with
        member _.ObjectType = eObject

        member _.TryEmitCall(compiledName, resultTy) =
            match lastSegment compiledName with
            | "printfn" -> ValueSome(emitPrintfn (zonk resultTy))
            | "op_Addition" -> ValueSome(arithmetic ILOpCode.Add)
            | "op_Subtraction" -> ValueSome(arithmetic ILOpCode.Sub)
            | "op_Multiply" -> ValueSome(arithmetic ILOpCode.Mul)
            | _ -> ValueNone

        member _.TryEmitCtor(className, tyArgs) =
            if className = PrintfSpec.printfFormatName then
                ValueSome(emitPrintfFormatCtor (List.map zonk tyArgs))
            else
                ValueNone

        member _.TryEmitInvoke(funcTy) =
            match zonk funcTy with
            | TyFun _ as ft -> ValueSome(emitInvoke ft)
            | _ -> ValueNone

        member _.EncodeLocalSignature(locals) = encodeLocalSignature locals
