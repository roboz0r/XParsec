namespace XParsec.FSharp.Codegen.Clr

open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// Stateless helpers shared by the converged `Assembler` and the `Codegen`
/// facade: signature builders and the hand-written-body test seam
/// (`assembleWith`).
module internal AssemblerScaffold =

    /// `int Main(string[])` — the synthesised entry point's signature.
    let mainSignature () : BlobBuilder =
        let sigB = BlobBuilder()

        BlobEncoder(sigB)
            .MethodSignature()
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().SZArray().String())
            )

        sigB

    /// `FTFun('A, 'B)` ⇒ `(['A], 'B)`.
    let rec decurry (t: FrozenType) : FrozenType list * FrozenType =
        match t with
        | FTFun(a, b) ->
            let ps, r = decurry b
            a :: ps, r
        | _ -> [], t

    let argNames (n: int) : string list =
        [ for i in 0 .. n - 1 -> sprintf "arg%d" i ]

    let private isUnitTy t =
        match t with
        | FTConst("unit", _) -> true
        | _ -> false

    /// The metadata parameter types of an abstract member after F#'s
    /// nullary-`unit` elision: `member : unit -> X` is a *no-arg* method, so a
    /// sole leading `unit` argument is dropped (matching concrete-member emission
    /// and the BCL slots these conform to — `IDisposable.Dispose()`,
    /// `IFormatSink.Line()`). Shared by the signature encoder and the `Param`-row /
    /// name emission so the two never disagree on arity (a mismatch makes the
    /// emitted method un-reflectable: "parameters and signature don't match").
    let abstractMethodParamTys (m: Frozen.TAbstractMethod) : FrozenType list =
        let paramTys, _ = decurry m.Signature

        match paramTys with
        | [ single ] when isUnitTy single -> []
        // A multi-arg abstract member (`abstract Invoke : 'A * 'B -> 'C`) is modelled
        // with a single tupled domain, but F# emits it as an N-param method — and the
        // conforming impl member (`member _.Invoke(a, b)`) emits N params too. Flatten
        // a sole leading tuple back to N parameters so the slot signature matches the
        // impl's `Param` rows; otherwise the runtime can't bind the impl to the slot
        // ("does not have an implementation"). This mirrors the external-method tuple
        // flattening in `ClrExternalMembers` (a genuine single `(('A*'B))` param is not
        // expressible in an abstract member sig, so there is no ambiguity here).
        | [ FTTuple elems ] when elems.Length >= 2 -> EqArray.toList elems
        | _ -> paramTys

    /// `instance <ret> <name><'C…>(<params…>)` for an abstract interface method. The
    /// signature's open typars are self-describing `TyTypar` nodes (Freeze remaps the
    /// declaring axis to `!i` and the method axis to `!!j`), encoded by the provider's
    /// `EncodeAbstractType` (the same `encodeType` the executable path uses).
    let abstractMethodSignature (provider: ClrProvider) (m: Frozen.TAbstractMethod) : BlobBuilder =
        let _, retTy = decurry m.Signature
        let paramTys = abstractMethodParamTys m
        let blob = BlobBuilder()

        BlobEncoder(blob)
            .MethodSignature(genericParameterCount = m.MethodTypeParams.Length, isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                // `-> unit` encodes as genuine `void`, not the `unit`-as-`ValueTuple`
                // value convention: an abstract slot is `callvirt`ed for effect, and a
                // conforming impl must bind to a `void` slot (cf. NominalEmit's
                // `returnsVoid` on the implementer side).
                (fun (ret: ReturnTypeEncoder) ->
                    if isUnitTy retTy then
                        ret.Void()
                    else
                        provider.EncodeAbstractType(ret.Type(), retTy)
                ),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        provider.EncodeAbstractType(pars.AddParameter().Type(), p)
                )
            )

        blob

    /// Shared assembly scaffolding for a hand-written `Main` body: module +
    /// assembly rows, a `Main` whose body comes from `build`, the `<Module>`
    /// pseudo-type, and the holder class. The hand-written body forms no function
    /// value / list literal / external member access (so `Vesper.Fun` / `Vesper.List`
    /// are never needed and `references` is empty), but it DOES name primitives —
    /// whose reprs are read from `symbols` (the single source: the real Vesper.Core
    /// provider the caller supplies), not a codegen-local table. Own-unit intrinsics
    /// are empty: a hand-written body declares no `(# … #)` types of its own.
    let assembleWith
        (symbols: IExternalSymbolProvider)
        (project: ProjectInfo)
        (build: MetadataContext -> ClrProvider -> (Il -> unit))
        : ClrArtifact =
        let ctx = MetadataContext()
        ctx.AddModuleAndAssembly(project.AssemblyName)

        let provider = ClrProvider(ctx, Map.empty, Map.empty, symbols, project.AssemblyName)

        let icodegen = provider :> ICodegenProvider

        let bodyOffset =
            Cil.buildBody (fun locals -> icodegen.EncodeLocalSignature locals) ctx.BodyStream (build ctx provider)

        let mainDef =
            ctx.AddMethod(
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.HideBySig,
                "Main",
                mainSignature (),
                bodyOffset
            )

        ctx.AddModuleType(mainDef)

        ctx.AddProgramType(
            "",
            project.ModuleName,
            provider.ObjectType,
            MetadataTokens.FieldDefinitionHandle(1),
            mainDef,
            true
        )
        |> ignore

        {
            AssemblyName = project.AssemblyName
            OutputPath = project.OutputPath
            Pe = ctx.Serialize(mainDef)
            ReferencedAssemblies = ctx.ReferencedAssemblyNames
            FSharpCoreDependencies = icodegen.FSharpCoreDependencies()
        }
