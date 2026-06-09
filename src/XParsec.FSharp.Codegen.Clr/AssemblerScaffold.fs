namespace XParsec.FSharp.Codegen.Clr

open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

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

    /// `instance <ret> <name><'C…>(<params…>)` for an abstract interface method. The
    /// signature's open typars are self-describing `TyTypar` nodes (Freeze remaps the
    /// declaring axis to `!i` and the method axis to `!!j`), encoded by the provider's
    /// `EncodeAbstractType` (the same `encodeType` the executable path uses).
    let abstractMethodSignature (provider: ClrProvider) (m: Frozen.TAbstractMethod) : BlobBuilder =
        let paramTys, retTy = decurry m.Signature
        let blob = BlobBuilder()

        BlobEncoder(blob)
            .MethodSignature(genericParameterCount = m.MethodTypeParams.Length, isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                (fun (ret: ReturnTypeEncoder) -> provider.EncodeAbstractType(ret.Type(), retTy)),
                (fun (pars: ParametersEncoder) ->
                    for p in paramTys do
                        provider.EncodeAbstractType(pars.AddParameter().Type(), p)
                )
            )

        blob

    /// Shared assembly scaffolding for a hand-written `Main` body: module +
    /// assembly rows, a `Main` whose body comes from `build`, the `<Module>`
    /// pseudo-type, and the holder class. The hand-written body forms no
    /// function value / list literal / external member access, so `Vesper.Fun`
    /// / `Vesper.List` are never needed and the symbol provider is the null one.
    let assembleWith (project: ProjectInfo) (build: MetadataContext -> ClrProvider -> (Il -> unit)) : ClrArtifact =
        let ctx = MetadataContext()
        ctx.AddModuleAndAssembly(project.AssemblyName)

        let provider =
            ClrProvider(ctx, IntrinsicRepr.defaults, Map.empty, ExternalSymbols.nullProvider, project.AssemblyName)

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
