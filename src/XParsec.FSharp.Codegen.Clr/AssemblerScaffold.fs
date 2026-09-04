namespace XParsec.FSharp.Codegen.Clr

open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

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

    /// `instance <ret> <name><'C…>(<params…>)` for an abstract interface method.
    let abstractMethodSignature (provider: ClrProvider) (m: Frozen.TAbstractMethod) : BlobBuilder =
        let _, retTy = uncurry m.Signature
        let paramTys = List.map snd (abstractMethodParams m)
        let blob = BlobBuilder()

        BlobEncoder(blob)
            .MethodSignature(genericParameterCount = m.MethodTypeParams.Length, isInstanceMethod = true)
            .Parameters(
                List.length paramTys,
                // `-> unit` encodes as genuine `void`, not the `unit`-as-`ValueTuple`
                // value: an abstract slot is `callvirt`ed for effect, and a conforming
                // impl must bind to a `void` slot.
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

    /// Assembly scaffolding for a hand-written `Main` body: module + assembly rows, a
    /// `Main` whose body comes from `build`, the `<Module>` pseudo-type and the "Program"
    /// class.  Primitive reprs are read from the caller's `symbols`.
    let assembleWith
        (symbols: ICodegenSymbols)
        (project: ProjectInfo)
        (build: MetadataContext -> ClrProvider -> (Il -> unit))
        : ClrArtifact =
        let ctx = MetadataContext()
        ctx.AddModuleAndAssembly(project.AssemblyName)

        // No own-file intrinsics and no assembly references: a hand-written body declares
        // no `(# … #)` types of its own and forms no function value or list literal.
        let provider =
            ClrProvider(ctx, System.Collections.Generic.Dictionary(), Map.empty, symbols)

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

        // The scaffold's Program class owns no values, so it keeps `BeforeFieldInit`.
        ctx.AddClass(
            TypeAttributes.Class
            ||| TypeAttributes.Public
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.AutoLayout
            ||| TypeAttributes.BeforeFieldInit,
            "",
            project.ModuleName,
            provider.ObjectType,
            MetadataTokens.FieldDefinitionHandle(1),
            mainDef
        )
        |> ignore

        {
            Project = project
            Pe = ctx.Serialize(mainDef)
            ReferencedAssemblies = ctx.ReferencedAssemblyNames
            SkippedAttributeRows = []
        }
