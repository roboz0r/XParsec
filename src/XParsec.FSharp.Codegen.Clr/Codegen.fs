namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// Entry points: the `compile` / `materialise` pair from
// [backend-design-plan](../XParsec.FSharp.SemanticAnalysis/docs/backend-design-plan.md).
// `compile` is pure-ish (deterministic given the same inputs); `materialise`
// is the only side effect.

/// The in-memory assembled PE plus enough to inspect / write it.
type ClrArtifact =
    {
        AssemblyName: string
        OutputPath: string option
        /// The serialised PE image.
        Pe: BlobBuilder
        /// The distinct FSharp.Core constructs the emission referenced
        /// (from `ICodegenProvider.FSharpCoreDependencies`). **Empty ⇒ the PE
        /// has no `FSharp.Core.dll` dependency**, so `materialiseApp` skips
        /// copying it; non-empty is the list of constructs still pinning it.
        FSharpCoreDependencies: string list
    }

module Codegen =

    /// `int Main(string[])` — the synthesised entry point's signature.
    let private mainSignature () : BlobBuilder =
        let sigB = BlobBuilder()

        BlobEncoder(sigB)
            .MethodSignature()
            .Parameters(
                1,
                (fun (ret: ReturnTypeEncoder) -> ret.Type().Int32()),
                (fun (pars: ParametersEncoder) -> pars.AddParameter().Type().SZArray().String())
            )

        sigB

    /// Shared assembly scaffolding: module + assembly rows, a `Main` whose
    /// body comes from `build`, the `<Module>` pseudo-type, and the holder
    /// class. `build` receives the wired context + provider so callers emit
    /// either from a TAST or from a hand-written op.
    let private assembleWith
        (project: ProjectInfo)
        (build: MetadataContext -> ClrProvider -> (Il -> unit))
        : ClrArtifact =
        let ctx = MetadataContext()
        ctx.AddModuleAndAssembly(project.AssemblyName)

        let provider = ClrProvider(ctx)
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

        // `<Module>` (row 1, empty method range) then the holder class, which
        // claims `Main` onward. No closures on this seam, so the holder owns
        // the (empty) field table from row 1.
        ctx.AddModuleType(mainDef)

        ctx.AddProgramType(project.ModuleName, provider.ObjectType, MetadataTokens.FieldDefinitionHandle(1), mainDef)
        |> ignore

        {
            AssemblyName = project.AssemblyName
            OutputPath = project.OutputPath
            Pe = ctx.Serialize(mainDef)
            // Captured after the body build above ran every encoder/recipe.
            FSharpCoreDependencies = icodegen.FSharpCoreDependencies()
        }

    /// The full multi-type program build (the `compile` path). Synthesised
    /// closures and the `Program` holder coexist with contiguous field/method
    /// ranges and only backward metadata references, achieved by emitting
    /// closures **leaves-first** (every inner closure before the outer one that
    /// constructs it, `Program` last) so each `newobj` resolves to an
    /// already-added ctor handle — no handle prediction. The discover/emit
    /// split mirrors the inline pre-pass: a cheap walk that makes the second
    /// walk's references resolvable.
    let private assembleProgram (project: ProjectInfo) (tast: TastFile) : ClrArtifact =
        let ctx = MetadataContext()
        ctx.AddModuleAndAssembly(project.AssemblyName)

        let provider = ClrProvider(ctx)
        let icodegen = provider :> ICodegenProvider
        let encodeLocals locals = icodegen.EncodeLocalSignature locals

        // One body-stream encoder shared by every method: its ctor requires a
        // 4-byte-aligned IL builder, so a fresh encoder per body would throw
        // once the first (tiny) body left the builder unaligned. `AddMethodBody`
        // realigns per body internally, so reuse is correct.
        let bodyStream = ctx.BodyStream

        let lowered = Emit.lower tast.Decls
        let closures, closureByNode = Emit.discoverClosures lowered
        let ctorHandleByNode = Dictionary<TExpr, EntityHandle>(HashIdentity.Reference)

        let closureAttrs =
            TypeAttributes.Class
            ||| TypeAttributes.Public
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.AutoLayout
            ||| TypeAttributes.AnsiClass
            ||| TypeAttributes.BeforeFieldInit

        let ctorAttrs =
            MethodAttributes.Public
            ||| MethodAttributes.HideBySig
            ||| MethodAttributes.SpecialName
            ||| MethodAttributes.RTSpecialName

        // Reuse-slot virtual (no `NewSlot`) so `Invoke` overrides the base
        // `FSharpFunc\`2::Invoke` abstract slot by its instantiated signature.
        let invokeAttrs =
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.HideBySig

        // Emit each closure leaves-first: add its capture fields, build its
        // ctor + `Invoke` bodies, then add its method rows. Defer the
        // `TypeDefinition` rows (collected here) until every field/method row
        // exists, so the type ranges are contiguous.
        let mutable fieldCount = 0
        let mutable firstMethod = ValueNone

        let closureTypes =
            ResizeArray<string * EntityHandle * FieldDefinitionHandle * MethodDefinitionHandle>()

        for c in closures do
            let firstField = MetadataTokens.FieldDefinitionHandle(fieldCount + 1)
            let captureFields = Dictionary<NodeKey, EntityHandle>()

            let fieldHandles =
                c.Captures
                |> List.mapi (fun i (k, ty) ->
                    let h =
                        ctx.AddField(FieldAttributes.Public, sprintf "capture%d" i, provider.FieldSignature ty)

                    captureFields.[k] <- toEntity h
                    fieldCount <- fieldCount + 1
                    toEntity h
                )

            let baseCtor = provider.FSharpFuncCtorRef(c.ParamTy, c.ResultTy)

            let ctorBodyOffset =
                Cil.buildBody encodeLocals bodyStream (Emit.emitClosureCtor baseCtor fieldHandles)

            let invokeBodyOffset =
                Cil.buildBody
                    encodeLocals
                    bodyStream
                    (Emit.emitClosureInvoke icodegen ctx closureByNode ctorHandleByNode c captureFields)

            let ctorHandle =
                ctx.AddMethod(
                    ctorAttrs,
                    ".ctor",
                    provider.ClosureCtorSignature(List.map snd c.Captures),
                    ctorBodyOffset
                )

            ctx.AddMethod(invokeAttrs, "Invoke", provider.InvokeSignature(c.ParamTy, c.ResultTy), invokeBodyOffset)
            |> ignore

            if firstMethod.IsNone then
                firstMethod <- ValueSome ctorHandle

            ctorHandleByNode.[c.Node] <- toEntity ctorHandle
            closureTypes.Add(c.Name, provider.ClosureBaseSpec(c.ParamTy, c.ResultTy), firstField, ctorHandle)

        let mainBodyOffset =
            Cil.buildBody encodeLocals bodyStream (Emit.emitMain icodegen ctx closureByNode ctorHandleByNode lowered)

        let mainDef =
            ctx.AddMethod(
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.HideBySig,
                "Main",
                mainSignature (),
                mainBodyOffset
            )

        let firstMethodHandle =
            match firstMethod with
            | ValueSome h -> h
            | ValueNone -> mainDef

        // TypeDefinition rows in ascending field/method order.
        ctx.AddModuleType(firstMethodHandle)

        for (name, baseSpec, firstField, ctorHandle) in closureTypes do
            ctx.AddClass(closureAttrs, name, baseSpec, firstField, ctorHandle) |> ignore

        let programFirstField = MetadataTokens.FieldDefinitionHandle(fieldCount + 1)

        ctx.AddProgramType(project.ModuleName, provider.ObjectType, programFirstField, mainDef)
        |> ignore

        {
            AssemblyName = project.AssemblyName
            OutputPath = project.OutputPath
            Pe = ctx.Serialize(mainDef)
            // Captured after every closure + Main body build above ran their
            // encoders/recipes, so the dependency set is complete.
            FSharpCoreDependencies = icodegen.FSharpCoreDependencies()
        }

    /// TAST + symbol context → in-memory PE artifact. The `symbols` provider
    /// is accepted per the shared-inputs posture; slice 1 reads everything it
    /// needs from the TAST and the target `ClrProvider`.
    let compile (_symbols: IExternalSymbolProvider) (project: ProjectInfo) (tast: TastFile) : ClrArtifact =
        assembleProgram project tast

    /// Assemble a single hand-written `Main` body (a typed `Op` from the empty
    /// stack) into an artifact. The testable seam for the `Cil` body DSL,
    /// independent of any TAST.
    let assembleMainOp (project: ProjectInfo) (op: Op<E, 'out>) : ClrArtifact =
        assembleWith project (fun _ _ -> fun il -> op null null il)

    /// Assemble a hand-written `Main` body that drives the untyped `Il`
    /// surface directly — the testable seam for bodies that declare locals
    /// (the typed `Op` CE can't thread a slot index without `Bind`).
    let assembleMainEmit (project: ProjectInfo) (build: Il -> unit) : ClrArtifact =
        assembleWith project (fun _ _ -> build)

    /// The serialised PE bytes.
    let toBytes (artifact: ClrArtifact) : byte[] = artifact.Pe.ToArray()

    /// The only side effect: write the PE to `OutputPath` when one is set.
    let materialise (artifact: ClrArtifact) : unit =
        match artifact.OutputPath with
        | Some path ->
            use stream = new FileStream(path, FileMode.Create, FileAccess.Write)
            artifact.Pe.WriteContentTo(stream)
        | None -> ()

    /// TFM + shared-framework version to target, derived from the runtime the
    /// codegen host runs on. The emitted `AssemblyRef`s bind against exactly
    /// the assemblies loaded in this process (`project_dotnet_provider_stack`),
    /// so the produced app must run on the same major — we read the version
    /// straight off the host rather than guess.
    let private hostFramework () : string * string =
        let v = System.Environment.Version
        sprintf "net%d.%d" v.Major v.Minor, sprintf "%d.%d.0" v.Major v.Minor

    /// The `runtimeconfig.json` a framework-dependent console app needs beside
    /// its dll — `dotnet <app>.dll` refuses to start without one. Targets the
    /// shared `Microsoft.NETCore.App`; `rollForward: Major` lets it run on a
    /// newer installed runtime than the exact version requested.
    let private runtimeConfigJson (tfm: string) (frameworkVersion: string) : string =
        System.String.Join(
            "\n",
            [
                "{"
                "  \"runtimeOptions\": {"
                sprintf "    \"tfm\": \"%s\"," tfm
                "    \"rollForward\": \"Major\","
                "    \"framework\": {"
                "      \"name\": \"Microsoft.NETCore.App\","
                sprintf "      \"version\": \"%s\"" frameworkVersion
                "    }"
                "  }"
                "}"
                ""
            ]
        )

    /// Materialise a *runnable* framework-dependent app: the PE (via
    /// `materialise`), its `runtimeconfig.json`, and a copy of `FSharp.Core.dll`
    /// (which the shared framework does *not* carry) into the PE's directory,
    /// where the loader's app-base probe finds it. `System.Private.CoreLib`
    /// resolves from the shared framework automatically. After this,
    /// `dotnet <OutputPath>` runs the program. Requires `OutputPath`.
    let materialiseApp (project: ProjectInfo) (artifact: ClrArtifact) : unit =
        match artifact.OutputPath with
        | None -> failwith "Codegen.materialiseApp: ProjectInfo.OutputPath must be set"
        | Some dllPath ->
            let dir = Path.GetDirectoryName dllPath
            Directory.CreateDirectory dir |> ignore
            materialise artifact

            let tfm, frameworkVersion =
                match project.TargetFramework with
                | Some t ->
                    let v = System.Environment.Version
                    t, sprintf "%d.%d.0" v.Major v.Minor
                | None -> hostFramework ()

            File.WriteAllText(
                Path.Combine(dir, project.AssemblyName + ".runtimeconfig.json"),
                runtimeConfigJson tfm frameworkVersion
            )

            // Copy FSharp.Core only when the PE actually references it. The
            // happy-path printf / arithmetic / interpolation lowerings touch no
            // FSharp.Core construct, so a program built entirely from them ships
            // without it. `FSharpCoreDependencies` is the authoritative signal:
            // every FSharp.Core reference is minted through `ClrProvider`, which
            // records each use-site, so an empty set means nothing in the IL can
            // bind against `FSharp.Core.dll`. (See its non-empty contents for the
            // constructs still pinning the dependency — the §D3 cut list.)
            if not (List.isEmpty artifact.FSharpCoreDependencies) then
                let fsharpCoreSrc =
                    match project.FSharpCorePath with
                    | Some p -> p
                    | None -> typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location

                let fsharpCoreDst = Path.Combine(dir, "FSharp.Core.dll")

                if
                    not (
                        System.String.Equals(
                            Path.GetFullPath fsharpCoreSrc,
                            Path.GetFullPath fsharpCoreDst,
                            System.StringComparison.OrdinalIgnoreCase
                        )
                    )
                then
                    File.Copy(fsharpCoreSrc, fsharpCoreDst, true)

            // The bootstrap printf runtime, like FSharp.Core, is absent from the
            // shared framework — copy it beside the PE so a fully-applied literal
            // printf (lowered to `Vesper.Formatter` calls) resolves at runtime.
            let vesperPrintfSrc = typeof<Vesper.PrintfRuntime>.Assembly.Location
            let vesperPrintfDst = Path.Combine(dir, "Vesper.Printf.dll")

            if
                not (
                    System.String.Equals(
                        Path.GetFullPath vesperPrintfSrc,
                        Path.GetFullPath vesperPrintfDst,
                        System.StringComparison.OrdinalIgnoreCase
                    )
                )
            then
                File.Copy(vesperPrintfSrc, vesperPrintfDst, true)
