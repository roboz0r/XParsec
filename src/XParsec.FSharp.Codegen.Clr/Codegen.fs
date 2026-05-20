namespace XParsec.FSharp.Codegen.Clr

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
        // claims `Main` onward.
        ctx.AddModuleType(mainDef)
        ctx.AddProgramType(project.ModuleName, provider.ObjectType, mainDef) |> ignore

        {
            AssemblyName = project.AssemblyName
            OutputPath = project.OutputPath
            Pe = ctx.Serialize(mainDef)
        }

    /// TAST + symbol context → in-memory PE artifact. The `symbols` provider
    /// is accepted per the shared-inputs posture; slice 1 reads everything it
    /// needs from the TAST and the target `ClrProvider`.
    let compile (_symbols: IExternalSymbolProvider) (project: ProjectInfo) (tast: TastFile) : ClrArtifact =
        assembleWith project (fun ctx provider -> Emit.emitMain (provider :> ICodegenProvider) ctx tast.Decls)

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
