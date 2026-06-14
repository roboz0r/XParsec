namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis

/// Per-build configuration for the JS backend — far thinner than the CLR
/// `ProjectInfo` (no assembly references, no TFM): a JS module is a single `.js`
/// file. `OutputPath = None` keeps the emitted source in-memory.
type JsProjectInfo =
    {
        ModuleName: string
        /// `None` keeps the emitted source in-memory only.
        OutputPath: string option
    }

module JsProjectInfo =
    let defaults (moduleName: string) : JsProjectInfo =
        {
            ModuleName = moduleName
            OutputPath = None
        }

/// The emitted artifact: the ESM source plus the resolved output path (if any).
type JsArtifact =
    {
        Source: string
        OutputPath: string option
    }

/// Entry point mirroring `Codegen.Clr.Codegen.compile`. Step 0a takes only the
/// project + frozen TAST: the `IExternalSymbolProvider` the CLR backend threads is
/// not yet needed (nothing resolves a runtime import until a real one lands), so
/// it is omitted rather than carried unused, and rejoins the signature when Step 5
/// imports `Vesper.List`.
module Codegen =

    /// Frozen TAST → in-memory JS artifact. Deterministic given the same input.
    let compile (project: JsProjectInfo) (tast: Frozen.TastFile) : JsArtifact =
        {
            Source = JsPrint.print (EmitJs.buildProgram tast)
            OutputPath = project.OutputPath
        }

    /// The emitted ESM source text.
    let toSource (artifact: JsArtifact) : string = artifact.Source

    /// The only side effect: write the `.js` to `OutputPath` when one is set.
    let materialise (artifact: JsArtifact) : unit =
        match artifact.OutputPath with
        | Some path -> System.IO.File.WriteAllText(path, artifact.Source)
        | None -> ()
