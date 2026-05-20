namespace XParsec.FSharp.Codegen.Clr

// Per-build configuration the CLR backend reads. Per
// [backend-design-plan](../XParsec.FSharp.SemanticAnalysis/docs/backend-design-plan.md):
// a record of `option`-typed slots, so targets read different fields and
// future additions stay non-breaking.

type ProjectInfo =
    {
        /// Emitted assembly's simple name (no extension).
        AssemblyName: string
        /// Name of the static class that holds top-level `let`s and `Main`
        /// (F#'s `<ModuleName>` convention). Cosmetic for execution; the
        /// entry point is found via the PE's entry-point token.
        ModuleName: string
        /// Where `materialise` writes the PE. `None` keeps the artifact
        /// in-memory only (the end-to-end tests load the bytes directly).
        OutputPath: string option
        /// Emitted assembly's target-framework moniker. `None` uses the v1
        /// default. Note this is the *emitted* assembly's TFM — the codegen
        /// project itself always targets net8.0.
        TargetFramework: string option
        /// Explicit `FSharp.Core.dll` path. `None` derives the reference
        /// identity from the FSharp.Core already loaded in the codegen host
        /// process (see [[project_dotnet_provider_stack]]).
        FSharpCorePath: string option
    }

module ProjectInfo =

    /// A v1 default `ProjectInfo` for an in-memory build of `assemblyName`.
    let defaults (assemblyName: string) : ProjectInfo =
        {
            AssemblyName = assemblyName
            ModuleName = "Program"
            OutputPath = None
            TargetFramework = None
            FSharpCorePath = None
        }
