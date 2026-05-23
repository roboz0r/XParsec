namespace XParsec.FSharp.Codegen.Clr

// Per-build configuration the CLR backend reads. Per
// [backend-design-plan](../XParsec.FSharp.SemanticAnalysis/docs/backend-design-plan.md):
// a record of `option`-typed slots, so targets read different fields and
// future additions stay non-breaking.

/// Whether `compile` emits an executable (a synthesised `Main` entry point +
/// the `Program` holder) or a library (declared types only, no entry point).
type OutputKind =
    | Exe
    | Library

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
        /// Executable (default) or library. `compile` routes `Library` to the
        /// no-entry-point `assembleLibrary` path.
        OutputKind: OutputKind
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
            OutputKind = Exe
        }

    /// A `ProjectInfo` for a runnable on-disk app: the PE lands at
    /// `<outDir>/<assemblyName>.dll`, where `Codegen.materialiseApp` also
    /// drops the `runtimeconfig.json` and `FSharp.Core.dll` it needs to run
    /// under `dotnet <assemblyName>.dll`.
    let app (assemblyName: string) (outDir: string) : ProjectInfo =
        { defaults assemblyName with
            OutputPath = Some(System.IO.Path.Combine(outDir, assemblyName + ".dll"))
        }

    /// A `ProjectInfo` for an in-memory library build — declared types only, no
    /// entry point. `OutputPath` stays `None` (callers load the bytes directly);
    /// set it for an on-disk DLL.
    let library (assemblyName: string) : ProjectInfo =
        { defaults assemblyName with
            OutputKind = Library
        }
