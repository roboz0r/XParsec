namespace XParsec.FSharp.Codegen.Clr

// Per-build configuration the CLR backend reads: a record of `option`-typed
// slots, so targets read different fields and future additions stay non-breaking.

type OutputKind =
    | Exe
    | Library

type ProjectInfo =
    {
        AssemblyName: string
        /// Cosmetic for execution; the entry point is found via the PE's
        /// entry-point token, not this name.
        ModuleName: string
        /// `None` keeps the artifact in-memory only.
        OutputPath: string option
        /// The *emitted* assembly's TFM (the codegen project itself always
        /// targets net8.0). `None` uses the default.
        TargetFramework: string option
        /// `None` derives the reference identity from the FSharp.Core already
        /// loaded in the codegen host process.
        FSharpCorePath: string option
        OutputKind: OutputKind
    }

module ProjectInfo =

    let defaults (assemblyName: string) : ProjectInfo =
        {
            AssemblyName = assemblyName
            ModuleName = "Program"
            OutputPath = None
            TargetFramework = None
            FSharpCorePath = None
            OutputKind = Exe
        }

    /// On-disk app at `<outDir>/<assemblyName>.dll`, where `materialiseApp`
    /// also drops the `runtimeconfig.json` + `FSharp.Core.dll` it needs to run.
    let app (assemblyName: string) (outDir: string) : ProjectInfo =
        { defaults assemblyName with
            OutputPath = Some(System.IO.Path.Combine(outDir, assemblyName + ".dll"))
        }

    let library (assemblyName: string) : ProjectInfo =
        { defaults assemblyName with
            OutputKind = Library
        }
