namespace XParsec.FSharp.Codegen.Clr

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
        /// Paths to the assemblies this build references, matched by simple name; each
        /// path's identity is read off the file. A missing `Vesper.*` fails the encode
        /// that needs it, while `FSharp.Core` falls back to the host-loaded copy.
        References: string list
        OutputKind: OutputKind
    }

module ProjectInfo =

    let defaults (assemblyName: string) : ProjectInfo =
        {
            AssemblyName = assemblyName
            ModuleName = "Program"
            OutputPath = None
            TargetFramework = None
            References = []
            OutputKind = Exe
        }

    /// On-disk app at `<outDir>/<assemblyName>.dll`, beside which `materialiseApp` drops
    /// the `runtimeconfig.json` and the referenced assemblies the PE binds.
    let app (assemblyName: string) (outDir: string) : ProjectInfo =
        { defaults assemblyName with
            OutputPath = Some(System.IO.Path.Combine(outDir, assemblyName + ".dll"))
        }

    let library (assemblyName: string) : ProjectInfo =
        { defaults assemblyName with
            OutputKind = Library
        }
