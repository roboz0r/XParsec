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
        /// Path to the compiled `Vesper.Core.dll` whose `Vesper.Fun\`2` interface
        /// every emitted function value implements (R1 / D3). `Some path` lets the
        /// backend mint the `Vesper.Core` `AssemblyRef` (identity read off the
        /// file) and `materialiseApp` copy it beside the PE. `None` means no
        /// external core — correct when compiling `Vesper.Core` itself (it *defines*
        /// `Fun`) or a program with no function values; a program that *does* form a
        /// function value then fails to encode (`Vesper.Fun` is unreferenceable).
        VesperCorePath: string option
        /// Path to the compiled `Vesper.List.dll` whose `Vesper.Collections.List\`1`
        /// a list literal / `List.fold` reference (package-split-plan PS2 — the list
        /// is its own package now, no longer in `Vesper.Core.dll`). `Some path` lets
        /// the backend mint the `Vesper.List` `AssemblyRef` (identity read off the
        /// file) and `materialiseApp` copy it beside the PE. `None` means no external
        /// list — correct when compiling `Vesper.List` itself or a program with no
        /// list; a program that *does* use a list then fails to encode (the list
        /// type is unreferenceable).
        VesperListPath: string option
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
            VesperCorePath = None
            VesperListPath = None
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
