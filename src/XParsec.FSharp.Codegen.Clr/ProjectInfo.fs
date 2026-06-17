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
        /// Paths to the assemblies this build references that are *not* resolved off
        /// the codegen host. Each path's identity is read off the file, so the
        /// emitted `AssemblyRef` matches that exact artifact rather than whatever the
        /// compiler happens to have loaded. The backend resolves which referenced assembly
        /// provides a given type by simple name:
        ///   - `Vesper.Core` owns `Vesper.Fun\`2` (every function value implements it
        ///     — a function value with no `Vesper.Core` reference fails to
        ///     encode (`Fun` is unreferenceable).
        ///   - `Vesper.List` owns `Vesper.Collections.List\`1` (a list literal /
        ///     `List.fold`); a list with no `Vesper.List`
        ///     reference fails to encode.
        ///   - `Vesper.Printf` owns `Vesper.Formatter` (the happy-path `printf` / `%A`
        ///     handler); a printf-bearing program with no `Vesper.Printf` reference
        ///     fails to encode (the C# DLL is off the backend's TPA, so there is no host fallback).
        ///   - `FSharp.Core` is *optional* here: when not listed, the provider falls
        ///     back to the host-loaded copy (so the cold-printf island resolves
        ///     without the caller wiring a path); when listed, that file's identity wins.
        /// A package never references itself (`Vesper.Core` lists no core, etc.).
        /// `materialiseApp` copies the referenced assemblies the emitted PE actually
        /// binds against beside it (so a happy-path bundle, referencing no
        /// FSharp.Core, ships none).
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

    /// On-disk app at `<outDir>/<assemblyName>.dll`, where `materialiseApp` also
    /// drops the `runtimeconfig.json` and copies the referenced assemblies the PE
    /// binds (its `Vesper.*` libraries / an FSharp.Core cold path) beside it to run.
    let app (assemblyName: string) (outDir: string) : ProjectInfo =
        { defaults assemblyName with
            OutputPath = Some(System.IO.Path.Combine(outDir, assemblyName + ".dll"))
        }

    let library (assemblyName: string) : ProjectInfo =
        { defaults assemblyName with
            OutputKind = Library
        }
