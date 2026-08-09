namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis

/// Where a `.mjs` sits in the emitted output tree, relative to the output ROOT: the
/// package directory it belongs to (`ValueNone` = the root itself) and its file name.
[<Struct>]
type JsModulePath =
    {
        Package: string voption
        FileName: string
    }

module JsModulePath =

    /// A committed runtime ASSET: copied to the output ROOT, one per package.
    let asset (fileName: string) : JsModulePath =
        {
            Package = ValueNone
            FileName = fileName
        }

    /// The module BASE NAME of a source file: its name without extensions, minus a trailing
    /// `.js` target segment (`ops-platform.js.fs` → `ops-platform`).
    let baseName (relative: string) : string =
        let noExt = System.IO.Path.GetFileNameWithoutExtension relative

        if noExt.EndsWith(".js", System.StringComparison.Ordinal) then
            noExt.Substring(0, noExt.Length - 3)
        else
            noExt

    /// One emitting source file of `package`, inside that package's own directory.
    let ofSource (package: string) (relative: string) : JsModulePath =
        {
            Package = ValueSome package
            FileName = baseName relative + ".mjs"
        }

    /// The module a specifier written FROM THE OUTPUT ROOT names: `./f.mjs` is a root asset,
    /// `./pkg/f.mjs` a module in `pkg`. The inverse of `specifierFrom ValueNone`, so it is
    /// `ValueNone` for anything that inverse never produces: a bare specifier (a node builtin
    /// or an npm package, the host's to resolve) or a `../` escape above the root.
    let tryOfRootSpecifier (specifier: string) : JsModulePath voption =
        if not (specifier.StartsWith("./", System.StringComparison.Ordinal)) then
            ValueNone
        else
            match (specifier.Substring 2).Split '/' with
            | [| file |] -> ValueSome(asset file)
            | [| package; file |] ->
                ValueSome
                    {
                        Package = ValueSome package
                        FileName = file
                    }
            | _ -> ValueNone

    /// The specifier a module in `fromPackage` (`ValueNone` = the output root) names
    /// `target` by: `./f.mjs` within one package, `../pkg/f.mjs` across packages,
    /// `../f.mjs` out to a root asset.
    let specifierFrom (fromPackage: string voption) (target: JsModulePath) : string =
        let toRoot =
            match fromPackage with
            | ValueNone -> "./"
            | ValueSome _ -> "../"

        match target.Package with
        | ValueSome p when fromPackage = ValueSome p -> "./" + target.FileName
        | ValueSome p -> toRoot + p + "/" + target.FileName
        | ValueNone -> toRoot + target.FileName

/// The home of something this build IMPORTS: an assembly, refined to a declaring source
/// file where the producer knew one.
[<Struct>]
type JsHome =
    {
        Assembly: string
        /// `ValueNone` wherever the producer knew only the assembly: a `.fsi` contract
        /// view, a TS manifest, a codegen-synthesised runtime entry.
        DeclaringFile: OriginPath voption
    }

module JsHome =

    /// The backend home a provider `Origin` names. `ValueNone` for one carrying no assembly.
    let tryOfOrigin (home: Origin) : JsHome voption =
        match home.AssemblyOption with
        | ValueNone -> ValueNone
        | ValueSome assembly ->
            ValueSome
                {
                    Assembly = assembly
                    DeclaringFile = home.DeclaringFile
                }

    /// The home an `Origin` names; fails when it carries no assembly, quoting `what`.
    let ofOrigin (what: string) (home: Origin) : JsHome =
        match tryOfOrigin home with
        | ValueSome h -> h
        | ValueNone -> failwithf "JS codegen: %s carries no home assembly" what

    /// A whole package's home, carrying no declaring file, so it resolves to the package's
    /// committed asset rather than to a per-file module.
    let ofAssembly (assembly: string) : JsHome =
        {
            Assembly = assembly
            DeclaringFile = ValueNone
        }
