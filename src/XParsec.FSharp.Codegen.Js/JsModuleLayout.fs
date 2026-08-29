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

    [<Literal>]
    let BarrelFileName = "index.mjs"

    /// A file at the output ROOT, which is where a compiled PROGRAM lands.
    let atRoot (fileName: string) : JsModulePath =
        {
            Package = ValueNone
            FileName = fileName
        }

    /// The barrel a consumer enters `package` by, re-exporting everything the package ships.
    let barrel (package: string) : JsModulePath =
        {
            Package = ValueSome package
            FileName = BarrelFileName
        }

    /// A committed runtime file of `package`, shipped inside that package's own directory.
    let asset (package: string) (fileName: string) : JsModulePath =
        {
            Package = ValueSome package
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

    /// The module a specifier written FROM `fromPackage` denotes — the inverse of `specifierFrom`,
    /// so it is `ValueNone` for anything that inverse never produces: a bare specifier (a node
    /// builtin or an npm package, the host's to resolve) or a `../` escape above the root.
    let tryOfSpecifier (fromPackage: string voption) (specifier: string) : JsModulePath voption =
        let named (segments: string[]) : JsModulePath voption =
            match segments with
            | [| file |] -> ValueSome(atRoot file)
            | [| package; file |] -> ValueSome(asset package file)
            | _ -> ValueNone

        if specifier.StartsWith("../", System.StringComparison.Ordinal) then
            // Only a module INSIDE a package has a `..` to climb, and it climbs to the root.
            match fromPackage with
            | ValueSome _ -> named ((specifier.Substring 3).Split '/')
            | ValueNone -> ValueNone
        elif specifier.StartsWith("./", System.StringComparison.Ordinal) then
            match fromPackage, (specifier.Substring 2).Split '/' with
            | ValueSome p, [| file |] -> ValueSome(asset p file)
            | ValueNone, segments -> named segments
            | ValueSome _, _ -> ValueNone
        else
            ValueNone

    /// The specifier a module in `fromPackage` (`ValueNone` = the output root) uses for
    /// `target`: `./f.mjs` within one package, `../pkg/f.mjs` across packages,
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

/// WHICH module of its home assembly something is imported from.
[<RequireQualifiedAccess>]
type JsHomeWhere =
    /// The declaring source file's own module, inside the package's output directory.
    | InFile of file: AssemblyFilePath
    /// The package as a whole, a consumer resolving a dependency knowing only which one
    /// declared the symbol.
    | Package
    /// A committed file of the package's output, which no source file declares: the
    /// structural runtime, the format runtime.
    | RuntimeAsset

/// The home of something this build IMPORTS.
[<Struct>]
type JsHome =
    { Assembly: string; Where: JsHomeWhere }

module JsHome =

    /// The backend home a provider `SymbolHome` identifies. `ValueNone` for one carrying no assembly.
    let tryOfOrigin (home: SymbolHome) : JsHome voption =
        match home with
        | SymbolHome.Unstamped -> ValueNone
        | SymbolHome.InAssembly a ->
            ValueSome
                {
                    Assembly = a.Name
                    Where = JsHomeWhere.Package
                }
        | SymbolHome.InFile f ->
            ValueSome
                {
                    Assembly = AssemblyName.toStored f.Assembly
                    Where = JsHomeWhere.InFile f
                }

    /// The home an `SymbolHome` identifies; fails when it carries no assembly, quoting `what`.
    let ofOrigin (what: string) (home: SymbolHome) : JsHome =
        match tryOfOrigin home with
        | ValueSome h -> h
        | ValueNone -> failwithf "JS codegen: %s carries no home assembly" what

    /// The home of a runtime entry the BACKEND synthesises: no source file declares it, so it
    /// resolves to a committed file of the package's output rather than to a compiled module.
    let ofAssembly (assembly: string) : JsHome =
        {
            Assembly = assembly
            Where = JsHomeWhere.RuntimeAsset
        }

/// WHICH JS class a nominal type is, and how the module being emitted reaches it. Every case
/// is an identifier in that module's scope, but they are bought differently, and only
/// `Imported` costs an import.
[<RequireQualifiedAccess>]
type JsClassRef =
    /// A class this module emits, named directly.
    | Local of name: string
    /// A class the runtime provides — `Error`, a `[<Global>]` type — named bare.
    | Global of name: string
    /// A class another package emits, imported from its home module.
    | Imported of home: JsHome * name: string

module JsClassRef =

    /// A nominal whose home says where it lives: `ValueNone` is this module's own.
    let ofHome (home: JsHome voption) (name: string) : JsClassRef =
        match home with
        | ValueSome h -> JsClassRef.Imported(h, name)
        | ValueNone -> JsClassRef.Local name

/// A class's `inherit` chain as JS lowers it: the immediate base first, each further base
/// after it, ending at the runtime class the chain bottoms out in. NON-EMPTY by construction,
/// so holding one is the proof that an `extends` clause is emittable.
type JsPrototypeChain = private { Bases: JsClassRef list }

module JsPrototypeChain =

    /// `ValueNone` for the empty walk, which is exactly the chain with nothing to extend.
    let tryOfBases (bases: JsClassRef list) : JsPrototypeChain voption =
        match bases with
        | [] -> ValueNone
        | _ -> ValueSome { Bases = bases }

    /// The class an emitted declaration writes in its `extends` clause.
    let extends (chain: JsPrototypeChain) : JsClassRef = List.head chain.Bases

/// What walking a class's `inherit` clause found — the ONE verdict both the emit filter and
/// the `extends` clause read, so they cannot disagree about a base.
[<RequireQualifiedAccess>]
type JsBaseVerdict =
    /// No `inherit` clause: the class stands alone.
    | Standalone
    /// The chain bottoms out in a runtime class, which the emitted declaration extends.
    | Extends of JsPrototypeChain
    /// The chain reaches a representation naming nothing at runtime (`"!Vesper.Attribute"`).
    /// There is no class to extend and nothing constructs one, so the declaration is dropped.
    | Erased
    /// The chain reaches neither: an ordinary hierarchy, whose members lower to free functions
    /// that no prototype chain would dispatch. Rejected rather than mis-run.
    | Unsupported
