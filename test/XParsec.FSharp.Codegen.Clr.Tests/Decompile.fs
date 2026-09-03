/// An emitted PE rendered back as C# through ICSharpCode.Decompiler, one top-level type at
/// a time.
module XParsec.FSharp.Codegen.Clr.Tests.Decompile

open System
open System.IO
open System.Reflection.Metadata.Ecma335
open System.Threading.Tasks
open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.CSharp
open ICSharpCode.Decompiler.Metadata
open ICSharpCode.Decompiler.TypeSystem
open XParsec.FSharp.Codegen.Clr

/// Layout-faithful rendering: a factory body shows its field stores in emitted order
/// rather than an object initialiser, and a constructor stays a constructor.
let private layoutSettings () : DecompilerSettings =
    let s = DecompilerSettings(LanguageVersion.CSharp12_0)
    s.ObjectOrCollectionInitializers <- false
    s.UsePrimaryConstructorSyntax <- false
    s.UsePrimaryConstructorSyntaxForNonRecordTypes <- false
    s.ShowXmlDocumentation <- false
    s.ThrowOnAssemblyResolveErrors <- false
    s

/// Resolves a compile's own references from `sources`; every other name — the BCL,
/// FSharp.Core — goes to `fallback`.
type private ReferenceResolver(sources: Map<string, string>, fallback: IAssemblyResolver) =
    let fromSources (reference: IAssemblyReference) : MetadataFile option =
        sources
        |> Map.tryFind reference.Name
        |> Option.map (fun path -> new PEFile(path) :> MetadataFile)

    interface IAssemblyResolver with
        member _.Resolve(reference) =
            match fromSources reference with
            | Some file -> file
            | None -> fallback.Resolve reference

        member _.ResolveAsync(reference) =
            match fromSources reference with
            | Some file -> Task.FromResult file
            | None -> fallback.ResolveAsync reference

        member _.ResolveModule(mainModule, moduleName) =
            fallback.ResolveModule(mainModule, moduleName)

        member _.ResolveModuleAsync(mainModule, moduleName) =
            fallback.ResolveModuleAsync(mainModule, moduleName)

/// Raises unless every `AssemblyRef` the PE binds resolves. An unresolved reference renders
/// by name, with its struct types shown as classes.
let private assertReferencesResolve (assemblyName: string) (resolver: IAssemblyResolver) (pe: PEFile) : unit =
    for reference in pe.AssemblyReferences do
        if isNull (resolver.Resolve reference) then
            failwithf "%s: referenced assembly '%s' did not resolve" assemblyName reference.Name

/// The decompiler over an artifact's bytes. The PE is read from memory; `path` names it and
/// seeds the probe directory for the framework references.
let private decompilerOf (artifact: ClrArtifact) : CSharpDecompiler =
    let path = Path.Combine(AppContext.BaseDirectory, artifact.AssemblyName + ".dll")
    let pe = new PEFile(path, new MemoryStream(Codegen.toBytes artifact))
    let sources = ProjectInfo.referenceSources artifact.Project

    let resolver =
        ReferenceResolver(sources, UniversalAssemblyResolver(path, false, pe.DetectTargetFrameworkId()))
        :> IAssemblyResolver

    assertReferencesResolve artifact.AssemblyName resolver pe
    new CSharpDecompiler(pe, resolver, layoutSettings ())

/// The given top-level types in order, each with every nested type and member, as one C#
/// rendering. `fullNames` are metadata spellings: `Ns.Name` or `Name`, with the arity suffix
/// for a generic type (`` GBox`1 ``). Raises on a name the assembly does not define.
let typesAsCSharp (artifact: ClrArtifact) (fullNames: string list) : string =
    let decompiler = decompilerOf artifact

    let handles =
        [
            for n in fullNames ->
                match decompiler.TypeSystem.MainModule.GetTypeDefinition(FullTypeName n) with
                | null -> failwithf "%s defines no type '%s'" artifact.AssemblyName n
                | td -> MetadataTokens.TypeDefinitionHandle(MetadataTokens.GetRowNumber td.MetadataToken)
        ]

    decompiler.DecompileTypesAsString handles

/// Every type the assembly defines, with its assembly- and module-level attributes, as one
/// C# rendering.
let moduleAsCSharp (artifact: ClrArtifact) : string =
    (decompilerOf artifact).DecompileWholeModuleAsString()
