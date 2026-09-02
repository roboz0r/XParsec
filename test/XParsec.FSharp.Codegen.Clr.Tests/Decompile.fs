/// An emitted PE rendered back as C# through ICSharpCode.Decompiler, one top-level type at
/// a time.
module XParsec.FSharp.Codegen.Clr.Tests.Decompile

open System
open System.IO
open System.Reflection.Metadata.Ecma335
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

/// The decompiler over an artifact's bytes. The PE is read from memory; the path only
/// seeds the resolver's search directory, and an unresolved reference renders by name.
let private decompilerOf (artifact: ClrArtifact) : CSharpDecompiler =
    let path = Path.Combine(AppContext.BaseDirectory, artifact.AssemblyName + ".dll")
    let pe = new PEFile(path, new MemoryStream(Codegen.toBytes artifact))
    let resolver = UniversalAssemblyResolver(path, false, pe.DetectTargetFrameworkId())
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
