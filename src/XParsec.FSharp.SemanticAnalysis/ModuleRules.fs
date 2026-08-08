namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// `IsNominalTypeName` asks: does the file being walked declare a record / union / class
/// by that short name?
[<NoEquality; NoComparison>]
type ModuleNaming =
    {
        Lexed: Lexed
        IsNominalTypeName: string -> bool
    }

module ModuleRules =

    /// The name of the static class a module compiles to: `Foo`, or `FooModule` when a
    /// nominal type in the same file is also called `Foo`, or when
    /// `[<CompilationRepresentation(ModuleSuffix)>]` pins the suffix.
    let compiledModuleNameOf (r: ModuleNaming) (attrs: Attributes<SyntaxToken> voption) (name: string) : string =
        if r.IsNominalTypeName name || VesperLibTypeTranslate.hasModuleSuffix r.Lexed attrs then
            name + "Module"
        else
            name

    let compiledModuleName (r: ModuleNaming) (md: ModuleDefn<SyntaxToken>) : string =
        let (ModuleDefn.ModuleDefn(attributes = attrs; ident = ident)) = md
        compiledModuleNameOf r attrs (VesperLibTypeTranslate.nameOfTok r.Lexed ident)

    /// Every container `c` sits in, OUTERMOST first — the declaring namespace, then each
    /// enclosing `module` — paired with the dotted path a local `open` writes for it
    /// (`List`), which is not the compiled module name (`ListModule`).
    let enclosingContainers (r: ModuleNaming) (c: DeclContainment<SyntaxToken>) : (string * ModuleContainer) list =
        let mutable container =
            ModuleContainer.InNamespace(SymbolKeyOps.namespaceKey c.Namespace)

        let mutable path = c.Namespace
        let scopes = ResizeArray(c.Modules.Length + 1)
        scopes.Add(path, container)

        for md in c.Modules do
            let (ModuleDefn.ModuleDefn(ident = ident)) = md
            let src = VesperLibTypeTranslate.nameOfTok r.Lexed ident
            container <- ModuleContainer.InModule(SymbolKeyOps.moduleKeyOf container (compiledModuleName r md))
            path <- if path.Length = 0 then src else path + "." + src
            scopes.Add(path, container)

        List.ofSeq scopes

    /// `namespace N` + `module A = module B =` yields `B ∈ A ∈ N`.
    let containerChain (r: ModuleNaming) (c: DeclContainment<SyntaxToken>) : ModuleContainer =
        enclosingContainers r c |> List.last |> snd

    let typeContainerOf (h: ModuleContainer) : TypeContainer =
        match h with
        | ModuleContainer.InNamespace ns -> TypeContainer.InNamespace ns
        | ModuleContainer.InModule m -> TypeContainer.InModule m

    let typeContainer (r: ModuleNaming) (c: DeclContainment<SyntaxToken>) : TypeContainer =
        typeContainerOf (containerChain r c)
