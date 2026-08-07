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
    let holderNameOf (r: ModuleNaming) (attrs: Attributes<SyntaxToken> voption) (name: string) : string =
        if r.IsNominalTypeName name || VesperLibTypeTranslate.hasModuleSuffix r.Lexed attrs then
            name + "Module"
        else
            name

    let holderName (r: ModuleNaming) (md: ModuleDefn<SyntaxToken>) : string =
        let (ModuleDefn.ModuleDefn(attributes = attrs; ident = ident)) = md
        holderNameOf r attrs (VesperLibTypeTranslate.nameOfTok r.Lexed ident)

    /// Every scope `c` sits in, OUTERMOST first — the declaring namespace, then each
    /// enclosing `module` — paired with the dotted path a local `open` writes for it
    /// (`List`), which is not the compiled name inside the holder (`ListModule`).
    let holderScopes (r: ModuleNaming) (c: DeclContainment<SyntaxToken>) : (string * ModuleHolder) list =
        let mutable holder = ModuleHolder.InNamespace(SymbolKeyOps.namespaceKey c.Namespace)
        let mutable path = c.Namespace
        let scopes = ResizeArray(c.Modules.Length + 1)
        scopes.Add(path, holder)

        for md in c.Modules do
            let (ModuleDefn.ModuleDefn(ident = ident)) = md
            let src = VesperLibTypeTranslate.nameOfTok r.Lexed ident
            holder <- ModuleHolder.InModule(SymbolKeyOps.moduleKeyOf holder (holderName r md))
            path <- if path.Length = 0 then src else path + "." + src
            scopes.Add(path, holder)

        List.ofSeq scopes

    /// `namespace N` + `module A = module B =` yields `B ∈ A ∈ N`.
    let holderChain (r: ModuleNaming) (c: DeclContainment<SyntaxToken>) : ModuleHolder =
        holderScopes r c |> List.last |> snd

    let typeHolderOf (h: ModuleHolder) : TypeHolder =
        match h with
        | ModuleHolder.InNamespace ns -> TypeHolder.InNamespace ns
        | ModuleHolder.InModule m -> TypeHolder.InModule m

    let typeHolder (r: ModuleNaming) (c: DeclContainment<SyntaxToken>) : TypeHolder = typeHolderOf (holderChain r c)
