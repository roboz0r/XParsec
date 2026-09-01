namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// The module-naming rules and the walk's containment entry. Nominal-type collisions are read
/// off the registry at CALL time, so a type declared textually BELOW the module it collides
/// with still renames it.
[<AutoOpen>]
module Containment =

    /// One scope of a declaration's enclosing chain.
    [<NoComparison>]
    type private EnclosingScope =
        {
            Container: ModuleContainer
            /// Where the scope's own NAME enters the environment of the scope declaring it.
            VisibleFrom: int
            /// The dotted path a local `open` writes for the scope (`List`).
            SourcePath: string
            /// What the module's declaration states. A namespace takes `ModuleFacts.plain`.
            Facts: ModuleFacts
        }

    /// A declaration's enclosing scopes, OUTERMOST first, with the innermost chain itself.
    [<NoComparison>]
    type private EnclosingChain =
        {
            Scopes: EnclosingScope list
            Chain: ModuleContainer
        }

    type PassContext with

        /// The name of the static class a module compiles to: `Foo`, or `FooModule` when a
        /// nominal type in the same file is also called `Foo`, or when
        /// `[<CompilationRepresentation(ModuleSuffix)>]` pins the suffix.
        member private this.CompiledModuleNameOf(attrs: ResolvedAttributes, name: string) : string =
            if
                TypeRegistry.isNominalTypeName this.Types name
                || AttributeDecode.hasModuleSuffix this.NameOf attrs
            then
                name + "Module"
            else
                name

        /// Every container `c` sits in: the declaring namespace, then each enclosing `module`.
        member private this.EnclosingChainOf(c: DeclContainment<SyntaxToken>) : EnclosingChain =
            let mutable container =
                ModuleContainer.InNamespace(SymbolKeyOps.namespaceKey c.Namespace)

            let mutable path = c.Namespace
            let scopes = ResizeArray(c.Modules.Length + 1)

            scopes.Add
                {
                    Container = container
                    VisibleFrom = BindingRank.unpositioned
                    SourcePath = path
                    Facts = ModuleFacts.plain
                }

            for md in c.Modules do
                let src = this.NameOf md.Ident
                let attrs = this.ResolveAttributes md.Attributes
                let compiled = this.CompiledModuleNameOf(attrs, src)
                container <- ModuleContainer.InModule(SymbolKeyOps.moduleKeyOf container src)
                path <- SymbolKeyOps.qualify path src

                scopes.Add
                    {
                        Container = container
                        VisibleFrom = md.ModuleToken.StartIndex
                        SourcePath = path
                        Facts =
                            {
                                CompiledName = CompiledName.OfPair(src, compiled)
                                RequiresQualifiedAccess = AttributeDecode.isRequireQualifiedAccess attrs
                                IsAutoOpen = AttributeDecode.isAutoOpen attrs
                            }
                    }

            {
                Scopes = List.ofSeq scopes
                Chain = container
            }

        /// `namespace N` + `module A = module B =` yields `B ∈ A ∈ N`.
        member this.ContainerChainOf(c: DeclContainment<SyntaxToken>) : ModuleContainer =
            (this.EnclosingChainOf c).Chain

        member this.TypeContainerOf(c: DeclContainment<SyntaxToken>) : TypeContainer =
            SymbolKeyOps.typeContainerOf (this.ContainerChainOf c)

        /// Enter a module containment: sets and returns the chain a by-name read from inside
        /// resolves against. Every enclosing scope is noted under the SOURCE path an `open`
        /// writes it as, and each enclosing module with what its declaration states. `recScope`
        /// is the innermost enclosing `rec` scope's keyword offset, which hoists a module
        /// declared inside it to the top of that scope.
        member this.EnterContainment(c: DeclContainment<SyntaxToken>, recScope: int voption) : ModuleContainer =
            let enclosing = this.EnclosingChainOf c

            for scope in enclosing.Scopes do
                TypeRegistry.noteLocalContainer
                    this.Types
                    scope.SourcePath
                    {
                        Container = scope.Container
                        VisibleFrom =
                            match recScope with
                            | ValueSome off -> min off scope.VisibleFrom
                            | ValueNone -> scope.VisibleFrom
                    }

                match scope.Container with
                | ModuleContainer.InModule m -> TypeRegistry.noteModule this.Types m scope.Facts
                | ModuleContainer.InNamespace _ -> ()

            this.Resolution.EnclosingContainer <- ValueSome enclosing.Chain
            enclosing.Chain

        /// The scopes in force at an element whose enclosing chain is `chain`: the resolved
        /// `open`s written above it, the chain and each scope enclosing it, then what is in
        /// scope with no `open` written for it, best rank first. A scope reached twice keeps
        /// its best rank.
        member private this.ScopeStackOf(chain: ModuleContainer, opens: ScopeEntry list) : ScopeEntry list =
            [
                yield! opens

                for h in chain.SelfAndAncestors do
                    {
                        Container = h
                        Route = ScopeRoute.Lexical h.Depth
                    }
            ]
            |> ScopeEntry.withAmbient this.ImplicitOpens

        /// Enter a walked module element, advancing every ambient fact a by-name read resolves
        /// against: the `open`s in scope, the module chain and the aliases in force. The `open`s
        /// resolve after the element's own chain is registered.
        member this.EnterElement(w: WalkedIn<SyntaxToken, 'Elem>) : unit =
            this.Resolution.OpenScope <- w.Scope
            let chain = this.EnterContainment(w.Containment, w.RecScopeOffset)
            let env = TypeRegistry.resolveScopeDecls this.Types this.Resolver.Scope w.Scope
            this.Resolution.Scopes <- this.ScopeStackOf(chain, env.Opens)
            this.Resolution.Env <- env

        /// Report `import`, written at `containment`, when it opens a
        /// `[<RequireQualifiedAccess>]` module: FS0892 naming the TARGET's full path, so an
        /// `open` written through a module abbreviation names what it reached. Reads the
        /// environment `EnterElement` set for the `open`, which holds the declarations above it.
        member this.ReportOpenTarget(containment: DeclContainment<SyntaxToken>, import: ImportDecl<SyntaxToken>) =
            match import with
            // `open type` is a member channel rather than a prefix, and carries no such refusal.
            | ImportDecl.ImportDeclType _ -> ()
            | ImportDecl.ImportDecl(openToken = kw; longIdent = li) ->
                match CstModuleTree.localOpen this.NameOf containment kw li with
                | ValueNone -> ()
                | ValueSome o ->
                    let scope = this.Resolver.Scope

                    match TypeRegistry.resolveInEnv this.Types scope this.Resolution.Env o.Scope o.Path o.Offset with
                    | ValueSome c when TypeRegistry.requiresQualifiedAccess this.Types scope c ->
                        this.Report(li.Idents.[0], Kind.RequireQualifiedAccessModule(SymbolKeyOps.containerFullName c))
                    | _ -> ()

        /// Report `abbrev`, written at `containment`, unless its target is a module: FS0039 for
        /// a target that reaches nothing, FS0965 for a namespace. Reads the environment
        /// `EnterElement` set for the abbreviation, which holds the declarations above it.
        member this.ReportAbbrevTarget(containment: DeclContainment<SyntaxToken>, abbrev: ModuleAbbrev<SyntaxToken>) =
            match CstModuleTree.localAbbrev this.NameOf containment abbrev with
            | ValueNone -> ()
            | ValueSome a ->
                let target =
                    TypeRegistry.resolveAbbrevTarget this.Types this.Resolver.Scope this.Resolution.Env a

                let (ModuleAbbrev.ModuleAbbrev(longIdent = li)) = abbrev

                match target with
                | TypeRegistry.AbbrevTarget.Module _ -> ()
                | TypeRegistry.AbbrevTarget.Namespace -> this.Report(li.Idents.[0], Kind.AbbreviatedNamespace a.Path)
                | TypeRegistry.AbbrevTarget.Unresolved ->
                    this.Report(li.Idents.[0], Kind.UnresolvedQualifiedName a.Path)
