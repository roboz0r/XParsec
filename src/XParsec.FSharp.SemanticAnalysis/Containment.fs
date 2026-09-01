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
            /// The class name the module emits as, where that differs from the name its source
            /// writes. A namespace has none.
            CompiledName: CompiledName voption
            /// `[<AutoOpen>]` is written on the module.
            IsAutoOpen: bool
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
                    CompiledName = ValueNone
                    IsAutoOpen = false
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
                        CompiledName = CompiledName.OfPair(src, compiled)
                        IsAutoOpen = AttributeDecode.isAutoOpen attrs
                    }

            {
                Scopes = List.ofSeq scopes
                Chain = container
            }

        /// Each `[<AutoOpen>]` module enclosing `c`, outermost first.
        member this.ImplicitOpensOf(c: DeclContainment<SyntaxToken>) : ImplicitOpen list =
            [
                for scope in (this.EnclosingChainOf c).Scopes do
                    match scope.Container with
                    | ModuleContainer.InModule m when scope.IsAutoOpen -> ImplicitOpen.AutoOpen m
                    | _ -> ()
            ]

        /// `namespace N` + `module A = module B =` yields `B ∈ A ∈ N`.
        member this.ContainerChainOf(c: DeclContainment<SyntaxToken>) : ModuleContainer =
            (this.EnclosingChainOf c).Chain

        member this.TypeContainerOf(c: DeclContainment<SyntaxToken>) : TypeContainer =
            SymbolKeyOps.typeContainerOf (this.ContainerChainOf c)

        /// Enter a module containment: sets and returns the chain a by-name read from inside
        /// resolves against. Every enclosing scope is noted under the SOURCE path an `open`
        /// writes it as, with the compiled class name of each module that has one. `recScope`
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
                | ModuleContainer.InModule m ->
                    match scope.CompiledName with
                    | ValueSome compiled -> TypeRegistry.noteCompiledModuleName this.Types m compiled
                    | ValueNone -> ()

                    if scope.IsAutoOpen then
                        TypeRegistry.noteAutoOpenModule this.Types m
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
            this.Resolution.Abbrevs <- env.Aliases

        /// Report `abbrev`, written at `containment`, unless its target is a module: FS0039 for
        /// a target that reaches nothing, FS0965 for a namespace. The target reads only the
        /// declarations above the abbreviation, and needs `EnterElement` on the abbreviation
        /// to have run first.
        member this.ReportAbbrevTarget(containment: DeclContainment<SyntaxToken>, abbrev: ModuleAbbrev<SyntaxToken>) =
            match CstModuleTree.localAbbrev this.NameOf containment abbrev with
            | ValueNone -> ()
            | ValueSome a ->
                let target =
                    TypeRegistry.resolveAbbrevTargetAt this.Types this.Resolver.Scope this.Resolution.OpenScope a

                let (ModuleAbbrev.ModuleAbbrev(longIdent = li)) = abbrev

                match target with
                | TypeRegistry.AbbrevTarget.Module _ -> ()
                | TypeRegistry.AbbrevTarget.Namespace -> this.Report(li.Idents.[0], Kind.AbbreviatedNamespace a.Path)
                | TypeRegistry.AbbrevTarget.Unresolved ->
                    this.Report(li.Idents.[0], Kind.UnresolvedQualifiedName a.Path)
