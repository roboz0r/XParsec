namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// The module-naming rules and the walk's containment entry. Nominal-type collisions are read
/// off the registry at CALL time, so a type declared textually BELOW the module it collides
/// with still renames it.
[<AutoOpen>]
module Containment =

    type PassContext with

        /// The name of the static class a module compiles to: `Foo`, or `FooModule` when a
        /// nominal type in the same file is also called `Foo`, or when
        /// `[<CompilationRepresentation(ModuleSuffix)>]` pins the suffix.
        member private this.CompiledModuleNameOf(attrs: Attributes<SyntaxToken> voption, name: string) : string =
            if
                TypeRegistry.isNominalTypeName this.Types name
                || AttributeDecode.hasModuleSuffix this.NameOf (this.ResolveAttributes attrs)
            then
                name + "Module"
            else
                name

        member private this.CompiledModuleName(md: DeclaredModule<SyntaxToken>) : string =
            this.CompiledModuleNameOf(md.Attributes, this.NameOf md.Ident)

        /// Every container `c` sits in (the declaring namespace, then each enclosing `module`),
        /// OUTERMOST first, paired with the dotted path a local `open` writes for it
        /// (`List`), which is not the compiled module name (`ListModule`).
        member private this.EnclosingContainersOf(c: DeclContainment<SyntaxToken>) : (string * ModuleContainer) list =
            let mutable container =
                ModuleContainer.InNamespace(SymbolKeyOps.namespaceKey c.Namespace)

            let mutable path = c.Namespace
            let scopes = ResizeArray(c.Modules.Length + 1)
            scopes.Add(path, container)

            for md in c.Modules do
                let src = this.NameOf md.Ident
                container <- ModuleContainer.InModule(SymbolKeyOps.moduleKeyOf container (this.CompiledModuleName md))
                path <- SymbolKeyOps.qualify path src
                scopes.Add(path, container)

            List.ofSeq scopes

        /// Each `[<AutoOpen>]` module enclosing `c`, outermost first: what something declared
        /// here is reachable through with no `open` written for it.
        member this.ImplicitOpensOf(c: DeclContainment<SyntaxToken>) : ImplicitOpen list =
            let mutable container =
                ModuleContainer.InNamespace(SymbolKeyOps.namespaceKey c.Namespace)

            let opened = ResizeArray<ImplicitOpen>()

            for md in c.Modules do
                let key = SymbolKeyOps.moduleKeyOf container (this.CompiledModuleName md)
                container <- ModuleContainer.InModule key

                if AttributeDecode.isAutoOpen (this.ResolveAttributes md.Attributes) then
                    opened.Add(ImplicitOpen.AutoOpen key)

            List.ofSeq opened

        /// `namespace N` + `module A = module B =` yields `B ∈ A ∈ N`.
        member this.ContainerChainOf(c: DeclContainment<SyntaxToken>) : ModuleContainer =
            this.EnclosingContainersOf c |> List.last |> snd

        member this.TypeContainerOf(c: DeclContainment<SyntaxToken>) : TypeContainer =
            SymbolKeyOps.typeContainerOf (this.ContainerChainOf c)

        /// Enter a module containment: sets and returns the chain a by-name read from inside
        /// resolves against. Every enclosing scope is noted under the SOURCE path an `open`
        /// writes it as.
        member this.EnterContainment(c: DeclContainment<SyntaxToken>) : ModuleContainer =
            let scopes = this.EnclosingContainersOf c

            for (path, container) in scopes do
                TypeRegistry.noteLocalContainer this.Types path container

            let chain = scopes |> List.last |> snd
            this.Resolution.EnclosingContainer <- ValueSome chain
            chain

        /// Enter a walked module element, advancing both ambient facts a by-name read resolves
        /// against: the `open`s in scope and the module chain.
        member this.EnterElement(w: WalkedIn<SyntaxToken, 'Elem>) : unit =
            this.Resolution.OpenScope <- w.Scope
            this.EnterContainment w.Containment |> ignore
