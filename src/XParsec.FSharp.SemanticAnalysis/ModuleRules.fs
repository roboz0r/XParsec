namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// The facts the compiled-holder-name rule reads, and NOTHING else. Deliberately not a
/// `PassContext`: the contract extractor runs UPSTREAM of every pass and has no
/// `PassContext` to offer, yet it must name a module's holder type exactly as the local
/// key mint does. Narrowing the rule's inputs to this record is what lets one
/// implementation serve both faces.
///
///   * `Lexed` / `Input` — the source text the module's attributes are read out of.
///   * `IsNominalTypeName` — does THIS file declare a record / union / class
///     by that short name? A live predicate, not a snapshot: each face answers it from
///     its own name table.
[<NoEquality; NoComparison>]
type ModuleNaming =
    {
        Lexed: Lexed
        Input: string
        IsNominalTypeName: string -> bool
    }

/// Module-specific rules — the compiled holder name a `module` takes, and the
/// `ModuleHolder` chain a declaration's containment sits in. Compiled BEFORE both of its
/// readers (the contract extractor and name resolution) so neither can re-implement half
/// of a rule the other implements whole.
module ModuleRules =

    /// The COMPILED holder-type name of a module: `Foo`, or `FooModule` when the module
    /// would otherwise collide with a same-named nominal type in this file, or when
    /// `[<CompilationRepresentation(ModuleSuffix)>]` pins the suffix. A module compiles to
    /// a static class, and this is that class's name.
    ///
    /// BOTH halves, always. A reader that applied only the attribute half would name the
    /// holder of `module SetTree` (alongside `type SetTree`) `SetTree` while a reader
    /// applying both named it `SetTreeModule` — two names for one class. That is why the
    /// rule is stated once, over an input set (`ModuleNaming`) narrow enough for every
    /// reader to construct.
    ///
    /// Stated over the two facts a module HEADER carries — its attributes and its SOURCE
    /// name — rather than over one syntax, because there are three: an implementation
    /// `ModuleDefn`, a signature `ModuleSignature`, and the trailing segment of a
    /// `module A.B.C` signature header. All three must name the holder class the same way.
    let holderNameOf (r: ModuleNaming) (attrs: Attributes<SyntaxToken> voption) (name: string) : string =
        if
            r.IsNominalTypeName name
            || VesperLibTypeTranslate.hasModuleSuffix r.Lexed r.Input attrs
        then
            name + "Module"
        else
            name

    /// `holderNameOf` over an implementation file's module header.
    let holderName (r: ModuleNaming) (md: ModuleDefn<SyntaxToken>) : string =
        let (ModuleDefn.ModuleDefn(attributes = attrs; ident = ident)) = md
        holderNameOf r attrs (VesperLibTypeTranslate.nameOfTok r.Lexed r.Input ident)

    /// Every scope `c` sits in, OUTERMOST first — the declaring namespace, then each
    /// enclosing `module` — each paired with the dotted SOURCE path a local `open` names it
    /// by. Both are built in ONE walk, so an `open`'s written path and the holder it opens
    /// cannot drift: `ModuleKey.Name` carries the module's COMPILED holder name
    /// (`ListModule`), which is precisely not what the source writes.
    ///
    /// SOURCE vs COMPILED module name, decided here: the COMPILED name is what a
    /// `ModuleKey` means at every other mint — `SymbolKeyOps.moduleFullName` renders it as
    /// a *type* name and `ClrEnv.externalModuleRef` emits a `TypeRef` for it — so a
    /// source-named local `ModuleKey` would make one type mean two things depending on
    /// which producer minted it. The SOURCE path exists only to answer "which scope does
    /// this `open` name", and never leaves that question.
    let holderScopes (r: ModuleNaming) (c: DeclContainment<SyntaxToken>) : (string * ModuleHolder) list =
        let mutable holder = ModuleHolder.InNamespace(SymbolKeyOps.namespaceKey c.Namespace)
        let mutable path = c.Namespace
        let scopes = ResizeArray(c.Modules.Length + 1)
        scopes.Add(path, holder)

        for md in c.Modules do
            let (ModuleDefn.ModuleDefn(ident = ident)) = md
            let src = VesperLibTypeTranslate.nameOfTok r.Lexed r.Input ident
            holder <- ModuleHolder.InModule(SymbolKeyOps.moduleKeyOf holder (holderName r md))
            path <- if path.Length = 0 then src else path + "." + src
            scopes.Add(path, holder)

        List.ofSeq scopes

    /// The `ModuleHolder` a declaration in `c` sits in: the declaring namespace, or — for a
    /// declaration inside a `module` — the FULL enclosing module chain rooted in that
    /// namespace. Every local holder chain is built here, so a chain is nested exactly as
    /// far as the source is: `module A = module B =` yields `B ∈ A ∈ N`, never `B ∈ N`.
    let holderChain (r: ModuleNaming) (c: DeclContainment<SyntaxToken>) : ModuleHolder =
        holderScopes r c |> List.last |> snd

    /// A containment chain read as a TYPE's holder. THE sole producer of
    /// `TypeHolder.InModule`: both faces that know about modules — local registration and
    /// the `.fsi` contract extractor — build the chain as a `ModuleHolder` and come here,
    /// so a type declared in a module gets one holder, not one per face.
    let typeHolderOf (h: ModuleHolder) : TypeHolder =
        match h with
        | ModuleHolder.InNamespace ns -> TypeHolder.InNamespace ns
        | ModuleHolder.InModule m -> TypeHolder.InModule m

    /// The `TypeHolder` a type declared in `c` sits in — `holderChain` read as a type's
    /// holder.
    let typeHolder (r: ModuleNaming) (c: DeclContainment<SyntaxToken>) : TypeHolder = typeHolderOf (holderChain r c)
