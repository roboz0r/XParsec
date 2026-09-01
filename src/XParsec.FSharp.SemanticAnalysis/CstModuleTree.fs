namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Parser

// A file's module tree, FLATTENED: every declaration paired with the ambient facts at its
// position, namely its `OpenScope`, the containment declaring it, and its `rec` scope.
// One walk serves both grammars, so a `.fs` and its `.fsi` place a declaration alike.

/// One enclosing `module`, narrowed to what the naming rules read: the attributes that may
/// pin the `Module` suffix, and the identifier the source writes it with. A `module Foo = …`
/// and a `module Foo = begin … end` signature reduce to the same pair.
type DeclaredModule<'T> =
    {
        Attributes: Attributes<'T> voption
        Ident: 'T
        /// The `module` keyword: what `rec` is measured from, and what orders the module
        /// against the declarations of its own scope.
        ModuleToken: 'T
    }

module DeclaredModule =

    let ofModuleDefn (md: ModuleDefn<'T>) : DeclaredModule<'T> =
        let (ModuleDefn.ModuleDefn(attributes = attrs; moduleToken = kw; ident = ident)) =
            md

        {
            Attributes = attrs
            Ident = ident
            ModuleToken = kw
        }

    let ofModuleSignature (ms: ModuleSignature<'T>) : DeclaredModule<'T> =
        let (ModuleSignature.ModuleSignature(attributes = attrs; moduleToken = kw; ident = ident)) =
            ms

        {
            Attributes = attrs
            Ident = ident
            ModuleToken = kw
        }

/// The declaring containment of an element: the `namespace` group it sits in (dotted;
/// `""` for an anonymous / global / named-module file) and the `module` declarations it
/// is nested in.
type DeclContainment<'T> =
    {
        Namespace: string
        /// Outermost first.
        Modules: DeclaredModule<'T> list
    }

module DeclContainment =

    let ofNamespace (ns: string) : DeclContainment<'T> = { Namespace = ns; Modules = [] }

    let enter (md: DeclaredModule<'T>) (c: DeclContainment<'T>) : DeclContainment<'T> =
        { c with Modules = c.Modules @ [ md ] }

    /// The dotted SOURCE path of this containment (`"N.A.B"`; `""` at the top of an
    /// anonymous module): the namespace plus each enclosing module's name AS WRITTEN,
    /// never its compiled module name (`ListModule`).
    let sourcePath (nameOf: 'T -> string) (c: DeclContainment<'T>) : string =
        let mutable path = c.Namespace

        for m in c.Modules do
            let seg = nameOf m.Ident
            path <- if path.Length = 0 then seg else path + "." + seg

        path

/// One flattened element of a module tree, nested modules already descended into, with
/// the ambient facts a pass needs at that position. `'Elem` is the grammar's element type.
type WalkedIn<'T, 'Elem> =
    {
        Elem: 'Elem
        Scope: OpenScope
        Containment: DeclContainment<'T>
        /// Source offset of the `module` / `namespace` keyword of the INNERMOST enclosing
        /// `rec` scope, `ValueNone` outside one. Under `rec` a declaration is visible from
        /// the top of that scope rather than from where it is written.
        RecScopeOffset: int voption
    }

type WalkedElem<'T> = WalkedIn<'T, ModuleElem<'T>>

type WalkedSigElem<'T> = WalkedIn<'T, ModuleSignatureElement<'T>>

module CstModuleTree =

    /// The module elements an analysis pass walks for an implementation file, FLATTENED:
    /// every `namespace` group's elements in source order, and a nested `module Foo = …`
    /// spliced into the enclosing list rather than surfaced as a `ModuleElem.Module`.
    let implFileElems (file: ImplementationFile<SyntaxToken>) : ModuleElems<SyntaxToken> =
        let b = ImmutableArray.CreateBuilder<ModuleElem<SyntaxToken>>()

        let rec add (elems: ModuleElems<SyntaxToken>) =
            for e in elems do
                match e with
                | ModuleElem.Module(ModuleDefn.ModuleDefn(body = ModuleDefnBody(elements = inner))) ->
                    match inner with
                    | ValueSome innerElems -> add innerElems
                    | ValueNone -> ()
                | _ -> b.Add e

        match file with
        | ImplementationFile.AnonymousModule elems -> add elems
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) -> add elems
        | ImplementationFile.Namespaces groups ->
            for g in groups do
                match g with
                | NamespaceDeclGroup.Named(elements = elems)
                | NamespaceDeclGroup.Global(elements = elems) -> add elems

        b.ToImmutable()

    // --- the module-tree walk ---------------------------------------------------------

    /// What the walk needs to see in ONE element of either grammar's module body.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type ModuleNode<'Elem> =
        /// A nested `module Foo = …`. The body is absent for a header the parser accepted
        /// without one, which the implementation grammar spells and the signature one does not.
        | Nested of
            declared: DeclaredModule<SyntaxToken> *
            isRec: SyntaxToken voption *
            body: ImmutableArray<'Elem> voption
        /// `open A.B`. NOT `open type`, which is a member channel rather than a prefix.
        | Import of openToken: SyntaxToken * path: LongIdent<SyntaxToken>
        | Abbrev of abbrev: ModuleAbbrev<SyntaxToken>
        /// Everything the walk passes through: it contributes no scope and no containment.
        | Plain

    /// One top-level body of a file: a `namespace` group, or the whole body of a module or
    /// anonymous file, which declare into the global namespace and add no prefix.
    [<NoEquality; NoComparison>]
    type ModuleGroup<'Elem> =
        {
            Namespace: LongIdent<SyntaxToken> voption
            /// The `namespace` / `module` keyword `rec` is measured from. Absent for an
            /// anonymous file, which has neither.
            Keyword: SyntaxToken voption
            IsRec: SyntaxToken voption
            Elements: ImmutableArray<'Elem>
        }

    /// The alias a `module R = A.B.C` element binds, written at `containment`. Absent when
    /// either half of the declaration is missing.
    let localAbbrev
        (nameOf: SyntaxToken -> string)
        (containment: DeclContainment<SyntaxToken>)
        (abbrev: ModuleAbbrev<SyntaxToken>)
        : LocalAbbrev voption =
        let (ModuleAbbrev.ModuleAbbrev(moduleToken = kw; ident = id; longIdent = li)) =
            abbrev

        let alias = nameOf id
        let target = li.Idents |> Seq.map nameOf |> String.concat "."

        if alias.Length = 0 || target.Length = 0 then
            ValueNone
        else
            ValueSome
                {
                    Alias = alias
                    Path = target
                    Scope = DeclContainment.sourcePath nameOf containment
                    ScopeDepth = List.length containment.Modules
                    Offset = kw.StartIndex
                }

    let private implNode (e: ModuleElem<SyntaxToken>) : ModuleNode<ModuleElem<SyntaxToken>> =
        match e with
        | ModuleElem.Module((ModuleDefn.ModuleDefn(isRec = isRec; body = ModuleDefnBody(elements = body))) as md) ->
            ModuleNode.Nested(DeclaredModule.ofModuleDefn md, isRec, body)
        | ModuleElem.Import(ImportDecl.ImportDecl(openToken = kw; longIdent = li)) -> ModuleNode.Import(kw, li)
        | ModuleElem.ModuleAbbrev abbrev -> ModuleNode.Abbrev abbrev
        | _ -> ModuleNode.Plain

    let private sigNode (e: ModuleSignatureElement<SyntaxToken>) : ModuleNode<ModuleSignatureElement<SyntaxToken>> =
        match e with
        | ModuleSignatureElement.Module((ModuleSignature.ModuleSignature(
            isRec = isRec; body = ModuleSignatureBody(elements = body))) as ms) ->
            ModuleNode.Nested(DeclaredModule.ofModuleSignature ms, isRec, ValueSome body)
        | ModuleSignatureElement.Import(ImportDecl.ImportDecl(openToken = kw; longIdent = li)) ->
            ModuleNode.Import(kw, li)
        | ModuleSignatureElement.ModuleAbbrev abbrev -> ModuleNode.Abbrev abbrev
        | _ -> ModuleNode.Plain

    let private group ns kw isRec elems : ModuleGroup<'Elem> =
        {
            Namespace = ns
            Keyword = kw
            IsRec = isRec
            Elements = elems
        }

    let private implGroups (file: ImplementationFile<SyntaxToken>) : ModuleGroup<ModuleElem<SyntaxToken>> list =
        match file with
        | ImplementationFile.AnonymousModule elems -> [ group ValueNone ValueNone ValueNone elems ]
        | ImplementationFile.NamedModule(NamedModule.NamedModule(moduleToken = kw; isRec = isRec; elements = elems)) ->
            [ group ValueNone (ValueSome kw) isRec elems ]
        | ImplementationFile.Namespaces groups ->
            [
                for g in groups do
                    match g with
                    | NamespaceDeclGroup.Named(namespaceToken = kw; isRec = isRec; longIdent = li; elements = elems) ->
                        group (ValueSome li) (ValueSome kw) isRec elems
                    | NamespaceDeclGroup.Global(elements = elems) -> group ValueNone ValueNone ValueNone elems
            ]

    let private sigGroups (file: SignatureFile<SyntaxToken>) : ModuleGroup<ModuleSignatureElement<SyntaxToken>> list =
        match file with
        | SignatureFile.AnonymousModule elems -> [ group ValueNone ValueNone ValueNone elems ]
        | SignatureFile.NamedModule(NamedModuleSignature.NamedModuleSignature(
            moduleToken = kw; isRec = isRec; elements = elems)) -> [ group ValueNone (ValueSome kw) isRec elems ]
        | SignatureFile.Namespaces groups ->
            [
                for g in groups do
                    match g with
                    | NamespaceDeclGroupSignature.Named(
                        namespaceToken = kw; isRec = isRec; longIdent = li; elements = elems) ->
                        group (ValueSome li) (ValueSome kw) isRec elems
                    | NamespaceDeclGroupSignature.Global(elements = elems) -> group ValueNone ValueNone ValueNone elems
            ]

    /// Flatten a file's module tree, nested modules descended into. A non-rec scope accumulates
    /// its `open`s in order; under `rec` every one covers the whole body. `onScope` fires once
    /// per body entered, with that body's PROPAGATED rec flag.
    let private walkTree
        (nameOf: SyntaxToken -> string)
        (node: 'Elem -> ModuleNode<'Elem>)
        (groups: ModuleGroup<'Elem> list)
        (ambient: OpenScope)
        (onScope: ImmutableArray<'Elem> -> bool -> unit)
        : WalkedIn<SyntaxToken, 'Elem> list =
        let out = ResizeArray<WalkedIn<SyntaxToken, 'Elem>>()

        let longIdentText (li: LongIdent<SyntaxToken>) : string =
            li.Idents |> Seq.map nameOf |> String.concat "."

        let addOpen
            (scope: OpenScope)
            (containment: DeclContainment<SyntaxToken>)
            (openToken: SyntaxToken)
            (li: LongIdent<SyntaxToken>)
            : OpenScope =
            let prefix = longIdentText li

            if prefix.Length = 0 then
                scope
            else
                LocalScopeDecl.Open
                    {
                        Path = prefix
                        Scope = DeclContainment.sourcePath nameOf containment
                        ScopeDepth = List.length containment.Modules
                        Offset = openToken.StartIndex
                    }
                :: scope

        let accumulate (containment: DeclContainment<SyntaxToken>) (scope: OpenScope) (e: 'Elem) : OpenScope =
            match node e with
            | ModuleNode.Import(kw, li) -> addOpen scope containment kw li
            | ModuleNode.Abbrev abbrev ->
                match localAbbrev nameOf containment abbrev with
                | ValueSome a -> LocalScopeDecl.Abbrev a :: scope
                | ValueNone -> scope
            | ModuleNode.Nested _
            | ModuleNode.Plain -> scope

        // The innermost enclosing rec scope wins.
        let innerRecScope (keyword: SyntaxToken) (isRec: SyntaxToken voption) (inherited: int voption) : int voption =
            if isRec.IsSome then
                ValueSome keyword.StartIndex
            else
                inherited

        // `isRec` is the scope's OWN rec flag (drives the constant-prelude shape).
        // `recScope` is the PROPAGATED one, so `onScope`'s flag is true in a rec
        // namespace's non-rec submodule too.
        let rec processElems
            (elems: ImmutableArray<'Elem>)
            (start: OpenScope)
            (isRec: bool)
            (recScope: int voption)
            (containment: DeclContainment<SyntaxToken>)
            : unit =
            onScope elems recScope.IsSome

            if isRec then
                // Constant prelude: every open/abbrev in this scope applies to the whole
                // body, regardless of position.
                let constScope = (start, elems) ||> Seq.fold (accumulate containment)

                for e in elems do
                    emit e constScope recScope containment
            else
                let mutable s = start

                for e in elems do
                    emit e s recScope containment
                    s <- accumulate containment s e

        and emit
            (e: 'Elem)
            (scope: OpenScope)
            (recScope: int voption)
            (containment: DeclContainment<SyntaxToken>)
            : unit =
            match node e with
            // A module is a *container*: it extends the containment's module chain and
            // leaves its `Namespace` alone.
            | ModuleNode.Nested(declared, innerRec, ValueSome body) ->
                processElems
                    body
                    scope
                    innerRec.IsSome
                    (innerRecScope declared.ModuleToken innerRec recScope)
                    (DeclContainment.enter declared containment)
            | ModuleNode.Nested(body = ValueNone) -> ()
            | ModuleNode.Import _
            | ModuleNode.Abbrev _
            | ModuleNode.Plain ->
                out.Add
                    {
                        Elem = e
                        Scope = scope
                        Containment = containment
                        RecScopeOffset = recScope
                    }

        for g in groups do
            let ns =
                match g.Namespace with
                | ValueSome li -> longIdentText li
                | ValueNone -> ""

            let recScope =
                match g.Keyword with
                | ValueSome kw -> innerRecScope kw g.IsRec ValueNone
                | ValueNone -> ValueNone

            processElems g.Elements ambient g.IsRec.IsSome recScope (DeclContainment.ofNamespace ns)

        List.ofSeq out

    /// Scope-preserving sibling of `implFileElems`, with `onScope` fired once per body entered,
    /// which is what a rule about a SCOPE rather than an element reads.
    let walkImplWith
        (nameOf: SyntaxToken -> string)
        (ambient: OpenScope)
        (onScope: ModuleElems<SyntaxToken> -> bool -> unit)
        (file: ImplementationFile<SyntaxToken>)
        : WalkedElem<SyntaxToken> list =
        walkTree nameOf implNode (implGroups file) ambient onScope

    /// `walkImplWith` for the consumers that have no rule about a scope as a whole.
    let walkImpl
        (nameOf: SyntaxToken -> string)
        (ambient: OpenScope)
        (file: ImplementationFile<SyntaxToken>)
        : WalkedElem<SyntaxToken> list =
        walkTree nameOf implNode (implGroups file) ambient (fun _ _ -> ())

    /// The `.fsi` analogue of `implFileElems`: every `namespace` group's elements in
    /// source order, with nested `module Foo = …` bodies spliced into the enclosing list.
    let sigFileElems (file: SignatureFile<SyntaxToken>) : ModuleSignatureElements<SyntaxToken> =
        let b = ImmutableArray.CreateBuilder<ModuleSignatureElement<SyntaxToken>>()

        let rec add (elems: ModuleSignatureElements<SyntaxToken>) =
            for e in elems do
                match e with
                | ModuleSignatureElement.Module(ModuleSignature.ModuleSignature(
                    body = ModuleSignatureBody(elements = inner))) -> add inner
                | _ -> b.Add e

        match file with
        | SignatureFile.AnonymousModule elems -> add elems
        | SignatureFile.NamedModule(NamedModuleSignature.NamedModuleSignature(elements = elems)) -> add elems
        | SignatureFile.Namespaces groups ->
            for g in groups do
                match g with
                | NamespaceDeclGroupSignature.Named(elements = elems)
                | NamespaceDeclGroupSignature.Global(elements = elems) -> add elems

        b.ToImmutable()

    /// The `.fsi` twin of `walkImpl`: the same walk, over the signature grammar's reading of
    /// a module body.
    let walkSig
        (nameOf: SyntaxToken -> string)
        (ambient: OpenScope)
        (file: SignatureFile<SyntaxToken>)
        : WalkedSigElem<SyntaxToken> list =
        walkTree nameOf sigNode (sigGroups file) ambient (fun _ _ -> ())
