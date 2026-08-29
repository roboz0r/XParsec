namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionTypeRefStamp
open NameResolutionScope
open NameResolutionTypeRegistration
open NameResolutionMemberRegistration

// Pre:  ctx.Desugared populated.
// Post: ctx.Bindings.Binding populated for every ident-use site resolving to a local
//       binding; a name no provider knows becomes an Error diagnostic.

module NameResolution =

    /// A class or union/record host's member bodies plus everything that enters their
    /// scope. The ctor-param, `val`-field, preamble and `inherit` fields are empty for a
    /// union/record host, which declares none of those.
    [<NoEquality; NoComparison>]
    type private TypeBodiesWalk =
        {
            ThisName: string
            ThisKey: NodeKey
            /// `base`'s own synthetic bound variable key, present only for a class with a
            /// resolved `inherit` clause. Enters the *instance* scope alongside `this`;
            /// statics never see `base`.
            BaseKey: NodeKey voption
            CtorParams: ClassCtorParamInfo[]
            /// Declared `val [mutable] x: T` fields. Nothing about them enters lexical
            /// scope; they are carried because they mint a named field on the type and so
            /// take part in the duplicate-field check below.
            InstanceFields: ClassFieldInfo[]
            /// The type's members. Not in lexical scope (a member reaches a sibling through
            /// `this`); only their NAMES are read here, since F# forbids a class `let` bound variable
            /// sharing a name with a member (FS0905).
            Members: TypeMemberInfo[]
            /// `static let` / `static do`, in declaration order. A `static let` name enters
            /// both the instance and the static member scope; the sequence's own expressions
            /// see only the static bound variables above them (no `this` / ctor params / instance lets).
            StaticPreamble: ClassPreambleEntry[]
            /// Instance `let` / `do`, in declaration order. Every bound variable enters the instance
            /// member scope; the sequence's own expressions see the ctor params and the
            /// instance bound variables ABOVE them only.
            InstancePreamble: ClassPreambleEntry[]
            /// Secondary constructors. Each body is walked in a scope of the `static let`s
            /// plus its own params (no `this` / primary-ctor params).
            SecondaryCtors: ClassSecondaryCtorInfo[]
            /// Primary-constructor `inherit Base(args)` argument expression, when the class
            /// has one. Name-resolved with the primary-ctor params and static lets in scope,
            /// but not `this`.
            InheritsExpr: Expr<SyntaxToken> voption
            /// The enclosing module's `let` bindings that are VISIBLE from this type's
            /// declaration. Entered as the lowest-priority layer of every member-body scope,
            /// so a member can reference a module sibling unqualified. Empty for a top-level type.
            EnclosingModuleScope: Scope
            Elements: TypeDefnElements<SyntaxToken>
        }

    /// The self-identifier token an *instance* member declares for its own body (`s` in
    /// `member s.Add …`). F# scopes it to that one member, independently of the type-level
    /// `as` alias. A `_` self-id is returned here and filtered by the caller.
    let private selfIdentOf (d: MethodOrPropDefn<SyntaxToken>) : SyntaxToken voption =
        match d with
        | MethodOrPropDefn.Method(ident = ValueSome(struct (selfId, _)))
        | MethodOrPropDefn.Property(ident = ValueSome(struct (selfId, _)))
        | MethodOrPropDefn.PropertyWithGetSet(identPrefix = ValueSome(struct (selfId, _))) -> ValueSome selfId
        | _ -> ValueNone

    /// Walk every method / property / auto-property body of a class or union under an
    /// instance scope binding `this` (or the `as` alias) and the primary-ctor params; a
    /// static member's scope has neither (F# spec §8.7). Members are mutually recursive.
    let private walkTypeBodies (ctx: PassContext) (walker: CstWalk.ExprWalker<Scope list>) (w: TypeBodiesWalk) : unit =
        // Primary-ctor params, `val` fields, `static let` and instance `let` bound variables each mint
        // a field carrying its source name, and no two fields of one type may share a name: a
        // CLR Field row is (Parent, Name, Signature), static-ness being a flag (ECMA-335 II.22.15).
        let mutable fieldNames: Set<string> = Set.empty

        // A LIMITATION, not invalid F#: fsc accepts every collision this rejects, uniquifying the
        // backing field name by source position (`v@4`). No such uniquification pass exists here.
        let declareField (name: string) (declTok: SyntaxToken) =
            if fieldNames.Contains name then
                ctx.Report(
                    declTok,
                    Kind.Message(
                        sprintf
                            "Duplicate field name `%s`: a constructor parameter, a `val` field and a class `let` binding each mint a field carrying its source name, and no two fields of one type may share a name (on the CLR a field's static-ness is not part of its identity). F# permits this by uniquifying the backing-field names; that pass is not implemented yet, so rename one of them."
                            name
                    )
                )
            else
                fieldNames <- Set.add name fieldNames

        // FS0905: a member's name is its public surface, so a class `let` bound variable may not share
        // it. Holds whichever side is static (probed against fsc).
        let memberNames = w.Members |> Array.map (fun m -> m.Name) |> Set.ofArray

        let mutable scopeMap: Scope = Map.empty
        scopeMap <- Map.add w.ThisName (w.ThisKey, false) scopeMap

        ctx.Bindings.Binding.Set(
            w.ThisKey,
            {
                BindingSite = w.ThisKey
                IsInline = false
                IsMutable = false
            }
        )

        // `base` is visible to instance member bodies of a derived class only, and resolves
        // to its own synthetic bound variable key. Left unbound without an `inherit` clause, so a
        // member mentioning it reports "Unresolved identifier: base".
        match w.BaseKey with
        | ValueSome bk ->
            scopeMap <- Map.add "base" (bk, false) scopeMap

            ctx.Bindings.Binding.Set(
                bk,
                {
                    BindingSite = bk
                    IsInline = false
                    IsMutable = false
                }
            )
        | ValueNone -> ()

        for p in w.CtorParams do
            let paramKey = BoundVarKey.identity p.DeclSite.BoundVar
            scopeMap <- Map.add p.Name (paramKey, false) scopeMap
            declareField p.Name p.DeclSite.Tok

            ctx.Bindings.Binding.Set(
                paramKey,
                {
                    BindingSite = paramKey
                    IsInline = false
                    IsMutable = false
                }
            )

        // `val` fields bind no name lexically; the duplicate-field rule is all they take part in.
        for f in w.InstanceFields do
            declareField f.Name f.DeclSite.Tok

        // The enclosing module's value bindings are visible unqualified to every member
        // body of a type nested in that module (F# spec §8.7). The lowest-priority layer, so
        // `this` / ctor params / preamble bound variables shadow on a name clash.
        let moduleMemberScope: Scope = w.EnclosingModuleScope

        /// Declare one preamble `let` bound variable: its binding site, its field, and the FS0905 check.
        let declarePreambleBoundVar (l: ClassLetInfo) =
            ctx.Bindings.Binding.Set(
                l.DeclKey,
                {
                    BindingSite = l.DeclKey
                    IsInline = false
                    // An instance `let mutable` is a mutable FIELD, so `c <- …` in a
                    // member body must type-check.
                    IsMutable = l.IsMutable
                }
            )

            let bindTok = (CstKeys.siteOfBinding l.Binding).Tok
            declareField l.Name bindTok

            if memberNames.Contains l.Name then
                ctx.Report(bindTok, Kind.MemberAndLocalBindingClash l.Name)

        // A preamble `let f x = …` binds a FUNCTION, so its `argumentPats` scope over the
        // initialiser. Only `let rec` puts the bound variable in scope of its own initialiser.
        let walkLetInit (outer: Scope list) (l: ClassLetInfo) =
            let b = l.Binding

            let inner =
                if b.argumentPats.IsEmpty then
                    outer
                else
                    extendScope ctx b.argumentPats Map.empty :: outer

            CstWalk.iterExpr walker inner b.expr

        // `static let` names enter scope for every member body, instance and static alike
        // (F# spec §8.7). Built incrementally, so a `static let` sees neither an instance
        // bound variable nor a later static one, matching F# (FS0039).
        let mutable staticLetScope: Scope = Map.empty

        for entry in w.StaticPreamble do
            match entry with
            | ClassPreambleEntry.Let l ->
                declarePreambleBoundVar l

                if l.IsRec then
                    staticLetScope <- Map.add l.Name (l.DeclKey, l.IsMutable) staticLetScope

                walkLetInit [ staticLetScope ] l
                staticLetScope <- Map.add l.Name (l.DeclKey, l.IsMutable) staticLetScope
            | ClassPreambleEntry.Do e -> CstWalk.iterExpr walker [ staticLetScope ] e

        // The instance sequence runs inside the primary ctor: it sees the ctor params, the static
        // bound variables, and the instance bound variables above it, but never `this` / `base` /
        // the `as` alias, so a preamble `let` referencing the object is rejected rather than typed.
        let ctorParamScope =
            (Map.empty, w.CtorParams)
            ||> Array.fold (fun acc p -> Map.add p.Name (BoundVarKey.identity p.DeclSite.BoundVar, false) acc)

        let instanceOuterScope = [ ctorParamScope; staticLetScope; moduleMemberScope ]

        let mutable instanceLetScope: Scope = Map.empty

        for entry in w.InstancePreamble do
            match entry with
            | ClassPreambleEntry.Let l ->
                declarePreambleBoundVar l

                if l.IsRec then
                    instanceLetScope <- Map.add l.Name (l.DeclKey, l.IsMutable) instanceLetScope

                walkLetInit (instanceLetScope :: instanceOuterScope) l
                instanceLetScope <- Map.add l.Name (l.DeclKey, l.IsMutable) instanceLetScope
            | ClassPreambleEntry.Do e -> CstWalk.iterExpr walker (instanceLetScope :: instanceOuterScope) e

        // Member bodies see EVERY preamble bound variable, static and instance alike: members are a
        // mutually-recursive group, so no ordering rule is left to enforce here.
        let mergePreamble (m: Scope) =
            let m = (m, staticLetScope) ||> Map.fold (fun acc k v -> Map.add k v acc)
            (m, instanceLetScope) ||> Map.fold (fun acc k v -> Map.add k v acc)

        let instanceScope = [ mergePreamble scopeMap; moduleMemberScope ]
        // Statics see neither `this` / ctor params nor any instance bound variable.
        let staticScope: Scope list = [ staticLetScope; moduleMemberScope ]

        // Primary `inherit Base(args)`: the `static let`s and primary-ctor params, without
        // `this` / `base` and without the instance bound variables, which are assigned only after
        // the base ctor returns.
        match w.InheritsExpr with
        | ValueSome e -> CstWalk.iterExpr walker instanceOuterScope e
        | ValueNone -> ()

        // A secondary ctor body is an `AdditionalConstrExpr`, not a plain `Expr`, so it is
        // walked here: each embedded expression goes through `walker`, and a `let` bound variable
        // enters scope for the remainder. Only the static lets and its own params are in scope.
        let rec walkCtorBody (scope: Scope list) (ace: AdditionalConstrExpr<SyntaxToken>) : unit =
            match ace with
            | AdditionalConstrExpr.LetIn(binding = b; body = body) ->
                let siblings = ImmutableArray.Create b
                let rhsScope = walker.EnterBindingRhs scope false siblings b
                CstWalk.iterExpr walker rhsScope b.expr
                let bodyScope = walker.EnterLetBody scope siblings
                walkCtorBody bodyScope body
            | AdditionalConstrExpr.SequenceAfter(stmt = s; rest = rest) ->
                CstWalk.iterExpr walker scope s
                walkCtorBody scope rest
            | AdditionalConstrExpr.SequenceBefore(before = before; expr = e) ->
                walkCtorBody scope before
                CstWalk.iterExpr walker scope e
            | AdditionalConstrExpr.Conditional(cond = c; thenBranch = t; elseBranch = el) ->
                CstWalk.iterExpr walker scope c
                walkCtorBody scope t
                walkCtorBody scope el
            | AdditionalConstrExpr.Init initExpr ->
                match initExpr with
                | AdditionalConstrInitExpr.Expression e
                | AdditionalConstrInitExpr.Delegated(expr = e) -> CstWalk.iterExpr walker scope e
                | AdditionalConstrInitExpr.Explicit(inherits = inh; initializers = inits) ->
                    match inh with
                    | ValueSome(ClassInheritsDecl(expr = ValueSome e)) -> CstWalk.iterExpr walker scope e
                    | _ -> ()

                    for FieldInitializer(expr = e) in inits do
                        CstWalk.iterExpr walker scope e

        for sc in w.SecondaryCtors do
            let mutable scScope = staticLetScope

            for p in sc.Params do
                let paramKey = BoundVarKey.identity p.DeclSite.BoundVar
                scScope <- Map.add p.Name (paramKey, false) scScope

                ctx.Bindings.Binding.Set(
                    paramKey,
                    {
                        BindingSite = paramKey
                        IsInline = false
                        IsMutable = false
                    }
                )

            walkCtorBody [ scScope; moduleMemberScope ] sc.Body

        // Shared by a class/union's own members and by each `interface IFace with member …`
        // block's: an interface member is an ordinary instance member whose body sees `this`.
        let walkMemberDefn (md: MemberDefn<SyntaxToken>) =
            match md with
            | MemberDefn.Member(staticToken = s; defn = d) ->
                // A member's own self-id (`member s.Add …`) is scoped to that member's body,
                // distinct from the type-level `as` alias already in `instanceScope`. It binds
                // to the same `ThisKey`, so it types as `this`. Statics have no self-id.
                let scope =
                    if s.IsSome then
                        staticScope
                    else
                        match selfIdentOf d with
                        | ValueSome selfId ->
                            let name = ctx.NameOf selfId

                            if name = "_" || name = w.ThisName then
                                instanceScope
                            else
                                Map.add name (w.ThisKey, false) Map.empty :: instanceScope
                        | ValueNone -> instanceScope

                // The argument pats bind the method's parameters; the bound pattern (the member
                // name) does NOT enter scope.
                let walkBindingBody (b: Binding<SyntaxToken>) =
                    let mutable inner = scope

                    if not b.argumentPats.IsEmpty then
                        inner <- extendScope ctx b.argumentPats Map.empty :: inner

                    CstWalk.iterExpr walker inner b.expr

                match d with
                | MethodOrPropDefn.AutoProperty(expr = e) -> CstWalk.iterExpr walker scope e
                | d ->
                    for b in CstWalk.memberBindings d do
                        walkBindingBody b
            | _ -> ()

        for el in w.Elements do
            match el with
            | TypeDefnElement.Member md -> walkMemberDefn md
            | TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(
                objectMembers = ValueSome(ObjectMembers(memberDefns = mds)))) ->
                for md in mds do
                    walkMemberDefn md
            | _ -> ()

    /// The bindings of the module the walk stands in VISIBLE from a local type's own
    /// declaration key. A type declaration is one contiguous element, so a module `let` is
    /// above it (visible to every member body) or below it (visible to none), hence FS0039,
    /// and `module rec` lifting it.
    let private enclosingModuleScope (ctx: PassContext) (declKey: NodeKey) : Scope =
        match ctx.Types.LocalContainerPaths.TryGetValue ctx.CurrentContainer with
        | true, path ->
            match ctx.Resolution.LocalModulePaths.TryGetValue path with
            | true, members ->
                let useSite = ctx.UseSiteAt declKey
                let mutable m = Map.empty

                for kv in members do
                    if kv.Value.VisibleFrom <= useSite.Offset then
                        m <- Map.add kv.Key (kv.Value.BindingSite, false) m

                m
            | false, _ -> Map.empty
        | false, _ -> Map.empty

    let private walkClassBodies
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<Scope list>)
        (m: ModuleElem<SyntaxToken>)
        : unit =
        // Resolve the decl to its registered info by the key the DECLARATION mints, not by
        // name: a sibling module's same-named class must not be reached, and an arity-overloaded
        // `Box\`1`/`Box\`2` does not resolve by bare name at all.
        let bodyOf (td: TypeDefn<SyntaxToken>) =
            match TypeDefnPatterns.tryClassLikeDecl td with
            | ValueSome d ->
                match tryDeclaredClass ctx d.TypeName with
                | ValueSome info -> ValueSome(info, d.Body)
                | ValueNone -> ValueNone
            | ValueNone -> ValueNone

        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match bodyOf td with
                | ValueSome(info, body) ->
                    walkTypeBodies
                        ctx
                        walker
                        {
                            ThisName = info.ThisName
                            ThisKey = BoundVarKey.identity info.ThisKey
                            BaseKey =
                                match info.Base with
                                | ValueSome _ -> ValueSome(BoundVarKey.identity info.BaseKey)
                                | ValueNone -> ValueNone
                            CtorParams = info.CtorParams
                            InstanceFields = info.InstanceFields
                            Members = info.Members
                            StaticPreamble = info.StaticPreamble
                            InstancePreamble = info.InstancePreamble
                            SecondaryCtors = info.SecondaryCtors
                            InheritsExpr =
                                // Walk the base-ctor args only when the parent resolved.
                                match info.Base, body.inherits with
                                | ValueSome _, ValueSome(ClassInheritsDecl(expr = e)) -> e
                                | _ -> ValueNone
                            EnclosingModuleScope = enclosingModuleScope ctx info.DeclSite.Key
                            Elements = body.elements
                        }
                | ValueNone -> ()
        | _ -> ()

    /// Name-resolve a union/record host's augmentation-member and `interface … with` impl
    /// bodies so `this` and its pattern bound variables get a `Binding` entry. A type with ONLY an
    /// interface impl and no augmentation members still needs its impl bodies resolved.
    let private walkNominalHostBodies
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<Scope list>)
        (name: string)
        (host: IInterfaceImplHost)
        (elems: TypeDefnElements<SyntaxToken>)
        : unit =
        if not (Array.isEmpty host.Members && Array.isEmpty host.InterfaceImpls) then
            walkTypeBodies
                ctx
                walker
                {
                    ThisName = host.ThisName
                    ThisKey = BoundVarKey.identity host.ThisKey
                    BaseKey = ValueNone
                    CtorParams = [||]
                    InstanceFields = [||]
                    Members = host.Members
                    StaticPreamble = [||]
                    InstancePreamble = [||]
                    SecondaryCtors = [||]
                    InheritsExpr = ValueNone
                    EnclosingModuleScope = enclosingModuleScope ctx host.DeclSite.Key
                    Elements = elems
                }

    let private walkNominalBodies
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<Scope list>)
        (m: ModuleElem<SyntaxToken>)
        : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                // Only a `with` block (`ValueSome elems`) carries augmentation / impl bodies.
                match TypeDefnPatterns.tryNonClassMemberHostDecl td with
                | ValueSome(struct (TypeName(ident = nameLi) as tn, ValueSome elems)) ->
                    match tryDeclaredNonClassHost ctx tn with
                    | ValueSome host -> walkNominalHostBodies ctx walker (ctx.NameOf nameLi.Idents.[0]) host elems
                    | ValueNone -> ()
                | _ -> ()
        | _ -> ()

    let private walkModuleElem
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<Scope list>)
        (scope: Scope list)
        (m: ModuleElem<SyntaxToken>)
        : Scope list =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(isRec = isRec; bindings = bindings)) ->
            let isRecursive = isRec.IsSome

            for b in bindings do
                // The return-type annotation only; the pattern annotations are stamped
                // through the RHS scope hook.
                stampBindingSigTypes ctx b
                let rhsScope = walker.EnterBindingRhs scope isRecursive bindings b
                CstWalk.iterExpr walker rhsScope b.expr
            // Writes binding-site self-entries to `ctx.Bindings.Binding` as a side effect.
            let newEntries = bindingsToScope ctx bindings

            match scope with
            | [] -> [ newEntries ]
            | top :: rest ->
                let merged = (top, newEntries) ||> Map.fold (fun acc k v -> Map.add k v acc)
                merged :: rest
        | ModuleElem.Expression e ->
            CstWalk.iterExpr walker scope e
            scope
        | ModuleElem.Type _ -> scope
        | _ -> scope

    let private walkElems
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<Scope list>)
        (elems: WalkedElem<SyntaxToken> list)
        =
        // Whole-file pre-scan: the `…Module` suffix rule reads `NominalTypeNames` at the very
        // first key mint, and a `module Foo` may textually precede the `type Foo` it collides
        // with, so every type name must be known before registration starts.
        for w in elems do
            noteNominalTypeNames ctx w.Elem

        // Type registration, top-down, one `type … and …` group at a time: when a group
        // registers, the name table holds every type above it and nothing below. Terms share
        // the scan, so a term's ANNOTATIONS classify here (`let f (a: A)` above `type A` is FS0039).
        for w in elems do
            ctx.EnterElement w

            match w.Elem with
            | ModuleElem.Type defs -> registerGroup ctx w.Containment w.RecScopeOffset defs
            | m -> classifyTermTypes ctx m

        // `walkModuleElem` skips `ModuleElem.Type`, so member bodies are walked here instead,
        // each under its own type's scope. `ctx.EnterElement` sets the element's `open` set,
        // so a member body resolves short external names against the `open`s visible there.
        for w in elems do
            ctx.EnterElement w
            walkClassBodies ctx walker w.Elem

        for w in elems do
            ctx.EnterElement w
            walkNominalBodies ctx walker w.Elem

        // Module-level VALUES, in declaration order: a `let`'s bound variables join the running scope
        // only AFTER its RHS is walked, so a use above it does not see it. A `rec` scope is a
        // contiguous RUN of elements sharing a `RecScopeOffset`, seeded before the run is walked.
        let elems = List.toArray elems
        let mutable scope = [ Map.empty ]
        let mutable i = 0

        while i < elems.Length do
            let recScope = elems.[i].RecScopeOffset
            let mutable j = i

            while j < elems.Length && elems.[j].RecScopeOffset = recScope do
                j <- j + 1

            if recScope.IsSome then
                let mutable seeded = List.head scope

                for k in i .. j - 1 do
                    ctx.EnterElement elems.[k]

                    match elems.[k].Elem with
                    | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
                        seeded <-
                            (seeded, bindingsToScope ctx bindings)
                            ||> Map.fold (fun acc k v -> Map.add k v acc)
                    | _ -> ()

                scope <- seeded :: List.tail scope

            for k in i .. j - 1 do
                ctx.EnterElement elems.[k]
                scope <- walkModuleElem ctx walker scope elems.[k].Elem

            i <- j

    /// Walk the *un-flattened* module tree and record, per scope's dotted SOURCE path, its
    /// direct `let` bindings into `LocalModulePaths`. The flattened walk erases these
    /// boundaries, so it runs first.
    let private registerLocalModules (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        let membersOf (path: string) =
            match ctx.Resolution.LocalModulePaths.TryGetValue path with
            | true, d -> d
            | false, _ ->
                let d = System.Collections.Generic.Dictionary<string, LocalModuleMember>()
                ctx.Resolution.LocalModulePaths.[path] <- d
                d

        let registerLet (path: string) (recScope: int voption) (bindings: ImmutableArray<Binding<SyntaxToken>>) =
            let byPath = membersOf path

            for b in bindings do
                for (name, key) in bindingsOfPat ctx b.pattern do
                    byPath.[name] <-
                        {
                            BindingSite = key
                            VisibleFrom =
                                match recScope with
                                | ValueSome off -> off
                                | ValueNone -> key.Offset
                        }

        // The innermost enclosing `rec` scope's keyword offset: its own when this scope is
        // itself `rec`, else whatever it inherited (a non-rec submodule of a `rec` namespace
        // is still inside that rec scope). A member is VISIBLE FROM that offset.
        let innerRecScope (keyword: SyntaxToken) (isRec: SyntaxToken voption) (inherited: int voption) : int voption =
            if isRec.IsSome then
                ValueSome keyword.StartIndex
            else
                inherited

        let rec walk (path: string) (recScope: int voption) (elems: ModuleElems<SyntaxToken>) =
            for e in elems do
                match e with
                | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
                    registerLet path recScope bindings
                | ModuleElem.Module(ModuleDefn.ModuleDefn(
                    moduleToken = kw; isRec = isRec; ident = ident; body = ModuleDefnBody(elements = inner))) ->
                    match inner with
                    | ValueSome innerElems ->
                        let name = ctx.NameOf ident
                        let innerPath = if path.Length = 0 then name else path + "." + name
                        walk innerPath (innerRecScope kw isRec recScope) innerElems
                    | ValueNone -> ()
                | _ -> ()

        // A whole-file `module A.B.C` homes its declarations in the global namespace, as
        // `CstModuleTree.walkImpl` does, so its path is the empty one.
        match file with
        | ImplementationFile.AnonymousModule elems -> walk "" ValueNone elems
        | ImplementationFile.NamedModule(NamedModule.NamedModule(moduleToken = kw; isRec = isRec; elements = elems)) ->
            walk "" (innerRecScope kw isRec ValueNone) elems
        | ImplementationFile.Namespaces groups ->
            for g in groups do
                match g with
                | NamespaceDeclGroup.Named(namespaceToken = kw; isRec = isRec; longIdent = li; elements = elems) ->
                    let path = li.Idents |> Seq.map ctx.NameOf |> String.concat "."
                    walk path (innerRecScope kw isRec ValueNone) elems
                | NamespaceDeclGroup.Global(elements = elems) -> walk "" ValueNone elems

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        // Capture local-module structure before the flattened walk erases it.
        registerLocalModules ctx file
        let walker = mkWalker ctx
        // Seeded from the ambient prelude, not `ctx.Resolution.OpenScope`, which `walkElems`
        // overwrites per element.
        walkElems ctx walker (CstModuleTree.walkImpl ctx.NameOf ctx.Resolution.AmbientOpenScope file)
