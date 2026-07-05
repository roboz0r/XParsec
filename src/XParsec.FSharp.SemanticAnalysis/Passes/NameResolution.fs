namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionScope
open NameResolutionTypeRegistration
open NameResolutionMemberRegistration

// Pre:  ctx.Desugared populated.
// Post: ctx.Bindings.Binding populated for every ident-use site resolving to a
//       local binding. Unresolved names the provider also doesn't know become
//       Error diagnostics.
//
// Recursion is delegated to CstWalk.iterExpr; the walker thread-restores scope
// at each recursive boundary, so a lambda/let body's locals never pollute the
// caller's scope. Scope-tracking and ident resolution live in NameResolutionScope;
// type/member registry stamping in NameResolution{Type,Member}Registration. This
// module walks type-member bodies and orchestrates the per-element passes.

module NameResolution =

    // Used by docs / external callers; the implementation lives in registration.
    let typarNamesOfTypeName = NameResolutionTypeRegistration.typarNamesOfTypeName

    /// Parameters for `walkTypeBodies`: a registry-driven walk over a class or
    /// union's member bodies that seeds ctx.Bindings.Binding with `this` (and any
    /// ctor params) and recurses each body through `walker`. `CtorParams` is `[||]`
    /// for unions (no primary ctor).
    [<NoEquality; NoComparison>]
    type private TypeBodiesWalk =
        {
            ThisName: string
            ThisKey: NodeKey
            /// `base` binder key, present only for a class with a resolved
            /// `inherit` clause (B-4). Enters the *instance* scope alongside
            /// `this`; statics never see `base`. `ValueNone` for unions and for
            /// classes without inheritance.
            BaseKey: NodeKey voption
            CtorParams: ClassCtorParamInfo[]
            /// Class-level `static let` bindings (B-10). Their names enter both the
            /// instance and static member scopes; their initialisers are walked
            /// under the static scope (no `this` / ctor params). `[||]` for unions.
            StaticLets: ClassStaticLetInfo[]
            /// Secondary constructors (B-11). Each body is walked in a scope of
            /// the `static let`s plus its own params (no `this` / primary-ctor
            /// params). `[||]` for unions.
            SecondaryCtors: ClassSecondaryCtorInfo[]
            /// Primary-constructor `inherit Base(args)` argument expression (B-4),
            /// when the class has one. Name-resolved under the *instance* scope
            /// (primary-ctor params + static lets in scope, but not `this`) so
            /// Unification's `fillBaseCtorCall` sees its idents bound. `ValueNone`
            /// for unions and classes without an `inherit` clause.
            InheritsExpr: Expr<SyntaxToken> voption
            /// G16: the enclosing module's `let` value/function bindings (member
            /// name → binding-site `NodeKey`), present when this type is declared
            /// inside a `module Foo = …`. Entered as the lowest-priority layer of
            /// every member-body scope so a nested type's method can reference a
            /// module sibling unqualified (`collapseLHS`, `notStarted` in
            /// `SetIterator`). `ValueNone` for a top-level type. The same map
            /// `LocalModules` registers for G15's *qualified* resolution.
            EnclosingModuleMembers: System.Collections.Generic.Dictionary<string, NodeKey> voption
            Elements: TypeDefnElements<SyntaxToken>
        }

    /// The self-identifier token an *instance* member declares for its own
    /// body (`s` in `member s.Add …`, `x` in `member x.Choose …`). F# scopes
    /// this name to that single member, independently of the type-level `as`
    /// alias. `AutoProperty` / `AbstractSignature` carry none; a `_` self-id is
    /// returned here and filtered by the caller (it binds nothing).
    let private selfIdentOf (d: MethodOrPropDefn<SyntaxToken>) : SyntaxToken voption =
        match d with
        | MethodOrPropDefn.Method(ident = ValueSome(struct (selfId, _)))
        | MethodOrPropDefn.Property(ident = ValueSome(struct (selfId, _)))
        | MethodOrPropDefn.PropertyWithGetSet(identPrefix = ValueSome(struct (selfId, _))) -> ValueSome selfId
        | _ -> ValueNone

    /// Walk every method / property / auto-property body of a class or union with
    /// an instance scope binding `this` (or the `as` alias) and every
    /// primary-constructor argument. Member names are NOT in lexical scope:
    /// sibling members reference one another only via `this.OtherMember`. Static
    /// scope is empty: statics don't see `this` or ctor args (F# spec §8.7).
    let private walkTypeBodies (ctx: PassContext) (walker: CstWalk.ExprWalker<Scope list>) (w: TypeBodiesWalk) : unit =
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

        // `base` is visible to instance member bodies of a derived class only.
        // It resolves to its own synthetic binder key (mirrors `this`); a class
        // without an `inherit` clause leaves `base` unbound, so a member that
        // mentions it diagnoses "Unresolved identifier: base".
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
            scopeMap <- Map.add p.Name (p.DeclKey, false) scopeMap

            ctx.Bindings.Binding.Set(
                p.DeclKey,
                {
                    BindingSite = p.DeclKey
                    IsInline = false
                    IsMutable = false
                }
            )

        // `static let` names enter scope for every member body (instance and
        // static alike — F# spec §8.7) and resolve to the static field's binder
        // key. Each initialiser is walked under the static lets declared *before*
        // it (no `this` / ctor params), so the binder map is built incrementally.
        let mutable staticLetScope: Scope = Map.empty

        for sl in w.StaticLets do
            CstWalk.iterExpr walker [ staticLetScope ] sl.Init

            staticLetScope <- Map.add sl.Name (sl.DeclKey, false) staticLetScope

            ctx.Bindings.Binding.Set(
                sl.DeclKey,
                {
                    BindingSite = sl.DeclKey
                    IsInline = false
                    IsMutable = false
                }
            )

        let mergeStaticLets (m: Scope) =
            (m, staticLetScope) ||> Map.fold (fun acc k v -> Map.add k v acc)

        // G16: the enclosing module's value bindings are visible — unqualified — to
        // every member body of a type nested in that module (F# spec §8.7). They
        // enter as the *lowest-priority* tail layer so `this` / ctor params /
        // static lets shadow on a name clash. `Map.empty` (no enclosing module)
        // leaves resolution unchanged.
        let moduleMemberScope: Scope =
            match w.EnclosingModuleMembers with
            | ValueSome members ->
                let mutable m = Map.empty

                for kv in members do
                    m <- Map.add kv.Key (kv.Value, false) m

                m
            | ValueNone -> Map.empty

        let instanceScope = [ mergeStaticLets scopeMap; moduleMemberScope ]
        let staticScope: Scope list = [ staticLetScope; moduleMemberScope ]

        // Primary `inherit Base(args)` expression (B-4): name-resolve under a
        // scope of `static let`s plus the primary-ctor params, but without
        // `this` / `base` — the base ctor runs before the instance exists, so
        // its args may only reference the constructor's own parameters.
        match w.InheritsExpr with
        | ValueSome e ->
            let mutable ctorScope = staticLetScope

            for p in w.CtorParams do
                ctorScope <- Map.add p.Name (p.DeclKey, false) ctorScope

            CstWalk.iterExpr walker [ ctorScope; moduleMemberScope ] e
        | ValueNone -> ()

        // Secondary constructors (B-11). The body is an `AdditionalConstrExpr`,
        // not a plain `Expr`, so it's walked manually: each embedded expression
        // goes through `walker`, and a `let`-preamble binder enters scope for the
        // remainder. No `this` / primary-ctor params — only the static lets and
        // the overload's own parameters are in scope.
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
                scScope <- Map.add p.Name (p.DeclKey, false) scScope

                ctx.Bindings.Binding.Set(
                    p.DeclKey,
                    {
                        BindingSite = p.DeclKey
                        IsInline = false
                        IsMutable = false
                    }
                )

            walkCtorBody [ scScope; moduleMemberScope ] sc.Body

        // Body walk shared by a class/union's own members and by each
        // `interface IFace with member …` block's members (B-2): an interface
        // member is an ordinary instance member whose body sees `this`.
        let walkMemberDefn (md: MemberDefn<SyntaxToken>) =
            match md with
            | MemberDefn.Member(staticToken = s; defn = d) ->
                // F# scopes each *instance* member's own self-identifier to that
                // member's body (`member s.Add …`, `member x.Choose …`), distinct
                // from the type-level `as` alias / default `this` that already
                // seeds `instanceScope`. Bind the member's self-id to the same
                // `ThisKey` (so Unification still types it as `this`) when it
                // differs from the bound self-name and isn't `_`. Without this a
                // member written with any other self-id leaves both `s` and
                // `s.Member` unresolved. Static members have no self-id.
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
                                // Highest-priority layer: a fresh self-id, so it can
                                // only shadow (never collide with) the instance scope.
                                Map.add name (w.ThisKey, false) Map.empty :: instanceScope
                        | ValueNone -> instanceScope

                match d with
                | MethodOrPropDefn.Method(defn = b)
                | MethodOrPropDefn.Property(defn = b) ->
                    // Extend with argument-pattern binders so method parameters
                    // resolve. The headPat (member name) does NOT enter scope.
                    let mutable inner = scope

                    if not b.argumentPats.IsEmpty then
                        inner <- extendScope ctx b.argumentPats Map.empty :: inner

                    CstWalk.iterExpr walker inner b.expr
                | MethodOrPropDefn.AutoProperty(expr = e) -> CstWalk.iterExpr walker scope e
                | _ -> ()
            | _ -> ()

        for el in w.Elements do
            match el with
            | TypeDefnElement.Member md -> walkMemberDefn md
            | TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(
                objectMembers = ValueSome(ObjectMembers(memberDefns = mds)))) ->
                for md in mds do
                    walkMemberDefn md
            | _ -> ()

    /// G16: the member bindings of the module a local type is declared inside, if
    /// any — looked up via the `TypeEnclosingModule` → `LocalModules` registries the
    /// pre-pass populated. `ValueNone` for a top-level type.
    let private enclosingModuleMembers
        (ctx: PassContext)
        (typeName: string)
        : System.Collections.Generic.Dictionary<string, NodeKey> voption =
        match ctx.Resolution.TypeEnclosingModule.TryGetValue typeName with
        | true, moduleName ->
            match ctx.Resolution.LocalModules.TryGetValue moduleName with
            | true, members -> ValueSome members
            | false, _ -> ValueNone
        | false, _ -> ValueNone

    let private walkClassBodies
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<Scope list>)
        (m: ModuleElem<SyntaxToken>)
        : unit =
        // Resolve the class-like decl to its registered `ClassTypeInfo` (paired with
        // the decl body). Keys by the arity-`SymbolKey`, not the bare name: an
        // overloaded `Box\`1`/`Box\`2` has no bare alias, and a bare-name miss would
        // skip BOTH classes' member bodies (their `this`/ctor params never enter
        // scope). Mirrors `fillClassMembers`.
        let bodyOf (td: TypeDefn<SyntaxToken>) =
            match TypeDefnPatterns.tryClassLikeDecl td with
            | ValueSome d ->
                let (TypeName(ident = nameLi)) = d.TypeName

                if nameLi.Idents.Length = 1 then
                    match
                        TypeRegistry.tryClassArity
                            ctx.Types
                            (ctx.NameOf nameLi.Idents.[0])
                            (arityOfTypeName ctx d.TypeName)
                    with
                    | ValueSome info -> ValueSome(info, d.Body)
                    | ValueNone -> ValueNone
                else
                    ValueNone
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
                            ThisKey = info.ThisKey
                            BaseKey =
                                match info.BaseType with
                                | ValueSome _ -> ValueSome info.BaseKey
                                | ValueNone -> ValueNone
                            CtorParams = info.CtorParams
                            StaticLets = info.StaticLets
                            SecondaryCtors = info.SecondaryCtors
                            InheritsExpr =
                                // Walk the primary base-ctor args only when
                                // `registerInheritedSlots` resolved the parent
                                // (otherwise it already diagnosed the clause).
                                match info.BaseType, body.inherits with
                                | ValueSome _, ValueSome(ClassInheritsDecl(expr = e)) -> e
                                | _ -> ValueNone
                            EnclosingModuleMembers = enclosingModuleMembers ctx info.Name
                            Elements = body.elements
                        }
                | ValueNone -> ()
        | _ -> ()

    /// Name-resolve a union/record host's augmentation-member and `interface … with`
    /// impl bodies so `this` (and any `this.Field` access or `match this with | Case
    /// payload` binders) get a `Binding` entry. Walk when the type has augmentation
    /// members OR interface impls — a type with ONLY an interface impl still needs its
    /// impl bodies resolved (mirrors `Unification.fillHostMembers`, which fills the
    /// impls outside the same `Members`-non-empty guard). Unions/records have no ctor
    /// params / static lets / secondary ctors / inherit, so those stay empty.
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
                    ThisKey = host.ThisKey
                    BaseKey = ValueNone
                    CtorParams = [||]
                    StaticLets = [||]
                    SecondaryCtors = [||]
                    InheritsExpr = ValueNone
                    EnclosingModuleMembers = enclosingModuleMembers ctx name
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
                | ValueSome(struct (nameLi, ValueSome elems)) ->
                    let name = ctx.NameOf nameLi.Idents.[0]

                    match TypeRegistry.tryNonClassMemberHost ctx.Types name with
                    | ValueSome host -> walkNominalHostBodies ctx walker name host elems
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
                let rhsScope = walker.EnterBindingRhs scope isRecursive bindings b
                CstWalk.iterExpr walker rhsScope b.expr
            // `bindingsToScope` writes binding-site self-entries to ctx.Bindings.Binding
            // as a side effect — same path used by EnterLetBody.
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
        (pairs: (ModuleElem<SyntaxToken> * OpenScope * string) list)
        =
        // Pre-pass: register every type so subsequent expression walks (and
        // Unification) resolve against the registry. Records and unions must both
        // finish before bindingsOfPat runs on any pattern, since the
        // ctor-vs-binder disambiguation reads ctx.Types.CtorIndex. Registration
        // resolves no external short names, so it ignores the per-element scope.
        // The third slot is the declaring namespace, threaded into each type's
        // minted `SymbolKey`.
        for (m, _, declNs) in pairs do
            registerRecordTypes ctx declNs m

        for (m, _, declNs) in pairs do
            registerUnionTypes ctx declNs m

        for (m, _, declNs) in pairs do
            registerEnumTypes ctx declNs m

        for (m, _, declNs) in pairs do
            registerAbbreviationTypes ctx declNs m

        for (m, _, declNs) in pairs do
            registerClassTypes ctx declNs m

        // Inheritance (B-4): stamp each class's BaseType / BaseCtorArgs after
        // every class is registered (so a parent declared later resolves), then
        // sweep for cycles once the whole graph is populated.
        for (m, _, _) in pairs do
            registerInheritedSlots ctx m

        checkInheritanceCycles ctx

        // Union/record augmentation members + interface impls register after the type
        // itself (P3d.3).
        for (m, _, _) in pairs do
            registerNominalMembers ctx m

        // walkModuleElem skips ModuleElem.Type, so class/union member bodies are
        // walked here with each type's own scope (`this` + ctor params), giving
        // member-body idents Binding entries before Unification types them.
        // ctx.Resolution.OpenScope is set per element so a member body resolves
        // short external names against the `open`s in scope at that element.
        for (m, openScope, _) in pairs do
            ctx.Resolution.OpenScope <- openScope
            walkClassBodies ctx walker m

        for (m, openScope, _) in pairs do
            ctx.Resolution.OpenScope <- openScope
            walkNominalBodies ctx walker m

        let mutable scope = [ Map.empty ]

        for (m, openScope, _) in pairs do
            ctx.Resolution.OpenScope <- openScope
            scope <- walkModuleElem ctx walker scope m

    /// The simple (last-segment) name of any named `TypeDefn` shape; `ValueNone`
    /// for the nameless `Missing` / `SkipsTokens` placeholders.
    let private typeDefnSimpleName (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : string voption =
        let nameOf (TypeName(ident = li)) =
            if li.Idents.Length >= 1 then
                ValueSome(ctx.NameOf li.Idents.[li.Idents.Length - 1])
            else
                ValueNone

        match td with
        | TypeDefn.Abbrev(typeName = tn)
        | TypeDefn.Record(typeName = tn)
        | TypeDefn.Union(typeName = tn)
        | TypeDefn.Anon(typeName = tn)
        | TypeDefn.Class(typeName = tn)
        | TypeDefn.Struct(typeName = tn)
        | TypeDefn.Interface(typeName = tn)
        | TypeDefn.Enum(typeName = tn)
        | TypeDefn.Delegate(typeName = tn)
        | TypeDefn.TypeExtension(typeName = tn)
        | TypeDefn.AbstractType(typeName = tn) -> nameOf tn
        | TypeDefn.Missing
        | TypeDefn.SkipsTokens _ -> ValueNone

    /// The implicit top-level module's stand-in name in `LocalModules` /
    /// `TypeEnclosingModule`. `$` is not a legal F# identifier character, so this
    /// never collides with a real `module Foo = …` short name. It lets a top-level
    /// type's member body resolve a top-level `let` sibling unqualified — the same
    /// G16 mechanism a *named*-module-nested type gets, extended to the anonymous
    /// /file module (a top-level `RuntimeFormatState`-style sink calling top-level
    /// `flatWidth` / `render`). Only the *unqualified* path uses it; no qualified
    /// reference ever names this segment, so G15 is unaffected.
    let private topLevelModuleSentinel = "$top"

    /// G15/G16 pre-pass. Walk the *un-flattened* module tree and, for every
    /// `module Foo = …` (and the implicit top-level module, keyed by
    /// `topLevelModuleSentinel`), record (a) its directly-`let`-bound
    /// values/functions into `LocalModules` (member name → binding-site `NodeKey`)
    /// and (b) each type it nests into `TypeEnclosingModule` (type name → `Foo`).
    /// Both registries are keyed by the innermost module short name. The subsequent
    /// flattened walk erases these boundaries, so this is the only place the module
    /// structure is captured for name resolution. Mirrors
    /// `Elaborate.translateModuleElem`'s holder walk (which records the same
    /// boundaries for *emission*).
    let private registerLocalModules (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        let registerLet (moduleName: string) (bindings: ImmutableArray<Binding<SyntaxToken>>) =
            let members =
                match ctx.Resolution.LocalModules.TryGetValue moduleName with
                | true, d -> d
                | false, _ ->
                    let d = System.Collections.Generic.Dictionary<string, NodeKey>()
                    ctx.Resolution.LocalModules.[moduleName] <- d
                    d

            for b in bindings do
                for (name, key) in bindingsOfPat ctx b.headPat do
                    members.[name] <- key

        let rec walk (moduleName: string voption) (elems: ModuleElems<SyntaxToken>) =
            for e in elems do
                match e with
                | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
                    match moduleName with
                    | ValueSome m -> registerLet m bindings
                    | ValueNone -> ()
                | ModuleElem.Type defs ->
                    match moduleName with
                    | ValueSome m ->
                        for td in defs do
                            match typeDefnSimpleName ctx td with
                            | ValueSome n -> ctx.Resolution.TypeEnclosingModule.[n] <- m
                            | ValueNone -> ()
                    | ValueNone -> ()
                | ModuleElem.Module(ModuleDefn.ModuleDefn(ident = ident; body = ModuleDefnBody(elements = inner))) ->
                    match inner with
                    | ValueSome innerElems -> walk (ValueSome(ctx.NameOf ident)) innerElems
                    | ValueNone -> ()
                | _ -> ()

        // The implicit top-level module is entered under the sentinel name (not
        // `ValueNone`), so its direct `let`s register as resolvable siblings for a
        // top-level type's member bodies (G16, extended to the file module).
        let top = ValueSome topLevelModuleSentinel

        match file with
        | ImplementationFile.AnonymousModule elems -> walk top elems
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) -> walk top elems
        | ImplementationFile.Namespaces groups ->
            for g in groups do
                match g with
                | NamespaceDeclGroup.Named(elements = elems)
                | NamespaceDeclGroup.Global(elements = elems) -> walk top elems

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        // G15/G16: capture local-module structure before the flattened walk erases it.
        registerLocalModules ctx file
        let walker = mkWalker ctx
        // Seed the walk from the stable ambient prelude. walkElems overwrites
        // ctx.Resolution.OpenScope per element, so the seed is read from
        // AmbientOpenScope, not the scope it mutates.
        // walkModuleTreeWith keeps the declaring-namespace slot walkElems threads into
        // each minted local `SymbolKey`; the no-op onScope hook is the plain walk.
        walkElems
            ctx
            walker
            (CstWalk.walkModuleTreeWith ctx.NameOf ctx.Resolution.AmbientOpenScope (fun _ _ -> ()) file)
