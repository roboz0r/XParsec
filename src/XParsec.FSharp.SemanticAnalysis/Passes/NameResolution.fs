namespace XParsec.FSharp.SemanticAnalysis.Passes

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
            CtorParams: ClassCtorParamInfo[]
            /// Class-level `static let` bindings (B-10). Their names enter both the
            /// instance and static member scopes; their initialisers are walked
            /// under the static scope (no `this` / ctor params). `[||]` for unions.
            StaticLets: ClassStaticLetInfo[]
            Elements: TypeDefnElements<SyntaxToken>
        }

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

        let instanceScope = [ mergeStaticLets scopeMap ]
        let staticScope: Scope list = [ staticLetScope ]

        for el in w.Elements do
            match el with
            | TypeDefnElement.Member(MemberDefn.Member(staticToken = s; defn = d)) ->
                let scope = if s.IsSome then staticScope else instanceScope

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

    let private walkClassBodies
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<Scope list>)
        (m: ModuleElem<SyntaxToken>)
        : unit =
        let bodyOf (td: TypeDefn<SyntaxToken>) =
            match TypeDefnPatterns.tryClassLikeDecl td with
            | ValueSome d ->
                let (TypeName(ident = nameLi)) = d.TypeName

                if nameLi.Idents.Length = 1 then
                    ValueSome(ctx.NameOf nameLi.Idents.[0], d.Body)
                else
                    ValueNone
            | ValueNone -> ValueNone

        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match bodyOf td with
                | ValueSome(name, body) ->
                    match ctx.Types.Class.TryGetValue name with
                    | true, info ->
                        walkTypeBodies
                            ctx
                            walker
                            {
                                ThisName = info.ThisName
                                ThisKey = info.ThisKey
                                CtorParams = info.CtorParams
                                StaticLets = info.StaticLets
                                Elements = body.elements
                            }
                    | false, _ -> ()
                | ValueNone -> ()
        | _ -> ()

    let private walkUnionBodies
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<Scope list>)
        (m: ModuleElem<SyntaxToken>)
        : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match td with
                | TypeDefn.Union(
                    typeName = TypeName(ident = nameLi); extensions = ValueSome(TypeExtensionElements(elements = elems))) when
                    nameLi.Idents.Length = 1
                    ->
                    let name = ctx.NameOf nameLi.Idents.[0]

                    match ctx.Types.Union.TryGetValue name with
                    | true, info when not (Array.isEmpty info.Members) ->
                        walkTypeBodies
                            ctx
                            walker
                            {
                                ThisName = info.ThisName
                                ThisKey = info.ThisKey
                                CtorParams = [||]
                                StaticLets = [||]
                                Elements = elems
                            }
                    | _ -> ()
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
        (pairs: (ModuleElem<SyntaxToken> * OpenScope) list)
        =
        // Pre-pass: register every type so subsequent expression walks (and
        // Unification) resolve against the registry. Records and unions must both
        // finish before bindingsOfPat runs on any pattern, since the
        // ctor-vs-binder disambiguation reads ctx.Types.CtorIndex. Registration
        // resolves no external short names, so it ignores the per-element scope.
        for (m, _) in pairs do
            registerRecordTypes ctx m

        for (m, _) in pairs do
            registerUnionTypes ctx m

        for (m, _) in pairs do
            registerAbbreviationTypes ctx m

        for (m, _) in pairs do
            registerClassTypes ctx m

        // Union augmentation members (P3d.3) register after the union itself.
        for (m, _) in pairs do
            registerUnionMembers ctx m

        // walkModuleElem skips ModuleElem.Type, so class/union member bodies are
        // walked here with each type's own scope (`this` + ctor params), giving
        // member-body idents Binding entries before Unification types them.
        // ctx.Resolution.OpenScope is set per element so a member body resolves
        // short external names against the `open`s in scope at that element.
        for (m, openScope) in pairs do
            ctx.Resolution.OpenScope <- openScope
            walkClassBodies ctx walker m

        for (m, openScope) in pairs do
            ctx.Resolution.OpenScope <- openScope
            walkUnionBodies ctx walker m

        let mutable scope = [ Map.empty ]

        for (m, openScope) in pairs do
            ctx.Resolution.OpenScope <- openScope
            scope <- walkModuleElem ctx walker scope m

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        let walker = mkWalker ctx
        // Seed the walk from the stable ambient prelude. walkElems overwrites
        // ctx.Resolution.OpenScope per element, so the seed is read from
        // AmbientOpenScope, not the scope it mutates.
        walkElems ctx walker (CstWalk.walkModuleTree ctx.NameOf ctx.Resolution.AmbientOpenScope file)
