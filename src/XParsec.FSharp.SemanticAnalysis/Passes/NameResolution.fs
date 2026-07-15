namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionTypeHeadStamp
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
            /// Declared `val [mutable] x: T` fields. Carried here — though nothing about them
            /// enters lexical scope — because they are one of the four families that mint a
            /// named field on the type, and so participate in the duplicate-field check below.
            /// `[||]` for unions.
            InstanceFields: ClassFieldInfo[]
            /// The type's members. Not in lexical scope (a member reaches a sibling through
            /// `this`), but their NAMES are needed here: F# forbids a class `let` binder sharing
            /// a name with a member (FS0905).
            Members: TypeMemberInfo[]
            /// `static let` / `static do`, in declaration order. A `static let` name enters
            /// both the instance and the static member scope; the sequence's own expressions
            /// are walked under the static binders above them alone (no `this` / ctor params
            /// / instance binders). `[||]` for unions.
            StaticPreamble: ClassPreambleEntry[]
            /// Instance `let` / `do`, in declaration order. Every binder enters the instance
            /// member scope (members are a mutually-recursive group); the sequence's own
            /// expressions see the ctor params and the instance binders ABOVE them only.
            /// `[||]` for unions.
            InstancePreamble: ClassPreambleEntry[]
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
            /// The enclosing module's `let` value/function bindings that are VISIBLE
            /// from this type's declaration, present when the type is declared inside a
            /// `module Foo = …`. Entered as the lowest-priority layer of
            /// every member-body scope so a nested type's method can reference a
            /// module sibling unqualified (`collapseLHS`, `notStarted` in
            /// `SetIterator`). Empty for a top-level type, and for a module whose `let`s all
            /// sit below the type. Built from the same `LocalModules` registry that
            /// *qualified* resolution reads, and scoped by the same offsets.
            EnclosingModuleScope: Scope
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
    ///
    /// A type body has the module's two-tier shape, and this walk is what enforces it:
    /// the `let` preamble is one strictly TOP-DOWN sequence — each initialiser is walked
    /// under a scope holding only the lets ABOVE it, so `let a = b` naming a later `b` is
    /// unresolved, as F# reports it (FS0039) — while the MEMBERS are a mutually-recursive
    /// group. The group falls out of members never entering lexical scope at all: a member
    /// reaches a sibling through `this`, whose type carries a placeholder TyVar for EVERY
    /// member from registration onwards, so a call to a member declared below resolves with
    /// no ordering constraint to satisfy.
    let private walkTypeBodies (ctx: PassContext) (walker: CstWalk.ExprWalker<Scope list>) (w: TypeBodiesWalk) : unit =
        // Four families mint a field on the type carrying its SOURCE name: primary-ctor params,
        // declared `val` fields, `static let` binders and instance `let` binders. The invariant a
        // backend needs is therefore one rule over their union — no two fields of one type may
        // share a name — and NOT a per-family rule. On the CLR a Field row is identified by
        // (Parent, Name, Signature) (ECMA-335 II.22.15) with static-ness in the flags, not the
        // signature, so even `static let v` beside an instance `let v` is a duplicate ROW, not two
        // fields. Accumulated in declaration order rather than re-scanned per family: this is
        // where "already declared above" is a live fact, and it names the second declaration as
        // the report site.
        let mutable fieldNames: Set<string> = Set.empty

        // F# ACCEPTS every collision this rejects — it uniquifies the BACKING field name by source
        // position (`v@4`) and keeps the plain source name where nothing collides. That
        // uniquification is deferred, not refused: local `let`s need it just as much (on JS
        // `let x = 1` / `let x = x + 1` in statement position emits two `const x` into one block,
        // a hard SyntaxError), so it is built once, upstream, rather than per backend. Until then
        // rejecting is the only alternative to minting two fields of one name and silently
        // miscompiling whichever one a backend picked. A LIMITATION, not invalid F#.
        let declareField (name: string) (declKey: NodeKey) =
            if fieldNames.Contains name then
                ctx.Diagnostics.Add
                    {
                        Key = declKey
                        Message =
                            sprintf
                                "Duplicate field name `%s`: a constructor parameter, a `val` field and a class `let` binding each mint a field carrying its source name, and no two fields of one type may share a name (on the CLR a field's static-ness is not part of its identity). F# permits this by uniquifying the backing-field names; that pass is not implemented yet, so rename one of them."
                                name
                        Code = ""
                        Severity = Severity.Error
                    }
            else
                fieldNames <- Set.add name fieldNames

        // FS0905 — a REAL F# rule, unlike the duplicate-field limitation above, and one that
        // binder uniquification would NOT lift: a member's name is its public surface, so a class
        // `let` binder may not share it. Holds for either side being static (probed against fsc).
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
            declareField p.Name p.DeclKey

            ctx.Bindings.Binding.Set(
                p.DeclKey,
                {
                    BindingSite = p.DeclKey
                    IsInline = false
                    IsMutable = false
                }
            )

        // `val` fields bind no name lexically, so they take part in nothing here but the
        // duplicate-field rule — which is exactly why the rule cannot live in the preamble scopes.
        for f in w.InstanceFields do
            declareField f.Name f.DeclKey

        // The enclosing module's value bindings are visible — unqualified — to
        // every member body of a type nested in that module (F# spec §8.7). They
        // enter as the *lowest-priority* tail layer so `this` / ctor params /
        // preamble binders shadow on a name clash. Already scoped to the type's position by
        // `enclosingModuleScope`; empty (no enclosing module, or nothing of it visible here)
        // leaves resolution unchanged.
        let moduleMemberScope: Scope = w.EnclosingModuleScope

        /// Declare one preamble `let` binder: its binding site, its field, and the FS0905 check.
        /// One entry point for all three so a preamble binder cannot be added to a scope without
        /// its field being declared — which is what makes the ordered accumulation exhaustive.
        let declarePreambleBinder (l: ClassLetInfo) =
            ctx.Bindings.Binding.Set(
                l.DeclKey,
                {
                    BindingSite = l.DeclKey
                    IsInline = false
                    // An instance `let mutable` is a mutable FIELD, so `c <- …` in a
                    // member body (or in a closure the preamble builds) must type-check.
                    IsMutable = l.IsMutable
                }
            )

            declareField l.Name l.DeclKey

            if memberNames.Contains l.Name then
                ctx.Diagnostics.Add
                    {
                        Key = l.DeclKey
                        Message = sprintf "A member and a local class binding both have the name '%s'" l.Name
                        Code = "FS0905"
                        Severity = Severity.Error
                    }

        // A preamble binding is an ordinary `let`: `let f x = …` binds a FUNCTION, so its
        // `argumentPats` scope over the initialiser exactly as a member's do. Only `let rec` puts
        // the binder in scope of its OWN initialiser — which is not shadowing, and stays legal.
        let walkLetInit (outer: Scope list) (l: ClassLetInfo) =
            let b = l.Binding

            let inner =
                if b.argumentPats.IsEmpty then
                    outer
                else
                    extendScope ctx b.argumentPats Map.empty :: outer

            CstWalk.iterExpr walker inner b.expr

        // `static let` names enter scope for every member body (instance and
        // static alike — F# spec §8.7) and resolve to the static field's binder
        // key. The static sequence is walked under the static binders declared *before*
        // each entry (no `this` / ctor params / instance binders), so the binder map is
        // built incrementally — and a `static let` therefore cannot see an instance
        // binder, matching F# (FS0039).
        let mutable staticLetScope: Scope = Map.empty

        for entry in w.StaticPreamble do
            match entry with
            | ClassPreambleEntry.Let l ->
                declarePreambleBinder l

                if l.IsRec then
                    staticLetScope <- Map.add l.Name (l.DeclKey, l.IsMutable) staticLetScope

                walkLetInit [ staticLetScope ] l
                staticLetScope <- Map.add l.Name (l.DeclKey, l.IsMutable) staticLetScope
            | ClassPreambleEntry.Do e -> CstWalk.iterExpr walker [ staticLetScope ] e

        // The instance sequence runs inside the primary ctor, so it sees the ctor params
        // and the static binders (the `.cctor` has already run), and — being strictly
        // top-down — the instance binders above it. NOT `this` / `base` / the `as` alias:
        // F# only makes the object nameable here through an explicit `as self`, and even
        // then calling a member from a preamble `let` throws at run time (initialisation
        // soundness). With no init-soundness analysis, leaving the alias unbound rejects
        // that program rather than silently reading a not-yet-initialised field.
        // A preamble binder's lambda still CAPTURES `this` — that is Elaborate's
        // field rewrite, one layer below scoping, and must not widen this scope.
        // The ctor params are their own layer, but nothing rides on its priority: `declareField`
        // has already rejected any name a ctor param shares with a static binder.
        let ctorParamScope =
            (Map.empty, w.CtorParams)
            ||> Array.fold (fun acc p -> Map.add p.Name (p.DeclKey, false) acc)

        let instanceOuterScope = [ ctorParamScope; staticLetScope; moduleMemberScope ]

        let mutable instanceLetScope: Scope = Map.empty

        for entry in w.InstancePreamble do
            match entry with
            | ClassPreambleEntry.Let l ->
                declarePreambleBinder l

                if l.IsRec then
                    instanceLetScope <- Map.add l.Name (l.DeclKey, l.IsMutable) instanceLetScope

                walkLetInit (instanceLetScope :: instanceOuterScope) l
                instanceLetScope <- Map.add l.Name (l.DeclKey, l.IsMutable) instanceLetScope
            | ClassPreambleEntry.Do e -> CstWalk.iterExpr walker (instanceLetScope :: instanceOuterScope) e

        // Member bodies see EVERY preamble binder (they are a mutually-recursive group, so
        // there is no ordering rule left to enforce here). The merge cannot lose a binder to a
        // same-named one: `declareField` has already rejected any class whose fields collide.
        let mergePreamble (m: Scope) =
            let m = (m, staticLetScope) ||> Map.fold (fun acc k v -> Map.add k v acc)
            (m, instanceLetScope) ||> Map.fold (fun acc k v -> Map.add k v acc)

        let instanceScope = [ mergePreamble scopeMap; moduleMemberScope ]
        // Statics see neither `this` / ctor params nor any instance binder.
        let staticScope: Scope list = [ staticLetScope; moduleMemberScope ]

        // Primary `inherit Base(args)` expression (B-4): name-resolve under a
        // scope of `static let`s plus the primary-ctor params, but without
        // `this` / `base` — and without the instance binders, which are only
        // assigned *after* the base ctor returns.
        match w.InheritsExpr with
        | ValueSome e -> CstWalk.iterExpr walker instanceOuterScope e
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

    /// The member bindings of the module a local type is declared inside that are
    /// VISIBLE from the type — looked up via the `TypeEnclosingModule` → `LocalModules`
    /// registries the pre-pass populated. Empty for a top-level type.
    ///
    /// The use site is the type's own declaration key, and that is exact rather than an
    /// approximation: a type declaration is one contiguous element, so a module `let` sits
    /// either wholly above it (visible to the type and to every one of its member bodies) or
    /// wholly below every member body (visible to none of them). Any offset inside the
    /// declaration renders the same verdict, so there is nothing finer to ask.
    ///
    /// This is why a member body cannot call a module function declared under its type
    /// (F# FS0039) while one declared above it resolves — and why `module rec` restores both:
    /// the binding's `VisibleFrom` moves to the `rec` keyword, above every type in the scope.
    let private enclosingModuleScope (ctx: PassContext) (typeName: string) (declKey: NodeKey) : Scope =
        match ctx.Resolution.TypeEnclosingModule.TryGetValue typeName with
        | true, moduleName ->
            match ctx.Resolution.LocalModules.TryGetValue moduleName with
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
        // Resolve the class-like decl to its registered `ClassTypeInfo` (paired with the decl
        // body) by the key the DECLARATION mints, not by its name: this is the class itself,
        // so a sibling module's same-named class must not answer, and an overloaded
        // `Box\`1`/`Box\`2` does not resolve by bare name at all (a miss would skip BOTH
        // classes' member bodies — their `this`/ctor params would never enter scope).
        // Mirrors `fillClassMembers`.
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
                            ThisKey = info.ThisKey
                            BaseKey =
                                match info.BaseType with
                                | ValueSome _ -> ValueSome info.BaseKey
                                | ValueNone -> ValueNone
                            CtorParams = info.CtorParams
                            InstanceFields = info.InstanceFields
                            Members = info.Members
                            StaticPreamble = info.StaticPreamble
                            InstancePreamble = info.InstancePreamble
                            SecondaryCtors = info.SecondaryCtors
                            InheritsExpr =
                                // Walk the primary base-ctor args only when
                                // `registerInheritedSlot` resolved the parent
                                // (otherwise it already diagnosed the clause).
                                match info.BaseType, body.inherits with
                                | ValueSome _, ValueSome(ClassInheritsDecl(expr = e)) -> e
                                | _ -> ValueNone
                            EnclosingModuleScope = enclosingModuleScope ctx info.Name info.DeclKey
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
                    InstanceFields = [||]
                    Members = host.Members
                    StaticPreamble = [||]
                    InstancePreamble = [||]
                    SecondaryCtors = [||]
                    InheritsExpr = ValueNone
                    EnclosingModuleScope = enclosingModuleScope ctx name host.DeclKey
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
                // Stamp the binding's return-type annotation head (its pattern
                // annotations are stamped by `stampPatCases` via the RHS scope hook).
                stampBindingSigTypes ctx b
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
        (elems: WalkedElem<SyntaxToken> list)
        =
        // The whole-unit type-name pre-scan, and the ONE scan that must see a type before
        // the registration scan reaches it: `moduleHolderName` (the `…Module` suffix rule)
        // reads `NominalTypeNames` at the very first key mint, and a `module Foo` may
        // textually precede the `type Foo` it collides with.
        for w in elems do
            noteNominalTypeNames ctx w.Elem

        // Type registration, top-down. F# type scoping is file-ordered — a type sees what
        // is declared above it plus its own `type … and …` group — and `ModuleElem.Type`
        // IS that group, so registering one group at a time in source order makes the rule
        // structural rather than checked: when a group registers, the name table holds
        // every type above it and nothing below, and each group RESOLVES its declared
        // structure (field / case / `val` / ctor-param / member-signature annotations, the
        // abbreviation RHS) against exactly that. The element's `Containment` (namespace +
        // enclosing modules) rides into each minted key; its `Scope` is the `open` set the
        // group's written heads resolve against.
        //
        // Types and module-level TERMS (`let` / `do` / a bare expression) are ONE ordered
        // sequence, not two passes — `let f (a: A) = …` above `type A` is FS0039 in F# — so
        // a term's type ANNOTATIONS are classified here, at the term's position in the same
        // scan, against exactly the types claimed above it. Only the annotations: the term's
        // value resolution and its body typing stay with the declaration-order body walk
        // below, which is ordered against `fillClassMembers` for the module↔class dependency.
        for w in elems do
            ctx.EnterElement w

            match w.Elem with
            | ModuleElem.Type defs -> registerGroup ctx w.Containment w.RecScopeOffset defs
            | m -> classifyTermTypes ctx m

        // walkModuleElem skips ModuleElem.Type, so class/union member bodies are
        // walked here with each type's own scope (`this` + ctor params), giving
        // member-body idents Binding entries before Unification types them.
        // ctx.Resolution.OpenScope is set per element so a member body resolves
        // short external names against the `open`s in scope at that element.
        for w in elems do
            ctx.EnterElement w
            walkClassBodies ctx walker w.Elem

        for w in elems do
            ctx.EnterElement w
            walkNominalBodies ctx walker w.Elem

        // Module-level VALUES, in declaration order: `walkModuleElem` adds a `let`'s binders
        // to the running scope only AFTER its RHS is walked, so a use above a `let` does not
        // see it. That IS F#'s rule, and the only exception is `rec`.
        //
        // A `rec` scope makes its bindings visible from its own keyword, so every `let` it
        // contains is in scope for every element of it. The flattened element list is a DFS,
        // so one `rec` scope is a contiguous RUN of elements sharing a `RecScopeOffset` — and
        // seeding that run's binders before walking any of it is the whole of the grant.
        // Outside a run the accumulator below is untouched, which is why a non-`rec` module
        // gets no forward visibility at all.
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
    /// mechanism a *named*-module-nested type gets, extended to the anonymous
    /// /file module (a top-level `RuntimeFormatState`-style sink calling top-level
    /// `flatWidth` / `render`). Only the *unqualified* path uses it; no qualified
    /// reference ever names this segment, so qualified resolution is unaffected.
    let private topLevelModuleSentinel = "$top"

    /// Local-module pre-pass. Walk the *un-flattened* module tree and, for every
    /// `module Foo = …` (and the implicit top-level module, keyed by
    /// `topLevelModuleSentinel`), record (a) its directly-`let`-bound
    /// values/functions into `LocalModules` (member name → binding-site `NodeKey`)
    /// and (b) each type it nests into `TypeEnclosingModule` (type name → `Foo`).
    /// Both registries are keyed by the innermost module short name. The subsequent
    /// flattened walk erases these boundaries, so this is the only place the module
    /// structure is captured for name resolution. Mirrors
    /// `Elaborate.translateModuleElem`'s holder walk (which records the same
    /// boundaries for *emission*).
    ///
    /// Each member records the offset it is VISIBLE FROM: its own, or — inside a
    /// `module rec` / `namespace rec` — the enclosing `rec` keyword's, which is the whole of
    /// what `rec` grants. The tree is walked in full before anything resolves, so this table
    /// knows the whole file; the offset is what keeps a reader from seeing below itself.
    let private registerLocalModules (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        let registerLet (moduleName: string) (recScope: int voption) (bindings: ImmutableArray<Binding<SyntaxToken>>) =
            let members =
                match ctx.Resolution.LocalModules.TryGetValue moduleName with
                | true, d -> d
                | false, _ ->
                    let d = System.Collections.Generic.Dictionary<string, LocalModuleMember>()
                    ctx.Resolution.LocalModules.[moduleName] <- d
                    d

            for b in bindings do
                for (name, key) in bindingsOfPat ctx b.headPat do
                    members.[name] <-
                        {
                            BindingSite = key
                            VisibleFrom =
                                match recScope with
                                | ValueSome off -> off
                                | ValueNone -> key.Offset
                        }

        // The innermost enclosing `rec` scope's keyword offset — its own when this scope is
        // itself `rec`, else whatever it inherited (a non-rec submodule of a `rec` namespace
        // is still inside that rec scope). Mirrors `CstWalk.walkModuleTreeWith`'s
        // `innerRecScope`, which computes the same fact for the flattened walk.
        let innerRecScope (keyword: SyntaxToken) (isRec: SyntaxToken voption) (inherited: int voption) : int voption =
            if isRec.IsSome then
                ValueSome keyword.StartIndex
            else
                inherited

        let rec walk (moduleName: string voption) (recScope: int voption) (elems: ModuleElems<SyntaxToken>) =
            for e in elems do
                match e with
                | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
                    match moduleName with
                    | ValueSome m -> registerLet m recScope bindings
                    | ValueNone -> ()
                | ModuleElem.Type defs ->
                    match moduleName with
                    | ValueSome m ->
                        for td in defs do
                            match typeDefnSimpleName ctx td with
                            | ValueSome n -> ctx.Resolution.TypeEnclosingModule.[n] <- m
                            | ValueNone -> ()
                    | ValueNone -> ()
                | ModuleElem.Module(ModuleDefn.ModuleDefn(
                    moduleToken = kw; isRec = isRec; ident = ident; body = ModuleDefnBody(elements = inner))) ->
                    match inner with
                    | ValueSome innerElems ->
                        walk (ValueSome(ctx.NameOf ident)) (innerRecScope kw isRec recScope) innerElems
                    | ValueNone -> ()
                | _ -> ()

        // The implicit top-level module is entered under the sentinel name (not
        // `ValueNone`), so its direct `let`s register as resolvable siblings for a
        // top-level type's member bodies (the same unqualified-sibling mechanism,
        // extended to the file module).
        let top = ValueSome topLevelModuleSentinel

        match file with
        | ImplementationFile.AnonymousModule elems -> walk top ValueNone elems
        | ImplementationFile.NamedModule(NamedModule.NamedModule(moduleToken = kw; isRec = isRec; elements = elems)) ->
            walk top (innerRecScope kw isRec ValueNone) elems
        | ImplementationFile.Namespaces groups ->
            for g in groups do
                match g with
                | NamespaceDeclGroup.Named(namespaceToken = kw; isRec = isRec; elements = elems) ->
                    walk top (innerRecScope kw isRec ValueNone) elems
                | NamespaceDeclGroup.Global(elements = elems) -> walk top ValueNone elems

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        // Capture local-module structure before the flattened walk erases it.
        registerLocalModules ctx file
        let walker = mkWalker ctx
        // Seed the walk from the stable ambient prelude. walkElems overwrites
        // ctx.Resolution.OpenScope per element, so the seed is read from
        // AmbientOpenScope, not the scope it mutates.
        // walkModuleTreeWith keeps the declaring containment walkElems threads into each
        // minted local `SymbolKey`; the no-op onScope hook is the plain walk.
        walkElems
            ctx
            walker
            (CstWalk.walkModuleTreeWith ctx.NameOf ctx.Resolution.AmbientOpenScope (fun _ _ -> ()) file)
