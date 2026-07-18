namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// Name / member resolution helpers for the Elaborate pass: the `try*` resolvers, the
// active patterns each `translateExpr` arm guards on, and the `receiver.seg`
// field-access projection. None depend on the recursive `translateExpr`; the
// expression projection (`ElaborateExpr`) opens this module.

module internal ElaborateResolve =

    // Public surface for the companion `Elaborate` (type-declaration) module: the
    // entry points it projects member bodies / ctor args / field types from.
    let typeOfKey (ctx: PassContext) (key: NodeKey) : SemType =
        match ctx.Bindings.TypeVar.TryGetValue key with
        | ValueSome tv -> Unification.zonk (TyVar tv)
        | ValueNone -> TyVar(TypeVar())

    /// The enum `TypeKey` a node's type carries, if it is an enum. Both the
    /// project-local and the external (TS-manifest) `E.C1` arms type their node
    /// `TyEnum key`, so this is the single signal the expression / pattern freeze
    /// arms read to reuse the same `StaticFieldGet` / `TPat.EnumCase` carrier — the
    /// key is the enum's identity whether the cases are emitted locally (object map)
    /// or imported from a TS module.
    let enumKeyOfTy (ty: SemType) : TypeKey voption =
        match Unification.zonk ty with
        | TyEnum key -> ValueSome key
        | _ -> ValueNone

    [<return: Struct>]
    let (|LocalClass|_|) (ctx: PassContext) (ty: SemType) : ClassTypeInfo voption =
        match Unification.zonk ty with
        | TyClass(key, _) -> TypeRegistry.tryClassByKey ctx.Types key
        | _ -> ValueNone

    [<return: Struct>]
    let (|LocalRecord|_|) (ctx: PassContext) (ty: SemType) : RecordTypeInfo voption =
        match Unification.zonk ty with
        | TyRecord(key, _) -> TypeRegistry.tryRecordByKey ctx.Types key
        | _ -> ValueNone

    [<return: Struct>]
    let (|LocalUnion|_|) (ctx: PassContext) (ty: SemType) : UnionTypeInfo voption =
        match Unification.zonk ty with
        | TyUnion(key, _) -> TypeRegistry.tryUnionByKey ctx.Types key
        | _ -> ValueNone

    /// The enum key for a two-segment `E.C1` access/pattern, binding it once and
    /// collapsing the formerly-separate local-registry and type-signal arms into a
    /// single freeze arm (used by both `ElaborateExpr`'s `StaticFieldGet` and
    /// `ElaboratePatterns`' `TPat.EnumCase`). `enumKeyOfTy` is the canonical signal —
    /// Unification types BOTH project-local and external `E.C1` as `TyEnum key`, so
    /// it alone resolves the valid case; the local registry is consulted only as the
    /// error-path fallback, where an invalid case (`E.BadCase`, already diagnosed
    /// upstream) left the node's type un-pinned. This preserves the prior arms'
    /// behaviour exactly while removing the duplicate arm and the guard re-lookups.
    [<return: Struct>]
    let (|EnumCaseAccess|_|) (ctx: PassContext) (ty: SemType) (li: LongIdent<SyntaxToken>) : TypeKey voption =
        if li.Idents.Length <> 2 then
            ValueNone
        else
            match enumKeyOfTy ty with
            | ValueSome key -> ValueSome key
            | ValueNone ->
                // The written enum name's own token is the use site — the same position
                // Unification resolved the head at, so the fallback cannot see an enum the
                // canonical `TyEnum` signal could not.
                let useSite =
                    ctx.UseSiteAt(NodeKey.ofToken (CstKeys.firstTokenOfLongIdent li) NodeKind.ExprIdent)

                match TypeRegistry.tryEnum ctx.Types useSite (ctx.NameOf li.Idents.[0]) with
                | ValueSome info -> ValueSome info.TypeKey
                | ValueNone -> ValueNone

    /// Class-name reference only when there's no local `Binding` entry — i.e. it
    /// really is a class name, not a shadowing local. An explicit type application
    /// (`Set<'T>(args)`) wraps the name in `Expr.TypeApp`; peel it so the
    /// construction lowers to `TExpr.New` exactly like the inference-pinned
    /// `Set(args)` form (the node's inferred type already carries the instantiation).
    ///
    /// A project-local class is read from `ctx.Types.Class`; an *external* head's
    /// identity is resolved ONCE upstream. NameResolution stamps the resolved type
    /// `SymbolKey` into `Resolution.ResolvedType`, keyed by this head node —
    /// opens-aware, so Elaborate reads the key rather than re-running `OpenScope.tryQualify`
    /// + a provider string lookup here (the resolve-once boundary). The stamp's
    /// PRESENCE is the "head names a constructible external type" verdict, mirroring
    /// Unification's `tryInferExternalCtorApp` / `tryInferExternalGenericCtorApp` — a
    /// `TypeApp` head's receiver carries the stamp, so peeling to the inner head finds
    /// it. Presence suffices WITHOUT a shape check because Elaborate runs after
    /// Unification: a stamped head that is not actually constructible (a generic
    /// union/record receiver) already failed inference, so it never reaches a
    /// well-typed lowering. The returned name is DIAGNOSTIC ONLY: both backends resolve the
    /// construction by the node's result-type `SymbolKey` (`TExpr.New`'s `ty`), never
    /// this string — so no abbreviation expansion is needed here, the node's `ty`
    /// already carries the expanded underlying class (`ResizeArray<'T>` → `List\`1`)
    /// Unification pinned.
    let rec private tryClassRef (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption =
        let key = CstKeys.ofExpr e

        if ctx.Bindings.Binding.ContainsKey key then
            ValueNone
        else
            let stampedExternal () =
                match ctx.Resolution.ResolvedType.TryGetValue key with
                | ValueSome k -> ValueSome(SymbolKeyOps.typeMetaName k)
                | ValueNone -> ValueNone

            // Scoped by the head's own position, exactly as Unification's ctor-as-function
            // read is: a class declared BELOW this head names nothing here, so the head is
            // not a class reference and must not lower to a construction of it. A head naming
            // the class through the module holding it (`A.Point`) is a class reference on the
            // same terms — the local read comes first, so a local class is never mistaken for
            // an external type of the same dotted spelling.
            let localClass (written: WrittenTypeName) : string voption =
                if (TypeRegistry.tryWrittenClass ctx.Types (ctx.UseSiteAt key) written).IsSome then
                    ValueSome written.Written
                else
                    stampedExternal ()

            match e with
            | Expr.Ident t -> localClass (WrittenTypeName.bare (ctx.NameOf t))
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> localClass (ctx.WrittenTypeNameOf li)
            | Expr.TypeApp(expr = inner) -> tryClassRef ctx inner
            | _ -> ValueNone

    /// The declaring nominal `TypeKey` of a class/union/record receiver type — the
    /// `Decl` slot of the `MemberKey` minted for an instance member access. Only
    /// called where the receiver is already known to be nominal (the active patterns /
    /// `InstanceMethodCall` guard on `TyNominal`), so a non-nominal type is an
    /// Elaborate invariant break.
    let nominalDeclKey (ty: SemType) : TypeKey =
        match Unification.zonk ty with
        | TyNominal(key, _) -> key
        | other -> failwithf "Elaborate: expected a class/union/record receiver for a member access, got %A" other

    /// Look up `memberName` on `typeName` — a class or (P3d.3) a union
    /// augmentation. Returns the declaring type's `SymbolKey` (`info.Key`)
    /// alongside the member so the static-member path can mint a local
    /// `SymbolKey.MemberKey` off the resolved type (Phase 4).
    let private tryClassMember
        (ctx: PassContext)
        (useSite: UseSite)
        (typeName: string)
        (memberName: string)
        : (TypeKey * TypeMemberInfo) voption =
        let pick (key: TypeKey) (members: TypeMemberInfo[]) =
            match members |> Array.tryFind (fun m -> m.Name = memberName) with
            | Some m -> ValueSome(key, m)
            | None -> ValueNone

        // The QUALIFIER is resolved from the access's own position, as Unification resolves
        // it: `Foo.Bar` written above `type Foo` names no type there, so it must not lower
        // to a static access on the class below — Elaborate's lowering and the front end's
        // verdict come off the same read.
        match TypeRegistry.tryClass ctx.Types useSite typeName with
        | ValueSome info -> pick info.TypeKey info.Members
        | ValueNone ->
            match TypeRegistry.tryUnionBare ctx.Types useSite typeName with
            | ValueSome info -> pick info.TypeKey info.Members
            | ValueNone ->
                match TypeRegistry.tryRecord ctx.Types useSite typeName with
                | ValueSome info -> pick info.TypeKey info.Members
                | ValueNone -> ValueNone

    /// Key-based sibling of `tryClassMember`: resolves the declaring class / union /
    /// record by its arity-qualified `SymbolKey` (`tryClassByKey`/`tryUnionByKey`/
    /// `tryRecordByKey`, which read the key's ``Name`arity`` verbatim), not the bare
    /// simple name. An arity-overloaded receiver (`Fun`2`/`Fun`3`) does not resolve by
    /// bare name, so a `simpleName`-keyed lookup would miss and the call would mis-lower
    /// to a `Vesper.Fun::Invoke` function application. This is the single member-key
    /// registry read every nominal member dispatch routes through — a record's members
    /// resolve here on the same path as a class's or union's. Callers holding the
    /// receiver's nominal key must route through here.
    let tryNominalMemberByKey
        (ctx: PassContext)
        (typeKey: TypeKey)
        (memberName: string)
        : (TypeKey * TypeMemberInfo) voption =
        let pick (key: TypeKey) (members: TypeMemberInfo[]) =
            match members |> Array.tryFind (fun m -> m.Name = memberName) with
            | Some m -> ValueSome(key, m)
            | None -> ValueNone

        match TypeRegistry.tryClassByKey ctx.Types typeKey with
        | ValueSome info -> pick info.TypeKey info.Members
        | ValueNone ->
            match TypeRegistry.tryUnionByKey ctx.Types typeKey with
            | ValueSome info -> pick info.TypeKey info.Members
            | ValueNone ->
                match TypeRegistry.tryRecordByKey ctx.Types typeKey with
                | ValueSome info -> pick info.TypeKey info.Members
                | ValueNone -> ValueNone

    // --- Implicit value→`obj` upcast ----------------------------------------
    //
    // The front end accepts a value / open typar flowing into an `obj` parameter
    // *without grounding* the typar (Engine's obj-absorption rule). The box that
    // upcast implies is made explicit here, at Elaborate, as a `TExpr.Upcast(arg,
    // obj)` node — codegen's existing `buildUpcast` handler materialises the box
    // (`box` for a value/typar source, a JIT no-op for a reference one). This is
    // the single home for the box policy; codegen no longer re-derives it per
    // emit site. The producers below supply each call/ctor/cons site's
    // per-argument parameter SemTypes; `wrapObjArg`/`wrapObjArgsEq` apply the rule.

    /// `obj` SemType for a synthesised `Upcast` target.
    let objTy: SemType = TyConst(RuntimeNames.objKey, EqArray.empty)

    let private isObjTy (t: SemType) : bool =
        UnificationEngine.isObjType (Unification.zonk t)

    /// Wrap an argument flowing into parameter `paramTy` in an explicit
    /// obj-`Upcast` when the parameter is the universal `obj` slot and the
    /// argument is not already obj (the latter only for tree cleanliness — an
    /// `Upcast(obj, obj)` would emit nothing anyway). A *tupled* multi-parameter
    /// slot — an external .NET method's flattened argument list arriving as a
    /// single `TExpr.Tuple` — wraps element-wise.
    let rec wrapObjArg (paramTy: SemType) (arg: TExpr) : TExpr =
        match Unification.zonk paramTy with
        | TyTuple ptys ->
            match arg with
            | TExpr.Tuple(elems, tupTy, tupTok) when ptys.Length = elems.Length ->
                TExpr.Tuple(
                    EqArray.ofSeq (seq { for i in 0 .. elems.Length - 1 -> wrapObjArg ptys.[i] elems.[i] }),
                    tupTy,
                    tupTok
                )
            | _ -> arg
        | zParam when UnificationEngine.isObjType zParam && not (isObjTy (TastWalk.exprTy arg)) ->
            // The box wraps an existing argument node; anchor the synthesised
            // `Upcast` at that argument's own source token.
            TExpr.Upcast(arg, objTy, TastWalk.exprTok arg)
        | _ -> arg

    /// Apply `wrapObjArg` per position over an arity-flattened argument array.
    /// Positions past the supplied `paramTys` (or an empty model — an external
    /// ctor / unknown member) are left raw.
    let wrapObjArgsEq (paramTys: SemType list) (args: EqArray<TExpr>) : EqArray<TExpr> =
        if List.isEmpty paramTys then
            args
        else
            let ptys = List.toArray paramTys

            EqArray.ofSeq (
                seq {
                    for i in 0 .. args.Length - 1 ->
                        if i < ptys.Length then
                            wrapObjArg ptys.[i] args.[i]
                        else
                            args.[i]
                }
            )

    /// The declared parameter SemType (the `obj`-slot model) for an external
    /// method call, read from the `ResolvedExternalMember.Signature` Unification
    /// recorded at `fnKey` — a method's `TyFun(param → … → ret)` domain, fed
    /// straight to `wrapObjArg` (a multi-parameter method's domain is a `TyTuple`,
    /// which `wrapObjArg` wraps element-wise). This — not the call node's own
    /// SemType — is the box source for an external method: a deferred dot-access
    /// (`comparer.GetHashCode(x)`, receiver grounded only after the body) is typed
    /// by `unifyAppliedSig`, which leaves the node's argument position as the
    /// *un-grounded* argument typar (the obj-absorption rule never grounds
    /// `'T → obj`), so the `obj` slot is visible only on the recorded declared
    /// signature. `ValueNone` for a property (no `TyFun` domain) or a missing
    /// record (the call still emits — just unwrapped).
    let externalMethodParamTy (ctx: PassContext) (fnKey: NodeKey) : SemType voption =
        match ctx.Resolution.ExternalAccess.TryGetValue fnKey with
        | ValueSome info ->
            match Unification.zonk info.Signature with
            | TyFun(dom, _) -> ValueSome dom
            | _ -> ValueNone
        | ValueNone -> ValueNone

    /// The `obj`-slot model for a *residual* application head (the spine fold and
    /// the single-`HighPrecedenceApp` arm share this probe): an external .NET
    /// method head reads it off the recorded declared signature
    /// (`externalMethodParamTy`), since its node SemType is the un-grounded applied
    /// shape, not the function type. `ValueNone` for any non-external head, whose
    /// `obj` slots read off its function-type domain at the call site instead.
    let externalHeadDom (ctx: PassContext) (fnKey: NodeKey) (fnT: TExpr) : SemType voption =
        match fnT with
        | TExpr.ExternalMember(_, _, _, MemberStorage.Method, _, _) -> externalMethodParamTy ctx fnKey
        | _ -> ValueNone

    /// Per-argument parameter SemTypes for a *member* call, flattened to the
    /// arity-flattened argument list. A tupled member `M(a, b)` carries a single
    /// `TyTuple` parameter; `peelCtorArgs` flattens its call args to two, so the
    /// tuple is expanded element-wise here to keep the indices aligned.
    let private flatMemberParams (memberTy: SemType) : SemType list =
        let rec arrows t =
            match Unification.zonk t with
            | TyFun(a, b) ->
                let ps, r = arrows b
                a :: ps, r
            | other -> [], other

        match arrows memberTy with
        | [ single ], _ ->
            match Unification.zonk single with
            | TyTuple elems -> EqArray.toList elems
            | other -> [ other ]
        | ps, _ -> ps

    /// Parameter SemTypes for an instance/static member call resolved to
    /// `declKey.memberName`; empty when the member is unresolved (the call still
    /// emits — just unwrapped, exactly as before this plan).
    let memberParamTys (ctx: PassContext) (declKey: TypeKey) (memberName: string) : SemType list =
        match tryNominalMemberByKey ctx declKey memberName with
        | ValueSome(_, m) -> flatMemberParams m.Type
        | ValueNone -> []

    /// Constructor parameter SemTypes for a project-local class construction of
    /// the given arity: the primary ctor when the arity matches its field count,
    /// else the arity-selected secondary ctor. Empty for an external ctor (no
    /// local param model — the provider recipe boxes), matching codegen's old
    /// `noObjSlots`.
    let ctorParamTys (ctx: PassContext) (classTy: SemType) (argCount: int) : SemType list =
        match classTy with
        | LocalClass ctx info ->
            if argCount = info.CtorParams.Length then
                [ for p in info.CtorParams -> p.Type ]
            else
                match info.SecondaryCtors |> Array.tryFind (fun sc -> sc.Params.Length = argCount) with
                | Some sc -> [ for p in sc.Params -> p.Type ]
                | None -> []
        | _ -> []

    /// The declared SemType of a record field, for boxing a value flowing into an
    /// `obj` field. Answers for a LOCAL record (registry field model) AND an
    /// external / cross-unit one (provider field shapes, instantiated at the receiver's
    /// type args), so `wrapObjArg` boxes an `obj`-typed field of a cross-unit record
    /// exactly as it does a local one. The external arm is load-bearing: field-init now
    /// COERCES a value into an `obj` field (`InferRecordAccess`'s `unifyArg`), so a
    /// cross-unit `{ X = v }` type-checks — without the box here it would emit invalid IL.
    /// `ValueNone` when the field is unknown or the shape is not a provider record.
    let recordFieldTy (ctx: PassContext) (recordTy: SemType) (fieldName: string) : SemType voption =
        match recordTy with
        | LocalRecord ctx info ->
            match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
            | Some f -> ValueSome f.Type
            | None -> ValueNone
        | _ ->
            match Unification.zonk recordTy with
            | TyRecord(key, args) ->
                match ctx.Provider.TryLookupType(SymbolKey.Type key) with
                | ValueSome(ExternalTypeShape.Record(_, fieldShapes, _)) ->
                    match fieldShapes |> Array.tryFind (fun f -> f.Name = fieldName) with
                    | Some f -> ValueSome(FrozenTypeBridge.instantiateDeclaring f.Frozen (args.AsSpan().ToArray()))
                    | None -> ValueNone
                | _ -> ValueNone
            | _ -> ValueNone

    /// Field SemTypes of a union case, in declaration order — for boxing a
    /// value-typed argument flowing into an `obj` case field (the union-cons obj
    /// gap codegen could not close: `EmittedCase.Fields` carries only handles, not
    /// the field types Elaborate has here). Empty for an external union.
    let unionCaseFieldTys (ctx: PassContext) (unionTy: SemType) (caseName: string) : SemType list =
        match unionTy with
        | LocalUnion ctx info ->
            match info.Cases |> Array.tryFind (fun c -> c.Name = caseName) with
            | Some c -> List.ofArray c.Fields
            | None -> []
        | _ -> []

    /// Resolve `head.M` when the head is a local binding of a `TyClass`/`TyUnion`
    /// with a known member `M`. The parser folds the dot into the long ident
    /// rather than emitting `DotLookup` when the head is a regular identifier.
    let private tryLongIdentClassTail
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : (NodeKey * SemType * TypeMemberInfo) voption =
        if li.Idents.Length <> 2 then
            ValueNone
        else
            let head = li.Idents.[0]
            let headKey = NodeKey.ofToken head NodeKind.ExprIdent

            match ctx.Bindings.Binding.TryGetValue headKey with
            | ValueNone -> ValueNone
            | ValueSome rb ->
                match ctx.Bindings.TypeVar.TryGetValue rb.BindingSite with
                | ValueNone -> ValueNone
                | ValueSome tv ->
                    match Unification.zonk (TyVar tv) with
                    | TyNominal(typeKey, _) ->
                        let memberName = ctx.NameOf li.Idents.[1]

                        // Resolve by the arity-qualified key, not the bare simple name:
                        // an arity-overloaded receiver (`Fun`2`/`Fun`3`) does not resolve
                        // by bare name, so a bare lookup would miss and `f.Invoke(a,b)`
                        // would mis-lower to a `Vesper.Fun::Invoke` function application.
                        match tryNominalMemberByKey ctx typeKey memberName with
                        | ValueSome(_, m) -> ValueSome(rb.BindingSite, Unification.zonk (TyVar tv), m)
                        | ValueNone -> ValueNone
                    | _ -> ValueNone

    /// Resolve `ClassName.MemberName` to its static member info. `ValueNone` if
    /// either is unknown or the member is an instance member (use
    /// `tryLongIdentClassTail` for instance dispatch on a local binding).
    let private tryLongIdentStaticMember
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : (TypeKey * TypeMemberInfo) voption =
        if li.Idents.Length <> 2 then
            ValueNone
        else
            let className = ctx.NameOf li.Idents.[0]
            let memberName = ctx.NameOf li.Idents.[1]
            // The written qualifier's own token IS the use site.
            let useSite =
                ctx.UseSiteAt(NodeKey.ofToken (CstKeys.firstTokenOfLongIdent li) NodeKind.ExprIdent)

            tryClassMember ctx useSite className memberName
            |> ValueOption.filter (fun (_, m) -> m.IsStatic)

    /// DU ctor reference (`Circle`, `Result2.Ok`, or an external `Some` / `None`),
    /// returning the case name. Excludes local bindings whose names happen to
    /// match a ctor — they have a `Binding` entry. An external case is read from
    /// NameResolution's `ExternalUnionCaseStamp` (keyed by this expression node);
    /// the case name alone is returned (the CtorRef arms read the declaring union
    /// off the node's resolved `TyUnion` type), so the local and external paths
    /// emit `TExpr.UnionCons` identically.
    let private tryCtorRef (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption =
        let key = CstKeys.ofExpr e

        if ctx.Bindings.Binding.ContainsKey key then
            ValueNone
        else
            // A local *or* external union declares this node's name as a case. The
            // external leg is the stamp's presence; a bare reference to an RQA
            // external case is never stamped — only its qualified form (the
            // length-2 arms below) is a ctor ref.
            let isCase (n: string) =
                TypeRegistry.isCaseName ctx.Types (ctx.UseSiteAt key) n
                || ctx.Resolution.ExternalUnionCaseStamp.ContainsKey key

            match e with
            | Expr.Ident t ->
                let n = ctx.NameOf t

                if isCase n then ValueSome n else ValueNone
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                let n = ctx.NameOf li.Idents.[0]

                if isCase n then ValueSome n else ValueNone
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                li.Idents.Length = 2
                && TypeRegistry.localQualifiedCase
                    ctx.Types
                    (ctx.UseSiteAt key)
                    (ctx.NameOf li.Idents.[0])
                    (ctx.NameOf li.Idents.[1])
                ->
                // `localQualifiedCase` already confirmed the case belongs to the
                // qualifier's union (arity-safe over `Choice\`2`…`Choice\`7`).
                ValueSome(ctx.NameOf li.Idents.[1])
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 2 ->
                // Qualified external union case (`Option.Some`): the head is an
                // external union, not a local one. NameResolution stamped this node
                // only when the resolved union's short name matched the written
                // qualifier, so the stamp's presence is the acceptance test.
                if ctx.Resolution.ExternalUnionCaseStamp.ContainsKey key then
                    ValueSome(ctx.NameOf li.Idents.[1])
                else
                    ValueNone
            | _ -> ValueNone

    // Active patterns wrap the four `try*` helpers so each `translateExpr` arm
    // computes its guard once and binds the destructured result directly,
    // rather than re-evaluating in the body with a `ValueNone -> failwith
    // "unreachable"` fall-through.

    [<return: Struct>]
    let (|ClassRef|_|) (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption = tryClassRef ctx e

    [<return: Struct>]
    let (|CtorRef|_|) (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption = tryCtorRef ctx e

    [<return: Struct>]
    let (|ClassTailMethod|_|) (ctx: PassContext) (li: LongIdent<SyntaxToken>) : (NodeKey * SemType * string) voption =
        match tryLongIdentClassTail ctx li with
        | ValueSome(bs, ty, m) when m.Kind = ClassMemberKind.Method ->
            ValueSome(bs, ty, ctx.NameOf li.Idents.[li.Idents.Length - 1])
        | _ -> ValueNone

    [<return: Struct>]
    let (|ClassTailProperty|_|) (ctx: PassContext) (li: LongIdent<SyntaxToken>) : (NodeKey * SemType * string) voption =
        match tryLongIdentClassTail ctx li with
        | ValueSome(bs, ty, m) when m.Kind = ClassMemberKind.Property ->
            ValueSome(bs, ty, ctx.NameOf li.Idents.[li.Idents.Length - 1])
        | _ -> ValueNone

    [<return: Struct>]
    let (|StaticMethod|_|) (ctx: PassContext) (li: LongIdent<SyntaxToken>) : (TypeKey * string) voption =
        match tryLongIdentStaticMember ctx li with
        | ValueSome(declKey, m) when m.Kind = ClassMemberKind.Method ->
            ValueSome(declKey, ctx.NameOf li.Idents.[li.Idents.Length - 1])
        | _ -> ValueNone

    [<return: Struct>]
    let (|StaticMember|_|) (ctx: PassContext) (li: LongIdent<SyntaxToken>) : (TypeKey * string) voption =
        match tryLongIdentStaticMember ctx li with
        | ValueSome(declKey, _) -> ValueSome(declKey, ctx.NameOf li.Idents.[li.Idents.Length - 1])
        | ValueNone -> ValueNone

    /// `ClassName<'args>.Member` — a static member access on an *explicitly*
    /// instantiated generic class. It parses as `DotLookup(TypeApp(ClassName,
    /// <'args>), .Member)` rather than the folded `LongIdent[ClassName; Member]`
    /// the bare `ClassName.Member` form takes (`StaticMember` / `StaticMethod`).
    /// The type args only pin the generic instantiation (already carried on the
    /// node's `ty`); the receiver is a type, so it lowers to the same
    /// receiver-less static get / call. Returns the member's `Kind` so the caller
    /// routes a property read vs a method call (the method form is `App`-wrapped).
    [<return: Struct>]
    let (|TypeAppStaticMember|_|)
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        : (TypeKey * string * ClassMemberKind) voption =
        match e with
        | Expr.DotLookup(expr = Expr.TypeApp(expr = classExpr); longIdentOrOp = LongIdentOrOp.LongIdent li) when
            li.Idents.Length = 1
            ->
            let classNameOpt =
                match classExpr with
                | Expr.Ident t -> ValueSome(ctx.NameOf t)
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent cli) when cli.Idents.Length = 1 ->
                    ValueSome(ctx.NameOf cli.Idents.[0])
                | _ -> ValueNone

            match classNameOpt with
            | ValueSome className ->
                let memberName = ctx.NameOf li.Idents.[0]

                match tryClassMember ctx (ctx.UseSiteAt(CstKeys.ofExpr e)) className memberName with
                | ValueSome(declKey, m) when m.IsStatic -> ValueSome(declKey, memberName, m.Kind)
                | _ -> ValueNone
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Dispatch discriminator for an instance member access. A `base.M(...)` / `base.X` receiver translates to a
    /// `TExpr.Var` whose binding site is some class's `BaseKey`; that must
    /// dispatch non-virtually so an `override` calling `base.M()` doesn't recurse
    /// into itself. The check is O(classes) per access — the gap doc accepts this
    /// for v1 (most files declare a handful of classes); a reverse index is a
    /// later optimisation.
    let viaOfReceiver (ctx: PassContext) (receiver: TExpr) : CallVia<SemType> =
        match receiver with
        | TExpr.Var(bindingSite, _, _) ->
            let mutable isBase = false

            for kv in ctx.Types.Class do
                if not isBase && kv.Value.BaseType.IsSome && kv.Value.BaseKey = bindingSite then
                    isBase <- true

            if isBase then CallVia.Base else CallVia.Self
        | _ -> CallVia.Self

    // --- Call / construction smart constructors -----------------------------
    //
    // Every TAST node that flows arguments into possibly-`obj` parameter slots is
    // built through one of these, so the implicit value→`obj` upcast
    // (`wrapObjArgsEq`) can never be forgotten by a `translateExpr` arm — the
    // single home of the box decision, in the TAST layer where the `Upcast` node
    // lives. Each picks the parameter model appropriate to its node kind; the arms
    // supply only the resolved callee and the peeled (un-wrapped) arguments.

    /// `New` for a class construction. Reads the front-end-chosen external `.ctor`'s
    /// `SymbolKey.MemberKey` from `Resolution.ExternalCtor`, keyed by the construction
    /// node's `key`, and records it on the node so codegen selects that exact `.ctor` by
    /// identity. `ValueNone` (absent) ⇒ a project-local / scratch class codegen resolves
    /// by result-type key + arity. Centralising the read here keeps every construction
    /// syntax's identity handshake in one place.
    let mkNew
        (ctx: PassContext)
        (className: string)
        (key: NodeKey)
        (ty: SemType)
        (args: EqArray<TExpr>)
        (tok: SyntaxToken)
        : TExpr =
        TExpr.New(
            className,
            ctx.Resolution.ExternalCtor.TryGetValue key,
            wrapObjArgsEq (ctorParamTys ctx ty args.Length) args,
            ty,
            tok
        )

    /// Instance `MethodCall` resolved to `declKey.memberName`, with the `CallVia`
    /// derived from the receiver. `callKey` is the call node's `NodeKey`: for an OVERLOADED
    /// name, Unification recorded the chosen overload's TOTAL frozen `MemberKey` there
    /// (`Resolution.LocalMemberCall`), read back verbatim so Freeze resolves the identical
    /// member by identity — no second name-based pick. A non-overloaded name has no entry
    /// and mints the placeholder `LocalSymbolKey.ofMember` key, which is already unique.
    let mkMethodCall
        (ctx: PassContext)
        (callKey: NodeKey)
        (receiver: TExpr)
        (declKey: TypeKey)
        (memberName: string)
        (args: EqArray<TExpr>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let key =
            match ctx.Resolution.LocalMemberCall.TryGetValue callKey with
            | ValueSome frozen -> frozen
            | ValueNone -> LocalSymbolKey.ofMember declKey memberName args.Length MemberKind.Method

        let argsList = wrapObjArgsEq (memberParamTys ctx declKey memberName) args
        TExpr.MethodCall(receiver, key, viaOfReceiver ctx receiver, argsList, ty, tok)

    /// Wall B (rung 3): instance `MethodCall` dispatched through an *interface* the
    /// receiver's typar is coerced to (`'T :> IFace`). `ifaceKey` is the interface's
    /// declaring `SymbolKey` (the member key's `decl`); `CallVia.Interface` tells
    /// codegen to emit `constrained. <receiver-typar> callvirt`. The parameter model
    /// for the obj-upcast comes from the interface's own member (the abstract slot).
    let mkInterfaceMethodCall
        (ctx: PassContext)
        (receiver: TExpr)
        (ifaceKey: TypeKey)
        (ifaceArgs: EqArray<SemType>)
        (memberName: string)
        (args: EqArray<TExpr>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let key = LocalSymbolKey.ofMember ifaceKey memberName args.Length MemberKind.Method
        let argsList = wrapObjArgsEq (memberParamTys ctx ifaceKey memberName) args
        TExpr.MethodCall(receiver, key, CallVia.Interface ifaceArgs, argsList, ty, tok)

    /// `StaticMethodCall` resolved to `declKey.memberName`.
    let mkStaticMethodCall
        (ctx: PassContext)
        (declKey: TypeKey)
        (memberName: string)
        (args: EqArray<TExpr>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let key = LocalSymbolKey.ofMember declKey memberName args.Length MemberKind.Method
        TExpr.StaticMethodCall(key, wrapObjArgsEq (memberParamTys ctx declKey memberName) args, ty, tok)

    /// `UnionCons` for case `caseName` of union `ty`.
    let mkUnionCons
        (ctx: PassContext)
        (caseName: string)
        (ty: SemType)
        (args: EqArray<TExpr>)
        (tok: SyntaxToken)
        : TExpr =
        TExpr.UnionCons(caseName, wrapObjArgsEq (unionCaseFieldTys ctx ty caseName) args, ty, tok)

    /// Recover segment `segName`'s declared type from receiver type `recvTy` — a
    /// record field, or a union / class instance-member return type — instantiated
    /// at the receiver's type arguments. `ValueNone` when the receiver isn't a
    /// known nominal or has no such member (the caller picks a fallback type).
    let recoverFieldStepTy (ctx: PassContext) (recvTy: SemType) (segName: string) : SemType voption =
        let memberTy (typeParams, args) (members: TypeMemberInfo[]) =
            members
            |> Array.tryPick (fun m ->
                if m.Name = segName && not m.IsStatic then
                    Some(Unification.instantiateMember (typeParams, args) m.Type)
                else
                    None
            )

        let resolved =
            match Unification.zonk recvTy with
            | TyRecord(recKey, args) ->
                match TypeRegistry.tryRecordByKey ctx.Types recKey with
                | ValueSome info ->
                    // A record's chain segment is a field OR an instance member
                    // (property) — check fields first, then members, mirroring the
                    // class arm below (a record has no inheritance, so no chain walk).
                    let fieldTy =
                        info.Fields
                        |> Array.tryPick (fun f ->
                            if f.Name = segName then
                                Some(Unification.instantiateMember (info.TypeParams, args) f.Type)
                            else
                                None
                        )

                    match fieldTy with
                    | Some _ -> fieldTy
                    | None -> memberTy (info.TypeParams, args) info.Members
                | ValueNone -> None
            | TyUnion(unionKey, args) ->
                match TypeRegistry.tryUnionByKey ctx.Types unionKey with
                | ValueSome info -> memberTy (info.TypeParams, args) info.Members
                | ValueNone -> None
            | TyClass(clsKey, args) ->
                match TypeRegistry.tryClassByKey ctx.Types clsKey with
                | ValueSome info ->
                    // A `this.x` chain segment may be an explicit `val` instance
                    // field or a primary-ctor parameter (both emitted as fields),
                    // not an instance member — `memberTy` alone misses it, and the
                    // caller would then fall back to the chain's *final* type,
                    // mis-typing the receiver (e.g. `this.stack.IsEmpty` typing
                    // `this.stack` as `bool`). Check fields first, then members.
                    let fieldTy =
                        Seq.append
                            (info.InstanceFields |> Seq.map (fun f -> f.Name, f.Type))
                            (info.CtorParams |> Seq.map (fun p -> p.Name, p.Type))
                        |> Seq.tryPick (fun (n, t) ->
                            if n = segName then
                                Some(Unification.instantiateMember (info.TypeParams, args) t)
                            else
                                None
                        )

                    match fieldTy with
                    | Some _ -> fieldTy
                    | None -> memberTy (info.TypeParams, args) info.Members
                | ValueNone -> None
            | _ -> None

        match resolved with
        | Some t -> ValueSome(Unification.zonk t)
        | None -> ValueNone

    /// One `receiver.seg` access node: `PropertyGet` for a class / union member,
    /// `FieldGet` otherwise. `recvTy` is the receiver's (un-zonked) type; `stepTy`
    /// is the segment's already-resolved result type.
    /// `chainKey` is the enclosing `LongIdent` chain's NodeKey — the identity
    /// Unification stamped the resolved `GetArrayLength` intrinsic under for the
    /// `arr.Length` array-length arm below (so the `ldlen` body splices by KEY).
    let fieldStep
        (ctx: PassContext)
        (chainKey: NodeKey)
        (receiver: TExpr)
        (recvTy: SemType)
        (segName: string)
        (stepTy: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let isMember (members: TypeMemberInfo[]) =
            members |> Array.exists (fun m -> m.Name = segName)

        // A flat nominal (union or record — neither has an inheritance chain): a
        // member-name segment is a `PropertyGet`, a non-member (a record field, a
        // union tag/case field) a `FieldGet`. The two kinds share ONE arm through
        // the member-key read (`tryNominalMemberByKey`); only which registry it
        // consults differs, and that is hidden inside the read.
        let flatNominalStep (typeKey: TypeKey) : TExpr =
            match tryNominalMemberByKey ctx typeKey segName with
            | ValueSome(declKey, _) ->
                let key = LocalSymbolKey.ofMember declKey segName 0 MemberKind.Property
                TExpr.PropertyGet(receiver, key, viaOfReceiver ctx receiver, stepTy, tok)
            | ValueNone -> TExpr.FieldGet(receiver, segName, stepTy, tok)

        match Unification.zonk recvTy with
        | TyClass(clsKey, args) ->
            match TypeRegistry.tryClassByKey ctx.Types clsKey with
            | ValueSome info when isMember info.Members ->
                let key = LocalSymbolKey.ofMember info.TypeKey segName 0 MemberKind.Property
                TExpr.PropertyGet(receiver, key, viaOfReceiver ctx receiver, stepTy, tok)
            | _ ->
                // An *inherited* member (declared on a base class, e.g. `node.Key`
                // where `Key` is on the parent `SetTree`): upcast the receiver to the
                // declaring ancestor so codegen's receiver-keyed
                // `resolveInstanceMember` resolves `get_<seg>` on the class that
                // emits it (a reference-type upcast is a codegen no-op). Reuses
                // inference's `inherit`-chain walk (`tryClassChainMemberDecl`) so this
                // read isn't a second chain walk that must stay in sync. Falls through
                // to `FieldGet` only when no ancestor declares it — a genuine ctor-param
                // / `val` field access. (The own-class case is the `isMember` arm
                // above, so the walk only ever resolves a strict ancestor here.)
                match Unification.tryClassChainMemberDecl ctx clsKey args segName with
                | ValueSome cm ->
                    let key =
                        LocalSymbolKey.ofMember (nominalDeclKey cm.DeclaringTy) segName 0 MemberKind.Property

                    TExpr.PropertyGet(
                        TExpr.Upcast(receiver, cm.DeclaringTy, TastWalk.exprTok receiver),
                        key,
                        viaOfReceiver ctx receiver,
                        stepTy,
                        tok
                    )
                | ValueNone -> TExpr.FieldGet(receiver, segName, stepTy, tok)
        // A flat nominal — union or record. The `TyClass` arm above forks for its
        // inheritance chain; union and record share `flatNominalStep` (a member is a
        // `PropertyGet`, a record field / union case field a `FieldGet`), so they land
        // here through `TyNominal` (class is already handled above, so this only ever
        // catches union/record).
        | TyNominal(nominalKey, _) -> flatNominalStep nominalKey
        // `arr.Length` on an intrinsic rank-1 array desugars to the core
        // `GetArrayLength` inline function (the `ldlen` mnemonic lives in
        // `ops-platform.fs`, spliced here by `InlineExpansion`). `array.Length`
        // parses as a local-headed LongIdent field chain (not `DotLookup`), so this
        // `fieldStep` arm is the one that fires; mirrors the `DotLookup` array guard.
        | TyArray _ when segName = "Length" ->
            let lenKey = ctx.Resolution.IntrinsicKey.TryGetValue chainKey
            TExpr.App(TExpr.External("GetArrayLength", lenKey, TyFun(recvTy, stepTy), tok), receiver, stepTy, tok)
        | _ -> TExpr.FieldGet(receiver, segName, stepTy, tok)

    /// `r.M(...)` where `r` has a class / union type and `M` is one of its
    /// instance methods. Returns the receiver expr + resolved member name so the
    /// `App` and `HighPrecedenceApp` invocation arms share one guard (the same
    /// convention as `ClassTailMethod` above) instead of repeating the
    /// receiver-type lookup verbatim.
    [<return: Struct>]
    let (|InstanceMethodCall|_|)
        (ctx: PassContext)
        (funcExpr: Expr<SyntaxToken>)
        : (Expr<SyntaxToken> * TypeKey * string) voption =
        match funcExpr with
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            let memberName = ctx.NameOf li.Idents.[0]

            match Unification.zonk (typeOfKey ctx (CstKeys.ofExpr r)) with
            | TyNominal(typeKey, _) ->
                match tryNominalMemberByKey ctx typeKey memberName with
                | ValueSome(declKey, m) when m.Kind = ClassMemberKind.Method -> ValueSome(r, declKey, memberName)
                | _ -> ValueNone
            | _ -> ValueNone
        | _ -> ValueNone

    /// `head.f.…g.M(args)` — a method call whose receiver is a *multi-segment*
    /// folded LongIdent chain (`head` a bound local, `f…g` intermediate field /
    /// property steps, `M` the trailing instance method). The parser folds any
    /// `Ident`-headed dotted path into one `LongIdent`, so `this.Source.MoveNext`
    /// arrives as `LongIdent[this; Source; MoveNext]` — *not* a `DotLookup`
    /// (`InstanceMethodCall`) and longer than the 2-segment `ClassTailMethod`. Walk
    /// the prefix's segment types with `recoverFieldStepTy` (the same recovery
    /// `translateLongIdentFieldChain` uses) to land the receiver type, then confirm
    /// the tail is one of its methods. Returns the *prefix* LongIdent (the receiver
    /// chain, last segment dropped) + the receiver type + the method name, so the
    /// `App` arm rebuilds the receiver via `translateLongIdentFieldChain`. Without
    /// this the chain falls through to the field-chain resolver, which mis-types the
    /// trailing method segment as a property and leaves the call's `()` as a spurious
    /// `App` lowered to `Vesper.Fun::Invoke` — malformed IL.
    [<return: Struct>]
    let (|ClassChainMethod|_|)
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : (LongIdent<SyntaxToken> * SemType * string) voption =
        let n = li.Idents.Length

        if n < 3 then
            // 2-segment `var.M(args)` is `ClassTailMethod`; this is the 3+ case.
            ValueNone
        else
            let head = li.Idents.[0]
            let headKey = NodeKey.ofToken head NodeKind.ExprIdent

            match ctx.Bindings.Binding.TryGetValue headKey with
            | ValueNone -> ValueNone
            | ValueSome rb ->
                // Walk the intermediate segments `[1 .. n-2]` to the receiver type,
                // bailing if any step can't be typed (then the generic path handles it).
                let mutable recvTy = Unification.zonk (typeOfKey ctx rb.BindingSite)
                let mutable ok = true

                for i in 1 .. n - 2 do
                    if ok then
                        match recoverFieldStepTy ctx recvTy (ctx.NameOf li.Idents.[i]) with
                        | ValueSome t -> recvTy <- Unification.zonk t
                        | ValueNone -> ok <- false

                if not ok then
                    ValueNone
                else
                    match recvTy with
                    | TyNominal(typeKey, _) ->
                        let memberName = ctx.NameOf li.Idents.[n - 1]

                        match tryNominalMemberByKey ctx typeKey memberName with
                        | ValueSome(_, m) when m.Kind = ClassMemberKind.Method ->
                            let prefixLi =
                                {
                                    Idents = li.Idents.RemoveAt(n - 1)
                                    Dots = li.Dots.RemoveAt(li.Dots.Length - 1)
                                }

                            ValueSome(prefixLi, recvTy, memberName)
                        | _ -> ValueNone
                    | _ -> ValueNone

    /// Wall B (rung 3): `head.…M(args)` whose receiver type is a generic typar
    /// coerced to a project-local interface (`'T :> IFace`). Unification resolved
    /// the member through the interface and recorded its `SymbolKey` in
    /// `TyparInterfaceCall` (keyed by the folded `LongIdent`'s `NodeKey`, the same
    /// `CstKeys.ofExpr` identity the inference step used). The receiver never grounds
    /// to a nominal, so neither `ClassTailMethod` nor `ClassChainMethod` fires; this
    /// pattern recognises the recorded call instead. Returns the receiver-prefix
    /// LongIdent (member segment dropped), the receiver's (typar) type, the interface
    /// `SymbolKey`, and the member name — mirroring `ClassChainMethod`'s shape so the
    /// `App` arms rebuild the receiver via `translateLongIdentFieldChain`.
    [<return: Struct>]
    let (|TyparInterfaceMethod|_|)
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : (LongIdent<SyntaxToken> * SemType * TypeKey * EqArray<SemType> * string) voption =
        let n = li.Idents.Length

        if n < 2 then
            ValueNone
        else
            let key = NodeKey.ofToken (CstKeys.firstTokenOfLongIdent li) NodeKind.ExprLongIdent

            match ctx.Resolution.TyparInterfaceCall.TryGetValue key with
            | ValueNone -> ValueNone
            | ValueSome(ifaceKey, ifaceArgs) ->
                let head = li.Idents.[0]
                let headKey = NodeKey.ofToken head NodeKind.ExprIdent

                match ctx.Bindings.Binding.TryGetValue headKey with
                | ValueNone -> ValueNone
                | ValueSome rb ->
                    // Walk the intermediate segments `[1 .. n-2]` to the receiver
                    // (typar) type, exactly as `ClassChainMethod` does.
                    let mutable recvTy = Unification.zonk (typeOfKey ctx rb.BindingSite)
                    let mutable ok = true

                    for i in 1 .. n - 2 do
                        if ok then
                            match recoverFieldStepTy ctx recvTy (ctx.NameOf li.Idents.[i]) with
                            | ValueSome t -> recvTy <- Unification.zonk t
                            | ValueNone -> ok <- false

                    if not ok then
                        ValueNone
                    else
                        let memberName = ctx.NameOf li.Idents.[n - 1]

                        let prefixLi =
                            {
                                Idents = li.Idents.RemoveAt(n - 1)
                                Dots = li.Dots.RemoveAt(li.Dots.Length - 1)
                            }

                        ValueSome(prefixLi, recvTy, ifaceKey, ifaceArgs, memberName)

    /// The `ResolvedExternalMember` Unification recorded for this node, if any.
    /// Used with a `&` conjunction so the external-member arms drop both the
    /// `ContainsKey` guard and the body's `failwith "unreachable"` re-lookup.
    [<return: Struct>]
    let (|ExternalAccess|_|) (ctx: PassContext) (e: Expr<SyntaxToken>) : ResolvedExternalMember voption =
        ctx.Resolution.ExternalAccess.TryGetValue(CstKeys.ofExpr e)
