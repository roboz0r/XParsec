namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaborateCalls

// Name resolution for the Elaborate pass: the `try*` resolvers that answer what a
// written name / dotted chain denotes, wrapped as the active patterns each
// `translateExpr` arm guards on. None depend on the recursive `translateExpr`; the
// expression projection (`ElaborateExpr`) opens this module.

module internal ElaborateResolve =

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

    /// Look up `memberName` on `typeName` — a class, or a union / record
    /// augmentation. Returns the declaring type's `TypeKey` alongside the member so
    /// the static-member path can mint a local `SymbolKey.MemberKey` off the resolved
    /// type.
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
                    match Unification.zonk ctx.Store (TyVar tv) with
                    | TyNominal(typeKey, _) ->
                        let memberName = ctx.NameOf li.Idents.[1]

                        // Resolve by the arity-qualified key, not the bare simple name:
                        // an arity-overloaded receiver (`Fun`2`/`Fun`3`) does not resolve
                        // by bare name, so a bare lookup would miss and `f.Invoke(a,b)`
                        // would mis-lower to a `Vesper.Fun::Invoke` function application.
                        match tryNominalMemberByKey ctx typeKey memberName with
                        | ValueSome(_, m) -> ValueSome(rb.BindingSite, Unification.zonk ctx.Store (TyVar tv), m)
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

            match Unification.zonk ctx.Store (typeOfKey ctx (CstKeys.ofExpr r)) with
            | TyNominal(typeKey, _) ->
                match tryNominalMemberByKey ctx typeKey memberName with
                | ValueSome(declKey, m) when m.Kind = ClassMemberKind.Method -> ValueSome(r, declKey, memberName)
                | _ -> ValueNone
            | _ -> ValueNone
        | _ -> ValueNone

    /// The receiver type a folded LongIdent chain's prefix segments `[1 .. n-2]` land
    /// on: the head's bound type walked through `recoverFieldStepTy` one field /
    /// property step at a time. `ValueNone` if the head is not a local binding or any
    /// step can't be typed — then the generic field-chain path handles the whole
    /// expression instead.
    let private tryChainReceiverTy (ctx: PassContext) (li: LongIdent<SyntaxToken>) : SemType voption =
        let head = li.Idents.[0]
        let headKey = NodeKey.ofToken head NodeKind.ExprIdent

        match ctx.Bindings.Binding.TryGetValue headKey with
        | ValueNone -> ValueNone
        | ValueSome rb ->
            let mutable recvTy = Unification.zonk ctx.Store (typeOfKey ctx rb.BindingSite)
            let mutable ok = true

            for i in 1 .. li.Idents.Length - 2 do
                if ok then
                    match recoverFieldStepTy ctx recvTy (ctx.NameOf li.Idents.[i]) with
                    | ValueSome t -> recvTy <- Unification.zonk ctx.Store t
                    | ValueNone -> ok <- false

            if ok then ValueSome recvTy else ValueNone

    /// The receiver chain of a folded member call: the same LongIdent with its
    /// trailing member segment (and the dot before it) dropped, so the caller rebuilds
    /// the receiver via `translateLongIdentFieldChain`.
    let private chainPrefix (li: LongIdent<SyntaxToken>) : LongIdent<SyntaxToken> =
        {
            Idents = li.Idents.RemoveAt(li.Idents.Length - 1)
            Dots = li.Dots.RemoveAt(li.Dots.Length - 1)
        }

    /// `head.f.…g.M(args)` — a method call whose receiver is a *multi-segment*
    /// folded LongIdent chain (`head` a bound local, `f…g` intermediate field /
    /// property steps, `M` the trailing instance method). The parser folds any
    /// `Ident`-headed dotted path into one `LongIdent`, so `this.Source.MoveNext`
    /// arrives as `LongIdent[this; Source; MoveNext]` — *not* a `DotLookup`
    /// (`InstanceMethodCall`) and longer than the 2-segment `ClassTailMethod`. Walk
    /// the prefix's segment types to land the receiver type, then confirm the tail is
    /// one of its methods. Returns the *prefix* LongIdent (the receiver chain, last
    /// segment dropped) + the receiver type + the method name. Without this the chain
    /// falls through to the field-chain resolver, which mis-types the trailing method
    /// segment as a property and leaves the call's `()` as a spurious `App` lowered to
    /// `Vesper.Fun::Invoke` — malformed IL.
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
            match tryChainReceiverTy ctx li with
            | ValueSome(TyNominal(typeKey, _) as recvTy) ->
                let memberName = ctx.NameOf li.Idents.[n - 1]

                match tryNominalMemberByKey ctx typeKey memberName with
                | ValueSome(_, m) when m.Kind = ClassMemberKind.Method -> ValueSome(chainPrefix li, recvTy, memberName)
                | _ -> ValueNone
            | _ -> ValueNone

    /// `head.…M(args)` whose receiver type is a generic typar coerced to a
    /// project-local interface (`'T :> IFace`). Unification resolved the member
    /// through the interface and recorded its `SymbolKey` in `TyparInterfaceCall`
    /// (keyed by the folded `LongIdent`'s `NodeKey`, the same `CstKeys.ofExpr`
    /// identity the inference step used). The receiver never grounds to a nominal, so
    /// neither `ClassTailMethod` nor `ClassChainMethod` fires; this pattern recognises
    /// the recorded call instead. Returns the receiver-prefix LongIdent (member
    /// segment dropped), the receiver's (typar) type, the interface `SymbolKey`, and
    /// the member name — mirroring `ClassChainMethod`'s shape so the `App` arms
    /// rebuild the receiver via `translateLongIdentFieldChain`.
    [<return: Struct>]
    let (|TyparInterfaceMethod|_|)
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        : (LongIdent<SyntaxToken> * SemType * TypeKey * EqArray<SemType> * string) voption =
        if li.Idents.Length < 2 then
            ValueNone
        else
            let key = NodeKey.ofToken (CstKeys.firstTokenOfLongIdent li) NodeKind.ExprLongIdent

            match ctx.Resolution.TyparInterfaceCall.TryGetValue key with
            | ValueNone -> ValueNone
            | ValueSome(ifaceKey, ifaceArgs) ->
                match tryChainReceiverTy ctx li with
                | ValueNone -> ValueNone
                | ValueSome recvTy ->
                    let memberName = ctx.NameOf li.Idents.[li.Idents.Length - 1]
                    ValueSome(chainPrefix li, recvTy, ifaceKey, ifaceArgs, memberName)

    /// The `ResolvedExternalMember` Unification recorded for this node, if any.
    /// Used with a `&` conjunction so the external-member arms drop both the
    /// `ContainsKey` guard and the body's `failwith "unreachable"` re-lookup.
    [<return: Struct>]
    let (|ExternalAccess|_|) (ctx: PassContext) (e: Expr<SyntaxToken>) : ResolvedExternalMember voption =
        ctx.Resolution.ExternalAccess.TryGetValue(CstKeys.ofExpr e)
