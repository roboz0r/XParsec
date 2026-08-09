namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis
open JsEmitHelpers
open EmitJsCapabilities
open EmitJsTypes
open EmitJsContext

/// A type's members: free type-prefixed functions, and the class methods a capability
/// impl becomes.
module EmitJsMembers =

    /// `const r = this;` — binds the object-argument name a member body or a class preamble's
    /// entries already use. Empty when that name is already `this`.
    let thisAlias (ctx: WalkCtx) (k: BoundVarKeyG<BoundVarId>) : JsStatement list =
        let objArgName = boundVarNameOf ctx.Pool (BoundVarKey.identity k)

        if objArgName = "this" then
            []
        else
            [ JsStatement.Const(objArgName, JsExpr.Identifier("this", ValueNone)) ]

    /// `thisAlias` for a member, or empty when the member is static.
    let thisBinding (ctx: WalkCtx) (m: TastAccessor.TypeMember) : JsStatement list =
        match m.ThisKey with
        | ValueSome k -> thisAlias ctx k
        | ValueNone -> []

    /// Emit a plain (non-generator) ATTACHED instance method: bound to JS `this`, the
    /// member's params curried-free, body returned. The runtimes dispatch on a REGISTRY SYMBOL,
    /// never a named method: `eq` calls `a[Symbol.for("vesper.equality")](b)`. So a capability
    /// impl's slot IS its member body, and only `key` tells the slots apart.
    let emitPlainMethod
        (buildExpr: WalkCtx -> TastAccessor.ExprId -> JsExpr)
        (ctx: WalkCtx)
        (key: JsMethodKey)
        (m: TastAccessor.TypeMember)
        : JsClassMethod =
        {
            Key = key
            Params = [ for (pk, _) in m.Params -> boundVarNameOf ctx.Pool (BoundVarKey.identity pk) ]
            Body = thisBinding ctx m @ [ JsStatement.Return(buildExpr ctx m.Body) ]
            Generator = false
        }

    /// An interface-impl or `override` member as a name-keyed method `M(a) { … }`.
    let emitAttachedMethod
        (buildExpr: WalkCtx -> TastAccessor.ExprId -> JsExpr)
        (ctx: WalkCtx)
        (m: TastAccessor.TypeMember)
        : JsClassMethod =
        emitPlainMethod buildExpr ctx (JsMethodKey.Named m.Name) m

    /// A `GetEnumerator` impl as a native generator:
    /// `*[Symbol.iterator]() { const e = <body>; while (e.MoveNext()) yield e.Current(); }`.
    let emitIteratorMethod
        (buildExpr: WalkCtx -> TastAccessor.ExprId -> JsExpr)
        (ctx: WalkCtx)
        (m: TastAccessor.TypeMember)
        : JsClassMethod =
        let eName = freshTemp ctx.Pool "_e"
        let eIdent = JsExpr.Identifier(eName, ValueNone)

        let attachedCall (name: string) =
            JsExpr.Call(JsExpr.Member(eIdent, JsExpr.Identifier(name, ValueNone), false, ValueNone), [], ValueNone)

        let body =
            thisBinding ctx m
            @ [
                JsStatement.Const(eName, buildExpr ctx m.Body)
                JsStatement.While(attachedCall "MoveNext", [ JsStatement.Yield(attachedCall "Current") ])
            ]

        {
            Key = JsMethodKey.Computed(nativeSymbol "iterator")
            Params = []
            Body = body
            Generator = true
        }

    /// A `Dispose` impl as the native `[Symbol.dispose]() { … }` method that `use` calls.
    let emitDisposeMethod
        (buildExpr: WalkCtx -> TastAccessor.ExprId -> JsExpr)
        (ctx: WalkCtx)
        (m: TastAccessor.TypeMember)
        : JsClassMethod =
        emitPlainMethod buildExpr ctx (JsMethodKey.Computed symbolDispose) m

    /// An eq/comp/hash impl as a registry-symbol slot: `registryName` of `vesper.equality`
    /// emits `[Symbol.for("vesper.equality")](b) { … }`, which the runtime `eq` calls.
    let emitProtocolMethod
        (buildExpr: WalkCtx -> TastAccessor.ExprId -> JsExpr)
        (ctx: WalkCtx)
        (registryName: string)
        (m: TastAccessor.TypeMember)
        : JsClassMethod =
        emitPlainMethod buildExpr ctx (JsMethodKey.Computed(registrySymbol registryName)) m

    /// Emit a record/union member as a free, curried, type-prefixed top-level function:
    /// `member this.Foo a b` → `<Type>__Foo = (this$) => (a) => (b) => <body>`.
    /// Static members drop the object argument; a static property emits as a plain value binding.
    let emitMemberFn
        (buildExpr: WalkCtx -> TastAccessor.ExprId -> JsExpr)
        (ctx: WalkCtx)
        (typeName: string)
        (m: TastAccessor.TypeMember)
        : JsStatement =
        let isProperty = (m.Kind = TMemberKind.Property)
        let name = JsExternalMembers.mangledName typeName m.IsStatic isProperty m.Name

        let objArgNames =
            if m.IsStatic then
                []
            else
                match m.ThisKey with
                | ValueSome k -> [ boundVarNameOf ctx.Pool (BoundVarKey.identity k) ]
                | ValueNone -> [ "this$" ]

        let paramNames =
            [ for (pk, _) in m.Params -> boundVarNameOf ctx.Pool (BoundVarKey.identity pk) ]

        let allNames = objArgNames @ paramNames
        let body = buildExpr ctx m.Body

        let init =
            match allNames with
            | [] -> body
            | _ -> nestUnaryArrows ValueNone allNames (JsFnBody.Expr body)

        // A member function is an arrow value, never reassigned → always `const`.
        topLevelBinding ctx false name init

    /// Every class-method form of a partitioned member set; its `Free` members become
    /// top-level functions via `emitMemberFn` instead.
    let emitCapabilityMethods
        (buildExpr: WalkCtx -> TastAccessor.ExprId -> JsExpr)
        (ctx: WalkCtx)
        (p: PartitionedMembers)
        : JsClassMethod list =
        [
            for m in p.Attached -> emitAttachedMethod buildExpr ctx m
            for m in p.Iterators -> emitIteratorMethod buildExpr ctx m
            for (sym, m) in p.Protocols -> emitProtocolMethod buildExpr ctx sym m
            for m in p.Disposers -> emitDisposeMethod buildExpr ctx m
        ]
