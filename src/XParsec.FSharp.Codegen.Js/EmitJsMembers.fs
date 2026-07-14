namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis
open JsEmitHelpers
open EmitJsCapabilities
open EmitJsTypes
open EmitJsContext

/// Emission of a type's MEMBERS: free receiver-first member functions, and the attached
/// class-method forms a capability impl becomes (`[Symbol.iterator]` generators,
/// `[Symbol.for("vesper.X")]` protocol slots, the `[Symbol.dispose]()` method, plain
/// `Object`/interface overrides). Each recurses into emission only for a member BODY, so
/// it takes `buildExpr` as a callback; `buildProgram` passes the walker in.
module EmitJsMembers =

    /// Re-bind a receiver binder (a member's `ThisKey`, or the class-level one the
    /// instance preamble reads its fields through) to JS `this` via a leading `const`,
    /// leaving the body's `TExpr.Var(thisKey)` references intact. Empty when the binder
    /// already resolves to `this` (avoids a no-op `const this = this;`).
    let thisAlias (ctx: WalkCtx) (k: NodeKey) : JsStatement list =
        let recvName = identName ctx.Source k

        if recvName = "this" then
            []
        else
            [ JsStatement.Const(recvName, JsExpr.Identifier("this", ValueNone)) ]

    /// `thisAlias` for a member — empty when the member is static.
    let thisBinding (ctx: WalkCtx) (m: Frozen.TTypeMember) : JsStatement list =
        match m.ThisKey with
        | ValueSome k -> thisAlias ctx k
        | ValueNone -> []

    /// Emit a plain (non-generator) ATTACHED instance method: receiver bound to JS
    /// `this` (not a curried param), the member's params curried-free, body returned.
    /// The runtimes dispatch by method presence — `Vesper.Core.eq` calls `a.Equals(b)`,
    /// `Vesper.Comparison.cmp` calls `a.CompareTo(b)`, `Vesper.Core.hashOf` calls
    /// `x.GetHashCode()`, `use` calls `obj[Symbol.dispose]()` — so a capability impl's
    /// slot IS its member body; only the `key` (a plain `Named`, a `Symbol.dispose`
    /// member-access, or a `Symbol.for("vesper.X")` registry call) tells them apart.
    let emitPlainMethod
        (buildExpr: WalkCtx -> Frozen.TExpr -> JsExpr)
        (ctx: WalkCtx)
        (key: JsMethodKey)
        (m: Frozen.TTypeMember)
        : JsClassMethod =
        {
            Key = key
            Params = [ for (pk, _) in m.Params -> identName ctx.Source pk ]
            Body = thisBinding ctx m @ [ JsStatement.Return(buildExpr ctx m.Body) ]
            Generator = false
        }

    /// An interface-impl / `Object`-override member (`Equals`/`CompareTo`/`GetHashCode`
    /// or a user interface method) as a name-keyed attached method.
    let emitAttachedMethod
        (buildExpr: WalkCtx -> Frozen.TExpr -> JsExpr)
        (ctx: WalkCtx)
        (m: Frozen.TTypeMember)
        : JsClassMethod =
        emitPlainMethod buildExpr ctx (JsMethodKey.Named m.Name) m

    /// Emit an enumerable-capability `GetEnumerator` impl as a native
    /// `*[Symbol.iterator]()` GENERATOR — the JS realisation of "implement `seq<'T>`
    /// ⇒ emit the target iteration protocol". The generator binds the enumerator the
    /// impl returns (`const e = <GetEnumerator body>`, with `this` re-bound via
    /// `thisBinding`), then drives the F# enumerator protocol (`MoveNext(): bool` +
    /// `Current`) into JS's: `while (e.MoveNext()) yield e.Current()`. `yield` makes the
    /// protocol adaptation free — it auto-produces the `{ value, done }` iterator
    /// results, so no object literal is built. The enumerator is itself an
    /// `IEnumerator<'T>` implementer, so its `MoveNext`/`Current` are ATTACHED JS methods
    /// (`e.MoveNext()` / `e.Current()`), dispatched directly on the runtime object — not
    /// the free receiver-first form a regular member call lowers to.
    let emitIteratorMethod
        (buildExpr: WalkCtx -> Frozen.TExpr -> JsExpr)
        (ctx: WalkCtx)
        (m: Frozen.TTypeMember)
        : JsClassMethod =
        // A fresh enumerator binder, keyed on the body token so it can't shadow a
        // source binder the `GetEnumerator` body itself introduces.
        let eName = "_e" + string (TastWalk.exprTok m.Body).StartIndex
        let eIdent = JsExpr.Identifier(eName, ValueNone)

        // Direct attached calls on the enumerator object: `e.MoveNext()` / `e.Current()`
        // (its `IEnumerator<'T>` impl members are attached methods, the property `Current`
        // emitted as a zero-arg method).
        let attachedCall (name: string) =
            JsExpr.Call(JsExpr.Member(eIdent, JsExpr.Identifier(name, ValueNone), false, ValueNone), [], ValueNone)

        let body =
            thisBinding ctx m
            @ [
                JsStatement.Const(eName, buildExpr ctx m.Body)
                JsStatement.While(attachedCall "MoveNext", [ JsStatement.Yield(attachedCall "Current") ])
            ]

        {
            // `Symbol.iterator` — a native well-known symbol, distinct from a registry
            // `Symbol.for("…")` call (the eq/comp/hash sub-slice).
            Key = JsMethodKey.Computed(nativeSymbol "iterator")
            Params = []
            Body = body
            Generator = true
        }

    /// Emit a disposable-capability `Dispose` impl as a NATIVE well-known
    /// `[Symbol.dispose]()` method — the JS analogue of the CLR `IDisposable::Dispose`
    /// slot, driven by `use`'s `obj[Symbol.dispose]()` lowering. A plain
    /// (non-generator) method keyed by the `Symbol.dispose` member-access node, NOT a
    /// `Symbol.for("…")` registry call.
    let emitDisposeMethod
        (buildExpr: WalkCtx -> Frozen.TExpr -> JsExpr)
        (ctx: WalkCtx)
        (m: Frozen.TTypeMember)
        : JsClassMethod =
        emitPlainMethod buildExpr ctx (JsMethodKey.Computed symbolDispose) m

    /// Emit an eq/comp/hash capability impl as a COMPUTED-KEY method
    /// `[Symbol.for("vesper.X")](params) { … }` — the registry-symbol dispatch slot the
    /// `Vesper.Core` / `Vesper.Comparison` runtimes look for (`a[Symbol.for("vesper.equality")](b)`,
    /// `a[Symbol.for("vesper.comparison")](b)`, `x[Symbol.for("vesper.hash")]()`). A registry
    /// symbol is present ONLY on a type that opted into the protocol, so it can't collide with a
    /// foreign object carrying an unrelated `.Equals`/`.CompareTo`/`.GetHashCode`. `registryName`
    /// is the registry key (`vesper.equality` etc.).
    let emitProtocolMethod
        (buildExpr: WalkCtx -> Frozen.TExpr -> JsExpr)
        (ctx: WalkCtx)
        (registryName: string)
        (m: Frozen.TTypeMember)
        : JsClassMethod =
        emitPlainMethod buildExpr ctx (JsMethodKey.Computed(registrySymbol registryName)) m

    /// Emit a record/union member as a free, curried, receiver-first top-level function:
    /// `member this.Foo a b` → `<Type>__Foo = (this$) => (a) => (b) => <body>`.
    /// Static members drop the receiver; a static property emits as a plain value binding.
    let emitMemberFn
        (buildExpr: WalkCtx -> Frozen.TExpr -> JsExpr)
        (ctx: WalkCtx)
        (typeName: string)
        (m: Frozen.TTypeMember)
        : JsStatement =
        let isProperty = (m.Kind = TMemberKind.Property)
        let name = JsExternalMembers.mangledName typeName m.IsStatic isProperty m.Name

        let receiverNames =
            if m.IsStatic then
                []
            else
                match m.ThisKey with
                | ValueSome k -> [ identName ctx.Source k ]
                | ValueNone -> [ "this$" ]

        let paramNames = [ for (pk, _) in m.Params -> identName ctx.Source pk ]
        let allNames = receiverNames @ paramNames
        let body = buildExpr ctx m.Body

        let init =
            match allNames with
            | [] -> body
            | _ -> nestUnaryArrows ValueNone allNames (JsFnBody.Expr body)

        // A member function is an arrow value, never reassigned → always `const`.
        topLevelBinding ctx false name init

    /// Emit every class-method form of a partitioned member set, in the one place the
    /// partition→emitter mapping lives: attached dispatch slots, `[Symbol.iterator]`
    /// generators, `[Symbol.for("vesper.X")]` protocol methods, and the
    /// `[Symbol.dispose]()` method. (`Free` members are emitted elsewhere as free
    /// functions.) Shared by the pending-class and pending-union emission.
    let emitCapabilityMethods
        (buildExpr: WalkCtx -> Frozen.TExpr -> JsExpr)
        (ctx: WalkCtx)
        (p: PartitionedMembers)
        : JsClassMethod list =
        [
            for m in p.Attached -> emitAttachedMethod buildExpr ctx m
            for m in p.Iterators -> emitIteratorMethod buildExpr ctx m
            for (sym, m) in p.Protocols -> emitProtocolMethod buildExpr ctx sym m
            for m in p.Disposers -> emitDisposeMethod buildExpr ctx m
        ]
