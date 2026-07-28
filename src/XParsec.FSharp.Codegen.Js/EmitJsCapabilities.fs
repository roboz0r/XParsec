namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// The JS language-capability protocol
///
/// A capability is a language-level interface (`seq<'T>`, `enumerator<'T>`, `disposable`,
/// `equatable<'T>`, `comparable<'T>`) whose identity is resolved through the provider
/// (`RuntimeNames.CapabilityIds`) rather than hardcoded. The CLR/JS asymmetry is that a CLR
/// anchor is a TYPE (`System.IDisposable`) so it stays in source, whereas a JS anchor is a
/// SYMBOL (`Symbol.iterator`, a dispatch key) — so it lives here, in the backend.
///
/// `capabilityOf` classifies a key ONCE, and the two tables below route off that one verdict:
///
///   * the IMPLEMENTER table (`EmitJsTypes.partitionClassMembers`) — the slot a type that
///     *implements* a capability emits;
///   * the CONSUMER table (`tryCapabilitySlot`) — how a *call to* a capability member lowers
///     at its use site, which is what makes the manual pull protocol
///     (`let e = src.GetEnumerator()` / `while e.MoveNext() do … e.Current`) lower on JS; the
///     CLR-idiomatic `for … in` is sugar for exactly that loop.
///
/// The two must agree on the emitted shape, and they do, by construction: an authored
/// `interface enumerator<'T>` lands its `MoveNext`/`Current` in the implementer's ATTACHED
/// bucket, and the consumer calls exactly those attached slots.
///
///   capability | JS anchor                       | implements as              | a call lowers to
///   -----------|---------------------------------|----------------------------|-------------------
///   Iteration  | Symbol.iterator                 | *[…]() GENERATOR           | enumeratorOf(src)
///   Cursor     | (none — plain named methods)    | attached MoveNext/Current  | e.MoveNext()
///   Disposal   | Symbol.dispose                  | […]() plain method         | e[Symbol.dispose]()
///   Equality   | Symbol.for("vesper.equality")   | […]() plain method         | (no use site)
///   Comparison | Symbol.for("vesper.comparison") | […]() plain method         | (no use site)
///
/// `Iteration` is the one consumer row that cannot dispatch on its receiver: a source's only
/// enumerable surface on JS *is* `Symbol.iterator` (a Vesper `seq<'T>` impl emits a
/// `*[Symbol.iterator]()` generator; a native array / TS iterable has nothing else), so no
/// `GetEnumerator` method exists on either to call. It routes to the `Vesper.Core.mjs`
/// `enumeratorOf` adapter, which holds the last `next()` result across the `MoveNext`/`Current`
/// split — the state a stateless `(# … #)` template cannot carry, and the whole reason the
/// protocol needs a runtime at all. The adapter's shape is exactly an authored enumerator's, so
/// a consumer cannot tell the two apart.
///
/// `Equality`/`Comparison` have no consumer row: `=` reaches those protocols through
/// `structuralEquals`, never through a member call. `hashing` is an `override GetHashCode`, not
/// an interface impl, so it has no capability identity — `partitionClassMembers` routes it to
/// `hashRegistryKey` on its own.
module EmitJsCapabilities =

    /// A JS capability protocol — the classification both tables route on.
    [<RequireQualifiedAccess>]
    type JsCapability =
        /// `seq<'T>` — the enumerable source.
        | Iteration
        /// `enumerator<'T>` — the split `MoveNext`/`Current` cursor (which inherits `disposable`).
        | Cursor
        /// `disposable`.
        | Disposal
        /// `equatable<'T>`.
        | Equality
        /// `comparable<'T>`.
        | Comparison

    /// Classify an interface's head `TypeKey` against the resolved capability identities.
    /// The ONE place a key becomes a capability; every routing decision reads this verdict, so
    /// the implementer and consumer tables cannot disagree about what a key is. `ValueNone` for
    /// an ordinary interface — and for every key under a provider-less compile
    /// (`CapabilityIds.none` names nothing).
    let capabilityOf (caps: RuntimeNames.CapabilityIds) (key: TypeKey) : JsCapability voption =
        if RuntimeNames.matchesKey caps.Enumerable key then
            ValueSome JsCapability.Iteration
        elif RuntimeNames.matchesKey caps.Enumerator key then
            ValueSome JsCapability.Cursor
        elif RuntimeNames.matchesKey caps.Disposable key then
            ValueSome JsCapability.Disposal
        elif RuntimeNames.matchesKey caps.Equatable key then
            ValueSome JsCapability.Equality
        elif RuntimeNames.matchesKey caps.Comparable key then
            ValueSome JsCapability.Comparison
        else
            ValueNone

    // ---- The JS anchors ------------------------------------------------------

    /// `Symbol.<name>` — a WELL-KNOWN symbol (`Symbol.iterator`, `Symbol.dispose`): a
    /// property of the `Symbol` global, NOT a registry (`Symbol.for`) call.
    let nativeSymbol (name: string) : JsExpr =
        JsExpr.Member(JsExpr.Identifier("Symbol", ValueNone), JsExpr.Identifier(name, ValueNone), false, ValueNone)

    /// `Symbol.for("<key>")` — a REGISTRY symbol: the eq/comp/hash protocols have no native
    /// JS dispatch, so they ride a process-wide `Symbol.for("vesper.X")` the Vesper runtimes
    /// look up — collision-proof against a foreign object's same-named string method.
    let registrySymbol (key: string) : JsExpr =
        JsExpr.Call(nativeSymbol "for", [ JsExpr.Literal(JsLiteral.String key, ValueNone) ], ValueNone)

    [<Literal>]
    let equalityRegistryKey = "vesper.equality"

    [<Literal>]
    let comparisonRegistryKey = "vesper.comparison"

    [<Literal>]
    let hashRegistryKey = "vesper.hash"

    /// The disposal capability's dispatch slot. Spelled ONCE, so the impl side
    /// (`EmitJsMembers.emitDisposeMethod`'s method key), the `use` lowering
    /// (`EmitJs.disposeStmts`), and the consumer table below can only ever name the same slot.
    let symbolDispose: JsExpr = nativeSymbol "dispose"

    /// `recv[Symbol.dispose]()` — a call on that slot (a COMPUTED member access, no args).
    let disposeSlotCall (recv: JsExpr) (loc: JsLoc voption) : JsExpr =
        JsExpr.Call(JsExpr.Member(recv, symbolDispose, true, loc), [], loc)

    /// The runtime entry behind `seq<'T>.GetEnumerator()`: the `Vesper.Core.mjs` adapter that
    /// wraps a source's native `Symbol.iterator` in the split `MoveNext`/`Current` cursor.
    /// Synthesised by the backend (like `EmitJsContext.structuralFormatRef`) — the protocol is
    /// a codegen concern, so no front-end symbol resolves to it, and no provider shape carries
    /// its home either: codegen names the key AND the module.
    /// `Vesper.Collections` is a NAMESPACE (`capabilities.fsi`) and the adapter a bare
    /// export of `Vesper.Core.mjs`, so the binding is held by the namespace itself.
    let private enumeratorOfRef: JsValueRef =
        {
            Key = ValueSome(SymbolKeyOps.valueKey (SymbolKeyOps.inNamespace "Vesper.Collections") "enumeratorOf")
            Home = Origin.InAssembly(AssemblyName "Vesper.Core")
            Form = ImportForm.Named
        }

    // ---- The CONSUMER table --------------------------------------------------

    /// How a call to `memberName` on a capability member lowers on its (already-emitted)
    /// receiver — `ValueNone` for a key that is not a routed capability member, which is the
    /// only "is this routed?" test there is: the guard and the table are one lookup, so they
    /// cannot drift apart.
    ///
    /// Each routed row is name-blind — the capability alone picks the lowering — because every
    /// member of the iteration/disposal cluster takes `unit` and each capability contributes a
    /// single member (the cursor's two share one form). The access is therefore COMPLETE at
    /// zero arguments: an applied call and a bare property read emit the same node (Vesper
    /// compiles an interface property to a zero-arg method, so reading `e.Current` IS calling
    /// `e.Current()`).
    let tryCapabilitySlot
        (caps: RuntimeNames.CapabilityIds)
        (imports: JsImports)
        (declKey: TypeKey)
        (memberName: string)
        : (JsExpr -> JsLoc voption -> JsExpr) voption =
        match capabilityOf caps declKey with
        | ValueSome JsCapability.Iteration ->
            ValueSome(fun recv loc ->
                let adapter =
                    JsExpr.Identifier(JsImports.addRef imports "enumeratorOf" enumeratorOfRef, ValueNone)

                JsExpr.Call(adapter, [ recv ], loc)
            )
        | ValueSome JsCapability.Cursor ->
            ValueSome(fun recv loc -> JsExternalMembers.attachedCall recv memberName [] loc)
        | ValueSome JsCapability.Disposal -> ValueSome disposeSlotCall
        // No use site: `=` reaches equality/comparison through `structuralEquals`, so an
        // `a.Equals(b)` call on such a receiver keeps the ordinary external-member lowering.
        | ValueSome JsCapability.Equality
        | ValueSome JsCapability.Comparison
        | ValueNone -> ValueNone

    /// An `ExternalMember` node that is a capability-member VALUE read (`e.Current`), reached
    /// WITHOUT an applying spine → its receiver and the slot's emitter. Vesper compiles an
    /// interface property to a zero-arg method, so the read IS the call: `e.Current` emits
    /// `e.Current()`.
    ///
    /// A capability METHOD reaching a value position is an un-applied reference (`let f =
    /// e.MoveNext`), which has no eta-wrap lowering — deliberately NOT matched, so it falls
    /// through to the ordinary external-member lowering and fails loudly there rather than
    /// emitting a mangled import of a runtime export that does not exist.
    [<return: Struct>]
    let (|CapabilityRead|_|)
        (caps: RuntimeNames.CapabilityIds)
        (imports: JsImports)
        (e: TastAccessor.ExprId)
        : struct (TastAccessor.ExprId * (JsExpr -> JsLoc voption -> JsExpr)) voption =
        match e with
        | JsExternalMembers.InstanceExternalMember(recv, em) when em.Storage.IsValueMember ->
            tryCapabilitySlot caps imports (JsExternalMembers.declKey em.Key) em.MemberName
            |> ValueOption.map (fun emit -> struct (recv, emit))
        | _ -> ValueNone

    /// The APPLIED form — `src.GetEnumerator()` / `e.MoveNext()` / `e.Dispose()`: an `App`
    /// whose head is a capability member and whose spine is the lone `unit` argument, folded
    /// into the zero-arg access. The capability analogue of `JsExternalMembers.tryAttachedCall`,
    /// and dispatched beside it in `EmitJs`'s `App` arm. `ValueNone` for every other head.
    ///
    /// The `unit` spine element is matched, not assumed: a capability member that ever takes a
    /// real argument would silently lose it here, so it falls through to the ordinary
    /// external-member lowering (which fails loudly) instead.
    let tryCapabilityCall
        (caps: RuntimeNames.CapabilityIds)
        (imports: JsImports)
        (build: TastAccessor.ExprId -> JsExpr)
        (head: TastAccessor.ExprId)
        (spine: (TastAccessor.ExprId * FrozenType * Anchor) list)
        (loc: JsLoc voption)
        : JsExpr voption =
        match head, spine with
        | JsExternalMembers.InstanceExternalMember(recv, em), [ (arg, _, _) ] when
            em.Storage = MemberStorage.Method
            && TastAccessor.exprKind arg = ExprShape.Const
            && TastAccessor.exprConstValue arg = TConstValue.Unit
            ->
            tryCapabilitySlot caps imports (JsExternalMembers.declKey em.Key) em.MemberName
            |> ValueOption.map (fun emit -> emit (build recv) loc)
        | _ -> ValueNone
