namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// The JS language-capability protocol. A CLR anchor is a TYPE (`System.IDisposable`) and stays
/// in source; a JS anchor is a dispatch SYMBOL, so it lives here in the backend.
module EmitJsCapabilities =

    /// The class dispatch slot a member's body is emitted into.
    [<RequireQualifiedAccess>]
    type MemberSlot =
        /// `M(a) { … }`, called as `x.M(a)`.
        | Named
        /// `*[Symbol.iterator]() { … }`, driving the body's `MoveNext`/`Current`.
        | Iterator
        /// `[Symbol.for("vesper.equality")](b) { … }` and its comparison/hash twins.
        | Protocol of registryKey: string
        /// `[Symbol.dispose]() { … }`, which `use` calls.
        | Dispose

    /// How a call through a capability member lowers. `imports` and the member name are the
    /// anchor's own inputs; the object argument and location come from the call site.
    type CapabilityLowering = JsImports -> string -> JsExpr -> JsLoc voption -> JsExpr

    /// One language capability: the front-end interface it is anchored to, and the slot an IMPL
    /// of it takes. A CALL's lowering follows from the slot, so the two cannot disagree.
    [<NoEquality; NoComparison>]
    type JsCapability =
        {
            /// Reads this capability's identity out of the provider-resolved set.
            Anchor: RuntimeNames.CapabilityIds -> RuntimeNames.CapabilityIdentity voption
            Slot: MemberSlot
        }

    // ---- The JS anchors ------------------------------------------------------

    /// `Symbol.<name>` — a WELL-KNOWN symbol: a property of the `Symbol` global.
    let nativeSymbol (name: string) : JsExpr =
        JsExpr.Member(JsExpr.Identifier("Symbol", ValueNone), JsExpr.Identifier(name, ValueNone), false, ValueNone)

    /// `Symbol.for("<key>")` — a REGISTRY symbol. The eq/comp/hash protocols have no native JS
    /// dispatch, and a registry symbol is collision-proof against a same-named string method.
    let registrySymbol (key: string) : JsExpr =
        JsExpr.Call(nativeSymbol "for", [ JsExpr.Literal(JsLiteral.String key, ValueNone) ], ValueNone)

    [<Literal>]
    let equalityRegistryKey = "vesper.equality"

    [<Literal>]
    let comparisonRegistryKey = "vesper.comparison"

    [<Literal>]
    let hashRegistryKey = "vesper.hash"

    let symbolDispose: JsExpr = nativeSymbol "dispose"

    /// `objArg[Symbol.dispose]()` — a COMPUTED member access, no args.
    let disposeSlotCall (objArg: JsExpr) (loc: JsLoc voption) : JsExpr =
        JsExpr.Call(JsExpr.Member(objArg, symbolDispose, true, loc), [], loc)

    /// The runtime entry behind `seq<'T>.GetEnumerator()`: a JS source's only enumerable surface
    /// is `Symbol.iterator`, so nothing exists to call and this adapter holds the state the split
    /// `MoveNext`/`Current` needs. No front-end symbol resolves to it, so codegen hardcodes its home.
    let private enumeratorOfRef: JsValueRef =
        {
            Key = SymbolKeyOps.bindingKeyOf (SymbolKeyOps.inNamespace "Vesper.Collections") "enumeratorOf"
            Home = ValueSome(JsHome.ofAssembly "Vesper.Core")
            Form = ImportForm.Named
        }

    // ---- The capability table ------------------------------------------------

    let private capabilities: JsCapability[] =
        [|
            // `seq<'T>`
            {
                Anchor = fun caps -> caps.Enumerable
                Slot = MemberSlot.Iterator
            }
            // `enumerator<'T>`, the one capability still on a NAME slot: its dispatch is the
            // plain pair `e.MoveNext()` / `e.Current()`, not a symbol method.
            {
                Anchor = fun caps -> caps.Enumerator
                Slot = MemberSlot.Named
            }
            // `disposable`
            {
                Anchor = fun caps -> caps.Disposable
                Slot = MemberSlot.Dispose
            }
            // `equatable<'T>` and `comparable<'T>`
            {
                Anchor = fun caps -> caps.Equatable
                Slot = MemberSlot.Protocol equalityRegistryKey
            }
            {
                Anchor = fun caps -> caps.Comparable
                Slot = MemberSlot.Protocol comparisonRegistryKey
            }
        |]

    /// Capability identity is resolved through the provider, so a provider-less compile
    /// recognises nothing: every anchor is `ValueNone` and no row matches.
    let capabilityOf (caps: RuntimeNames.CapabilityIds) (key: TypeKey) : JsCapability voption =
        let mutable hit = ValueNone
        let mutable i = 0

        while hit.IsNone && i < capabilities.Length do
            if RuntimeNames.matchesKey (capabilities.[i].Anchor caps) key then
                hit <- ValueSome capabilities.[i]

            i <- i + 1

        hit

    // ---- The CONSUMER side ---------------------------------------------------

    /// How a call REACHES `slot`, which is the same slot an impl of it was emitted into.
    /// `ValueNone` → the ordinary external-member lowering: `=` reaches equality and
    /// comparison through structural equality, never a member call.
    let private loweringFor (slot: MemberSlot) : CapabilityLowering voption =
        match slot with
        | MemberSlot.Named ->
            ValueSome(fun _ memberName objArg loc -> JsExternalMembers.attachedCall objArg memberName [] loc)
        | MemberSlot.Iterator ->
            ValueSome(fun imports _ objArg loc ->
                let adapter = JsExpr.Identifier(JsImports.addRef imports enumeratorOfRef, ValueNone)

                JsExpr.Call(adapter, [ objArg ], loc)
            )
        | MemberSlot.Dispose -> ValueSome(fun _ _ objArg loc -> disposeSlotCall objArg loc)
        | MemberSlot.Protocol _ -> ValueNone

    /// Name-blind: every routed member takes `unit`, so an applied call and a bare property
    /// read emit the same node.
    let tryCapabilityLowering
        (caps: RuntimeNames.CapabilityIds)
        (imports: JsImports)
        (declKey: TypeKey)
        (memberName: string)
        : (JsExpr -> JsLoc voption -> JsExpr) voption =
        capabilityOf caps declKey
        |> ValueOption.bind (fun c -> loweringFor c.Slot)
        |> ValueOption.map (fun lower -> lower imports memberName)

    /// An un-applied capability-member VALUE read: an interface property compiles to a zero-arg
    /// method, so `e.Current` IS the call `e.Current()`.
    [<return: Struct>]
    let (|CapabilityRead|_|)
        (caps: RuntimeNames.CapabilityIds)
        (imports: JsImports)
        (e: TastAccessor.ExprId)
        : struct (TastAccessor.ExprId * (JsExpr -> JsLoc voption -> JsExpr)) voption =
        match e with
        | JsExternalMembers.InstanceExternalMember(objArg, em) when em.Storage.IsValueMember ->
            tryCapabilityLowering caps imports (JsExternalMembers.declKey em.Key) em.MemberName
            |> ValueOption.map (fun emit -> struct (objArg, emit))
        | _ -> ValueNone

    /// `e.MoveNext()`, the APPLIED form, folds into the same zero-arg access. The `unit`
    /// argument is matched, not assumed: a capability member taking a real argument keeps the
    /// ordinary lowering, which passes it, rather than being folded down and losing it.
    let tryCapabilityCall
        (caps: RuntimeNames.CapabilityIds)
        (imports: JsImports)
        (build: TastAccessor.ExprId -> JsExpr)
        (fn: TastAccessor.ExprId)
        (appArgs: TastAccessor.AppliedArg list)
        (loc: JsLoc voption)
        : JsExpr voption =
        match fn, appArgs with
        | JsExternalMembers.InstanceExternalMember(objArg, em), [ a ] when
            em.Storage = MemberStorage.Method
            && TastAccessor.exprKind a.Arg = ExprShape.Const
            && TastAccessor.exprConstValue a.Arg = TConstValue.Unit
            ->
            tryCapabilityLowering caps imports (JsExternalMembers.declKey em.Key) em.MemberName
            |> ValueOption.map (fun emit -> emit (build objArg) loc)
        | _ -> ValueNone
