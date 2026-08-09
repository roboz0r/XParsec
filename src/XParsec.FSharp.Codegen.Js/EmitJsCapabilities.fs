namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// The JS language-capability protocol. A CLR anchor is a TYPE (`System.IDisposable`) and stays
/// in source; a JS anchor is a dispatch SYMBOL, so it lives here in the backend.
module EmitJsCapabilities =

    [<RequireQualifiedAccess>]
    type JsCapability =
        /// `seq<'T>`.
        | Iteration
        /// `enumerator<'T>` — the split `MoveNext`/`Current` cursor.
        | Cursor
        /// `disposable`.
        | Disposal
        /// `equatable<'T>`.
        | Equality
        /// `comparable<'T>`.
        | Comparison

    /// Capability identity is resolved through the provider, so a provider-less compile
    /// recognises nothing: every key is `ValueNone`.
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
    /// `MoveNext`/`Current` needs. No front-end symbol resolves to it, so codegen names its home.
    let private enumeratorOfRef: JsValueRef =
        {
            Key = ValueSome(SymbolKeyOps.valueKey (SymbolKeyOps.inNamespace "Vesper.Collections") "enumeratorOf")
            Home = ValueSome(JsHome.ofAssembly "Vesper.Core")
            Form = ImportForm.Named
        }

    // ---- The CONSUMER table --------------------------------------------------

    /// Rows are name-blind: every routed member takes `unit`, so an applied call and a bare
    /// property read emit the same node.
    let tryCapabilitySlot
        (caps: RuntimeNames.CapabilityIds)
        (imports: JsImports)
        (declKey: TypeKey)
        (memberName: string)
        : (JsExpr -> JsLoc voption -> JsExpr) voption =
        match capabilityOf caps declKey with
        | ValueSome JsCapability.Iteration ->
            ValueSome(fun objArg loc ->
                let adapter =
                    JsExpr.Identifier(JsImports.addRef imports "enumeratorOf" enumeratorOfRef, ValueNone)

                JsExpr.Call(adapter, [ objArg ], loc)
            )
        | ValueSome JsCapability.Cursor ->
            ValueSome(fun objArg loc -> JsExternalMembers.attachedCall objArg memberName [] loc)
        | ValueSome JsCapability.Disposal -> ValueSome disposeSlotCall
        // `=` reaches equality/comparison through structural equality, never a member call, so
        // these keep the ordinary external-member lowering.
        | ValueSome JsCapability.Equality
        | ValueSome JsCapability.Comparison
        | ValueNone -> ValueNone

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
            tryCapabilitySlot caps imports (JsExternalMembers.declKey em.Key) em.MemberName
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
        (appArgs: (TastAccessor.ExprId * FrozenType * Anchor) list)
        (loc: JsLoc voption)
        : JsExpr voption =
        match fn, appArgs with
        | JsExternalMembers.InstanceExternalMember(objArg, em), [ (arg, _, _) ] when
            em.Storage = MemberStorage.Method
            && TastAccessor.exprKind arg = ExprShape.Const
            && TastAccessor.exprConstValue arg = TConstValue.Unit
            ->
            tryCapabilitySlot caps imports (JsExternalMembers.declKey em.Key) em.MemberName
            |> ValueOption.map (fun emit -> emit (build objArg) loc)
        | _ -> ValueNone
