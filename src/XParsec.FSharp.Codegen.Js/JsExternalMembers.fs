namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open JsEmitHelpers

/// The EXTERNAL-member lowering cluster — everything the walker keys off the
/// provider's external world: the declaring type's `ExternalClassFlags`, the `exn`
/// repr climb, member-name mangling, the attached-member arity contract, and the
/// three call shapes an external member lowers to (native attached access, the
/// erased-grouping bare export, the mangled receiver-first import).
///
/// As in `JsFlatFns` (the precedent), each function that must lower a
/// sub-expression takes a `build: TastAccessor.ExprId -> JsExpr` callback (the
/// `EmitJs.buildExpr ctx` closure) — keeping this cluster out of the `buildExpr`
/// mutual-recursion group is what lets it live in its own file and keeps `EmitJs`
/// legible: `EmitJs`'s `ExternalMember` / `App` arms shrink to a dispatch over
/// (`classFlagsOf` × receiver); the lowering bodies live here.
module JsExternalMembers =

    /// The declaring type's `TypeKey` from a member-call node's `key`. A member node's
    /// key IS a `MemberKey` — the narrowing is the IR seam's, stated once in
    /// `SymbolKeyOps.asMemberKey`, not a per-site guess. (Handing the MEMBER key back as
    /// if it were the DECLARING key, as a lenient fallback here would, sends every
    /// downstream reader — `LocalInterfaces`, `classFlagsOf`, the import oracle — looking
    /// for a type under a member's identity and silently missing.)
    let declKey (key: SymbolKey) : TypeKey =
        SymbolKeyOps.declTypeKeyOf "EmitJs: member node" key

    /// An `ExternalMember` node reached with an INSTANCE receiver → that receiver and the
    /// member's payload view. `ValueNone` for a static member (`ValueNone` receiver) or any
    /// non-`ExternalMember` node. The shared prologue of every instance-external-member
    /// recognizer (`tryAttachedCall`, `EmitJsCapabilities.CapabilityRead`/`tryCapabilityCall`);
    /// each site adds its own `Storage` guard (a value-member read vs. a `Method` call).
    [<return: Struct>]
    let (|InstanceExternalMember|_|)
        (e: TastAccessor.ExprId)
        : struct (TastAccessor.ExprId * TastAccessor.ExternalMemberView) voption =
        match TastAccessor.exprKind e with
        | ExprShape.ExternalMember ->
            let em = TastAccessor.exprExternalMember e

            match em.Receiver with
            | ValueSome recv -> ValueSome(struct (recv, em))
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Instance method → `<Type>__<member>`; instance property getter →
    /// `<Type>__get_<Prop>`; static member → `<Type>_<member>`.
    let mangledName (typeName: string) (isStatic: bool) (isProperty: bool) (memberName: string) : string =
        if isStatic then typeName + "_" + memberName
        elif isProperty then typeName + "__get_" + memberName
        else typeName + "__" + memberName

    /// THE `key -> home` oracle for an external type — what names the module its exports
    /// are imported from. A `SymbolKey` is a nominal identity and carries no home, so the
    /// only answer is the one the provider stamped on the type's RESOLVED SHAPE
    /// (`SymbolOrigin.Home`). Consulted strictly PAST the local/external verdict (which
    /// the emitted-type tables make, not the key): a type that is external but whose shape
    /// names no home cannot be imported at all, so it fails loudly rather than emitting a
    /// dangling reference.
    let homeOf (provider: IExternalSymbolProvider) (key: SymbolKey) (what: string) : Origin =
        // A shape whose home is unstamped names no importable module — the local/external
        // verdict is already past, so an unstamped home here is a real failure, not a
        // fall-back.
        let home =
            match provider.TryLookupType key with
            | ValueSome(ExternalTypeShape.Union(_, _, _, o))
            | ValueSome(ExternalTypeShape.Record(_, _, o))
            | ValueSome(ExternalTypeShape.Enum(_, o)) -> o.Home
            | ValueSome(ExternalTypeShape.Class shape) -> shape.Origin.Home
            | _ -> Origin.Unstamped

        match home.AssemblyOption with
        | ValueSome _ -> home
        | ValueNone -> failwithf "EmitJs: %s has no resolvable home assembly (key %A)" what key

    /// The declaring type's `ExternalClassFlags`, resolved through the provider
    /// (`TryLookupType` → `Class` shape → `Flags`) in ONE lookup — the
    /// `ExternalMember` dispatch reads `Erased` and `AttachMembers` off the same
    /// result. `ValueNone` when the key names no `Class` shape; every flag then reads
    /// as `false`, so the normal mangled static-member path is untouched. The flags
    /// the dispatch consumes:
    ///
    ///   * `Erased` — a SYNTHETIC erased grouping type (Tier 2 item 9b): the TS
    ///     provider groups a module's overloaded free functions as static members
    ///     of one F#-visible type purely so the front end can resolve the overload
    ///     set. The type does not exist at runtime — a call to one of its members
    ///     must ERASE to the bare module export (`Util.format(x)` → `format(x)`,
    ///     see `erasedGroupingRef`), never the mangled `Util_format` an ordinary
    ///     external static member would import. `false` for every real
    ///     (metadata/contract) class.
    ///   * `AttachMembers` — the type carries its instance members as NATIVE
    ///     object methods (`receiver.member(args)` / property reads) rather than
    ///     the receiver-first free-fn imports Vesper's own runtimes emit; stamped
    ///     by the provider on a real manifest Interface/Class.
    /// The declaring type's resolved `Class` shape — the ONE provider lookup the three
    /// predicates below project, so they cannot disagree about what the key names.
    /// `ValueNone` when the key names no `Class` shape.
    let private classShapeOf (provider: IExternalSymbolProvider) (declKey: TypeKey) : ExternalClassShape voption =
        match provider.TryLookupType(SymbolKey.Type declKey) with
        | ValueSome(ExternalTypeShape.Class shape) -> ValueSome shape
        | _ -> ValueNone

    let classFlagsOf (provider: IExternalSymbolProvider) (declKey: TypeKey) : ExternalClassFlags voption =
        classShapeOf provider declKey |> ValueOption.map (fun shape -> shape.Flags)

    /// Whether `declKey` names an INTERFACE, as the provider resolved it.
    let isInterface (provider: IExternalSymbolProvider) (declKey: TypeKey) : bool =
        match classShapeOf provider declKey with
        | ValueSome shape -> shape.IsInterface
        | ValueNone -> false

    /// Whether an instance member of `declKey` dispatches as a NATIVE attached method
    /// (`recv.member(args)`) rather than a receiver-first free-function import.
    ///
    /// Two ways to be one, and they are the same rule: a manifest object whose members ARE
    /// prototype methods (`MemberLowering.AttachedNative`), and ANY interface. An interface
    /// has no runtime existence on JS — no module, no export, no free-function form — so
    /// its implementations are attached methods on the implementing class, and WHERE it was
    /// declared (this file, a sibling file, another library) cannot change that.
    let attachesMembers (provider: IExternalSymbolProvider) (declKey: TypeKey) : bool =
        match classShapeOf provider declKey with
        | ValueSome shape -> shape.IsInterface || shape.Flags.MemberLowering = MemberLowering.AttachedNative
        | ValueNone -> false

    /// `Vesper.Fun` at any of its arities — the curried `Fun<'A,'B>` and the flat
    /// overloads `Fun<'A,'B,'C>` … `Fun<..,'E>`.
    ///
    /// THE carve-out from the attach-members rule above: a `Fun` value is an ECMAScript
    /// arrow function at run time (that is what a Vesper lambda emits as), so `f.Invoke(a)`
    /// is APPLICATION — `f(a)` — and not a member on an object that has none. The flat
    /// overloads dispatch a saturated call in ONE call (`f(a, b)`), the curried one stays
    /// curried (`f(a)(b)`); the member's own arity says which.
    let isFunInterface (declKey: TypeKey) : bool =
        [ 2..5 ] |> List.exists (fun arity -> RuntimeNames.vesperFunKey arity = declKey)

    /// Walk a type's `inherit` chain up to the `exn` intrinsic root and resolve its
    /// `(# "Error" #)` repr to the native runtime class name (`Error` on JS). Returns
    /// `ValueNone` when the type is not an `exn` subtype or the repr names no provider class.
    let exnReprOf (provider: IExternalSymbolProvider) (ty: FrozenType) : string voption =
        let shapeOf (ft: FrozenType) : ExternalTypeShape voption =
            match ft with
            | FTClass(key, _)
            | FTUnion(key, _)
            | FTRecord(key, _) -> provider.TryLookupType(SymbolKey.Type key)
            // An intrinsic's canon key is a nominal identity like any other — ask the store
            // by the KEY, exactly as the nominal arms above do.
            | FTConst(key, _) -> provider.TryLookupType key
            | _ -> ValueNone

        // Depth cap backstops a malformed cyclic `inherit`; each hop is a strict
        // ancestor so the chain is finite in practice.
        let rec climb (depth: int) (ft: FrozenType) : string voption =
            if depth > 16 then
                ValueNone
            else
                match shapeOf ft with
                | ValueSome(ExternalTypeShape.Intrinsic {
                                                            Id = {
                                                                     Platform = IntrinsicPlatform.Repr platform
                                                                 }
                                                        }) ->
                    // Read the PLATFORM repr, not `canon` — `canon` is the unifier's
                    // identity key (`"System.Exception"`) and has no JS class analogue.
                    match ExternalSymbols.tryRuntimeType provider platform with
                    | ValueSome(ExternalTypeShape.Class _) -> ValueSome platform
                    | _ -> ValueNone
                | ValueSome(ExternalTypeShape.Class shape) ->
                    match shape.FrozenBaseType with
                    | ValueSome b -> climb (depth + 1) b
                    | ValueNone -> ValueNone
                | _ -> ValueNone

        climb 0 ty

    // ---- The attached-member arity contract -----------------------------------

    /// The parameter count of an external attached member — the SHARED tupled-call width, so
    /// this flatten and the pre-freeze splice of the same member open its one argument to the
    /// same number of positions.
    let memberArgCount (key: SymbolKey) (memberName: string) : int =
        SymbolKeyOps.memberArity (sprintf "EmitJs: attached member '%s'" memberName) key

    /// `recv.<member>` — the shared attached-member access shape. A manifest
    /// Property read IS this bare Member node (a JS DATA property, not a zero-arg
    /// call); the call forms wrap it (`attachedCall`).
    let attachedMember (recvJs: JsExpr) (memberName: string) (loc: JsLoc voption) : JsExpr =
        JsExpr.Member(recvJs, JsExpr.Identifier(memberName, ValueNone), false, loc)

    /// `recv.<member>(args…)` — the attached-member CALL shape, assembled in one
    /// place so every applied-call site builds the identical node.
    let attachedCall (recvJs: JsExpr) (memberName: string) (args: JsExpr list) (loc: JsLoc voption) : JsExpr =
        JsExpr.Call(attachedMember recvJs memberName loc, args, loc)

    /// Forward a native attached-member escape's SINGLE eta-wrap parameter (`argVar`)
    /// to the member's JS positional arguments. An external method is tupled, so an
    /// escaped `box.get` value is a one-parameter `arg -> ret`: the one wrapper param is
    /// DROPPED for a 0-param (`unit`) member, passed straight for 1, or spread
    /// element-wise (`argVar[j]`) for a ≥2-param (tupled) member. `argVar` is a JS array
    /// with no expression behind it, so there is no literal tuple to recognise — this
    /// opens by INDEX at every position, which is why it does not share the arity open.
    let attachedForwardArgs (argVar: JsExpr) (argCount: int) : JsExpr list =
        if argCount = 0 then
            []
        elif argCount = 1 then
            [ argVar ]
        else
            [ for j in 0 .. argCount - 1 -> JsFlatFns.indexMember argVar j ]

    // ---- The lowerings ---------------------------------------------------------

    /// A NATIVE attached-member call. An `ExternalMember` head whose declaring type
    /// attaches its members (`attachesMembers` — a real manifest object, or any interface)
    /// folds every applied argument into ONE `receiver.member(args)`; it is NOT a
    /// receiver-first free-fn import (the form Vesper's OWN runtimes emit as a tree-shaking
    /// optimisation). The member is tupled (.NET convention): it consumes the FIRST argument
    /// as its argument list, opened to the key's `argSig` width — and any residual
    /// over-application folds on as unary calls. `ValueNone` for every other head:
    /// the `App` arm falls through to the flat-call / curried dispatch.
    let tryAttachedCall
        (provider: IExternalSymbolProvider)
        (pool: PoolBuilder)
        (build: TastAccessor.ExprId -> JsExpr)
        (head: TastAccessor.ExprId)
        (appArgs: (TastAccessor.ExprId * FrozenType * Anchor) list)
        (loc: JsLoc voption)
        : JsExpr voption =
        match head with
        | InstanceExternalMember(recv, em) when
            em.Storage = MemberStorage.Method && attachesMembers provider (declKey em.Key)
            ->
            match appArgs with
            | (argExpr, _, _) :: rest ->
                let args, argSpills =
                    CompiledFns.tupledMemberPlan
                        (sprintf "EmitJs: external attached member '%s'" em.MemberName)
                        (memberArgCount em.Key em.MemberName)
                        argExpr
                    |> JsFlatFns.renderFlatSteps pool build

                // A spill hoists the argument out of the call, so the receiver hoists with it
                // (ahead of it) or the two swap evaluation order.
                let recvJs, spills =
                    match argSpills with
                    | [] -> build recv, []
                    | _ ->
                        let tmp = freshTemp pool "_recv"
                        JsExpr.Identifier(tmp, ValueNone), (tmp, build recv) :: argSpills

                let call =
                    if isFunInterface (declKey em.Key) then
                        JsExpr.Call(recvJs, args, loc)
                    else
                        attachedCall recvJs em.MemberName args loc

                rest
                |> List.fold (fun acc (a, _, _) -> JsExpr.Call(acc, [ build a ], ValueNone)) call
                |> fun folded -> JsFlatFns.wrapSpills spills folded loc
                |> ValueSome
            | [] -> ValueNone // unreachable: the `App` arm guarantees ≥ 1 argument
        | _ -> ValueNone

    /// A METHOD on an `AttachMembers` type extracted as a VALUE (`let f = box.get`):
    /// eta-wrap so `this` binds at the eventual call — a detached `recv.member`
    /// loses `this` in JS. The receiver is spilled to a temp unless it is a trivial
    /// `Var`, so it evaluates exactly once — the spill is `(name, value) voption`,
    /// so the at-most-one-binding invariant is in the type. An external method is
    /// tupled, so the escaped value is a one-parameter `arg -> ret`: one wrapper
    /// param, forwarded per the member's `argSig` arity (`attachedForwardArgs`).
    let etaWrapAttachedMethod
        (build: TastAccessor.ExprId -> JsExpr)
        (recv: TastAccessor.ExprId)
        (key: SymbolKey)
        (memberName: string)
        (pool: PoolBuilder)
        (loc: JsLoc voption)
        : JsExpr =
        let recvJs, spill =
            match TastAccessor.exprKind recv with
            | ExprShape.Var -> build recv, ValueNone
            | _ ->
                let tmp = freshTemp pool "_recv"
                JsExpr.Identifier(tmp, ValueNone), ValueSome(tmp, build recv)

        let argName = freshTemp pool "_a"
        let argVar = JsExpr.Identifier(argName, ValueNone)

        let call =
            JsExpr.Call(
                attachedMember recvJs memberName ValueNone,
                attachedForwardArgs argVar (memberArgCount key memberName),
                loc
            )

        let arrow = JsExpr.Arrow([ argName ], JsFnBody.Expr call, loc)

        match spill with
        | ValueNone -> arrow
        | ValueSome(name, value) -> JsExpr.Call(JsExpr.Arrow([ name ], JsFnBody.Expr arrow, ValueNone), [ value ], loc)

    /// ERASE (Tier 2 item 9b): the declaring type is a synthetic grouping of
    /// overloaded free functions with no runtime existence. Resolve the callee the
    /// FREE-FUNCTION way — `addRef` of the BARE member name (the real module export)
    /// from the type's home module — so `Util.format(x)` emits `import { format … }`
    /// + `format(x)`, NOT the mangled `Util_format` an ordinary external static
    /// member would import (no such export exists).
    let erasedGroupingRef
        (provider: IExternalSymbolProvider)
        (imports: JsImports)
        (declKey: TypeKey)
        (memberName: string)
        (form: ImportForm)
        (loc: JsLoc voption)
        : JsExpr =
        // A free function is a binding held DIRECTLY by the namespace its export sits in
        // (`TsManifestProvider`); the grouping type is a synthetic type in that same
        // namespace. So the sibling binding is built from the grouping type's own
        // `NamespaceKey`, and its home is the grouping type's home (`homeOf` — the
        // grouping type and the functions it groups share one module by construction) —
        // so `addRef` imports the same bare export from the same home module the bare
        // free function would.
        let valueKey =
            SymbolKeyOps.valueKey (ModuleHolder.InNamespace declKey.Namespace) memberName

        let home =
            homeOf provider (SymbolKey.Type declKey) (sprintf "erased grouping member '%s'" memberName)

        // `form` is the group's import shape, stamped on the grouping type's flags by
        // `buildOverloadGroupingTypes`: `Named` → `import { format }`; `Default`/
        // `CommonJs` → `import format`; `Namespace` → `import * as util; util.format`.
        let valueRef =
            {
                Key = ValueSome valueKey
                Home = home
                Form = form
            }

        JsExpr.Identifier(JsImports.addRef imports memberName valueRef, loc)

    /// The mangled receiver-first import: `<Type>__<member>` / `<Type>_<member>`
    /// aliased from the declaring type's runtime-js module, applied to the receiver
    /// when present. This export shape is only satisfiable by a Vesper-provided
    /// runtime module — `JsImports.entryFor` fails loudly when the package has none
    /// (a real npm package cannot export a mangled name).
    let mangledMemberAccess
        (provider: IExternalSymbolProvider)
        (imports: JsImports)
        (build: TastAccessor.ExprId -> JsExpr)
        (declKey: TypeKey)
        (receiver: TastAccessor.ExprId voption)
        (memberName: string)
        (isProperty: bool)
        (loc: JsLoc voption)
        : JsExpr =
        let isStatic = ValueOption.isNone receiver

        // Backend name emission: the JS export identifier is mangled from the type's name,
        // which carries no arity on the target.
        let (DisplayName declName) = SymbolKeyOps.typeSimpleName declKey
        let exportName = mangledName declName isStatic isProperty memberName

        let home =
            homeOf provider (SymbolKey.Type declKey) (sprintf "external member '%s'" memberName)

        let local = JsImports.addMemberRef imports home exportName

        match receiver with
        | ValueSome r -> JsExpr.Call(JsExpr.Identifier(local, ValueNone), [ build r ], loc)
        | ValueNone -> JsExpr.Identifier(local, loc)
