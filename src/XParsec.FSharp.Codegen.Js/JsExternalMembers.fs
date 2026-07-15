namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// The EXTERNAL-member lowering cluster — everything the walker keys off the
/// provider's external world: the declaring type's `ExternalClassFlags`, the `exn`
/// repr climb, member-name mangling, the attached-member arity contract, and the
/// three call shapes an external member lowers to (native attached access, the
/// erased-grouping bare export, the mangled receiver-first import).
///
/// As in `JsFlatFns` (the precedent), each function that must lower a
/// sub-expression takes a `build: Frozen.TExpr -> JsExpr` callback (the
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

    /// Instance method → `<Type>__<member>`; instance property getter →
    /// `<Type>__get_<Prop>`; static member → `<Type>_<member>`.
    let mangledName (typeName: string) (isStatic: bool) (isProperty: bool) (memberName: string) : string =
        if isStatic then typeName + "_" + memberName
        elif isProperty then typeName + "__get_" + memberName
        else typeName + "__" + memberName

    /// THE `key -> home assembly` oracle for an external type — the module its exports are
    /// imported from. A `SymbolKey` is a nominal identity and carries no home, so the only
    /// answer is the one the provider stamped on the type's RESOLVED SHAPE
    /// (`SymbolOrigin.Assembly`). Consulted strictly PAST the local/external verdict (which
    /// the emitted-type tables make, not the key): a type that is external but whose shape
    /// names no home cannot be imported at all, so it fails loudly rather than emitting a
    /// dangling reference.
    let assemblyOf (provider: IExternalSymbolProvider) (key: SymbolKey) (what: string) : string =
        // A shape whose home is unstamped names no importable module — the local/external
        // verdict is already past, so a `ValueNone` here is a real failure, not a fall-back.
        let homeName (o: SymbolOrigin) : string voption =
            match o.Home with
            | Origin.InAssembly a -> ValueSome a.Name
            | Origin.Unstamped -> ValueNone

        let home =
            match provider.TryLookupType key with
            | ValueSome(ExternalTypeShape.Union(_, _, _, o))
            | ValueSome(ExternalTypeShape.Record(_, _, o))
            | ValueSome(ExternalTypeShape.Enum(_, o)) -> homeName o
            | ValueSome(ExternalTypeShape.Class shape) -> homeName shape.Origin
            | _ -> ValueNone

        match home with
        | ValueSome a -> a
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
    let classFlagsOf (provider: IExternalSymbolProvider) (declKey: TypeKey) : ExternalClassFlags voption =
        match provider.TryLookupType(SymbolKey.Type declKey) with
        | ValueSome(ExternalTypeShape.Class shape) -> ValueSome shape.Flags
        | _ -> ValueNone

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
                | ValueSome(ExternalTypeShape.Intrinsic { Id = { Platform = Some platform } }) ->
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

    /// The parameter count of an external attached member, from its key's `argSig`.
    /// AUTHORITATIVE over the argument expression's surface shape — a genuine single
    /// `(int * int)` parameter is `argCount = 1`, not a flattened 2-param call (the same
    /// reason the CLR `ExternalMember` arm reads `argSig`, not `memberTy`).
    let memberArgCount (key: SymbolKey) (memberName: string) : int =
        (SymbolKeyOps.asMemberKey (sprintf "EmitJs: attached member '%s'" memberName) key).ArgSig.Length

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
    /// escaped `box.get` value is a single-arrow `arg -> ret`: the one wrapper param is
    /// DROPPED for a 0-param (`unit`) member, passed straight for 1, or spread
    /// element-wise (`argVar[j]`) for a ≥2-param (tupled) member. Parallels the
    /// applied-call flatten (`attachedMemberArgs`), reading a JS array rather than a
    /// `TExpr` tuple.
    let attachedForwardArgs (argVar: JsExpr) (argCount: int) : JsExpr list =
        if argCount = 0 then
            []
        elif argCount = 1 then
            [ argVar ]
        else
            [ for j in 0 .. argCount - 1 -> JsFlatFns.indexMember argVar j ]

    /// Flatten a native attached-member call's tupled argument (one `App` per .NET
    /// convention) into its JS positional arguments: dropped for a 0-param (`unit`)
    /// member (`recv.get()`, not `recv.get(undefined)`), the lone value for 1, or the
    /// literal tuple's elements for ≥2. Mirrors the CLR `ExternalMember` arg push.
    let attachedMemberArgs (build: Frozen.TExpr -> JsExpr) (argCount: int) (argExpr: Frozen.TExpr) : JsExpr list =
        if argCount = 0 then
            []
        elif argCount = 1 then
            [ build argExpr ]
        else
            match argExpr with
            | TExprG.Tuple(elems, _, _) when elems.Length = argCount -> [ for el in elems -> build el ]
            | _ ->
                failwithf
                    "EmitJs: external attached member expects %d tupled arguments but the argument is not a literal %d-tuple"
                    argCount
                    argCount

    // ---- The lowerings ---------------------------------------------------------

    /// A NATIVE attached-member call. An `ExternalMember` head whose declaring
    /// type carries `AttachMembers` (a real manifest object — its instance
    /// members are genuine prototype/own methods) folds its whole application
    /// spine into ONE `receiver.member(args)`; it is NOT a receiver-first free-fn
    /// import (the form Vesper's OWN runtimes emit as a tree-shaking optimisation).
    /// The member is tupled (.NET convention): it consumes the FIRST spine
    /// element as its argument list — the key's `argSig` length drives the
    /// flatten (0 → drop the lone `unit`, 1 → the value, ≥2 → spread the literal
    /// tuple), mirroring the CLR `ExternalMember` arg push — and any residual
    /// over-application folds on as unary calls. `ValueNone` for every other head:
    /// the `App` arm falls through to the flat-call / curried dispatch.
    let tryAttachedCall
        (provider: IExternalSymbolProvider)
        (build: Frozen.TExpr -> JsExpr)
        (head: Frozen.TExpr)
        (spine: (Frozen.TExpr * FrozenType * SyntaxToken) list)
        (loc: JsLoc voption)
        : JsExpr voption =
        match head with
        | TExprG.ExternalMember(ValueSome recv, key, memberName, MemberStorage.Method, _, _) when
            classFlagsOf provider (declKey key)
            |> ValueOption.exists (fun flags -> flags.MemberLowering = MemberLowering.AttachedNative)
            ->
            match spine with
            | (argExpr, _, _) :: rest ->
                let call =
                    attachedCall
                        (build recv)
                        memberName
                        (attachedMemberArgs build (memberArgCount key memberName) argExpr)
                        loc

                rest
                |> List.fold (fun acc (a, _, _) -> JsExpr.Call(acc, [ build a ], ValueNone)) call
                |> ValueSome
            | [] -> ValueNone // unreachable: the `App` arm guarantees ≥ 1 spine element
        | _ -> ValueNone

    /// A METHOD on an `AttachMembers` type extracted as a VALUE (`let f = box.get`):
    /// eta-wrap so `this` binds at the eventual call — a detached `recv.member`
    /// loses `this` in JS. The receiver is spilled to a temp unless it is a trivial
    /// `Var`, so it evaluates exactly once — the spill is `(name, value) voption`,
    /// so the at-most-one-binding invariant is in the type. An external method is
    /// tupled, so the escaped value is a single-arrow `arg -> ret`: one wrapper
    /// param, forwarded per the member's `argSig` arity (`attachedForwardArgs`).
    /// `off` (the member node's token offset) disambiguates the synthetic arg name.
    let etaWrapAttachedMethod
        (build: Frozen.TExpr -> JsExpr)
        (recv: Frozen.TExpr)
        (key: SymbolKey)
        (memberName: string)
        (off: int)
        (loc: JsLoc voption)
        : JsExpr =
        let recvJs, spill =
            match recv with
            | TExprG.Var _ -> build recv, ValueNone
            | _ ->
                let tmp = "_recv" + string (TastWalk.exprTok recv).StartIndex
                JsExpr.Identifier(tmp, ValueNone), ValueSome(tmp, build recv)

        let argName = "_a" + string off
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
        // `NamespaceKey`, and its home is the grouping type's home (`assemblyOf` — the
        // grouping type and the functions it groups share one module by construction) —
        // so `addRef` imports the same bare export from the same home module the bare
        // free function would.
        let valueKey =
            SymbolKeyOps.valueKey (ModuleHolder.InNamespace declKey.Namespace) memberName

        let home =
            assemblyOf provider (SymbolKey.Type declKey) (sprintf "erased grouping member '%s'" memberName)

        // `form` is the group's import shape, stamped on the grouping type's flags by
        // `buildOverloadGroupingTypes`: `Named` → `import { format }`; `Default`/
        // `CommonJs` → `import format`; `Namespace` → `import * as util; util.format`.
        let valueRef =
            {
                Key = ValueSome valueKey
                Home = Origin.InAssembly(AssemblyName home)
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
        (build: Frozen.TExpr -> JsExpr)
        (declKey: TypeKey)
        (receiver: Frozen.TExpr voption)
        (memberName: string)
        (isProperty: bool)
        (loc: JsLoc voption)
        : JsExpr =
        let isStatic = ValueOption.isNone receiver

        // Backend name emission: the JS export identifier is mangled from the type's name,
        // which carries no arity on the target.
        let (DisplayName declName) = SymbolKeyOps.typeSimpleName declKey
        let exportName = mangledName declName isStatic isProperty memberName

        let asm =
            assemblyOf provider (SymbolKey.Type declKey) (sprintf "external member '%s'" memberName)

        let local = JsImports.addMemberRef imports asm exportName

        match receiver with
        | ValueSome r -> JsExpr.Call(JsExpr.Identifier(local, ValueNone), [ build r ], loc)
        | ValueNone -> JsExpr.Identifier(local, loc)
