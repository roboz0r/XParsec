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

    /// The declaring type's `SymbolKey` from a member-call node's `key`.
    let declKey (key: SymbolKey) : SymbolKey =
        match key with
        | SymbolKey.MemberKey(decl, _, _, _) -> decl
        | _ -> key

    /// Instance method → `<Type>__<member>`; instance property getter →
    /// `<Type>__get_<Prop>`; static member → `<Type>_<member>`.
    let mangledName (typeName: string) (isStatic: bool) (isProperty: bool) (memberName: string) : string =
        if isStatic then typeName + "_" + memberName
        elif isProperty then typeName + "__get_" + memberName
        else typeName + "__" + memberName

    /// The home assembly of an external type, for selecting its runtime-js module.
    /// Falls back to the provider's type-shape `origin` when the key has no assembly.
    let assemblyOf (provider: IExternalSymbolProvider) (key: SymbolKey) (what: string) : string =
        match SymbolKeyOps.keyAsm key with
        | Some a -> a
        | None ->
            let origin =
                match provider.TryLookupType key with
                | ValueSome(ExternalTypeShape.Union(_, _, _, o))
                | ValueSome(ExternalTypeShape.Record(_, _, o)) -> o.Assembly
                | ValueSome(ExternalTypeShape.Class shape) -> shape.Origin.Assembly
                | _ -> None

            match origin with
            | Some a -> a
            | None -> failwithf "EmitJs (Step 7): %s has no resolvable home assembly (key %A)" what key

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
    let classFlagsOf (provider: IExternalSymbolProvider) (declKey: SymbolKey) : ExternalClassFlags voption =
        match provider.TryLookupType declKey with
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
            | FTRecord(key, _) -> provider.TryLookupType key
            | FTConst(key, _) -> ExternalSymbols.tryRuntimeType provider (SymbolKeyOps.simpleName key)
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
        match key with
        | SymbolKey.MemberKey(_, _, argSig, _) -> argSig.Length
        | other -> failwithf "EmitJs: attached member '%s' key is not a MemberKey: %A" memberName other

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
        (imports: JsImports)
        (declKey: SymbolKey)
        (memberName: string)
        (form: ImportForm)
        (loc: JsLoc voption)
        : JsExpr =
        // The free-function `External` path carries a `ValueKey(Some home, ns, name)`;
        // mirror it from the grouping type's key so `addRef` imports the same bare
        // export (`name`) from the same home module the bare free function would.
        let valueKey =
            match declKey with
            | SymbolKey.TypeKey(home, ns, _) -> SymbolKey.ValueKey(home, ns, memberName)
            | _ ->
                failwithf
                    "EmitJs (Step 9b): erased grouping member '%s' has a non-type declaring key %A"
                    memberName
                    declKey

        // `form` is the group's import shape, stamped on the grouping type's flags by
        // `buildOverloadGroupingTypes`: `Named` → `import { format }`; `Default`/
        // `CommonJs` → `import format`; `Namespace` → `import * as util; util.format`.
        JsExpr.Identifier(JsImports.addRef imports memberName (ValueSome valueKey) form, loc)

    /// The mangled receiver-first import: `<Type>__<member>` / `<Type>_<member>`
    /// aliased from the declaring type's runtime-js module, applied to the receiver
    /// when present. This export shape is only satisfiable by a Vesper-provided
    /// runtime module — `JsImports.entryFor` fails loudly when the package has none
    /// (a real npm package cannot export a mangled name).
    let mangledMemberAccess
        (provider: IExternalSymbolProvider)
        (imports: JsImports)
        (build: Frozen.TExpr -> JsExpr)
        (declKey: SymbolKey)
        (receiver: Frozen.TExpr voption)
        (memberName: string)
        (isProperty: bool)
        (loc: JsLoc voption)
        : JsExpr =
        let isStatic = ValueOption.isNone receiver

        let exportName =
            mangledName (SymbolKeyOps.simpleName declKey) isStatic isProperty memberName

        let asm = assemblyOf provider declKey (sprintf "external member '%s'" memberName)
        let local = JsImports.addMemberRef imports asm exportName

        match receiver with
        | ValueSome r -> JsExpr.Call(JsExpr.Identifier(local, ValueNone), [ build r ], loc)
        | ValueNone -> JsExpr.Identifier(local, loc)
