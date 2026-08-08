namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open JsEmitHelpers

/// HOW an `ExternalMember` lowers. ONE verdict for both the unapplied member reference
/// and the applied call, so a new lowering is a case here plus an arm at each.
type MemberDispatch =
    /// The object argument IS the callable: `f.Invoke(a)` is `f(a)`.
    | Application
    /// A native data property: `objArg.prop`.
    | NativeData
    /// A prototype/own method: `objArg.member(args)`, eta-wrapped when unapplied.
    | AttachedMethod
    /// Vesper compiles those as zero-arg methods, so the read is the call: `objArg.prop()`.
    | InterfaceProperty
    /// A synthetic grouping type with no runtime existence: the bare module export.
    | ErasedBare of ImportForm
    /// The type-prefixed `<Type>__<member>` import Vesper's own runtimes export.
    | TypePrefixedImport

/// The EXTERNAL-member lowering cluster. Each function that must lower a sub-expression takes
/// a `build` callback, which keeps it out of the `buildExpr` mutual-recursion group.
module JsExternalMembers =

    /// Handing the MEMBER key back instead would send every downstream reader looking for a
    /// type under a member's identity, and silently missing.
    let declKey (key: SymbolKey) : TypeKey =
        SymbolKeyOps.declTypeKeyOf "EmitJs: member node" key

    /// The shared prologue of every instance-external-member recognizer; each site adds its
    /// own `Storage` guard.
    [<return: Struct>]
    let (|InstanceExternalMember|_|)
        (e: TastAccessor.ExprId)
        : struct (TastAccessor.ExprId * TastAccessor.ExternalMemberView) voption =
        match TastAccessor.exprKind e with
        | ExprShape.ExternalMember ->
            let em = TastAccessor.exprExternalMember e

            match em.ObjArg with
            | ValueSome objArg -> ValueSome(struct (objArg, em))
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Instance method → `<Type>__<member>`; instance property getter →
    /// `<Type>__get_<Prop>`; static member → `<Type>_<member>`.
    let mangledName (typeName: string) (isStatic: bool) (isProperty: bool) (memberName: string) : string =
        if isStatic then typeName + "_" + memberName
        elif isProperty then typeName + "__get_" + memberName
        else typeName + "__" + memberName

    /// THE `key -> home` oracle: a `SymbolKey` carries no home, so the only answer is what the
    /// provider stamped on the RESOLVED SHAPE. Consulted PAST the local/external verdict, so
    /// an unstamped home fails loudly rather than emitting a dangling reference.
    let homeOf (provider: IExternalSymbolProvider) (key: SymbolKey) (what: string) : JsHome =
        match provider.TryLookupType key with
        | ValueSome(ExternalTypeShape.Union(_, _, _, o))
        | ValueSome(ExternalTypeShape.Record(_, _, o))
        | ValueSome(ExternalTypeShape.Enum(_, o)) -> o.Home
        | ValueSome(ExternalTypeShape.Class shape) -> shape.Origin.Home
        | _ -> Origin.Unstamped
        |> JsHome.ofOrigin (sprintf "%s (key %A)" what key)

    /// The ONE lookup the flags accessor and the dispatch classifier both project.
    let private classShapeOf (provider: IExternalSymbolProvider) (declKey: TypeKey) : ExternalClassShape voption =
        match provider.TryLookupType(SymbolKey.Type declKey) with
        | ValueSome(ExternalTypeShape.Class shape) -> ValueSome shape
        | _ -> ValueNone

    /// `ValueNone` when the key names no `Class` shape; every flag then reads as `false`.
    let classFlagsOf (provider: IExternalSymbolProvider) (declKey: TypeKey) : ExternalClassFlags voption =
        classShapeOf provider declKey |> ValueOption.map (fun shape -> shape.Flags)

    /// Built once: the emit walk tests every external member against it.
    let private funKeys: TypeKey[] =
        [| for arity in 2..5 -> RuntimeNames.vesperFunKey arity |]

    /// THE `ExternalMember` classification, read by both consumers so they cannot drift.
    let dispatchOf (provider: IExternalSymbolProvider) (declKey: TypeKey) (storage: MemberStorage) : MemberDispatch =
        // A `Fun` is an ECMAScript arrow at run time, so `f.Invoke(a)` is application, not a
        // member on an object that has none — settled by KEY, before any shape lookup.
        if Array.contains declKey funKeys then
            MemberDispatch.Application
        else
            match classShapeOf provider declKey with
            | ValueNone -> MemberDispatch.TypePrefixedImport
            | ValueSome shape ->
                match shape.Flags.MemberLowering with
                | MemberLowering.ErasedBare -> MemberDispatch.ErasedBare shape.Flags.ImportForm
                // A manifest Property is a genuine data slot, not a zero-arg method.
                | MemberLowering.AttachedNative ->
                    if storage.IsValueMember then
                        MemberDispatch.NativeData
                    else
                        MemberDispatch.AttachedMethod
                // An interface has no runtime existence on JS, so its impls attach to the class.
                | MemberLowering.TypePrefixed when shape.IsInterface ->
                    if storage.IsValueMember then
                        MemberDispatch.InterfaceProperty
                    else
                        MemberDispatch.AttachedMethod
                | MemberLowering.TypePrefixed -> MemberDispatch.TypePrefixedImport

    /// Walk a type's `inherit` chain to the `exn` root and resolve its `(# "Error" #)` repr to
    /// the native runtime class name. `ValueNone` if it is no `exn` subtype.
    let exnReprOf (provider: IExternalSymbolProvider) (ty: FrozenType) : string voption =
        let shapeOf (ft: FrozenType) : ExternalTypeShape voption =
            match ft with
            | FTClass(key, _)
            | FTUnion(key, _)
            | FTRecord(key, _) -> provider.TryLookupType(SymbolKey.Type key)
            // An intrinsic's canon key is a nominal identity — ask by KEY, as the arms above do.
            | FTConst(key, _) -> provider.TryLookupType key
            | _ -> ValueNone

        // Depth cap backstops a malformed cyclic `inherit`; each hop is a strict ancestor.
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
                    // The PLATFORM repr, not `canon`: `canon` has no JS class analogue.
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

    /// The SHARED tupled-call width, so this flatten and the pre-freeze splice of the same
    /// member open its one argument to the same number of positions.
    let memberArgCount (key: SymbolKey) (memberName: string) : int =
        SymbolKeyOps.memberArity (sprintf "EmitJs: attached member '%s'" memberName) key

    /// `objArg.<member>` — a manifest Property read IS this bare Member node (a JS DATA
    /// property, not a zero-arg call); the call forms wrap it.
    let attachedMember (objArgJs: JsExpr) (memberName: string) (loc: JsLoc voption) : JsExpr =
        JsExpr.Member(objArgJs, JsExpr.Identifier(memberName, ValueNone), false, loc)

    /// `objArg.<member>(args…)`, so every applied-call site builds the identical node.
    let attachedCall (objArgJs: JsExpr) (memberName: string) (args: JsExpr list) (loc: JsLoc voption) : JsExpr =
        JsExpr.Call(attachedMember objArgJs memberName loc, args, loc)

    /// The eta-wrap's SINGLE parameter forwarded to the member's positional arguments: dropped
    /// at 0, straight through at 1, spread element-wise for a tupled ≥2. `argVar` has no
    /// expression behind it, so this opens by INDEX rather than sharing the arity open.
    let attachedForwardArgs (argVar: JsExpr) (argCount: int) : JsExpr list =
        if argCount = 0 then
            []
        elif argCount = 1 then
            [ argVar ]
        else
            [ for j in 0 .. argCount - 1 -> JsFlatFns.indexMember argVar j ]

    // ---- The lowerings ---------------------------------------------------------

    /// A function dispatching on the object argument folds every applied argument into ONE
    /// `objArg.member(args)`. The member is tupled, so it consumes the FIRST argument as its
    /// argument list, at the key's `argSig` width; residual application folds on as unary calls.
    let tryAttachedCall
        (provider: IExternalSymbolProvider)
        (pool: PoolBuilder)
        (build: TastAccessor.ExprId -> JsExpr)
        (fn: TastAccessor.ExprId)
        (appArgs: (TastAccessor.ExprId * FrozenType * Anchor) list)
        (loc: JsLoc voption)
        : JsExpr voption =
        match fn with
        | InstanceExternalMember(objArg, em) ->
            // ONE argument plan for both object-argument-dispatched shapes; only the callee differs.
            let saturate (callee: JsExpr -> JsExpr list -> JsExpr) : JsExpr voption =
                match appArgs with
                | (argExpr, _, _) :: rest ->
                    let args, argSpills =
                        CompiledFns.tupledMemberPlan
                            (sprintf "EmitJs: external attached member '%s'" em.MemberName)
                            (memberArgCount em.Key em.MemberName)
                            argExpr
                        |> JsFlatFns.renderFlatSteps pool build

                    // A spill hoists the argument out of the call, so the object argument hoists
                    // ahead of it or the two swap evaluation order.
                    let objArgJs, spills =
                        match argSpills with
                        | [] -> build objArg, []
                        | _ ->
                            let tmp = freshTemp pool "_objArg"
                            JsExpr.Identifier(tmp, ValueNone), (tmp, build objArg) :: argSpills

                    rest
                    |> List.fold (fun acc (a, _, _) -> JsExpr.Call(acc, [ build a ], ValueNone)) (callee objArgJs args)
                    |> fun folded -> JsFlatFns.wrapSpills spills folded loc
                    |> ValueSome
                | [] -> ValueNone // unreachable: the `App` arm guarantees ≥ 1 argument

            match dispatchOf provider (declKey em.Key) em.Storage with
            | MemberDispatch.Application -> saturate (fun objArgJs args -> JsExpr.Call(objArgJs, args, loc))
            | MemberDispatch.AttachedMethod ->
                saturate (fun objArgJs args -> attachedCall objArgJs em.MemberName args loc)
            | MemberDispatch.NativeData
            | MemberDispatch.InterfaceProperty
            | MemberDispatch.ErasedBare _
            | MemberDispatch.TypePrefixedImport -> ValueNone
        | _ -> ValueNone

    /// An external member escaping as a VALUE: the object argument spills to a temp unless it is
    /// a trivial `Var`, and a tupled member escapes as a ONE-parameter `arg -> ret`.
    let private etaWrapMember
        (build: TastAccessor.ExprId -> JsExpr)
        (objArg: TastAccessor.ExprId)
        (argCount: int)
        (callee: JsExpr -> JsExpr list -> JsExpr)
        (pool: PoolBuilder)
        (loc: JsLoc voption)
        : JsExpr =
        let objArgJs, spill =
            match TastAccessor.exprKind objArg with
            | ExprShape.Var -> build objArg, ValueNone
            | _ ->
                let tmp = freshTemp pool "_objArg"
                JsExpr.Identifier(tmp, ValueNone), ValueSome(tmp, build objArg)

        let argName = freshTemp pool "_a"
        let argVar = JsExpr.Identifier(argName, ValueNone)

        let arrow =
            JsExpr.Arrow([ argName ], JsFnBody.Expr(callee objArgJs (attachedForwardArgs argVar argCount)), loc)

        match spill with
        | ValueNone -> arrow
        | ValueSome(name, value) -> JsExpr.Call(JsExpr.Arrow([ name ], JsFnBody.Expr arrow, ValueNone), [ value ], loc)

    /// A METHOD on an `AttachMembers` type extracted as a VALUE (`let f = box.get`):
    /// eta-wrap so `this` binds at the eventual call — a detached `objArg.member` loses it.
    let etaWrapAttachedMethod
        (build: TastAccessor.ExprId -> JsExpr)
        (objArg: TastAccessor.ExprId)
        (key: SymbolKey)
        (memberName: string)
        (pool: PoolBuilder)
        (loc: JsLoc voption)
        : JsExpr =
        etaWrapMember
            build
            objArg
            (memberArgCount key memberName)
            (fun objArgJs args -> JsExpr.Call(attachedMember objArgJs memberName ValueNone, args, loc))
            pool
            loc

    /// A member dispatching on its object argument as a VALUE (`let g = f.Invoke`). At ONE
    /// parameter it already has that shape; the flat `Fun` arities are N-POSITIONAL, so they wrap.
    let etaWrapApplication
        (build: TastAccessor.ExprId -> JsExpr)
        (objArg: TastAccessor.ExprId)
        (key: SymbolKey)
        (memberName: string)
        (pool: PoolBuilder)
        (loc: JsLoc voption)
        : JsExpr =
        match memberArgCount key memberName with
        | 1 -> build objArg
        | argCount ->
            etaWrapMember build objArg argCount (fun objArgJs args -> JsExpr.Call(objArgJs, args, loc)) pool loc

    /// ERASE: the declaring type is a synthetic grouping with no runtime existence, so the
    /// callee is the BARE member name — the real export — not an external static's mangled one.
    let erasedGroupingRef
        (provider: IExternalSymbolProvider)
        (imports: JsImports)
        (declKey: TypeKey)
        (memberName: string)
        (form: ImportForm)
        (loc: JsLoc voption)
        : JsExpr =
        // A free function is held DIRECTLY by the namespace its export sits in, which the
        // synthetic grouping type shares — home included, by construction.
        let valueKey =
            SymbolKeyOps.valueKey (ModuleContainer.InNamespace declKey.Namespace) memberName

        let home =
            homeOf provider (SymbolKey.Type declKey) (sprintf "erased grouping member '%s'" memberName)

        // `form` is the group's import shape, stamped on the grouping type's flags.
        let valueRef =
            {
                Key = ValueSome valueKey
                Home = ValueSome home
                Form = form
            }

        JsExpr.Identifier(JsImports.addRef imports memberName valueRef, loc)

    /// Aliased from the declaring type's JS runtime module. Only a Vesper-provided runtime can
    /// satisfy this export shape — a real npm package cannot export a mangled name.
    let mangledMemberAccess
        (provider: IExternalSymbolProvider)
        (imports: JsImports)
        (build: TastAccessor.ExprId -> JsExpr)
        (declKey: TypeKey)
        (objArg: TastAccessor.ExprId voption)
        (memberName: string)
        (isProperty: bool)
        (loc: JsLoc voption)
        : JsExpr =
        let isStatic = ValueOption.isNone objArg

        // The JS export identifier is mangled from the type's name, which carries no arity.
        let (DisplayName declName) = SymbolKeyOps.typeSimpleName declKey
        let exportName = mangledName declName isStatic isProperty memberName

        let home =
            homeOf provider (SymbolKey.Type declKey) (sprintf "external member '%s'" memberName)

        let local = JsImports.addMemberRef imports home exportName

        match objArg with
        | ValueSome r -> JsExpr.Call(JsExpr.Identifier(local, ValueNone), [ build r ], loc)
        | ValueNone -> JsExpr.Identifier(local, loc)
