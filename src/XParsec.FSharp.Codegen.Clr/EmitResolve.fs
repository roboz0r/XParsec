namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower

/// Member/field handle resolution shared by `EmitExpr`. Everything here is the
/// mono-vs-generic handle decision (`memberRef`) and the nominal-receiver
/// destructure (`nominalShape`) plus the per-shape `resolve*` lookups that build
/// on them. Lifted out of `EmitExpr` so the expression dispatcher stays at one
/// altitude (the resolvers are pure handle plumbing with no recursion into
/// `buildExpr`).
module EmitResolve =
    /// Fold an argument list + result into the curried `FrozenType` shape
    /// (`arg → … → ret`) that the provider's signature-recovery contracts expect.
    let curriedFun (args: FrozenType list) (ret: FrozenType) : FrozenType =
        List.foldBack (fun a acc -> FTFun(a, acc)) args ret

    /// A member handle on a user type: the member's own `Def` token for a
    /// monomorphic type, or a `MemberRef` on the receiver's instantiated
    /// `TypeSpec` for a generic one (`List<int>::Cons`, `Box<int>::Value`).
    /// Centralises the mono/generic split every union/record/class access makes.
    /// `monoHandle` is evaluated eagerly — fine for every caller whose mono
    /// branch is a handle already in scope; a site whose mono branch is itself a
    /// fallible lookup (the closure ctor in `EmitExpr.Lambda`) must stay inline.
    let memberRef
        (env: EmitEnv)
        (typars: 'a list)
        (key: SymbolKey)
        (tyArgs: FrozenType list)
        (kind: UserMemberKind)
        (monoHandle: EntityHandle)
        : EntityHandle =
        if List.isEmpty typars then
            monoHandle
        else
            env.Provider.UserGenericMemberRef(key, tyArgs, kind)

    /// Destructure a nominal receiver type into its `(SymbolKey, tyArgs)`, failing
    /// with a `what`-tagged diagnostic for a non-nominal one. `what` names the
    /// construct being emitted (`"RecordCons"`, `"field 'X' access"`, …).
    let nominalShape (what: string) (ty: FrozenType) : SymbolKey * FrozenType list =
        match receiverShape ty with
        | ValueSome(k, xs) -> k, xs
        | ValueNone -> failwithf "Emit: %s on non-nominal type %A" what ty

    /// Recover a generic member's instantiation by structurally matching its
    /// declared *open* curried signature (`ParamTys -> RetTy`, in declaring-/method-axis
    /// markers) against the call's *instantiated* argument + result types. Returns
    /// `(declaringArgs, methodArgs)` — the parent-`TypeSpec` instantiation and the
    /// `MethodSpec` method args. The single home for the `openT`/`instT` reconstruction
    /// the `RecoverOpenTypars` contract expects, shared by the generic-static-from-concrete
    /// resolve (`instantiationFor`) and the generic-instance-method call site
    /// (`EmitMember.buildMethodCall`) so the two can't drift.
    let recoverMemberInst
        (env: EmitEnv)
        (m: EmittedMember)
        (declArity: int)
        (argTys: FrozenType list)
        (resultTy: FrozenType)
        : FrozenType list * FrozenType list =
        let openT = curriedFun m.ParamTys m.RetTy
        let instT = curriedFun argTys resultTy
        env.Provider.RecoverOpenTypars(declArity, m.MethodTyparCount, openT, instT)

    /// The codegen analog of front-end
    /// `Engine.tryUpcastWitness` / `subtypeInterfacesOf`, and the project-local head
    /// of the seq-interface witness (`FrozenTypeBridge.pickInterfaceWitness` is the
    /// shared tail; `ClrRecipes.tryExternalInterfaceWitness` is the external head).
    /// Given a project-local nominal `FTClass(classKey, classArgs)` (structs are
    /// `FTClass` with `IsValueType = true`; records/unions carry their impls
    /// elsewhere so are not covered here), look the class up in `env.Classes` and
    /// pick the implemented-interface TEMPLATE (`EmittedClass.Interfaces`, over the
    /// class's declaring typars) matching `ifaceKey`, instantiated at THIS receiver
    /// (`FTTypar(Declaring, i) := classArgs.[i]`). `ValueNone` when the nominal is
    /// not a project-local class or implements no matching interface.
    ///
    /// Direct-declared interfaces only (the registry's `Interfaces` list is the
    /// frozen direct-impl set); the front-end walk additionally recurses base
    /// classes / transitive interfaces — deferred until a consumer needs it. Read by
    /// the call-site phantom-typar solve (`EmitCall`) to
    /// recover a phantom enumerator typar from the constrained source's seq impl.
    let tryInterfaceWitness (env: EmitEnv) (nominal: FrozenType) (ifaceKey: SymbolKey) : EqArray<FrozenType> voption =
        match nominal with
        | FTClass(classKey, classArgs) ->
            match env.Classes.TryGetValue classKey with
            | true, cls ->
                let ifaces =
                    cls.Interfaces
                    |> List.choose (fun ifaceTmpl ->
                        match ifaceTmpl with
                        | FTClass(k, ifaceArgs)
                        | FTRecord(k, ifaceArgs)
                        | FTUnion(k, ifaceArgs) -> Some(SymbolKeyOps.qualifiedName k, ifaceArgs.AsSpan().ToArray())
                        | _ -> None
                    )

                pickInterfaceWitness (SymbolKeyOps.qualifiedName ifaceKey) (classArgs.AsSpan().ToArray()) ifaces
            | false, _ -> ValueNone
        | _ -> ValueNone

    /// The head identity of a `FrozenType` for overload-candidate matching: the
    /// nominal name (arity suffix / namespace dropped to the comparable key), or a
    /// structural tag. An open typar (`FTTypar`) is never compared — a generic
    /// parameter accepts any argument — so it has no head here.
    let private headOf (t: FrozenType) : string =
        match t with
        | FTConst(key, _) -> SymbolKeyOps.simpleName key
        | FTClass(k, _)
        | FTUnion(k, _)
        | FTRecord(k, _)
        | FTEnum k -> SymbolKeyOps.qualifiedName k
        | FTFun _ -> "->"
        | FTTuple _ -> "tuple"
        | FTOr _ -> "obj"
        // A literal erases to its base primitive — match on that head.
        | FTLiteral v -> v.BaseName
        // A carried type-level computation is external-vocabulary only and must be
        // ground-EVALUATED before codegen; `encodeType` rejects a residual carrier
        // loudly upstream, so one can never reach overload-head matching on the CLR.
        | FTKeyOf _
        | FTIndexedAccess _
        | FTConditional _ ->
            failwithf "EmitResolve.headOf: unreachable carried type-level node reached the CLR backend: %A" t
        | FTTypar _ -> "!typar"
        | FTUnknown n -> n
        // A body-local typar has no nominal head and — unlike `FTTypar` — no declared
        // slot on the enclosing method that an overload could be generic in. Its
        // identity is the `(binder, index)` pair, so it heads-matches only itself.
        | FTLocalTypar(binder, i) -> "!local:" + string binder + ":" + string i

    /// Does a candidate's declared (open) parameter accept a call argument of type
    /// `arg`? A method-/declaring-typar parameter (`FTTypar`) is a generic hole and
    /// accepts anything; a concrete parameter matches by head identity. Heads, not
    /// full structure — two overloads differing only in a generic argument tie and
    /// fall to declaration order, which is acceptable (the front end already
    /// type-checked the call).
    let private paramAccepts (param: FrozenType) (arg: FrozenType) : bool =
        match param with
        | FTTypar _ -> true
        | _ -> headOf param = headOf arg

    /// Pick the overload of `name` whose signature matches the call's argument
    /// types (ECMA-335 §I.10.2: CLS overloading is by number + types of
    /// parameters). The candidates are in declaration order (the type's own members
    /// first, interface-impl members last — see `NominalEmit`), so the *first*
    /// equally-good match wins, which keeps an own member ahead of an interface-impl
    /// member of the same signature (`Set.Add`). The common, non-overloaded case is
    /// a single candidate and short-circuits.
    let pickOverload (name: string) (candidates: EmittedMember list) (argTys: FrozenType list) : EmittedMember =
        match candidates with
        | [] -> failwithf "Emit: no emitted member '%s'" name
        | [ single ] -> single
        | many ->
            let arity = List.length argTys
            let sameArity = many |> List.filter (fun m -> List.length m.ParamTys = arity)

            match sameArity with
            | [] -> List.head many // arity mismatch (unexpected) — first, fail later
            | [ single ] -> single
            | multi ->
                match multi |> List.tryFind (fun m -> List.forall2 paramAccepts m.ParamTys argTys) with
                | Some m -> m
                | None -> List.head multi

    /// Resolve the member-call handle for an instance access on `receiverTy`
    /// A monomorphic union/class uses the member's `Def` token directly;
    /// a *generic* one goes through a `MemberRef` on the receiver's
    /// instantiated `TypeSpec` (`List<int>::get_Head`, `Box<int>::get_Value`).
    /// The implicit value→`obj` box is now synthesised at Elaborate as an explicit
    /// `Upcast`, so this resolver no longer returns the member's parameter types.
    /// Returns the member-call handle *and* the resolved `EmittedMember` — the
    /// latter so a generic-instance-method call site (`set.Map mapping`) can read
    /// `MethodTyparCount` + the declared signature to mint the `MethodSpec`. A
    /// 0-typar member (the common case) ignores the second component. `argTys` are
    /// the call's actual argument types, used to pick among same-name overloads.
    let resolveInstanceMember
        (env: EmitEnv)
        (receiverTy: FrozenType)
        (name: string)
        (argTys: FrozenType list)
        : EntityHandle * EmittedMember =
        // This resolver only serves project-local receivers (external instance
        // members route through `externalInstanceMemberRef`), so the table key is
        // the receiver's nominal `SymbolKey` directly
        let key, tyArgs = nominalShape (sprintf "member '%s' access" name) receiverTy

        match env.Unions.TryGetValue key with
        | true, u ->
            match u.Members.TryGetValue name with
            | true, candidates ->
                let m = pickOverload name candidates argTys

                memberRef
                    env
                    u.Typars
                    key
                    tyArgs
                    (UserMemberKind.Member(m.MetaName, false, m.MethodTyparCount, m.ParamTys, m.RetTy))
                    m.Handle,
                m
            | false, _ -> failwithf "Emit: union '%A' has no emitted member '%s'" key name
        | false, _ ->
            match env.Classes.TryGetValue key with
            | true, c ->
                match c.Members.TryGetValue name with
                | true, candidates ->
                    let m = pickOverload name candidates argTys

                    memberRef
                        env
                        c.Typars
                        key
                        tyArgs
                        (UserMemberKind.Member(m.MetaName, false, m.MethodTyparCount, m.ParamTys, m.RetTy))
                        m.Handle,
                    m
                | false, _ -> failwithf "Emit: class '%A' has no emitted member '%s'" key name
            | false, _ ->
                // An interface-typed receiver (`(x :> IFace).M()` — or, later, an
                // interface-constrained typar): resolve the abstract slot and let
                // `emitInstanceMember` `callvirt` it (interface ⇒ not a value type, so
                // it takes the `Callvirt` arm). Same member-table shape as a class.
                match env.Interfaces.TryGetValue key with
                | true, iface ->
                    match iface.Members.TryGetValue name with
                    | true, candidates ->
                        let m = pickOverload name candidates argTys

                        memberRef
                            env
                            iface.Typars
                            key
                            tyArgs
                            (UserMemberKind.Member(m.MetaName, false, m.MethodTyparCount, m.ParamTys, m.RetTy))
                            m.Handle,
                        m
                    | false, _ -> failwithf "Emit: interface '%A' has no emitted member '%s'" key name
                | false, _ -> failwithf "Emit: no emitted type carrying members for receiver '%A'" key

    /// Member handle for an instance access on an *external* (referenced-package)
    /// type. A union/record receiver carries its instantiation in its own type
    /// args, but its arity can't be recovered from the bare contract name
    /// (`"Vesper.Option"` has no `` `1 `` suffix) and `externalClassRef` resolves
    /// only a `Class` — so the recover-by-signature `ExternalMemberRef` fails on
    /// it (`… did not resolve at emit`). Route a union/record receiver through
    /// `ExternalMemberRefOn`, which reads the parent `TypeSpec` and the marker
    /// count straight off the receiver type. A class receiver keeps the existing, tested recover
    /// path.
    let externalInstanceMemberRef
        (env: EmitEnv)
        (key: SymbolKey)
        (receiverTy: FrozenType)
        (isProperty: bool)
        (memberTy: FrozenType)
        : EntityHandle =
        // A capability member (`enumerator<'T>.MoveNext()`) is keyed by its canonical
        // capability, which reconciles to a BCL platform face — but a member's true
        // declaring type may be a BASE of that face (`MoveNext` lives on the non-generic
        // `System.Collections.IEnumerator`, not on `IEnumerator`1`). Rebasing the key onto
        // that base BEFORE the receiver-shape routing makes its declaring key differ from
        // the receiver key, so the recover path below mints the ref against the base — the
        // same declaring types `for … in` lowers through (`EmitLoops`). A no-op for every
        // non-capability member and for capability members declared on the face itself.
        let key =
            match env.Provider.TryCapabilityBaseMemberKey key with
            | ValueSome baseKey -> baseKey
            | ValueNone -> key

        match receiverTy with
        | FTUnion _
        | FTRecord _ -> env.Provider.ExternalMemberRefOn(key, receiverTy, isProperty, false, memberTy)
        // A *generic* external class receiver (`ResizeArray<int>` = `List`1<int>`)
        // carries its instantiation in its own args. Recovering the declaring
        // typars from the member's open signature (the `ExternalMemberRef` path)
        // fails for a member that does not mention `'T` — `Count: int` recovers
        // nothing for declaring arg 0. Read the declaring instantiation straight
        // off the receiver type instead (like the union/record case), BUT only
        // when the member is declared on the receiver's *own* generic type: the
        // `ExternalMemberRefOn` parent is the receiver type itself, so it is wrong
        // for a member inherited from a different declaring type — e.g.
        // `IEnumerator`1<int>.MoveNext()` is really `IEnumerator::MoveNext()` on
        // the non-generic base, which the recover path mints correctly (declArity
        // 0). Gate on declaring-key == receiver-key.
        | FTClass(rKey, args) when
            args.Length > 0
            && (
                match key with
                | SymbolKey.MemberKey(declKey, _, _, _) ->
                    SymbolKeyOps.qualifiedName declKey = SymbolKeyOps.qualifiedName rKey
                | _ -> false
            )
            ->
            env.Provider.ExternalMemberRefOn(key, receiverTy, isProperty, false, memberTy)
        | _ -> env.Provider.ExternalMemberRef(key, isProperty, false, memberTy)

    /// The static-member equivalent. Generic-union *static* augmentation members
    /// are out of scope (a static member's typars aren't tied to the type's
    /// via `this`, so the front-end leaves them un-remapped — the type's generic
    /// `Cons` / `Empty` come from its case factories instead), so a generic union
    /// fails here loudly rather than minting a malformed `Def` call. Classes
    /// route through the same `Member` arm as instances; a generic class's
    /// static member uses the class `MemberRef` instead of the union one.
    /// `resultTy` is still needed for `instantiationFor` (the generic deferred-gap
    /// fix); the obj-box decision moved to Elaborate, so no param types are returned.
    /// `argTys` are the call's actual argument types (empty for a property get) —
    /// the second instantiation-recovery source after the result type.
    let resolveStaticMember
        (env: EmitEnv)
        (memberKey: SymbolKey)
        (argTys: FrozenType list)
        (resultTy: FrozenType)
        : EntityHandle =
        // The call site carries the resolved local `SymbolKey.MemberKey`: the
        // declaring type is `decl`, the
        // member name is `memberName` — the emitted tables are keyed by `SymbolKey`
        // directly, so no class-name reverse index is needed.
        let key, name =
            match memberKey with
            | SymbolKey.MemberKey(decl, n, _, _) -> decl, n
            | _ -> failwithf "Emit: expected a MemberKey for a static member call, got %A" memberKey

        // The declaring type's instantiation at *this* call site. A static member
        // on a generic class compiles to a `MemberRef` on the class `TypeSpec`, so
        // the args must be the call-site instantiation — not the bare declaring
        // typars. Calling `Set<'T>.Empty` from inside the *non-generic* `SetModule`
        // holder (where `Set.empty<'T>` lowers to a method-typar `!!0`) with the
        // hardcoded declaring `!0` minted `Set\`1<!0>::Empty`, an open typar with no
        // owning generic context — a `BadImageFormatException` at JIT.
        // The instantiation is recovered from the node's *result*
        // type when its head is the declaring type (every self-returning static
        // member — `Empty`/`Singleton`/`Intersection`/`Union` in `set.fs`).
        //
        // When the result type does not surface the instantiation (`Box<'T>.Describe
        // (x: 'T) : int` from a concrete context), recover it
        // by structurally matching the member's declared open signature against the
        // call's actual argument + result types (`RecoverOpenTypars`, declaring
        // axis) — the static analogue of the generic-instance-method recovery in
        // `buildMethodCall`. The bare declaring-typar list (`!0`, …) stays the final
        // fallback for a member whose signature mentions the typar nowhere (only
        // reachable from a declaring context today, where `!0` is correct).
        let instantiationFor (typars: 'a list) (m: EmittedMember) : FrozenType list =
            let declaringTypars =
                [ for i in 0 .. List.length typars - 1 -> FTTypar(TyparAxis.Declaring, i) ]

            match receiverShape resultTy with
            | ValueSome(rk, rargs) when rk = key && List.length rargs = List.length typars -> rargs
            | _ when List.isEmpty typars -> declaringTypars
            | _ ->
                // Match `m`'s open curried signature (declaring-/method-axis markers)
                // against the call's instantiated arg/result types. `recoverMemberInst`
                // throws when a slot is unrecoverable (the typar surfaces nowhere) —
                // fall back to the bare declaring typars in that case.
                try
                    let declaringArgs, _ = recoverMemberInst env m (List.length typars) argTys resultTy
                    declaringArgs
                with _ ->
                    declaringTypars

        match env.Unions.TryGetValue key with
        | true, u ->
            match u.Members.TryGetValue name with
            | true, candidates ->
                let m = pickOverload name candidates argTys

                if List.isEmpty u.Typars then
                    m.Handle
                else
                    failwithf "Emit: generic-union static augmentation member '%A.%s' is out of scope (R2)" key name
            | false, _ -> failwithf "Emit: union '%A' has no emitted static member '%s'" key name
        | false, _ ->
            match env.Classes.TryGetValue key with
            | true, c ->
                match c.Members.TryGetValue name with
                | true, candidates ->
                    let m = pickOverload name candidates argTys

                    memberRef
                        env
                        c.Typars
                        key
                        (instantiationFor c.Typars m)
                        (UserMemberKind.Member(m.MetaName, true, m.MethodTyparCount, m.ParamTys, m.RetTy))
                        m.Handle
                | false, _ -> failwithf "Emit: class '%A' has no emitted static member '%s'" key name
            | false, _ -> failwithf "Emit: no emitted type carrying static members for '%A'" key

    /// Resolve a class `static let` backing field to its `ldsfld`/`stsfld` handle.
    /// The handle was chosen at emit time (`NominalEmit`): a mono class stores the field's `Def` token, a *generic*
    /// class stores a `MemberRef` on the open self-`TypeSpec` (`Set\`1<!0>::empty`). Either way this is a direct dictionary read.
    let resolveStaticField (env: EmitEnv) (declKey: SymbolKey) (name: string) : EntityHandle =
        // `declKey` is the declaring class's nominal `SymbolKey.TypeKey`, carried on
        // the `StaticFieldGet` node — the emitted class table is keyed by
        // it directly.
        match env.Classes.TryGetValue declKey with
        | true, c ->
            match c.StaticFields.TryGetValue name with
            | true, h -> h
            | false, _ -> failwithf "Emit: class '%A' has no emitted static field '%s'" declKey name
        | false, _ -> failwithf "Emit: no emitted class carrying static fields for '%A'" declKey

    /// The integral enum-case load: the `ldc` pushing the literal's raw value, paired with
    /// the primitive type it boxes to. Shared by the numeric case load, the struct-enum
    /// literal push, and the `| E.A` field compare, so those sites can't drift.
    ///
    /// Neither half is spelled here, and neither is the guard. `TEnumCases.integralValue`
    /// rejects anything a `System.Enum` cannot be based on — the pointer pair included — so
    /// the width reaching `intConstLoad` never needs the pointer-width conversion
    /// `pushIntConst` would add, and the bare load is exactly right. The width's NAME is
    /// `IntWidth.name`, the same projection the elaborator picked the enum's underlying type
    /// by: asking the question twice is how the two come to disagree about an enum a new
    /// width appears in.
    let enumIntLoad (v: TConstValue) : ILInstr * FrozenType =
        let w, bits = TEnumCases.integralValue v
        EmitTypes.intConstLoad w bits, FTConst(RuntimeNames.primitiveKey (IntWidth.name w), EqArray.empty)

    /// Push a string/mixed enum case literal as the wrapper `.ctor`'s single
    /// argument: a string case is `ldstr` (a `string` ref — assignable to a
    /// `string` or `obj` field, no box); a mixed int case is its `ldc` then `box`
    /// to its CLR primitive so the wrapped `obj` carries the boxed integer (and
    /// `EqualityComparer<obj>` matches it structurally at a `| E.A` pattern).
    /// Shared by the `.cctor` construction (Assembler) and the field compare
    /// (EmitPattern), so the two literal lowerings stay identical.
    let enumLiteralPush
        (typeToken: FrozenType -> EntityHandle)
        (internString: string -> UserStringHandle)
        (lit: TEnumLiteral)
        : ILInstr list =
        match lit with
        | TEnumLiteral.String s -> [ ILInstr.Ldstr(internString s) ]
        | TEnumLiteral.Int v ->
            let load, ty = enumIntLoad v
            [ load; ILInstr.Box(typeToken ty) ]

    /// The IL load of an enum case used as a value (`E.A` / `| E.A`). A *numeric*
    /// enum value IS its integer at runtime (the `literal` field is metadata-only),
    /// so this pushes the underlying constant; a *string/mixed* enum case is a real
    /// `static initonly` field, so this `ldsfld`s it. `ValueNone` when `declKey` is
    /// not an emitted enum (the caller falls back to `resolveStaticField` for a class
    /// `static let`).
    let tryResolveEnumCaseLoad (env: EmitEnv) (declKey: SymbolKey) (name: string) : ILInstr voption =
        match env.Enums.TryGetValue declKey with
        | true, e ->
            match e.Repr with
            | EmittedEnumRepr.NumericEnum caseValues ->
                match caseValues.TryGetValue name with
                | true, v -> ValueSome(fst (enumIntLoad v))
                | false, _ -> failwithf "Emit: enum '%A' has no emitted case '%s'" declKey name
            | EmittedEnumRepr.StructEnum(_, _, caseFields, _) ->
                match caseFields.TryGetValue name with
                | true, h -> ValueSome(ILInstr.Ldsfld h)
                | false, _ -> failwithf "Emit: struct enum '%A' has no emitted case '%s'" declKey name
        | false, _ -> ValueNone

    /// Resolve a field by name on a record / class receiver to its emit handle.
    /// A monomorphic type returns the field's `Def` token; a *generic* one
    /// returns a `MemberRef` on the receiver's instantiated `TypeSpec`
    /// (`Box<int>::Value`) — the mirror of
    /// `resolveInstanceMember`. A referenced-assembly record
    /// goes through the provider's `TryResolveExternalRecordField`. Classes reach
    /// here for primary-ctor parameter accesses rewritten to `FieldGet(this,
    /// name)` by `Elaborate.translateClassMember`.
    let resolveRecordField (env: EmitEnv) (receiverTy: FrozenType) (fieldName: string) : EntityHandle =
        // Project-local tables key by the receiver's nominal `SymbolKey`; the
        // external record-field lookup derives the qualified compiled name from it
        let key, tyArgs = nominalShape (sprintf "field '%s' access" fieldName) receiverTy

        match env.Records.TryGetValue key with
        | true, r ->
            match r.Fields |> List.tryFind (fun (n, _, _) -> n = fieldName) with
            | Some(_, h, _) ->
                memberRef env r.Typars key tyArgs (UserMemberKind.RecordMember(RecordMember.Field fieldName)) h
            | None -> failwithf "Emit: record '%A' has no field '%s'" key fieldName
        | false, _ ->
            match env.Classes.TryGetValue key with
            | true, c ->
                // Primary-ctor backing fields first, then explicit `val` instance
                // fields — both resolve identically through the `ClassMember.Field`
                // member ref.
                match (c.Fields @ c.InstanceFields) |> List.tryFind (fun (n, _, _) -> n = fieldName) with
                | Some(_, h, _) ->
                    memberRef env c.Typars key tyArgs (UserMemberKind.ClassMember(ClassMember.Field fieldName)) h
                | None -> failwithf "Emit: class '%A' has no field '%s'" key fieldName
            | false, _ ->
                let qualName = SymbolKeyOps.qualifiedName key

                match env.Provider.TryResolveExternalRecordField(key, tyArgs, fieldName) with
                | ValueSome(handle, _) -> handle
                | ValueNone -> failwithf "Emit: no emitted type for field access on '%s'" qualName
