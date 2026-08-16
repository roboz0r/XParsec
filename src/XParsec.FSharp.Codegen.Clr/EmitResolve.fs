namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower

/// Member/field handle resolution: the mono-vs-generic handle decision and the per-shape
/// `resolve*` lookups over it.
module EmitResolve =
    /// `[a; b]` and `r` → `FTFun(a, FTFun(b, r))`.
    let curriedFun (args: FrozenType list) (ret: FrozenType) : FrozenType =
        List.foldBack (fun a acc -> FTFun(a, acc)) args ret

    /// A member handle on a user type: the member's own `Def` token for a
    /// monomorphic type, or a `MemberRef` on the object argument's instantiated
    /// `TypeSpec` for a generic one (`List<int>::Cons`, `Box<int>::Value`).
    let memberRef
        (env: EmitEnv)
        (typars: 'a list)
        (key: TypeKey)
        (tyArgs: FrozenType list)
        (kind: UserMemberKind)
        (monoHandle: EntityHandle)
        : EntityHandle =
        if List.isEmpty typars then
            monoHandle
        else
            env.Provider.UserGenericMemberRef(key, tyArgs, kind)

    /// Recover a generic member's instantiation by structurally matching its declared
    /// OPEN curried signature (declaring-/method-axis markers) against the call's
    /// INSTANTIATED argument + result types: the parent `TypeSpec`'s args, then the `MethodSpec`'s.
    let recoverMemberInst
        (env: EmitEnv)
        (m: EmittedMember)
        (declTyparArity: int)
        (argTys: FrozenType list)
        (resultTy: FrozenType)
        : FrozenType list * FrozenType list =
        let openT = curriedFun m.ParamTys m.RetTy
        let instT = curriedFun argTys resultTy
        env.Provider.RecoverOpenTypars(declTyparArity, m.MethodTyparCount, openT, instT)

    /// The same recovery for a caller carrying a fallback: `ValueNone` when a typar surfaces
    /// in no parameter and no result.
    let tryRecoverMemberInst
        (env: EmitEnv)
        (m: EmittedMember)
        (declTyparArity: int)
        (argTys: FrozenType list)
        (resultTy: FrozenType)
        : (FrozenType list * FrozenType list) voption =
        let openT = curriedFun m.ParamTys m.RetTy
        let instT = curriedFun argTys resultTy
        env.Provider.TryRecoverOpenTypars(declTyparArity, m.MethodTyparCount, openT, instT)

    /// Pick the interface template a project-local class implements matching `ifaceKey`,
    /// instantiated at THIS object argument (`FTTypar(Declaring, i) := classArgs.[i]`).
    /// Direct-declared interfaces only, not those of base classes or transitive interfaces.
    let tryInterfaceWitness (env: EmitEnv) (nominal: FrozenType) (ifaceKey: TypeKey) : EqArray<FrozenType> voption =
        match nominal with
        | FTClass(classKey, classArgs) ->
            match env.Classes.TryGetValue classKey with
            | true, cls -> pickInterfaceWitness ifaceKey (classArgs.AsSpan().ToArray()) cls.Interfaces
            | false, _ -> ValueNone
        | _ -> ValueNone

    /// A `FrozenType`'s outermost type constructor as a string identity, for
    /// overload-candidate matching: the nominal's FULLY-QUALIFIED name, or a structural
    /// tag. Qualified for an intrinsic too, not its display name, because two types match
    /// only when these identities are equal.
    let private tyCtorOf (t: FrozenType) : string =
        match t with
        | FTConst(k, _) -> SymbolKeyOps.typeMetaName k
        | FTClass(k, _)
        | FTUnion(k, _)
        | FTRecord(k, _)
        | FTEnum k -> SymbolKeyOps.typeMetaName k
        | FTFun _ -> "->"
        | FTTuple _ -> "tuple"
        | FTOr _ -> SymbolKeyOps.typeMetaName RuntimeNames.objKey
        // A literal erases to its base primitive, so match on that instead.
        | FTLiteral v -> SymbolKeyOps.typeMetaName (RuntimeNames.literalBaseKey v)
        | FTKeyOf _
        | FTIndexedAccess _
        | FTConditional _ ->
            failwithf "EmitResolve.tyCtorOf: unreachable carried type-level node reached the CLR backend: %A" t
        | FTTypar _ -> "!typar"
        // No type, so nothing to match on: every untyped position ties with every other, and
        // with no real type. The `!` prefix keeps it out of the qualified-name space above.
        | FTUnknown _ -> "!unknown"
        // A body-local typar's identity is its `(scheme, index)` pair, so it matches only
        // itself. Unlike `FTTypar`, no overload can be generic in it.
        | FTLocalTypar(SchemeId scheme, i) -> "!local:" + string scheme + ":" + string i

    /// Does a candidate's declared (open) parameter accept a call argument of type `arg`?
    /// An `FTTypar` parameter is a generic hole and accepts anything; a concrete one matches
    /// by TYPE CONSTRUCTOR only, so two overloads differing just in a generic argument tie.
    let private paramAccepts (param: FrozenType) (arg: FrozenType) : bool =
        match param with
        | FTTypar _ -> true
        | _ -> tyCtorOf param = tyCtorOf arg

    /// Pick the overload of `name` matching the call's argument types (ECMA-335 §I.10.2:
    /// overloading is by number + types of parameters). Candidates arrive own-members-first, so
    /// the FIRST equally-good match wins: `Set.Add` beats its same-signature interface impl.
    let pickOverload (name: string) (candidates: EmittedMember list) (argTys: FrozenType list) : EmittedMember =
        match candidates with
        | [] -> failwithf "Emit: no emitted member '%s'" name
        | [ single ] -> single
        | many ->
            let arity = List.length argTys
            let sameArity = many |> List.filter (fun m -> List.length m.ParamTys = arity)

            match sameArity with
            | [] -> List.head many // no candidate has this arity, so take the first and fail later
            | [ single ] -> single
            | multi ->
                match multi |> List.tryFind (fun m -> List.forall2 paramAccepts m.ParamTys argTys) with
                | Some m -> m
                | None -> List.head multi

    /// Resolve the member-call handle for an instance access on `objArgTy`
    /// (`List<int>::get_Head`), plus the `EmittedMember` whose `MethodTyparCount` + signature a
    /// generic-instance-method site mints its `MethodSpec` from. `argTys` pick the overload.
    let resolveInstanceMember
        (env: EmitEnv)
        (objArgTy: FrozenNominal)
        (name: string)
        (argTys: FrozenType list)
        : EntityHandle * EmittedMember =
        // Project-local types only. An external one goes to `externalInstanceMemberRef`.
        let key, tyArgs = keyAndTyArgs objArgTy

        // The member-key registry read, identical across every emitted-nominal kind: pick
        // the overload by argument types, then mint the `Def`-token or generic `MemberRef`.
        // Only the table and its declaring typars differ per kind.
        let fromMembers
            (kindLabel: string)
            (typars: string list)
            (members: System.Collections.Generic.Dictionary<string, EmittedMember list>)
            =
            match members.TryGetValue name with
            | true, candidates ->
                let m = pickOverload name candidates argTys

                memberRef
                    env
                    typars
                    key
                    tyArgs
                    (UserMemberKind.Member(m.MetaName, false, m.MethodTyparCount, m.ParamTys, m.RetTy))
                    m.Handle,
                m
            | false, _ -> failwithf "Emit: %s '%A' has no emitted member '%s'" kindLabel key name

        match env.Unions.TryGetValue key with
        | true, u -> fromMembers "union" u.Typars u.Members
        | false, _ ->
            match env.Classes.TryGetValue key with
            | true, c -> fromMembers "class" c.Typars c.Members
            | false, _ ->
                match env.Records.TryGetValue key with
                | true, r -> fromMembers "record" r.Typars r.Members
                | false, _ ->
                    // An interface-typed object argument (`(x :> IFace).M()`) resolves to
                    // the abstract slot, dispatched `callvirt` (an interface is not a value
                    // type). Same member-table shape as a class.
                    match env.Interfaces.TryGetValue key with
                    | true, iface -> fromMembers "interface" iface.Typars iface.Members
                    | false, _ -> failwithf "Emit: no emitted type carrying members for object argument '%A'" key

    /// Member handle for an instance access on an EXTERNAL (referenced-package) type. A
    /// union/record object argument routes through `ExternalMemberRefOn`, reading the parent
    /// `TypeSpec` off it, because `"Vesper.Option"` carries no `` `1 `` and its arity is
    /// otherwise unrecoverable.
    let externalInstanceMemberRef
        (env: EmitEnv)
        (key: SymbolKey)
        (objArgTy: FrozenType)
        (isProperty: bool)
        (memberTy: FrozenType)
        : EntityHandle =
        // A capability member may really be declared on a BASE of the BCL interface its
        // capability reconciles to, as `MoveNext` is on non-generic `IEnumerator`. Rebase
        // before the routing below so the ref is minted against that base.
        let key =
            match env.Provider.TryCapabilityBaseMemberKey key with
            | ValueSome baseKey -> baseKey
            | ValueNone -> key

        let declKey = SymbolKeyOps.declTypeKeyOf "Emit: external instance member" key

        match objArgTy with
        | FTUnion _
        | FTRecord _ -> env.Provider.ExternalMemberRefOn(key, objArgTy, isProperty, false, memberTy)
        // A generic external class object arg (`ResizeArray<int>`) carries its instantiation
        // in its own args, which signature recovery cannot get from `Count: int`. Gated on
        // declKey = rKey since the parent IS the object argument, and the instantiation would
        // be wrong for an inherited member.
        | FTClass(rKey, args) when args.Length > 0 && declKey = rKey ->
            env.Provider.ExternalMemberRefOn(key, objArgTy, isProperty, false, memberTy)
        | _ -> env.Provider.ExternalMemberRef(key, isProperty, false, memberTy)

    /// Where a static member's DECLARING instantiation comes from at a call site, in
    /// precedence order.
    type private DeclaringInstantiation =
        /// The result IS the declaring nominal (`Set<int>.Empty : Set<int>`), which names it.
        | FromResultTy of FrozenType list
        /// Matched out of the member's open signature against the call's types.
        | FromSignature of FrozenType list
        /// Nothing at the call site mentions the typars, so the declaring `!0…` stand in.
        | OpenDeclaring

    /// The declaring type's instantiation at THIS call site: a static member on a generic
    /// class compiles to a `MemberRef` on the class `TypeSpec`, so a hardcoded declaring `!0`
    /// mints `Set\`1<!0>::Empty`, an open typar with no owner, and the JIT throws
    /// `BadImageFormatException`.
    let private declaringInstantiation
        (env: EmitEnv)
        (key: TypeKey)
        (typarCount: int)
        (argTys: FrozenType list)
        (resultTy: FrozenType)
        (m: EmittedMember)
        : DeclaringInstantiation =
        match FrozenNominal.TryOfFrozen resultTy with
        | ValueSome r when r.Key = key && r.Args.Length = typarCount -> FromResultTy(EqArray.toList r.Args)
        | _ ->
            match typarCount with
            | 0 -> OpenDeclaring
            | n ->
                match tryRecoverMemberInst env m n argTys resultTy with
                | ValueSome(declaringArgs, _) -> FromSignature declaringArgs
                // `Box<'T>.Describe (x: 'T) : int` called from a concrete context.
                | ValueNone -> OpenDeclaring

    /// The static-member equivalent; `argTys` (empty for a property get) and `resultTy` recover
    /// the instantiation. A GENERIC union fails loudly below rather than mint a malformed `Def`
    /// call: a static member's typars aren't tied to the type's via `this`, so they stay open.
    let resolveStaticMember
        (env: EmitEnv)
        (memberKey: SymbolKey)
        (argTys: FrozenType list)
        (resultTy: FrozenType)
        : EntityHandle =
        // The emitted tables are keyed by `SymbolKey` directly, so the member key's `Decl`
        // and `Name` are the whole lookup and no class-name reverse index is needed.
        let key, name =
            let mk = SymbolKeyOps.asMemberKey "Emit: static member call" memberKey
            mk.Decl, mk.Name

        let instantiationFor (typars: 'a list) (m: EmittedMember) : FrozenType list =
            let typarCount = List.length typars

            match declaringInstantiation env key typarCount argTys resultTy m with
            | FromResultTy args
            | FromSignature args -> args
            | OpenDeclaring -> [ for i in 0 .. typarCount - 1 -> FTTypar(TyparAxis.Declaring, i) ]

        match env.Unions.TryGetValue key with
        | true, u ->
            match u.Members.TryGetValue name with
            | true, candidates ->
                let m = pickOverload name candidates argTys

                if List.isEmpty u.Typars then
                    m.Handle
                else
                    failwithf
                        "Emit: '%A.%s' is a static augmentation member on a GENERIC union, which this compiler does not emit"
                        key
                        name
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

    /// Resolve a class `static let` backing field to its `ldsfld`/`stsfld` handle. The stored
    /// handle is the field's `Def` token for a mono class, a `MemberRef` on the open
    /// self-`TypeSpec` (`Set\`1<!0>::empty`) for a generic one, so this is a direct read.
    let resolveStaticField (env: EmitEnv) (declKey: TypeKey) (name: string) : EntityHandle =
        match env.Classes.TryGetValue declKey with
        | true, c ->
            match c.StaticFields.TryGetValue name with
            | true, h -> h
            | false, _ -> failwithf "Emit: class '%A' has no emitted static field '%s'" declKey name
        | false, _ -> failwithf "Emit: no emitted class carrying static fields for '%A'" declKey

    /// The integral enum-case load: the `ldc` pushing the literal's raw value, paired with the
    /// primitive type it boxes to. `TEnumCases.integralValue` rejects any width a `System.Enum`
    /// cannot be based on, `nativeint` included, so the bare load needs no width conversion.
    let enumIntLoad (v: TConstValue) : ILInstr * FrozenType =
        let w, bits = TEnumCases.integralValue v
        EmitTypes.intConstLoad w bits, FTConst(RuntimeNames.intWidthKey w, EqArray.empty)

    /// Push a string/mixed enum case literal as the wrapper `.ctor`'s single argument: a
    /// string case is `ldstr` (a ref, assignable to a `string` or `obj` field unboxed); a
    /// mixed int case `box`es, so `EqualityComparer<obj>` matches it at a `| E.A` pattern.
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

    /// The IL load of an enum case used as a value (`E.A` / `| E.A`). A NUMERIC enum value IS
    /// its integer at runtime and its `literal` field is metadata-only, so this pushes the
    /// constant; a string/mixed case is a real `static initonly` field, so this `ldsfld`s it.
    let tryResolveEnumCaseLoad (env: EmitEnv) (declKey: TypeKey) (name: string) : ILInstr voption =
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

    /// Resolve a field by name on a record / class object arg: the field's `Def` token for a
    /// monomorphic type, a `MemberRef` on its instantiated `TypeSpec` for a generic
    /// one (`Box<int>::Value`). A class reaches here via elaboration's `FieldGet(this, name)`.
    let resolveRecordField (env: EmitEnv) (objArgTy: FrozenNominal) (fieldName: string) : EntityHandle =
        let key, tyArgs = keyAndTyArgs objArgTy

        match env.Records.TryGetValue key with
        | true, r ->
            match r.Fields |> List.tryFind (fun (n, _, _) -> n = fieldName) with
            | Some(_, h, _) ->
                memberRef env r.Typars key tyArgs (UserMemberKind.RecordMember(RecordMember.Field fieldName)) h
            | None -> failwithf "Emit: record '%A' has no field '%s'" key fieldName
        | false, _ ->
            match env.Classes.TryGetValue key with
            | true, c ->
                // Primary-ctor backing fields first, then explicit `val` instance fields.
                match (c.Fields @ c.InstanceFields) |> List.tryFind (fun (n, _, _) -> n = fieldName) with
                | Some(_, h, _) ->
                    memberRef env c.Typars key tyArgs (UserMemberKind.ClassMember(ClassMember.Field fieldName)) h
                | None -> failwithf "Emit: class '%A' has no field '%s'" key fieldName
            | false, _ ->
                let qualName = SymbolKeyOps.typeMetaName key

                match env.Provider.TryResolveExternalRecordField(key, tyArgs, fieldName) with
                | ValueSome(handle, _) -> handle
                | ValueNone -> failwithf "Emit: no emitted type for field access on '%s'" qualName
