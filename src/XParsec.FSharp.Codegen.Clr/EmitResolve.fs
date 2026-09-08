namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open Vesper
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

    /// A project-local class's constructors as `(declared parameter types, the member kind
    /// reaching it, its handle)`: the emitted primary first when the class has one, then each
    /// secondary in declaration order — the catalogue Unification ranks and
    /// `FrozenSignature.ctorsOf` publishes.
    let localCtors (c: EmittedClass) : (FrozenType list * UserMemberKind * EntityHandle) list =
        [
            if c.HasPrimaryCtor then
                [ for (_, _, t) in c.Fields -> t ], UserMemberKind.ClassMember ClassMember.Ctor, c.Ctor

            for (_, paramTys, h) in c.SecondaryCtors do
                paramTys, UserMemberKind.ClassMember(ClassMember.SecondaryCtor paramTys), h
        ]

    /// The constructor of `c` a construction selected: by ARITY, and where the class declares
    /// two of the same arity (`Shape(x: int)` beside `new(s: string)`), by the argument types,
    /// which the front end has already unified against the overload it chose. `site` names the
    /// construction in the failure message.
    let pickLocalCtor
        (site: string)
        (c: EmittedClass)
        (tyArgs: FrozenType list)
        (argTypes: FrozenType list)
        : UserMemberKind * EntityHandle =
        let argCount = List.length argTypes

        match localCtors c |> List.filter (fun (ps, _, _) -> List.length ps = argCount) with
        | [] -> failwithf "Emit: no constructor of arity %d on class '%s'" argCount site
        | [ (_, kind, h) ] -> kind, h
        | sameArity ->
            let declaringArgs = FrozenType.typeSlotArgs (Block.ofList tyArgs)

            let admits (ps: FrozenType list) =
                List.forall2 (fun p a -> FrozenTypeBridge.substituteDeclaring declaringArgs p = a) ps argTypes

            match sameArity |> List.filter (fun (ps, _, _) -> admits ps) with
            | [ (_, kind, h) ] -> kind, h
            | _ ->
                failwithf
                    "Emit: class '%s' declares %d constructors of arity %d, and argument types %A select none of them uniquely"
                    site
                    (List.length sameArity)
                    argCount
                    argTypes

    /// Recover a generic member's instantiation by structurally matching its declared OPEN
    /// curried signature (`FTTypar` markers of the type's and the member's scope) against the
    /// call's INSTANTIATED argument + result types: the parent `TypeSpec`'s args, then the
    /// `MethodSpec`'s.
    let recoverMemberInst
        (env: EmitEnv)
        (m: EmittedMember)
        (declTyparArity: int<typeSlot>)
        (argTys: FrozenType list)
        (resultTy: FrozenType)
        : FrozenType list * FrozenType list =
        let openT = curriedFun m.ParamTys m.RetTy
        let instT = curriedFun argTys resultTy
        env.Provider.RecoverOpenTypars(declTyparArity, m.MethodTyparCount, openT, instT)

    /// Pick the interface template a project-local class implements matching `ifaceKey`,
    /// instantiated at THIS object argument (`FTTypar(Type _, i) := classArgs.[i]`).
    /// Direct-declared interfaces only, not those of base classes or transitive interfaces.
    let tryInterfaceWitness (env: EmitEnv) (nominal: FrozenType) (ifaceKey: TypeKey) : Block<FrozenType> voption =
        match nominal with
        | FTClass(classKey, classArgs) ->
            match env.Classes.TryGetValue classKey with
            | true, cls -> pickInterfaceWitness ifaceKey (FrozenType.typeSlotArgs classArgs) cls.Interfaces
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
        | FTConditional _
        | FTMeasure _ ->
            failwithf "EmitResolve.tyCtorOf: unreachable carried type-level node reached the CLR backend: %A" t
        // A body-local typar's identity is its `(binding, index)` pair, so it matches only
        // itself. Unlike a type's or a function's typar, no overload can be generic in it.
        | FTTypar(TyparScope.LocalFunction(LocalBindingId binding), i) -> "!local:" + string binding + ":" + string i
        | FTTypar _ -> "!typar"
        // No type, so nothing to match on: every untyped position ties with every other, and
        // with no real type. The `!` prefix keeps it out of the qualified-name space above.
        | FTUnknown _ -> "!unknown"

    /// Does a candidate's declared (open) parameter accept a call argument of type `arg`?
    /// A type's or a function's typar parameter is a generic hole and accepts anything; a
    /// concrete one matches by TYPE CONSTRUCTOR only, so two overloads differing just in a
    /// generic argument tie.
    let private paramAccepts (param: FrozenType) (arg: FrozenType) : bool =
        match param with
        | FTTypar(scope, _) when not scope.IsLocal -> true
        | _ -> tyCtorOf param = tyCtorOf arg

    /// Pick the overload of `name` matching the call's argument types (ECMA-335 §I.10.2:
    /// overloading is by number + types of parameters). Candidates arrive own-members-first, so
    /// the FIRST equally-good match wins: `Set.Add` beats its same-signature interface impl.
    let pickOverload (name: string) (candidates: Block<EmittedMember>) (argTys: FrozenType list) : EmittedMember =
        match candidates.Length with
        | 0 -> failwithf "Emit: no emitted member '%s'" name
        | 1 -> candidates.[0]
        | _ ->
            let arity = List.length argTys

            let sameArity = candidates |> Block.filter (fun m -> List.length m.ParamTys = arity)

            match sameArity.Length with
            | 0 -> candidates.[0] // no candidate has this arity, so take the first and fail later
            | 1 -> sameArity.[0]
            | _ ->
                match
                    sameArity
                    |> Block.tryFind (fun m -> List.forall2 paramAccepts m.ParamTys argTys)
                with
                | ValueSome m -> m
                | ValueNone -> sameArity.[0]

    /// Resolve the member-call handle for an instance access on `objArgTy`
    /// (`List<int>::get_Head`), plus the `EmittedMember` whose `MethodTyparCount` + signature a
    /// generic-instance-method site mints its `MethodSpec` from. `argTys` pick the overload.
    let resolveInstanceMember
        (env: EmitEnv)
        (objArgTy: FrozenNominal)
        (name: string)
        (argTys: FrozenType list)
        : EntityHandle * EmittedMember =
        // Project-local types only.
        let key, tyArgs = keyAndTyArgs objArgTy

        // The member-key registry read, identical across every emitted-nominal kind: pick
        // the overload by argument types, then mint the `Def`-token or generic `MemberRef`.
        // Only the table and its declaring typars differ per kind.
        let fromMembers
            (kindLabel: string)
            (typars: string list)
            (members: System.Collections.Generic.Dictionary<string, Block<EmittedMember>>)
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

    /// Where the parent `TypeSpec` of an external instance member ref comes from.
    type private ExternalParent =
        /// The object argument identifies it: `"Vesper.Option"` carries no `` `1 ``, and
        /// `ResizeArray<int>`'s instantiation is not recoverable from `Count: int`.
        | FromObjArg of FrozenType
        /// Nothing at the site mentions the parent, so the open signature recovers it.
        | RecoverFromSignature

    /// The class arm is gated `declKey = rKey`: an INHERITED member is parented on a base,
    /// whose instantiation the object argument's own args do not give.
    let private externalParent (declKey: TypeKey) (objArgTy: FrozenType) : ExternalParent =
        match objArgTy with
        | FTUnion _
        | FTRecord _ -> FromObjArg objArgTy
        | FTClass(rKey, args) when args.Length > 0 && declKey = rKey -> FromObjArg objArgTy
        | _ -> RecoverFromSignature

    /// Member handle for an instance access on an EXTERNAL (referenced-package) type.
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

        match externalParent declKey objArgTy with
        | FromObjArg parent -> env.Provider.ExternalMemberRefOn(key, parent, isProperty, false, memberTy)
        | RecoverFromSignature -> env.Provider.ExternalMemberRef(key, isProperty, false, memberTy)

    /// The static-member equivalent. `declArgs` instantiates the class `TypeSpec` the
    /// `MemberRef` is minted on, so an arity mismatch against a generic declaring type fails,
    /// as does a static member on a generic union.
    let resolveStaticMember
        (env: EmitEnv)
        (memberKey: SymbolKey)
        (declArgs: FrozenType list)
        (argTys: FrozenType list)
        : EntityHandle =
        // The emitted tables are keyed by `SymbolKey` directly, so the member key's `Decl`
        // and `Name` are the whole lookup and no class-name reverse index is needed.
        let key, name =
            let mk = SymbolKeyOps.asMemberKey "Emit: static member call" memberKey
            mk.Decl, mk.Name

        let instantiationFor (typars: 'a list) : FrozenType list =
            match List.length typars with
            | 0 -> []
            | n when List.length declArgs = n -> declArgs
            | n ->
                failwithf
                    "Emit: static member '%A.%s' declares %d typar(s) but the node carries %d declaring args; emitting open declaring typars would not load"
                    key
                    name
                    n
                    (List.length declArgs)

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
                        (instantiationFor c.Typars)
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
    /// primitive type it boxes to. `TEnumCases.integralValue` rejects any kind a `System.Enum`
    /// cannot be based on, `nativeint` included, so the bare load needs no width conversion.
    let enumIntLoad (v: TConstValue) : ILInstr * FrozenType =
        let n = TEnumCases.integralValue v
        EmitTypes.intConstLoad n, FTConst(RuntimeNames.intKindKey (IntValue.kind n), Block.empty)

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
            | EmittedEnumRepr.NumericEnum(_, caseValues) ->
                match caseValues.TryGetValue name with
                | true, v -> ValueSome(fst (enumIntLoad v))
                | false, _ -> failwithf "Emit: enum '%A' has no emitted case '%s'" declKey name
            | EmittedEnumRepr.StructEnum(_, _, caseFields, _) ->
                match caseFields.TryGetValue name with
                | true, h -> ValueSome(ILInstr.Ldsfld h)
                | false, _ -> failwithf "Emit: struct enum '%A' has no emitted case '%s'" declKey name
        | false, _ -> ValueNone

    /// A record field's accessor in `role` at a use site: the `Def` token for a monomorphic
    /// record, a `MemberRef` on its instantiated `TypeSpec` for a generic one
    /// (`Box<int>::get_Value`). Throws for the setter of an immutable field.
    let recordFieldAccessor
        (env: EmitEnv)
        (r: EmittedRecord)
        (key: TypeKey)
        (tyArgs: FrozenType list)
        (f: EmittedRecordField)
        (role: TAccessorRole)
        : EntityHandle =
        match RecordFieldAccessorRefs.tryRole role f.Accessors with
        | ValueSome monoHandle ->
            memberRef
                env
                r.Typars
                key
                tyArgs
                (UserMemberKind.RecordMember(RecordMember.Accessor(f.Name, role)))
                monoHandle
        | ValueNone -> failwithf "Emit: record field '%s' on '%A' is immutable" f.Name key

    /// A class field by name on a class object arg, which elaboration reaches via
    /// `FieldGet(this, name)`: its `Def` token for a monomorphic class, a `MemberRef` on its
    /// instantiated `TypeSpec` for a generic one. `ValueNone` ⇒ not a class emitted here.
    /// Throws when the class is emitted here and declares no such field, which analysis
    /// rejects as `NoMember` before codegen runs.
    let tryClassField (env: EmitEnv) (objArgTy: FrozenNominal) (fieldName: string) : EntityHandle voption =
        let key, tyArgs = keyAndTyArgs objArgTy

        match env.Classes.TryGetValue key with
        | true, c ->
            // Primary-ctor backing fields first, then explicit `val` instance fields.
            match (c.Fields @ c.InstanceFields) |> List.tryFind (fun (n, _, _) -> n = fieldName) with
            | Some(_, h, _) ->
                ValueSome(memberRef env c.Typars key tyArgs (UserMemberKind.ClassMember(ClassMember.Field fieldName)) h)
            | None -> failwithf "Emit: class '%A' has no field '%s'" key fieldName
        | false, _ -> ValueNone

    /// A `FieldGet` off a class object arg → the object arg and the class field's handle.
    [<return: Struct>]
    let (|ClassFieldGet|_|) (env: EmitEnv) (e: TastAccessor.ExprId) : (TastAccessor.ExprId * EntityHandle) voption =
        match e with
        | TastAccessor.EFieldGet fieldGet ->
            match tryClassField env (nominalOfExpr fieldGet.ObjArg) fieldGet.FieldName with
            | ValueSome h -> ValueSome(fieldGet.ObjArg, h)
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Resolve a field by name on a record / class object arg, for one accessor role: a
    /// record field's accessor, own or imported, or a class field's handle.
    let resolveRecordField
        (env: EmitEnv)
        (objArgTy: FrozenNominal)
        (fieldName: string)
        (role: TAccessorRole)
        : FieldAccess =
        let key, tyArgs = keyAndTyArgs objArgTy

        match env.Records.TryGetValue key with
        | true, r ->
            match r.Fields |> List.tryFind (fun f -> f.Name = fieldName) with
            | Some f -> FieldAccess.Accessor(recordFieldAccessor env r key tyArgs f role)
            | None -> failwithf "Emit: record '%A' has no field '%s'" key fieldName
        | false, _ ->
            match tryClassField env objArgTy fieldName with
            | ValueSome h -> FieldAccess.Direct h
            | ValueNone ->
                let qualName = SymbolKeyOps.typeMetaName key

                match env.Provider.TryResolveExternalRecordField(key, tyArgs, fieldName, role) with
                | ValueSome handle -> FieldAccess.Accessor handle
                | ValueNone -> failwithf "Emit: no emitted type for field access on '%s'" qualName
