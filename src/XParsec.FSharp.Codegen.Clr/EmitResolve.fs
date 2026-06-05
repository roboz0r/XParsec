namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
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

    /// Resolve the member-call handle for an instance access on `receiverTy`
    /// (P3d.3, generalised to generic unions in R2 and to classes in Phase 1 /
    /// B-1). A monomorphic union/class uses the member's `Def` token directly;
    /// a *generic* one goes through a `MemberRef` on the receiver's
    /// instantiated `TypeSpec` (`List<int>::get_Head`, `Box<int>::get_Value`).
    let resolveInstanceMember (env: EmitEnv) (receiverTy: FrozenType) (name: string) : EntityHandle =
        // This resolver only serves project-local receivers (external instance
        // members route through `externalInstanceMemberRef`), so the table key is
        // the receiver's nominal `SymbolKey` directly (Phase 6D).
        let key, tyArgs = nominalShape (sprintf "member '%s' access" name) receiverTy

        match env.Unions.TryGetValue key with
        | true, u ->
            match u.Members.TryGetValue name with
            | true, m ->
                memberRef
                    env
                    u.Typars
                    key
                    tyArgs
                    (UserMemberKind.UnionMember(UnionMember.Member(m.MetaName, false, m.ParamTys, m.RetTy)))
                    m.Handle
            | false, _ -> failwithf "Emit: union '%A' has no emitted member '%s'" key name
        | false, _ ->
            match env.Classes.TryGetValue key with
            | true, c ->
                match c.Members.TryGetValue name with
                | true, m ->
                    memberRef
                        env
                        c.Typars
                        key
                        tyArgs
                        (UserMemberKind.ClassMember(ClassMember.Member(m.MetaName, false, m.ParamTys, m.RetTy)))
                        m.Handle
                | false, _ -> failwithf "Emit: class '%A' has no emitted member '%s'" key name
            | false, _ -> failwithf "Emit: no emitted type carrying members for receiver '%A'" key

    /// Member handle for an instance access on an *external* (referenced-package)
    /// type. A union/record receiver carries its instantiation in its own type
    /// args, but its arity can't be recovered from the bare contract name
    /// (`"Vesper.Option"` has no `` `1 `` suffix) and `externalClassRef` resolves
    /// only a `Class` — so the recover-by-signature `ExternalMemberRef` fails on
    /// it (`… did not resolve at emit`). Route a union/record receiver through
    /// `ExternalMemberRefOn`, which reads the parent `TypeSpec` and the marker
    /// count straight off the receiver type (vesper-lib-test-plan Gap 2 — the
    /// Layer A *backend* half, `(Some 5).IsSome` / `o.Value` at runtime). A class
    /// receiver keeps the existing, tested recover path.
    let externalInstanceMemberRef
        (env: EmitEnv)
        (key: SymbolKey)
        (receiverTy: FrozenType)
        (isProperty: bool)
        (memberTy: FrozenType)
        : EntityHandle =
        match receiverTy with
        | FTUnion _
        | FTRecord _ -> env.Provider.ExternalMemberRefOn(key, receiverTy, isProperty, false, memberTy)
        | _ -> env.Provider.ExternalMemberRef(key, isProperty, false, memberTy)

    /// The static-member equivalent. Generic-union *static* augmentation members
    /// are out of scope in R2 (a static member's typars aren't tied to the type's
    /// via `this`, so the front-end leaves them un-remapped — the type's generic
    /// `Cons` / `Empty` come from its case factories instead), so a generic union
    /// fails here loudly rather than minting a malformed `Def` call. Classes
    /// route through the same `Member` arm as instances; a generic class's
    /// static member uses the class `MemberRef` instead of the union one.
    let resolveStaticMember (env: EmitEnv) (memberKey: SymbolKey) : EntityHandle =
        // The call site carries the resolved local `SymbolKey.MemberKey`: the
        // declaring type is `decl`, the
        // member name is `memberName` — the emitted tables are keyed by `SymbolKey`
        // directly, so no class-name reverse index is needed.
        let key, name =
            match memberKey with
            | SymbolKey.MemberKey(decl, n, _, _) -> decl, n
            | _ -> failwithf "Emit: expected a MemberKey for a static member call, got %A" memberKey

        match env.Unions.TryGetValue key with
        | true, u ->
            match u.Members.TryGetValue name with
            | true, m ->
                if List.isEmpty u.Typars then
                    m.Handle
                else
                    failwithf "Emit: generic-union static augmentation member '%A.%s' is out of scope (R2)" key name
            | false, _ -> failwithf "Emit: union '%A' has no emitted static member '%s'" key name
        | false, _ ->
            match env.Classes.TryGetValue key with
            | true, c ->
                match c.Members.TryGetValue name with
                | true, m ->
                    memberRef
                        env
                        c.Typars
                        key
                        [ for i in 0 .. List.length c.Typars - 1 -> FTTypar(TyparAxis.Declaring, i) ]
                        (UserMemberKind.ClassMember(ClassMember.Member(m.MetaName, true, m.ParamTys, m.RetTy)))
                        m.Handle
                | false, _ -> failwithf "Emit: class '%A' has no emitted static member '%s'" key name
            | false, _ -> failwithf "Emit: no emitted type carrying static members for '%A'" key

    /// Resolve a class `static let` backing field to its `ldsfld`/`stsfld` handle
    /// (vesper-set-sprint-plan §1.8 / B-10). Only monomorphic classes declare
    /// `static let`s (generic `static let` is deferred), so the field handle is
    /// always a `Def` token — no `MemberRef`-on-`TypeSpec` path.
    let resolveStaticField (env: EmitEnv) (declKey: SymbolKey) (name: string) : EntityHandle =
        // `declKey` is the declaring class's nominal `SymbolKey.TypeKey`, carried on
        // the `StaticFieldGet` node (Phase 4) — the emitted class table is keyed by
        // it directly.
        match env.Classes.TryGetValue declKey with
        | true, c ->
            match c.StaticFields.TryGetValue name with
            | true, h -> h
            | false, _ -> failwithf "Emit: class '%A' has no emitted static field '%s'" declKey name
        | false, _ -> failwithf "Emit: no emitted class carrying static fields for '%A'" declKey

    /// Resolve a field by name on a record / class receiver to its emit handle.
    /// A monomorphic type returns the field's `Def` token; a *generic* one
    /// returns a `MemberRef` on the receiver's instantiated `TypeSpec`
    /// (`Box<int>::Value`) — the mirror of
    /// `resolveInstanceMember`. A referenced-assembly record
    /// goes through the provider's `TryResolveExternalRecordField`. Classes reach
    /// here for primary-ctor parameter accesses rewritten to `FieldGet(this,
    /// name)` by `Freeze.translateClassMember` (vesper-set-sprint-plan Phase 1 /
    /// B-1).
    let resolveRecordField (env: EmitEnv) (receiverTy: FrozenType) (fieldName: string) : EntityHandle =
        // Project-local tables key by the receiver's nominal `SymbolKey`; the
        // external record-field lookup derives the qualified compiled name from it
        // (Phase 6D).
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
                match c.Fields |> List.tryFind (fun (n, _, _) -> n = fieldName) with
                | Some(_, h, _) ->
                    memberRef env c.Typars key tyArgs (UserMemberKind.ClassMember(ClassMember.Field fieldName)) h
                | None -> failwithf "Emit: class '%A' has no field '%s'" key fieldName
            | false, _ ->
                let qualName = SymbolKeyOps.qualifiedName key

                match env.Provider.TryResolveExternalRecordField(qualName, tyArgs, fieldName) with
                | ValueSome(handle, _) -> handle
                | ValueNone -> failwithf "Emit: no emitted type for field access on '%s'" qualName
