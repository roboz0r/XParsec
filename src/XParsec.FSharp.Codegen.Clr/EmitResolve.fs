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
        (typeName: string)
        (tyArgs: SemType list)
        (kind: UserMemberKind)
        (monoHandle: EntityHandle)
        : EntityHandle =
        if List.isEmpty typars then
            monoHandle
        else
            env.Provider.UserGenericMemberRef(typeName, tyArgs, kind)

    /// Destructure a nominal receiver type into its `(typeName, tyArgs)`, failing
    /// with a `what`-tagged diagnostic for a non-nominal one. `what` names the
    /// construct being emitted (`"RecordCons"`, `"field 'X' access"`, …).
    let nominalShape (what: string) (ty: SemType) : string * SemType list =
        match receiverShape ty with
        | ValueSome(n, xs) -> n, xs
        | ValueNone -> failwithf "Emit: %s on non-nominal type %A" what ty

    /// Resolve the member-call handle for an instance access on `receiverTy`
    /// (P3d.3, generalised to generic unions in R2 and to classes in Phase 1 /
    /// B-1). A monomorphic union/class uses the member's `Def` token directly;
    /// a *generic* one goes through a `MemberRef` on the receiver's
    /// instantiated `TypeSpec` (`List<int>::get_Head`, `Box<int>::get_Value`).
    let resolveInstanceMember (env: EmitEnv) (receiverTy: SemType) (name: string) : EntityHandle =
        let typeName, tyArgs = nominalShape (sprintf "member '%s' access" name) receiverTy

        match env.Unions.TryGetValue typeName with
        | true, u ->
            match u.Members.TryGetValue name with
            | true, m ->
                memberRef
                    env
                    u.Typars
                    typeName
                    tyArgs
                    (UserMemberKind.UnionMember(UnionMember.Member(m.MetaName, false, m.ParamTys, m.RetTy)))
                    m.Handle
            | false, _ -> failwithf "Emit: union '%s' has no emitted member '%s'" typeName name
        | false, _ ->
            match env.Classes.TryGetValue typeName with
            | true, c ->
                match c.Members.TryGetValue name with
                | true, m ->
                    memberRef
                        env
                        c.Typars
                        typeName
                        tyArgs
                        (UserMemberKind.ClassMember(ClassMember.Member(m.MetaName, false, m.ParamTys, m.RetTy)))
                        m.Handle
                | false, _ -> failwithf "Emit: class '%s' has no emitted member '%s'" typeName name
            | false, _ -> failwithf "Emit: no emitted type carrying members for receiver '%s'" typeName

    /// The static-member equivalent. Generic-union *static* augmentation members
    /// are out of scope in R2 (a static member's typars aren't tied to the type's
    /// via `this`, so the front-end leaves them un-remapped — the type's generic
    /// `Cons` / `Empty` come from its case factories instead), so a generic union
    /// fails here loudly rather than minting a malformed `Def` call. Classes
    /// route through the same `Member` arm as instances; a generic class's
    /// static member uses the class `MemberRef` instead of the union one.
    let resolveStaticMember (env: EmitEnv) (typeName: string) (name: string) : EntityHandle =
        match env.Unions.TryGetValue typeName with
        | true, u ->
            match u.Members.TryGetValue name with
            | true, m ->
                if List.isEmpty u.Typars then
                    m.Handle
                else
                    failwithf
                        "Emit: generic-union static augmentation member '%s.%s' is out of scope (R2)"
                        typeName
                        name
            | false, _ -> failwithf "Emit: union '%s' has no emitted static member '%s'" typeName name
        | false, _ ->
            match env.Classes.TryGetValue typeName with
            | true, c ->
                match c.Members.TryGetValue name with
                | true, m ->
                    memberRef
                        env
                        c.Typars
                        typeName
                        [ for t in c.Typars -> TyConst t ]
                        (UserMemberKind.ClassMember(ClassMember.Member(m.MetaName, true, m.ParamTys, m.RetTy)))
                        m.Handle
                | false, _ -> failwithf "Emit: class '%s' has no emitted static member '%s'" typeName name
            | false, _ -> failwithf "Emit: no emitted type carrying static members for '%s'" typeName

    /// Resolve a class `static let` backing field to its `ldsfld`/`stsfld` handle
    /// (vesper-set-sprint-plan §1.8 / B-10). Only monomorphic classes declare
    /// `static let`s (generic `static let` is deferred), so the field handle is
    /// always a `Def` token — no `MemberRef`-on-`TypeSpec` path.
    let resolveStaticField (env: EmitEnv) (typeName: string) (name: string) : EntityHandle =
        match env.Classes.TryGetValue typeName with
        | true, c ->
            match c.StaticFields.TryGetValue name with
            | true, h -> h
            | false, _ -> failwithf "Emit: class '%s' has no emitted static field '%s'" typeName name
        | false, _ -> failwithf "Emit: no emitted class carrying static fields for '%s'" typeName

    /// Resolve a field by name on a record / class receiver to its emit handle.
    /// A monomorphic type returns the field's `Def` token; a *generic* one
    /// returns a `MemberRef` on the receiver's instantiated `TypeSpec`
    /// (`Box<int>::Value`) — the records-plan §B3 mirror of
    /// `resolveInstanceMember`. A referenced-assembly record (records-plan §B7)
    /// goes through the provider's `TryResolveExternalRecordField`. Classes reach
    /// here for primary-ctor parameter accesses rewritten to `FieldGet(this,
    /// name)` by `Freeze.translateClassMember` (vesper-set-sprint-plan Phase 1 /
    /// B-1).
    let resolveRecordField (env: EmitEnv) (receiverTy: SemType) (fieldName: string) : EntityHandle =
        let typeName, tyArgs =
            nominalShape (sprintf "field '%s' access" fieldName) receiverTy

        match env.Records.TryGetValue typeName with
        | true, r ->
            match r.Fields |> List.tryFind (fun (n, _, _) -> n = fieldName) with
            | Some(_, h, _) ->
                memberRef env r.Typars typeName tyArgs (UserMemberKind.RecordMember(RecordMember.Field fieldName)) h
            | None -> failwithf "Emit: record '%s' has no field '%s'" typeName fieldName
        | false, _ ->
            match env.Classes.TryGetValue typeName with
            | true, c ->
                match c.Fields |> List.tryFind (fun (n, _, _) -> n = fieldName) with
                | Some(_, h, _) ->
                    memberRef env c.Typars typeName tyArgs (UserMemberKind.ClassMember(ClassMember.Field fieldName)) h
                | None -> failwithf "Emit: class '%s' has no field '%s'" typeName fieldName
            | false, _ ->
                match env.Provider.TryResolveExternalRecordField(typeName, tyArgs, fieldName) with
                | ValueSome(handle, _) -> handle
                | ValueNone -> failwithf "Emit: no emitted type for field access on '%s'" typeName
