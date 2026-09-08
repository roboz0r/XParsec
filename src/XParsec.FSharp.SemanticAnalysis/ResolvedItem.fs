namespace XParsec.FSharp.SemanticAnalysis

open Vesper

/// A module-level `let` of this file, visible to a use site at or after `VisibleFrom`: its
/// own offset, or the enclosing `rec` scope's keyword when there is one. A read inside the
/// group's own RHS is excluded separately (`PassContextResolution.PendingBindings`).
[<Struct>]
type LocalModuleMember =
    {
        Key: BindingKey
        BindingSite: NodeKey
        IsMutable: bool
        VisibleFrom: int
        /// WHERE the binding enters the name environment within its depth: its own pattern
        /// offset, or `BindingRank.afterPrelude` under `rec`.
        EntersAt: int
    }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ResolvedValue =
    | Local of LocalModuleMember
    | External of ExternalSymbol

    member this.BindingKey: BindingKey =
        match this with
        | Local m -> m.Key
        | External sym -> sym.Key

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ResolvedUnionCase =
    | Local of UnionCaseInfo
    | External of ExternalUnionCase

    member this.CaseName: string =
        match this with
        | Local info -> info.Name
        | External uc -> uc.Case.Name

    /// The declaring union's short name, as a qualifier is written.
    member this.UnionName: string =
        match this with
        | Local info -> info.UnionName
        | External uc -> uc.UnionKey.Name

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ResolvedTypeRef =
    | Local of TypeIdentity
    | External of key: TypeKey * shape: ExternalTypeShape

    member this.Key: TypeKey =
        match this with
        | Local claim -> claim.Key
        | External(key, _) -> key

/// The entity a segment was looked up inside.
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ResolutionScope =
    /// The use site's own environment: the enclosing scopes, the `open`s and the prelude.
    | Environment
    | Container of ModuleContainer
    | Type of ResolvedTypeRef

/// The segment that resolved to nothing, and where it was looked up.
[<NoEquality; NoComparison>]
type UnresolvedName =
    {
        Segment: string
        Within: ResolutionScope
    }

/// What a written type name at a written arity denotes at its use site.
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TypeNameResolution =
    | Type of ResolvedTypeRef
    /// A type declared in this file, in scope under the written name but claiming a different
    /// arity (FS0033). `claim.TyparArity` is the arity the name must be written at.
    | LocalAtOtherArity of claim: TypeIdentity
    /// A type published by a referenced contract under the written name, claiming a
    /// different arity (FS0033). `shape.TyparArity` is the arity the name must be written at.
    | ExternalAtOtherArity of key: TypeKey * shape: ExternalTypeShape
    | Unresolved of UnresolvedName

[<NoEquality; NoComparison>]
type ResolvedEnumCase =
    {
        Name: string
        /// `ValueNone` for a case of this file whose written value was rejected at
        /// registration.
        Value: TConstValue voption
    }

/// What a written name denotes at its use site, in the position it is written in.
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ResolvedItem =
    | Value of ResolvedValue
    /// `requiresQualification` is set for a case of a `[<RequireQualifiedAccess>]` union
    /// reached other than through the union's own name, which F# reports as FS0035.
    | UnionCase of case: ResolvedUnionCase * requiresQualification: bool
    | EnumCase of owner: ResolvedTypeRef * case: ResolvedEnumCase
    /// A type name that denotes its constructor: written in expression position, and
    /// constructible from the bare name (a class of this file, or a referenced class whose
    /// arity the written form fixes).
    | Ctor of ResolvedTypeRef
    | Type of ResolvedTypeRef
    | StaticMember of owner: ResolvedTypeRef * name: string
    /// Every segment consumed by the module path, so the name denotes no value.
    | ModuleOrNamespace of ModuleContainer
    /// Several unions visible at the use site each declare a case `name`. A local claim
    /// shadows every referenced one, so the claims are all local or all referenced.
    | AmbiguousCase of name: string * claims: ResolvedUnionCase[]
    /// Types called `name` reach the use site at several arities. The bare name requires a
    /// written instantiation. `arities` is ascending.
    | AmbiguousTypeArity of name: string * arities: Block<int>
    | Unresolved of UnresolvedName
