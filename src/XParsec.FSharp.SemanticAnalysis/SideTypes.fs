namespace XParsec.FSharp.SemanticAnalysis

/// Accessibility as DECLARED, never pre-thresholded: each export filter applies its own
/// threshold.
[<RequireQualifiedAccess>]
type Accessibility =
    | Public
    | Internal
    | Private

/// The value-struct closure verdict for one source lambda, keyed by the lambda argument's
/// `NodeKey`. Recorded only for a lambda threaded through a `:> Fun<a,b>` slot.
type FunVerdict =
    {
        /// The flat `FunN` arity the slot constrains the argument to: `1` for a
        /// `Fun<a,b>` slot, `2` for a `Fun<a,b,c>` slot.
        Arity: int
        /// The type-argument POSITION the constrained `'TFunc` occupies in the combinator's
        /// RESULT nominal (`0` for `mk : ('TF:>Fun) -> Container<'TF>`); `ValueNone` for a
        /// result that does not mention it (`fold`).
        ResultTyparPos: int voption
    }

/// The kind of a bound on a typar.
[<RequireQualifiedAccess>]
type TyparConstraintKindG<'ty> =
    /// `when 'a : equality`.
    | Equality
    /// `when 'a : comparison`.
    | Comparison
    /// `when 'a : struct`.
    | Struct
    /// `when 'a : not struct`.
    | ReferenceType
    /// `when 'a : null`.
    | Nullness
    /// `when 'a : not null`.
    | NotNull
    /// `when 'a :> <ty>`.
    | Coercion of target: 'ty

/// A typar bound on a generic declaration, declared or inferred. `TyparIndex` is on the
/// owner's axis: the declaring axis for a type declaration's typars, the method axis for a
/// binding's, a member's or an abstract slot's own typars.
type TyparConstraintG<'ty> =
    {
        TyparIndex: int
        Kind: TyparConstraintKindG<'ty>
    }

/// A typar bound with a frozen `Coercion` target, whose typar leaves are `FTTypar(axis, i)`.
type FrozenConstraint = TyparConstraintG<FrozenType>

module TyparConstraintKind =
    let map (f: 'a -> 'b) (kind: TyparConstraintKindG<'a>) : TyparConstraintKindG<'b> =
        match kind with
        | TyparConstraintKindG.Equality -> TyparConstraintKindG.Equality
        | TyparConstraintKindG.Comparison -> TyparConstraintKindG.Comparison
        | TyparConstraintKindG.Struct -> TyparConstraintKindG.Struct
        | TyparConstraintKindG.ReferenceType -> TyparConstraintKindG.ReferenceType
        | TyparConstraintKindG.Nullness -> TyparConstraintKindG.Nullness
        | TyparConstraintKindG.NotNull -> TyparConstraintKindG.NotNull
        | TyparConstraintKindG.Coercion target -> TyparConstraintKindG.Coercion(f target)

    /// The bound's kind, a `Coercion` target mapped through `target`. `ValueNone` for a
    /// printf format family's `OneOf`, which is solved at the format literal and has no
    /// spelling on a declared typar.
    let ofSemantic (target: SemType -> 'ty) (kind: SemanticConstraintKind) : TyparConstraintKindG<'ty> voption =
        match kind with
        | SemanticConstraintKind.Equality -> ValueSome TyparConstraintKindG.Equality
        | SemanticConstraintKind.Comparison -> ValueSome TyparConstraintKindG.Comparison
        | SemanticConstraintKind.Struct -> ValueSome TyparConstraintKindG.Struct
        | SemanticConstraintKind.ReferenceType -> ValueSome TyparConstraintKindG.ReferenceType
        | SemanticConstraintKind.Nullness -> ValueSome TyparConstraintKindG.Nullness
        | SemanticConstraintKind.NotNull -> ValueSome TyparConstraintKindG.NotNull
        | SemanticConstraintKind.Coercion t -> ValueSome(TyparConstraintKindG.Coercion(target t))
        | SemanticConstraintKind.OneOf _ -> ValueNone

    /// The store's form of the bound, a `Coercion` target mapped through `target`.
    let toSemantic (target: 'ty -> SemType) (kind: TyparConstraintKindG<'ty>) : SemanticConstraintKind =
        match kind with
        | TyparConstraintKindG.Equality -> SemanticConstraintKind.Equality
        | TyparConstraintKindG.Comparison -> SemanticConstraintKind.Comparison
        | TyparConstraintKindG.Struct -> SemanticConstraintKind.Struct
        | TyparConstraintKindG.ReferenceType -> SemanticConstraintKind.ReferenceType
        | TyparConstraintKindG.Nullness -> SemanticConstraintKind.Nullness
        | TyparConstraintKindG.NotNull -> SemanticConstraintKind.NotNull
        | TyparConstraintKindG.Coercion t -> SemanticConstraintKind.Coercion(target t)

module TyparConstraint =
    let map (f: 'a -> 'b) (c: TyparConstraintG<'a>) : TyparConstraintG<'b> =
        {
            TyparIndex = c.TyparIndex
            Kind = TyparConstraintKind.map f c.Kind
        }

    /// The bound `kind` places on the typar at `typarIndex`; `ValueNone` for `OneOf`.
    let ofSemantic
        (typarIndex: int)
        (target: SemType -> 'ty)
        (kind: SemanticConstraintKind)
        : TyparConstraintG<'ty> voption =
        match TyparConstraintKind.ofSemantic target kind with
        | ValueSome k -> ValueSome { TyparIndex = typarIndex; Kind = k }
        | ValueNone -> ValueNone

    /// The constrained typar's index and the supertype, for a `Coercion` bound only.
    let tryCoercion (c: TyparConstraintG<'ty>) : (int * 'ty) voption =
        match c.Kind with
        | TyparConstraintKindG.Coercion target -> ValueSome(c.TyparIndex, target)
        | TyparConstraintKindG.Equality
        | TyparConstraintKindG.Comparison
        | TyparConstraintKindG.Struct
        | TyparConstraintKindG.ReferenceType
        | TyparConstraintKindG.Nullness
        | TyparConstraintKindG.NotNull -> ValueNone

/// How codegen resolves the `GetEnumerator` handle of a `Pattern` for-in source.
[<RequireQualifiedAccess>]
type ForInGetEnumG<'ty> =
    /// An *external* (BCL) source: `GetEnumerator` is minted from this provider-interned
    /// key, whose `unit → E` return recovers the source instantiation.
    | External of getEnumerator: SymbolKey
    /// A *project-local* class: `GetEnumerator` resolves against the source expression's
    /// type, so no key is needed.
    | Local
    /// A *generic typar* constrained to a project-local seq interface declaring
    /// `GetEnumerator(): E`. Emits `constrained. <Source> callvirt iface::GetEnumerator`,
    /// so a struct source dispatches by address.
    | ConstrainedInterface of iface: TypeKey * ifaceArgs: EqArray<'ty>

/// How codegen resolves the `MoveNext` / `Current` handles of a `Pattern` enumerator `E`.
/// The element type comes from the loop pattern, so only the dispatch keys are carried.
[<RequireQualifiedAccess>]
type ForInEnumMembersG<'ty> =
    /// `E` is *external* (a BCL `List<'T>.Enumerator`): both members are minted against the
    /// enumerator's instantiation, a T-free `MoveNext(): bool` not recovering its declarer.
    | External of moveNext: SymbolKey * current: SymbolKey
    /// `E` is a *project-local* `TypeDef`: both members resolve against the enumerator type.
    | Local
    /// `E` is itself a *generic typar* constrained to an enumerator interface declaring
    /// `MoveNext(): bool` and a `Current` property. Emits `constrained. <E> callvirt
    /// iface::MoveNext`, dispatching a struct by address.
    | ConstrainedInterface of iface: TypeKey * ifaceArgs: EqArray<'ty>

/// The duck-typed for-in walk over a concrete enumerator `E`.
type ForInPatternG<'ty> =
    {
        /// `E`, the concrete type `GetEnumerator()` returns.
        EnumeratorTy: 'ty
        GetEnumerator: ForInGetEnumG<'ty>
        Members: ForInEnumMembersG<'ty>
        /// `E` is a value type, selecting the non-boxing object-argument walk.
        IsValueType: bool
        /// `E` carries `IDisposable`, so the walk is wrapped in a `finally` disposing it
        /// through the `System.IDisposable::Dispose` slot.
        Dispose: bool
    }

[<RequireQualifiedAccess>]
type ForInEnumeratorG<'ty> =
    /// Lower through the `IEnumerable<'T>` / `IEnumerator<'T>` interface slots with
    /// `callvirt`. The default for the range form and for any source implementing them.
    | Interface
    /// The duck-typed path: the source exposes a parameterless `GetEnumerator()` returning
    /// a concrete `E` with `MoveNext(): bool` and `Current`, *without* implementing
    /// `IEnumerable<'T>`.
    | Pattern of ForInPatternG<'ty>

/// The `SemType` instantiations, consumed before the freeze.
type ForInGetEnum = ForInGetEnumG<SemType>
type ForInEnumMembers = ForInEnumMembersG<SemType>
type ForInPattern = ForInPatternG<SemType>
type ForInEnumerator = ForInEnumeratorG<SemType>
