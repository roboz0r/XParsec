namespace XParsec.FSharp.SemanticAnalysis

open Vesper

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

[<RequireQualifiedAccess>]
module TyparConstraint =

    /// The constraint's kind, every embedded type mapped through `target`. `ValueNone` for a
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
        | SemanticConstraintKind.DefaultConstructor -> ValueSome TyparConstraintKindG.DefaultConstructor
        | SemanticConstraintKind.Unmanaged -> ValueSome TyparConstraintKindG.Unmanaged
        | SemanticConstraintKind.Enum u -> ValueSome(TyparConstraintKindG.Enum(target u))
        | SemanticConstraintKind.Delegate(a, r) -> ValueSome(TyparConstraintKindG.Delegate(target a, target r))
        | SemanticConstraintKind.OneOf _ -> ValueNone

    /// The store's form of the constraint, every embedded type mapped through `target`.
    let toSemantic (target: 'ty -> SemType) (kind: TyparConstraintKindG<'ty>) : SemanticConstraintKind =
        match kind with
        | TyparConstraintKindG.Equality -> SemanticConstraintKind.Equality
        | TyparConstraintKindG.Comparison -> SemanticConstraintKind.Comparison
        | TyparConstraintKindG.Struct -> SemanticConstraintKind.Struct
        | TyparConstraintKindG.ReferenceType -> SemanticConstraintKind.ReferenceType
        | TyparConstraintKindG.Nullness -> SemanticConstraintKind.Nullness
        | TyparConstraintKindG.NotNull -> SemanticConstraintKind.NotNull
        | TyparConstraintKindG.Coercion t -> SemanticConstraintKind.Coercion(target t)
        | TyparConstraintKindG.DefaultConstructor -> SemanticConstraintKind.DefaultConstructor
        | TyparConstraintKindG.Unmanaged -> SemanticConstraintKind.Unmanaged
        | TyparConstraintKindG.Enum u -> SemanticConstraintKind.Enum(target u)
        | TyparConstraintKindG.Delegate(a, r) -> SemanticConstraintKind.Delegate(target a, target r)

/// A statically resolved member constraint on a function scheme:
/// `when (^T or ^U) : (static member (+) : ^T * ^U -> ^V)`. `TyparIndices` is the trait's
/// support set, indexing the scheme's `Types`; `MemberName` is the compiled name
/// (`op_Addition`).
type MemberTrait =
    {
        TyparIndices: Block<int>
        MemberName: string
        ArgTypes: Block<FrozenType>
        ReturnType: FrozenType
    }

/// A member's, module function's or local function's scheme: its own typars, each with its
/// constraints, and its member traits. Every function typar referenced by a trait, or by a
/// type embedded in a constraint, indexes `Typars.Types`.
type FunctionScheme =
    private
        {
            typars: TyparList
            traits: Block<MemberTrait>
        }

    member this.Typars: TyparList = this.typars
    member this.Traits: Block<MemberTrait> = this.traits

    /// The type-kinded count: the emitted method-typar count.
    member this.TyparArity: int = this.typars.TypeArity

[<RequireQualifiedAccess>]
module FunctionScheme =

    /// Faults on a trait index, or on a function typar referenced by a constraint's or a
    /// trait's type, at or past `typars.TypeArity`.
    let create (typars: TyparList) (traits: Block<MemberTrait>) : FunctionScheme =
        let arity = typars.TypeArity

        let rec checkType (t: FrozenType) =
            match t with
            | FTFunctionTypar i when i >= arity ->
                failwithf "FunctionScheme: constraint references method typar %d, arity %d" i arity
            | t -> FrozenType.iterChildren checkType t

        TyparList.iter checkType typars

        for mt in traits do
            for i in mt.TyparIndices do
                if i >= arity then
                    failwithf "FunctionScheme: trait on typar %d, arity %d" i arity

            Block.iter checkType mt.ArgTypes
            checkType mt.ReturnType

        { typars = typars; traits = traits }

    /// A scheme over `typars` alone.
    let ofTypars (typars: TyparList) : FunctionScheme = create typars Block.empty

    /// A scheme over `n` positional, unconstrained typars.
    let unconstrained (n: int) : FunctionScheme = ofTypars (TyparList.positional n)

    /// The scheme of a binding that quantifies nothing.
    let monomorphic: FunctionScheme = ofTypars TyparList.empty

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
    | ConstrainedInterface of iface: TypeKey * ifaceArgs: Block<'ty>

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
    | ConstrainedInterface of iface: TypeKey * ifaceArgs: Block<'ty>

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
