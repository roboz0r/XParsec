namespace XParsec.FSharp.SemanticAnalysis

/// Accessibility as DECLARED, never pre-thresholded: each export filter applies its own
/// threshold.
[<RequireQualifiedAccess>]
type Accessibility =
    | Public
    | Internal
    | Private

/// The exportable identity of a module-level `let`. `InModule m` identifies the compiled module
/// type, because an F# module is a static class (`Vesper.Collections.ListModule::fold`).
type ModuleBindingInfo =
    {
        Container: ModuleContainer
        Name: string
    }

    member this.Key: SymbolKey = SymbolKeyOps.valueKey this.Container this.Name

    /// The named module this binding is declared in, or `ValueNone` for a top-level `let`.
    member this.DeclaringModule: ModuleKey voption =
        match this.Container with
        | ModuleContainer.InModule m -> ValueSome m
        | ModuleContainer.InNamespace _ -> ValueNone

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

/// A project-local generalised binding's typar bound, frozen over the binding's METHOD
/// typars: `target`'s typar leaves are `FTTypar(Method, idx)`.
[<RequireQualifiedAccess>]
type FrozenConstraint =
    /// `when 'a :> <ty>`; `typarIndex` is on the method axis.
    | Coercion of typarIndex: int * target: FrozenType

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

/// The `SemType` instantiations, spoken before the freeze.
type ForInGetEnum = ForInGetEnumG<SemType>
type ForInEnumMembers = ForInEnumMembersG<SemType>
type ForInPattern = ForInPatternG<SemType>
type ForInEnumerator = ForInEnumeratorG<SemType>
