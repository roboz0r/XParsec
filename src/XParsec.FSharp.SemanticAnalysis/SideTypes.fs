namespace XParsec.FSharp.SemanticAnalysis

// TAST-adjacent value types pulled ahead of `Tast.fs` in compile order so
// `TExpr` / `TastFile` can name them while the provider surface
// (`IExternalSymbolProvider`, `PassContext` in `SideTables`) compiles *after*
// `Tast` — which is what lets `IExternalSymbolProvider` name `TDecl` for the
// cross-package inline-body channel. All are provider-free pure data
// (`SemType` / `SymbolKey` / `NodeKey` only); their definitions used to sit in
// `SideTables.fs` (`ModuleMemberInfo`, `ForInEnumerator`) and in `PassContext`'s
// recursive group (`Diagnostic`, `Severity`).

// `RequireQualifiedAccess` because this now compiles ahead of the VesperLib /
// ReferencedProject extractors, whose `Result` plumbing uses a bare `Error`
// constructor — an unqualified `Severity.Error` case would shadow it. Every use
// site already writes `Severity.Error` / `.Warning` / `.Info` (or is qualified
// here).
[<RequireQualifiedAccess; Struct>]
type Severity =
    | Error
    | Warning
    | Info

/// TODO: range + sub-severities still pending. `Code` lets the sprint group
/// related diagnostics (e.g. for tooling); existing call sites pass `""` —
/// new ones should mint a short identifier (e.g. `"V001"`).
[<Struct>]
type Diagnostic =
    {
        Key: NodeKey
        Code: string
        Message: string
        Severity: Severity
    }

/// Where a module-level `let` should be emitted: a *named* holder type (an F#
/// module compiles to a static class) rather than the anonymous "Program" holder
/// the backend uses for top-level functions. Recorded for every binding inside a
/// `module Foo = …`; the backend keys this off the binding's `NodeKey` to give the
/// emitted static method its source `Name` on the `Holder` type in `Namespace`
/// (e.g. `Vesper.Collections.ListModule::fold`). The `Module` suffix follows the
/// F# rule that a module sharing a name with a type in its namespace compiles to
/// `<Name>Module`.
type ModuleMemberInfo =
    {
        Namespace: string option
        Holder: string
        Name: string
    }

/// How a `for x in src do …` (`TExpr.ForIn`) sources its enumerator — resolved by
/// `Unification.inferForIn` and read by `Freeze` to enrich the node, because
/// codegen can't re-derive the struct-vs-interface decision from the element type
/// alone. Defined here (ahead of `Tast.fs`
/// in compile order) so both the `SideTables` side table and the `TExpr.ForIn`
/// field can name it.
/// How codegen resolves the `GetEnumerator` handle of a `Pattern` for-in source —
/// one of the two independent axes of a duck-typed walk (the other is
/// `ForInEnumMembers`).
[<RequireQualifiedAccess>]
type ForInGetEnumG<'ty> =
    /// The source is *external* (a BCL type): codegen mints `GetEnumerator` via
    /// `ExternalMemberRef` from this provider-interned key (its `unit → E` return
    /// recovers the source instantiation).
    | External of getEnumerator: SymbolKey
    /// The source is a *project-local* class: codegen resolves `GetEnumerator`
    /// through `EmitResolve.resolveInstanceMember` against the source expression's
    /// type — no key needed.
    | Local
    /// Rung-3: the source is a *generic typar* (or a value whose only enumerable
    /// surface is a custom interface) constrained to a project-local seq interface
    /// `iface<ifaceArgs>` declaring `GetEnumerator(): E`. Codegen addresses the
    /// source receiver and emits `constrained. <Source> callvirt iface::GetEnumerator`,
    /// so a struct source dispatches by address (no box) and a class source by
    /// reference. The slot is resolved off the `EmittedInterface` registry by name.
    | ConstrainedInterface of iface: SymbolKey * ifaceArgs: EqArray<'ty>

/// How codegen resolves the `MoveNext` / `Current` handles of a `Pattern`
/// enumerator `E` — the second independent axis of a duck-typed walk. The element
/// type comes from the loop pattern, so only the external dispatch keys (when `E`
/// is external) are carried.
[<RequireQualifiedAccess>]
type ForInEnumMembersG<'ty> =
    /// `E` is *external* (a BCL `List<'T>.Enumerator`): codegen mints both members
    /// via `ExternalMemberRefOn` against `EnumeratorTy`'s instantiation (a T-free
    /// `MoveNext(): bool` can't recover the declaring type, so it must be supplied).
    | External of moveNext: SymbolKey * current: SymbolKey
    /// `E` is a *project-local* `TypeDef`: codegen resolves both members through
    /// `EmitResolve.resolveInstanceMember` against `EnumeratorTy`.
    | Local
    /// Rung-3: `E` is itself a *generic typar* constrained to an enumerator interface
    /// `iface<ifaceArgs>` declaring `MoveNext(): bool` and a `Current` property.
    /// Codegen emits `constrained. <E> callvirt iface::MoveNext / iface::get_Current`,
    /// dispatching a struct enumerator typar by address (no box). The slots are
    /// resolved off the `EmittedInterface` registry by name.
    | ConstrainedInterface of iface: SymbolKey * ifaceArgs: EqArray<'ty>

[<RequireQualifiedAccess>]
type ForInEnumeratorG<'ty> =
    /// The §4.2 interface path: lower through the `IEnumerable<'T>` /
    /// `IEnumerator<'T>` interface slots with `callvirt`. The default for the
    /// range form and for every source whose enumerable surface is (or includes)
    /// the interface.
    | Interface
    /// The §4.4 / Gap 2-3 *pattern* (duck-typed) path: the source exposes a public
    /// parameterless `GetEnumerator()` returning a concrete enumerator `E`
    /// (`EnumeratorTy`) that exposes `MoveNext(): bool` and a `Current` property
    /// *without* the source implementing `IEnumerable<'T>` (C#'s non-boxing
    /// `foreach`). `getEnumerator` and `members` carry, *independently*, how codegen
    /// resolves the source's `GetEnumerator` and `E`'s members — external
    /// (provider-interned keys) or project-local (`resolveInstanceMember`). The three
    /// historically-distinct forms are now the valid combinations of those two axes:
    /// external/external (a BCL source like `List<'T>`), local/external (a user
    /// source whose `E` is a BCL enumerator), and local/local (a user source with a
    /// user `E`); external/local is unrepresentable in practice (an external source
    /// never hands back a project-local enumerator). `isValueType` is `E`'s
    /// value-type-ness: it selects value-receiver emission (`ldloca` + a by-address
    /// `call`, or `constrained.` for disposal) and the non-boxing struct walk.
    /// `dispose` is `true` iff `E : IDisposable`; disposal is always the
    /// `System.IDisposable::Dispose` interface slot codegen mints itself (so no key
    /// is carried), else the `finally` is elided (C# parity).
    | Pattern of
        enumeratorTy: 'ty *
        getEnumerator: ForInGetEnumG<'ty> *
        members: ForInEnumMembersG<'ty> *
        isValueType: bool *
        dispose: bool

/// The `SemType`-domain axes + `ForInEnumerator` (inference + `SideTables.ForInShape`
/// + the pre-freeze `TExpr.ForIn`). The frozen aliases live in `Tast.fs`'s `Frozen`
/// module; `Freeze` maps the `'ty` payloads through `toFrozen` via `TastConvert`.
type ForInGetEnum = ForInGetEnumG<SemType>
type ForInEnumMembers = ForInEnumMembersG<SemType>
type ForInEnumerator = ForInEnumeratorG<SemType>
