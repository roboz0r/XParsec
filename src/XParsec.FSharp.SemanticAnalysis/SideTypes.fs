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
[<RequireQualifiedAccess>]
type ForInEnumeratorG<'ty> =
    /// The §4.2 interface path: lower through the `IEnumerable<'T>` /
    /// `IEnumerator<'T>` interface slots with `callvirt`. The default for the
    /// range form and for every source whose enumerable surface is (or includes)
    /// the interface — the only path codegen emits today.
    | Interface
    /// The §4.4 duck-typed path: the source exposes a public parameterless
    /// `GetEnumerator()` returning `EnumeratorTy`, which itself exposes
    /// `MoveNext(): bool` and a `Current` property *without* the source
    /// implementing `IEnumerable<'T>` (C#'s non-boxing `foreach`). The member
    /// `SymbolKey`s are the provider-interned identities (`GetEnumerator` on the
    /// source; `MoveNext` / `Current` on `EnumeratorTy`). `IsValueType` is the
    /// *enumerator*'s value-type-ness: it selects value-receiver emission for the
    /// `EnumeratorTy` member calls (`ldloca` + `constrained.`/`call`); `Dispose` is
    /// `ValueSome key` iff `EnumeratorTy : IDisposable`, else the `finally` is
    /// elided. Codegen for this arm has landed (`EmitExpr.fs`, §4.4). Still scoped
    /// to *external* sources with a *reference* source receiver — a value-type
    /// source and project-local sources remain gaps (`docs/get-enumerator-gaps.md`).
    | DuckTyped of
        enumeratorTy: 'ty *
        getEnumerator: SymbolKey *
        moveNext: SymbolKey *
        current: SymbolKey *
        isValueType: bool *
        dispose: SymbolKey voption

/// The `SemType`-domain `ForInEnumerator` (inference + `SideTables.ForInShape` +
/// the pre-freeze `TExpr.ForIn`). The frozen alias lives in `Tast.fs`'s `Frozen`
/// module; `Freeze` maps `enumeratorTy` through `toFrozen` via `TastConvert`.
type ForInEnumerator = ForInEnumeratorG<SemType>
