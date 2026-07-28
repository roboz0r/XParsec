namespace XParsec.FSharp.SemanticAnalysis

// TAST-adjacent value types pulled ahead of `Tast.fs` in compile order so
// `TExpr` / `TastFile` can name them while the provider surface
// (`IExternalSymbolProvider`, `PassContext.fs`) compiles *after* `Tast` — which is
// what lets `IExternalSymbolProvider` name `TDecl` for the cross-package
// inline-body channel. All are provider-free pure data (`SemType` / `SymbolKey` /
// `NodeKey` only).

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

/// A SECONDARY place a diagnostic points at, and what it means there. Labelled, because
/// an unlabelled list of extra positions leaves a consumer guessing from list order what
/// each one meant. The unclosed-delimiter diagnostic is the shape: primary at the hole
/// where the delimiter belonged, one label back on the delimiter left open.
type Label = { Site: Site; Message: string }

/// TODO: sub-severities still pending. `Code` lets the sprint group related
/// diagnostics (e.g. for tooling); most call sites pass `""` — new ones should mint a
/// short identifier (e.g. `"V001"`).
///
/// A REFERENCE type: it is allocated only on error paths and copied through
/// `ResizeArray` / `list` at every seam, so the struct layout bought nothing while
/// bounding what the record may carry.
///
/// NAME COLLISION: `XParsec.FSharp.Parser` declares its own `Diagnostic` (the parser's
/// recovery record), so in any file that `open`s the parser the bare name binds to THAT
/// one. Such files alias or fully qualify this type; the alias is what the `type
/// Diagnostic = …` lines elsewhere in this assembly are for.
type Diagnostic =
    {
        Code: string
        Message: string
        Severity: Severity
        /// The primary position — what a one-line renderer points at.
        Site: Site
        /// Secondary positions, in the order a renderer should show them.
        Related: Label list
    }

[<RequireQualifiedAccess>]
module Diagnostic =

    /// An error about a whole unit or package rather than about a place in one: a lex or
    /// parse failure, a driver refusal, a conformance verdict on a signature. One
    /// constructor, so "names no place" is spelled `Site.Nowhere` exactly once and a field
    /// added to the record does not have to be answered at every such site.
    let nowhere (code: string) (message: string) : Diagnostic =
        {
            Code = code
            Message = message
            Severity = Severity.Error
            Site = Site.Nowhere
            Related = []
        }

/// Declared accessibility of an EXPORTED entity — a token-free 3-state stored
/// HONESTLY (never pre-thresholded), so the two export filters read ONE fact: the
/// cross-package `.fsi` extractor keeps public-only, the intra-assembly file→file
/// projection keeps internal-or-better (same-assembly visible). The CST
/// `Access<_>` keyword token is classified into this at the Elaborate populate
/// site and threaded to freeze on `TastFile.Accessibility`. An entity ABSENT from
/// that table is `Public` — the F# default for an unmarked declaration.
[<RequireQualifiedAccess>]
type Accessibility =
    | Public
    | Internal
    | Private

/// Where a module-level `let` should be emitted: a *named* holder type (an F#
/// module compiles to a static class) rather than the anonymous "Program" holder
/// the backend uses for top-level functions. Recorded for every binding inside a
/// `module Foo = …`; the backend keys this off the binding's `NodeKey` to give the
/// emitted static method its source `Name` on the holder type (e.g.
/// `Vesper.Collections.ListModule::fold`). The `Module` suffix follows the F# rule
/// that a module sharing a name with a type in its namespace compiles to
/// `<Name>Module`, and is applied by the producer, so `Holder.Name` IS the compiled
/// holder-type name.
///
/// `Holder` is a `ModuleKey` — the containment chain, not a `(namespace, module)`
/// pair of strings. That is what makes `Key` a DIRECT construction rather than a
/// re-parse: the binding's identity is the chain plus the name, with nothing thrown
/// away at the boundary and nothing guessed back. `Freeze` mints an inline value's
/// published `SymbolKey` through it (an inline binding never reaches codegen, so
/// nothing else would ever mint its identity), and it agrees BY CONSTRUCTION with
/// the key a `.fsi` contract extractor mints for the same binding — both build the
/// same holder chain from the same three facts.
type ModuleBindingInfo =
    {
        Holder: ModuleKey
        Name: string
    }

    /// The binding's interned identity — the key a use-site `TExpr.External` carries.
    member this.Key: SymbolKey =
        SymbolKeyOps.valueKey (ModuleHolder.InModule this.Holder) this.Name

    /// The compiled holder-TYPE name (the module's own simple name, `ModuleSuffix`
    /// already applied). What the backend names the emitted static class.
    member this.HolderName: string = this.Holder.Name

    /// The declaring namespace, dotted; `None` for the global namespace.
    member this.Namespace: string option =
        match SymbolKeyOps.holderFullName this.Holder.Holder with
        | "" -> None
        | ns -> Some ns

/// The per-source-lambda value-struct closure verdict, keyed
/// (in the side table / `TastFile`) by the lambda argument's `NodeKey`. One record
/// makes the subset invariant explicit: a lambda threaded through a `:> Fun<a,b>`/`:> Fun<a,b,c>`
/// slot always has an `Arity`; only one whose constrained `'TFunc` *also* surfaces in
/// the producing combinator's RESULT nominal carries a `ResultTyparPos`. Recorded in
/// `inferApp`; `Arity` read by codegen's `discoverClosures` (flat `Invoke` sizing),
/// `ResultTyparPos` by `ClosureVerdictRewrite` (stored-slot / result-type rewrite).
type FunVerdict =
    {
        /// The flat `FunN` arity the slot constrains the argument to: `1` for a
        /// `Fun<a,b>` slot, `2` for a `Fun<a,b,c>` slot.
        Arity: int
        /// The type-argument POSITION the lambda's constrained `'TFunc` occupies in
        /// the combinator's RESULT nominal (`0` for `mk : ('TF:>Fun) -> Holder<'TF>`),
        /// or `ValueNone` for a terminal combinator (`fold`/`apply2`) whose result
        /// does not mention `'TFunc` — such a binding needs no slot rewrite.
        ResultTyparPos: int voption
    }

/// A project-local generalised binding's typar
/// `when 'a :> <ty>` bound, frozen as a method-axis-indexed template. The sibling
/// of `ExternalConstraint` (`ExternalSymbols.fs`) for the project-local head —
/// `target` is a `FrozenType` over the binding's METHOD typars, i.e. its typar
/// leaves are `FTTypar(Method, idx)` carrying the SAME indices the binding's body
/// freezes with. Threaded end-to-end onto `StaticFn`/`StaticMethodRef` (the
/// per-binding `FrozenConstraint list` is keyed by `NodeKey` in `TastFile`) and
/// read by the call-site phantom-typar solve (`EmitCall`) to recover a phantom
/// typar (`fold`'s `'E`) from the constrained source's seq interface impl.
[<RequireQualifiedAccess>]
type FrozenConstraint =
    /// `when 'a :> <ty>` — coercion. `typarIndex` is the constrained typar's
    /// method-axis position; `target` is the required supertype as a `FrozenType`
    /// template over the binding's method typars.
    | Coercion of typarIndex: int * target: FrozenType

/// How a `for x in src do …` (`TExpr.ForIn`) sources its enumerator — resolved by
/// `Unification.inferForIn` and read by `Elaborate` to enrich the node, because
/// codegen can't re-derive the struct-vs-interface decision from the element type
/// alone. Defined here (ahead of `Tast.fs`
/// in compile order) so both the `ForInShape` side table and the `TExpr.ForIn`
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
    /// The source is a *generic typar* (or a value whose only enumerable
    /// surface is a custom interface) constrained to a project-local seq interface
    /// `iface<ifaceArgs>` declaring `GetEnumerator(): E`. Codegen addresses the
    /// source receiver and emits `constrained. <Source> callvirt iface::GetEnumerator`,
    /// so a struct source dispatches by address (no box) and a class source by
    /// reference. The slot is resolved off the `EmittedInterface` registry by name.
    | ConstrainedInterface of iface: TypeKey * ifaceArgs: EqArray<'ty>

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
    /// `E` is itself a *generic typar* constrained to an enumerator interface
    /// `iface<ifaceArgs>` declaring `MoveNext(): bool` and a `Current` property.
    /// Codegen emits `constrained. <E> callvirt iface::MoveNext / iface::get_Current`,
    /// dispatching a struct enumerator typar by address (no box). The slots are
    /// resolved off the `EmittedInterface` registry by name.
    | ConstrainedInterface of iface: TypeKey * ifaceArgs: EqArray<'ty>

[<RequireQualifiedAccess>]
type ForInEnumeratorG<'ty> =
    /// The interface path: lower through the `IEnumerable<'T>` /
    /// `IEnumerator<'T>` interface slots with `callvirt`. The default for the
    /// range form and for every source whose enumerable surface is (or includes)
    /// the interface.
    | Interface
    /// The *pattern* (duck-typed) path: the source exposes a public
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

/// The `SemType`-domain axes + `ForInEnumerator` (inference + `PassContext`'s
/// `ForInShape` table + the pre-freeze `TExpr.ForIn`). The frozen aliases live in `Tast.fs`'s `Frozen`
/// module; `Elaborate` maps the `'ty` payloads through `toFrozen` via `TastConvert`.
type ForInGetEnum = ForInGetEnumG<SemType>
type ForInEnumMembers = ForInEnumMembersG<SemType>
type ForInEnumerator = ForInEnumeratorG<SemType>
