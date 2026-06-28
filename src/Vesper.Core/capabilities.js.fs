namespace Vesper

#nowarn "42" // This construct is deprecated: it is only for use in the F# library

// JS-target impl (`.js.fs`) side: per-target identity for each language-capability
// anchor. Harvested in preference to `capabilities.fs` by the target-aware repr
// harvest (`ReferencedProject.buildProviderWith (Some "js")`), the intrinsic-repr
// analogue of the manifest's `inline-bodies-js` override (cf. `prim-types-exn.js.fs`).
//
// The equatable / comparable reprs deliberately keep the SAME `System.IEquatable\`1`
// / `System.IComparable\`1` byte-spelling as the CLR `.fs`. On JS these BCL
// interfaces are ERASED at runtime (the JS backend re-keys a custom-eq/comp impl to a
// registry-symbol method `obj[Symbol.for("vesper.equality"|"vesper.comparison")]` that the
// runtime dispatches on), but they survive as PROVIDER METADATA: `JsNativeSymbols` surfaces
// exactly those arity-suffixed names, and a user's `interface System.IEquatable<Self>`
// impl resolves through that provider to the same qualified name. So binding the
// capability identity to these reprs makes `resolveCapabilities` mint a
// `QualifiedName` that MATCHES the user impl — the reconciliation `validateCustomEqCompImpls`
// (`implementsSelf`) needs. A byte mismatch (namespace / backtick arity) silently
// trades one FS0378 for another, so this must stay in lock-step with
// `JsNativeSymbols.mkErasedGenericIface`'s keys.
//
// `disposable` keeps the SAME `System.IDisposable` byte-spelling as the CLR `.fs`:
// `JsNativeSymbols` surfaces exactly that name (a Vesper class implementing the disposal
// capability writes `interface System.IDisposable with member this.Dispose() = …`), so the
// impl resolves through the JS provider to the qualified name `caps.Disposable` matches,
// and the backend re-keys the matched `Dispose` to a native `[Symbol.dispose]()` method
// (the disposal analogue of `seq` → `[Symbol.iterator]`). `use` then lowers to
// `obj[Symbol.dispose]()`. Must stay in lock-step with `JsNativeSymbols`'s `System.IDisposable`.
//
// Iteration (`seq<'T>` / `IEnumerable<'T>`) is resolved off the existing `seq`
// abbreviation, not anchored here — see `capabilities.fsi`.

type disposable = (# "System.IDisposable" #)
type equatable<'T> = (# "System.IEquatable`1" #)
type comparable<'T> = (# "System.IComparable`1" #)
