namespace System

// JS-only capability compat shim. Abbreviates the BCL interface spellings to the
// canonical BCL-free `Vesper.*` capabilities (`capabilities.fsi`), so BCL-spelled
// source resolves on JS the same way the real BCL metadata answers it on CLR — the
// front end stays target-neutral while the contract proper names no BCL type. This
// is the ONLY place the BCL spelling appears on JS. Appended after `capabilities.fsi`
// via the manifest's `files-js` key, so its RHS is already in the registry; an
// `interface System.IDisposable` impl thus records the canonical `Vesper.disposable`
// key (`CapabilityFace = ValueNone` on JS), which the backend routes to
// `[Symbol.dispose]`. Replaces the retired `JsNativeSymbols` fabrication and
// `capabilities.js.fs`.

type IDisposable = Vesper.disposable

type IEquatable<'T> = Vesper.equatable<'T>

type IComparable<'T> = Vesper.comparable<'T>
