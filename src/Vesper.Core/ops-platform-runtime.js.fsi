namespace Vesper

// JS-only runtime entries — NOT part of the CLR contract, so they live here rather than
// in the shared `ops-platform.fsi`: the CLR `=` / `<>` / `hash` bodies use
// `EqualityComparer<^T>.Default` inline and the CLR arithmetic bodies are CIL mnemonics
// that fault by themselves, so a CLR-visible signature declaring these would be an
// over-declaration (a `val` with no CLR `.fs` body — an FS0240-style conformance gap).
// The `ops-platform.js.fs` bodies DO call them; the implementations are imported from
// `Vesper.Core.mjs` (a runtime asset, not a `.fs`) through the ordinary external-call
// path. Listed in the manifest's `files-js`, so the JS front end resolves them while the
// CLR contract does not carry them.
//
// Each entry is a real, NON-INLINE `val`. That is load-bearing, not incidental: a JS
// template that merely NAMES a function emits an unresolved global, because an import is
// registered only when the F# body actually CALLS the value (`JsImports.addRef`). So a
// clause that needs runtime code delegates to a `val` here rather than spelling a bare
// name in a `(# … #)` string.
[<AutoOpen>]
module StructuralRuntime =

    /// Structural equality of two values (JS runtime entry for aggregate operands).
    val structuralEquals: x: 'T -> y: 'T -> bool when 'T: equality

    /// Structural hash of a value (JS runtime entry for aggregate operands).
    val structuralHash: obj: 'T -> int when 'T: equality

[<AutoOpen>]
module ArithmeticRuntime =

    /// The zero-divisor guard behind every INTEGRAL `/` and `%` clause. CIL `div` / `rem`
    /// fault on a zero divisor; JS `/` yields `Infinity`, and `Infinity | 0` is `0` — so a
    /// bare masking template would quietly answer `0` where the CLR raises. This returns
    /// its argument (or throws), which is what lets a clause wrap it in the width mask and
    /// still read the divisor exactly ONCE: `(# "($0 / $1) & 0xFF" x (checkedDivisor y) #)`.
    /// A template repeating a `$N` hole would DOUBLE-EVALUATE the operand, which is why the
    /// check cannot be an inline ternary guard.
    ///
    /// ONE guard covers every integral width — the ≤32-bit widths are all JS `number` and
    /// int64 / uint64 are `bigint`s, and the body tests both zeros. `float` / `float32` deliberately
    /// do NOT route through it: IEEE division by zero yielding `Infinity` is the correct
    /// answer for a float, not a fault.
    val checkedDivisor: divisor: 'T -> 'T
