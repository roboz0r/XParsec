namespace Vesper

[<AutoOpen>]
module JsInterop =

    let inline jsNative<'T> : 'T =
        (# "(() => { throw new Error('jsNative: this binding is served by its [<Import>] declaration') })()" : 'T #)

[<AutoOpen>]
module ArithmeticRuntime =

    // Inline, not `[<Import>]`-served: this file precedes `compiler-attributes.fsi`, so no
    // attribute is a compiler marker here. The arrow binds the divisor once; `d === 0` also
    // catches `-0`, and `0n` catches int64's `bigint`.
    let inline checkedDivisor (divisor: 'T) : 'T =
        (# "((d) => (d === 0 || d === 0n) ? (() => { throw new Error('Attempted to divide by zero.') })() : d)($0)"
            divisor
            : 'T #)
