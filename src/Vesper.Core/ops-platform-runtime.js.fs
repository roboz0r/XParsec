namespace Vesper

[<AutoOpen>]
module JsInterop =

    let inline jsNative<'T> : 'T =
        (# "(() => { throw new Error('jsNative: this binding is served by its [<Import>] declaration') })()" : 'T #)

[<AutoOpen>]
module StructuralRuntime =

    [<Import("structuralEquals", "./Vesper.Core.mjs")>]
    let structuralEquals (x: 'T) (y: 'T when 'T: equality) : bool = jsNative

    [<Import("structuralHash", "./Vesper.Core.mjs")>]
    let structuralHash (obj: 'T when 'T: equality) : int = jsNative

[<AutoOpen>]
module ArithmeticRuntime =

    [<Import("checkedDivisor", "./Vesper.Core.mjs")>]
    let checkedDivisor (divisor: 'T) : 'T = jsNative
