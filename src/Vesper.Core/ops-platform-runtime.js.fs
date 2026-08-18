namespace Vesper

open Vesper.JsInterop

[<AutoOpen>]
module StructuralRuntime =

    [<Import("structuralEquals", "./Vesper.Core.mjs")>]
    let structuralEquals (x: 'T) (y: 'T when 'T: equality) : bool = jsNative

    [<Import("structuralHash", "./Vesper.Core.mjs")>]
    let structuralHash (obj: 'T when 'T: equality) : int = jsNative
