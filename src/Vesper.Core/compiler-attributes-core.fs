namespace Vesper

type AttributeTargets =
    | Assembly = 1
    | Module = 2
    | Class = 4
    | Struct = 8
    | Enum = 16
    | Constructor = 32
    | Method = 64
    | Property = 128
    | Field = 256
    | Event = 512
    | Interface = 1024
    | Parameter = 2048
    | Delegate = 4096
    | ReturnValue = 8192
    | GenericParameter = 16384
    | All = 32767

type AttributeUsageAttribute(validOn: AttributeTargets) =
    inherit Attribute()
    let mutable allowMultiple = false
    let mutable isInherited = true
    member _.ValidOn = validOn

    member _.AllowMultiple
        with get () = allowMultiple
        and set v = allowMultiple <- v

    member _.Inherited
        with get () = isInherited
        and set v = isInherited <- v

// FSharp.Core/prim-types.fs:56
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type SealedAttribute(value: bool) =
    inherit Attribute()
    member _.Value = value
    new() = SealedAttribute(true)

// FSharp.Core/prim-types.fs:359
[<Sealed>]
type AutoOpenAttribute(path: string) =
    inherit Attribute()
    member _.Path = path
    new() = AutoOpenAttribute("")
