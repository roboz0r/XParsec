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

[<Sealed>]
type CallAtMostOnceAttribute() =
    inherit Attribute()

[<Sealed>]
type StructuralEqualityAttribute() =
    inherit Attribute()

[<Sealed>]
type StructuralComparisonAttribute() =
    inherit Attribute()

[<Sealed>]
type ReferenceEqualityAttribute() =
    inherit Attribute()

[<Sealed>]
type NoEqualityAttribute() =
    inherit Attribute()

[<Sealed>]
type CustomEqualityAttribute() =
    inherit Attribute()

[<Sealed>]
type CustomComparisonAttribute() =
    inherit Attribute()

[<Sealed>]
type NoComparisonAttribute() =
    inherit Attribute()

[<Sealed>]
type AllowNullLiteralAttribute() =
    inherit Attribute()

[<Sealed>]
type GlobalAttribute() =
    inherit Attribute()

[<Sealed>]
type ImportAttribute(selector: string, path: string) =
    inherit Attribute()

// FSharp.Core/prim-types.fs:183
[<Sealed>]
type StructAttribute() =
    inherit Attribute()

// FSharp.Core/prim-types.fs:354
[<Sealed>]
type RequireQualifiedAccessAttribute() =
    inherit Attribute()

// FSharp.Core/prim-types.fs:359
[<Sealed>]
type AutoOpenAttribute(path: string) =
    inherit Attribute()
    member _.Path = path
    new() = AutoOpenAttribute("")

// FSharp.Core/prim-types.fs:177
[<Sealed>]
type CompiledNameAttribute(compiledName: string) =
    inherit Attribute()
    member _.CompiledName = compiledName

// FSharp.Core/prim-types.fs:44
type CompilationRepresentationFlags =
    | None = 0
    | Static = 1
    | Instance = 2
    | ModuleSuffix = 4
    | UseNullAsTrueValue = 8
    | Event = 16

// FSharp.Core/prim-types.fs:246
[<Sealed>]
type CompilationRepresentationAttribute(flags: CompilationRepresentationFlags) =
    inherit Attribute()
    member _.Flags = flags

// FSharp.Core/prim-types.fs:208
[<Sealed>]
type LiteralAttribute() =
    inherit Attribute()

// FSharp.Core/prim-types.fs:188
[<Sealed>]
type MeasureAttribute() =
    inherit Attribute()

// FSharp.Core/prim-types.fs:313
[<Sealed>]
type CompilerMessageAttribute(message: string, messageNumber: int) =
    inherit Attribute()
    let mutable isError = false
    let mutable isHidden = false
    member _.Message = message
    member _.MessageNumber = messageNumber

    member _.IsError
        with get () = isError
        and set v = isError <- v

    member _.IsHidden
        with get () = isHidden
        and set v = isHidden <- v

// FSharp.Core/prim-types.fs:68
[<Sealed>]
type EqualityConditionalOnAttribute() =
    inherit Attribute()

// FSharp.Core/prim-types.fs:344
[<Sealed>]
type GeneralizableValueAttribute() =
    inherit Attribute()

// FSharp.Core/prim-types.fs:260
[<Sealed>]
type ExperimentalAttribute(message: string) =
    inherit Attribute()
    member _.Message = message

// FSharp.Core/prim-types.fs:90
[<Sealed>]
type DefaultAugmentationAttribute(value: bool) =
    inherit Attribute()
    member _.Value = value
