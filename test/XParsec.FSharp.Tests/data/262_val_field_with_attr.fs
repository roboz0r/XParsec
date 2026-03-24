module Test

open System.Runtime.InteropServices

[<Struct; StructLayout(LayoutKind.Explicit)>]
type ByteUnion =
    [<FieldOffset(0)>]
    val mutable First: byte

    [<FieldOffset(0)>]
    val mutable Second: int
