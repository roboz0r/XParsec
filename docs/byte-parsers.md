---
category: Documentation
categoryindex: 0
index: 4
---

# Binary Parsers

The `XParsec.ByteParsers` module provides a set of highly efficient parsers for reading primitive numeric types directly from a stream of bytes. This is essential for parsing binary file formats, network protocols, and other non-textual data.

These parsers are designed to work with inputs where the stream item type `'T` is `byte`. They are built on top of the performant `System.Buffers.Binary.BinaryPrimitives` API.

**Note:** This module is only available when targeting .NET runtimes. Some newer types like `Half` and `Int128` require a modern .NET version (i.e., .NET 8+).

## Naming Convention

All parsers in this module follow a consistent naming pattern:

`p<Type><Endianness>`

- **`<Type>`**: The numeric type to parse (e.g., `Int16`, `Float64`, `UInt32`).
- **`<Endianness>`**: The byte order of the data in the stream.
  - `BE`: **Big-Endian** (most significant byte first).
  - `LE`: **Little-Endian** (least significant byte first).

## Example Usage

Parsing binary data involves creating a `Reader` from a byte source (like a `byte[]` or a `Stream`) and then applying the appropriate parsers.

```fsharp
open System
open XParsec
open XParsec.ByteParsers

// Let's define a simple binary record:
// - A 16-bit "magic number" (unsigned, big-endian)
// - A 32-bit "payload size" (unsigned, little-endian)
type Header = { Magic: uint16; PayloadSize: uint32 }

let pHeader = parser {
    let! magic = pUInt16BE
    let! size = pUInt32LE
    return { Magic = magic; PayloadSize = size }
}

// Example byte data:
// Magic: 0xCAFE (big-endian) -> [| 0xCAuy; 0xFEuy |]
// Size: 1024 (little-endian) -> [| 0x00uy; 0x04uy; 0x00uy; 0x00uy |]
let data = [| 0xCAuy; 0xFEuy; 0x00uy; 0x04uy; 0x00uy; 0x00uy |]

// Run the parser
match run pHeader (Reader.ofArray data ()) with
| Ok success ->
    printfn $"Parsed Header: Magic=0x{success.Parsed.Magic:X}, Size={success.Parsed.PayloadSize}"
    // Output: Parsed Header: Magic=0xCAFE, Size=1024
| Error err ->
    printfn "Failed to parse header."
```

---

## Signed Integer Parsers

| Parser | Description |
|---|---|
| `pInt16BE` / `pInt16LE` | Parses a 16-bit signed integer (`int16`). |
| `pInt32BE` / `pInt32LE` | Parses a 32-bit signed integer (`int32`). |
| `pInt64BE` / `pInt64LE` | Parses a 64-bit signed integer (`int64`). |
| `pInt128BE` / `pInt128LE` | Parses a 128-bit signed integer (`Int128`). (.NET 8+) |
| `pIntPtrBE` / `pIntPtrLE` | Parses a native-sized signed integer (`IntPtr`). (.NET 8+) |

---

## Unsigned Integer Parsers

| Parser | Description |
|---|---|
| `pUInt16BE` / `pUInt16LE` | Parses a 16-bit unsigned integer (`uint16`). |
| `pUInt32BE` / `pUInt32LE` | Parses a 32-bit unsigned integer (`uint32`). |
| `pUInt64BE` / `pUInt64LE` | Parses a 64-bit unsigned integer (`uint64`). |
| `pUInt128BE` / `pUInt128LE`| Parses a 128-bit unsigned integer (`UInt128`). (.NET 8+) |
| `pUIntPtrBE` / `pUIntPtrLE`| Parses a native-sized unsigned integer (`UIntPtr`). (.NET 8+) |

---

## Floating-Point Parsers

| Parser | Description |
|---|---|
| `pFloat16BE` / `pFloat16LE` | Parses a 16-bit half-precision float (`Half`). (.NET 8+) |
| `pFloat32BE` / `pFloat32LE` | Parses a 32-bit single-precision float (`float32`). |
| `pFloat64BE` / `pFloat64LE` | Parses a 64-bit double-precision float (`float`). |
