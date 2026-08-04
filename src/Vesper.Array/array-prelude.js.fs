namespace Vesper.Collections

module ArrayPrelude =

    /// Identical to the CLR body, as `GetArray` / `SetArray` / `GetArrayLength` are:
    /// `newarr` is a named IR node this backend implements in its own terms
    /// (`Array($0).fill(null)` — dense, so no slot is a hole and unset slots read
    /// `null` rather than an element-type zero). Spelling it here rather than in
    /// `array.fs` is the point: the divergence, when it comes, has a file to happen in.
    ///
    /// If that divergence is a `$0` TEMPLATE rather than a mnemonic, the JS purity
    /// guard needs the same edit: it keys duplicability on the OPCODE, and an
    /// allocation judged pure is duplicated into a second array at each use site.
    let inline NewArray (count: int) : 'T[] = (# "newarr !0" type ('T) count : 'T[] #)
