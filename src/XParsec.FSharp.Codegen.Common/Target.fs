namespace XParsec.FSharp.Codegen.Common

/// Backend target tag constants — the `<t>` a backend resolves a package directory against to
/// reach its `manifest.<t>.toml`.
[<RequireQualifiedAccess>]
module Target =

    [<Literal>]
    let Clr = "clr"

    [<Literal>]
    let Js = "js"
