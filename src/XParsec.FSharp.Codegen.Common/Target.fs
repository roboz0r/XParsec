namespace XParsec.FSharp.Codegen.Common

/// Backend target tag constants — the `[targets.<t>]` table names a manifest keys its
/// per-target lists under. The CLR is an ordinary target here, not an unnamed base.
[<RequireQualifiedAccess>]
module Target =

    [<Literal>]
    let Clr = "clr"

    [<Literal>]
    let Js = "js"
