namespace XParsec.FSharp.Codegen.Common

/// Backend target tag constants — the `<t>` a package manifest's `[targets.<t>]` table
/// keys its per-target file lists under.
[<RequireQualifiedAccess>]
module Target =

    [<Literal>]
    let Clr = "clr"

    [<Literal>]
    let Js = "js"
