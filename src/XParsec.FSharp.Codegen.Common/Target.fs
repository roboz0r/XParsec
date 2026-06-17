namespace XParsec.FSharp.Codegen.Common

/// Backend target tag constants — single source for the suffix strings passed to
/// `ReferencedProject.resolveImpl` / `resolveInlineBodies`.
[<RequireQualifiedAccess>]
module Target =

    [<Literal>]
    let Js = "js"
