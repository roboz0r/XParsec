namespace XParsec.FSharp.Codegen.Common

/// Backend target tags. SemanticAnalysis stays target-agnostic — it captures
/// `impl-<t>` / `inline-bodies-<t>` overrides by bare suffix and never enumerates
/// the target set (codegen-js-steps.md F0). These constants are the *backend*
/// side's single source for the suffix strings it hands to
/// `ReferencedProject.resolveImpl` / `resolveInlineBodies` and
/// `SymbolProviders.contractInlineBodiesFor`, so the literal `"js"` lives in one
/// place rather than scattered across call sites.
[<RequireQualifiedAccess>]
module Target =

    /// The JS backend's manifest suffix (`inline-bodies-js`) and `resolve*` tag.
    [<Literal>]
    let Js = "js"
