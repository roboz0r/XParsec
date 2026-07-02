namespace XParsec.FSharp.SemanticAnalysis

/// The short `mkUnion` alias for `SemType.MkUnion`, the smart constructor for
/// anonymous unions. The canonicalisation logic now lives as `SemType.MkUnion` /
/// `UnionMembers` (backed by the set-semantic `EqSet`) in `SemanticInfo.fs`. This
/// module just keeps the short name every producer already calls.
[<AutoOpen>]
module SemTypeOps =

    /// The ONLY sanctioned producer of `TyOr` (delegates to `SemType.MkUnion`):
    ///   * flatten  `(A | B) | C ≡ A | B | C`
    ///   * dedup    `A | A ≡ A`        (structural `=`)
    ///   * collapse `TyOr [A] ≡ A`     (a one-member union is just that member)
    ///   * set id   `string | int ≡ int | string` (EqSet order-insensitive equality,
    ///              NOT a canonical sort — no total order on `SemType` exists)
    /// `mkUnion []` is `TyOr []` = `never` (bottom). Members are not resolved /
    /// zonked here — canonicalisation is purely structural; a member that is itself
    /// a still-free `TyVar` is a v2 concern (annotation-driven v1 members are
    /// ground).
    let mkUnion (members: SemType seq) : SemType = SemType.MkUnion members
