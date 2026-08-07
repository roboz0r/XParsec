namespace XParsec.FSharp.SemanticAnalysis

[<AutoOpen>]
module SemTypeOps =

    let mkUnion (members: SemType seq) : SemType = SemType.MkUnion members
