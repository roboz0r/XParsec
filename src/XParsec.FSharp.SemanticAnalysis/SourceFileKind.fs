namespace XParsec.FSharp.SemanticAnalysis

/// Which kind of source file a path is. The EXTENSION decides.
[<RequireQualifiedAccess>]
type SourceFileKind =
    | Signature
    | Implementation

[<RequireQualifiedAccess>]
module SourceFileKind =

    /// `.fsi` ⇒ `Signature`, `.fs` ⇒ `Implementation`; `ValueNone` for any other extension.
    let tryOfPath (relative: string) : SourceFileKind voption =
        if relative.EndsWith(".fsi", System.StringComparison.Ordinal) then
            ValueSome SourceFileKind.Signature
        elif relative.EndsWith(".fs", System.StringComparison.Ordinal) then
            ValueSome SourceFileKind.Implementation
        else
            ValueNone
