namespace XParsec.FSharp.SemanticAnalysis

/// An `open` written in this unit, with its position.
[<NoComparison>]
type LocalOpen =
    {
        /// The dotted path as WRITTEN (`"A"`, `"N.A"`).
        Path: string
        /// The dotted SOURCE path of the scope the `open` is written in (`"N.M"`; `""` at
        /// the top of an anonymous module).
        Scope: string
        /// How many `module`s enclose the `open` (a namespace body is 0).
        ScopeDepth: int
        /// Source offset of the `open` keyword: what orders it against the declarations
        /// of its own scope.
        Offset: int
    }

/// A `module R = A.B.C` written in this unit, with its position: one segment bound to one
/// container, for the rest of the file's scope it is written in.
[<NoComparison>]
type LocalAbbrev =
    {
        /// The segment the abbreviation binds (`"R"`).
        Alias: string
        /// The dotted target path as WRITTEN (`"A.B.C"`).
        Path: string
        /// The dotted SOURCE path of the scope the abbreviation is written in (`"N.M"`; `""`
        /// at the top of an anonymous module).
        Scope: string
        /// How many `module`s enclose the abbreviation (a namespace body is 0).
        ScopeDepth: int
        /// Source offset of the `module` keyword: what orders the alias against the
        /// declarations of its own scope.
        Offset: int
    }

/// One scope-shaping declaration written in this unit: an `open` or a module abbreviation.
[<RequireQualifiedAccess; NoComparison>]
type LocalScopeDecl =
    | Open of LocalOpen
    | Abbrev of LocalAbbrev

[<RequireQualifiedAccess>]
module LocalScopeDecl =

    let offset (d: LocalScopeDecl) : int =
        match d with
        | LocalScopeDecl.Open o -> o.Offset
        | LocalScopeDecl.Abbrev a -> a.Offset

/// The `open`s and module abbreviations in force in a lexical scope, WRITTEN IN THIS FILE,
/// most-recent-first, with their positions: what orders a name they bring against the
/// declarations around them.
type OpenScope = LocalScopeDecl list

module OpenScope =

    let empty: OpenScope = []
