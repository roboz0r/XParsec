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

/// The opens and module abbreviations in force in a lexical scope.
type OpenScope =
    {
        /// The opens WRITTEN IN THIS FILE, most-recent-first, with their positions: what
        /// orders a name an `open` brings against the declarations around it.
        Locals: LocalOpen list
        /// Module-abbrev aliases (`module R = A.B.C` ⇒ `"R" → "A.B.C"`), expanded
        /// on the anchor segment of a dotted name before probing.
        Abbrevs: Map<string, string>
    }

module OpenScope =

    let empty: OpenScope = { Locals = []; Abbrevs = Map.empty }
