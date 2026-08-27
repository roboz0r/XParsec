namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Parser

/// An `open` written in this unit, with the positional facts the flat dotted `Prefixes`
/// list cannot express.
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

/// The active namespace prefixes in a lexical scope, most-recent-first (a later `open`
/// shadows an earlier one). Under `open System.Collections.Generic`, a bare
/// `EqualityComparer` qualifies to `System.Collections.Generic.EqualityComparer`.
type OpenScope =
    {
        /// Empty prefixes are never stored.
        Prefixes: string list
        /// The opens WRITTEN IN THIS FILE, with their positions: what orders a name an
        /// `open` brings against the declarations around it.
        Locals: LocalOpen list
        /// Module-abbrev aliases (`module R = A.B.C` ⇒ `"R" → "A.B.C"`), expanded
        /// on the anchor segment of a dotted name before probing.
        Abbrevs: Map<string, string>
    }

module OpenScope =

    let empty: OpenScope =
        {
            Prefixes = []
            Locals = []
            Abbrevs = Map.empty
        }

    let private candidates (scope: OpenScope) (name: string) : string list =
        let expanded =
            let dot = name.IndexOf '.'
            let anchor = if dot < 0 then name else name.Substring(0, dot)

            match Map.tryFind anchor scope.Abbrevs with
            | Some target -> target + (if dot < 0 then "" else name.Substring dot)
            | None -> name

        expanded
        :: [
            for p in scope.Prefixes do
                if p.Length > 0 then
                    yield p + "." + expanded
        ]

    /// The fully-qualified name `name` resolves under, trying the bare/abbrev-expanded
    /// name then each active prefix; first `probe` hit wins.
    let tryQualify (scope: OpenScope) (probe: string -> bool) (name: string) : string voption =
        let rec go cs =
            match cs with
            | [] -> ValueNone
            | c :: rest -> if probe c then ValueSome c else go rest

        go (candidates scope name)
