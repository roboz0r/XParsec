namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// WHERE a diagnostic points, in the token space of the file that produced it.
[<RequireQualifiedAccess>]
type Site =
    /// No place in the file: a whole-file lex/parse failure, a conformance verdict
    /// about a signature rather than a position, a pass with no node in hand.
    | Nowhere
    /// One token.
    | At of token: int<token>
    /// A run of tokens, INCLUSIVE of both ends. `Between(t, t)` is `At t`; the
    /// module's smart constructor collapses it.
    | Between of first: int<token> * last: int<token>
    /// The GAP after a token — a zero-width position, for something that is
    /// MISSING. A recovery-inserted virtual `)` is here and nowhere else: it has
    /// no token index of its own, so it cannot be `At` anything.
    | After of token: int<token>

[<RequireQualifiedAccess>]
module Site =

    /// The place a token names. A VIRTUAL token yields `Nowhere` — it carries no lexed
    /// index, so there is nothing to point at. This is deliberately NOT `Anchor.ofToken`,
    /// which faults instead: an anchor may never be virtual, whereas a recovery-inserted
    /// token is exactly what a diagnostic wants to blame. A producer that means "a `)` is
    /// missing here" says `Site.After` of the REAL token before the gap, which is
    /// information this conversion does not have.
    let ofToken (tok: SyntaxToken) : Site =
        match tok.Index with
        | TokenIndex.Regular i -> Site.At i
        | TokenIndex.Virtual -> Site.Nowhere

    /// The place `tok` names, or `fallback` when it names none — for a caller holding an
    /// ENCLOSING span (the declaration the head sits in) that is still a real place when
    /// the head itself is a recovery insertion.
    let ofTokenOr (fallback: Site) (tok: SyntaxToken) : Site =
        match ofToken tok with
        | Site.Nowhere -> fallback
        | positioned -> positioned

    /// A run of tokens in the ONE canonical form the type admits: ends in order, and a
    /// one-token run collapsed to `At` so a renderer meets exactly one spelling of "here".
    /// The bare `Between` constructor can express neither rule, so every producer builds a
    /// run through here.
    let between (first: int<token>) (last: int<token>) : Site =
        let lo = min first last
        let hi = max first last

        if lo = hi then Site.At lo else Site.Between(lo, hi)

    /// The canonical form of any `Site`, including one a caller built with the bare
    /// `Between` constructor or a blob decoded from bytes nothing wrote. The codec
    /// normalises through this on BOTH sides, so a degenerate or inverted range cannot
    /// survive a round trip in one form on the way out and another on the way back.
    let normalise (s: Site) : Site =
        match s with
        | Site.Between(first, last) -> between first last
        | Site.Nowhere
        | Site.At _
        | Site.After _ -> s

    /// The run a SEQUENCE of tokens covers, from the leftmost that names a place to the
    /// rightmost. Recovery insertions are skipped rather than faulting the whole span — a
    /// run containing one is still somewhere — and `Nowhere` only when NO token names a
    /// place. THE span builder: `between` takes indices, this takes what a caller holds.
    let spanning (toks: SyntaxToken seq) : Site =
        let mutable lo = ValueNone
        let mutable hi = ValueNone

        for tok in toks do
            match tok.Index with
            | TokenIndex.Regular i ->
                lo <-
                    ValueSome(
                        match lo with
                        | ValueSome l -> min l i
                        | ValueNone -> i
                    )

                hi <-
                    ValueSome(
                        match hi with
                        | ValueSome h -> max h i
                        | ValueNone -> i
                    )
            | TokenIndex.Virtual -> ()

        match lo, hi with
        | ValueSome l, ValueSome h -> between l h
        | _ -> Site.Nowhere

    /// The run a written long-ident covers (`A.B.T` — first segment to last).
    let ofLongIdent (li: LongIdent<SyntaxToken>) : Site = spanning li.Idents
