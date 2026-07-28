namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// WHERE a node sits: the index of its anchor token in the `Lexed` of the file it belongs
/// to, or NOWHERE for a node no source spells. An index and not a `SyntaxToken` because the
/// text, span and line the struct carried are all recoverable from the index against that
/// `Lexed`, at a quarter of the width on the columns that dominate a frozen file.
///
/// An index is a position IN ONE FILE and means nothing against another's tokens, which is
/// why a tree leaving its pool (`Wire.TDecl`) sits `nowhere` throughout.
[<Struct>]
type Anchor =
    private
        {
            /// Negative for `nowhere`; no token index is. Private, so `Index` is the only
            /// reading of it and no consumer meets a raw negative.
            Raw: int<token>
        }

    member this.Index: int<token> voption =
        if this.Raw < 0<token> then
            ValueNone
        else
            ValueSome this.Raw

[<RequireQualifiedAccess>]
module Anchor =

    /// A node no source spells: a lowering's own derived node, an `.fsi` contract's
    /// reconstructed pattern, a declaration's pattern-less binder slot.
    let nowhere: Anchor = { Raw = -1<token> }

    /// THE anchor of a node of a FILE, taken from the token that spells it. A node with no
    /// anchor does not come through here — it takes `nowhere`.
    ///
    /// A VIRTUAL token faults: it carries no lexed index, so there is nothing to store and
    /// nothing that could be resolved back. Every anchor rule
    /// (`CstKeys.firstTokenOfExpr`/`firstTokenOfPat`) picks a keyword, a real operator, a
    /// real opening delimiter, or recurses — N individually correct choices, and this is the
    /// one place they are all answerable.
    let ofToken (tok: SyntaxToken) : Anchor =
        match tok.Index with
        | TokenIndex.Regular i -> { Raw = i }
        | TokenIndex.Virtual ->
            failwithf "Anchor.ofToken: the VIRTUAL token %A names no place in the source, so it anchors nothing" tok

    /// The blob form, for the one consumer that must put an anchor on the wire and read it
    /// back (`FrozenCodecPrimitives`). Nothing else may construct from an integer.
    let toStored (a: Anchor) : int =
        match a.Index with
        | ValueSome i -> int i
        | ValueNone -> -1

    let ofStored (raw: int) : Anchor =
        if raw < 0 then nowhere else { Raw = raw * 1<token> }
