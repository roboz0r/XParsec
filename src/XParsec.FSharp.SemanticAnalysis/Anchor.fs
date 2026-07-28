namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Where a node SITS: the INDEX of its anchor token in the `Lexed` of the file it belongs
/// to. A frozen node stores this and not the `SyntaxToken` itself — the token's text, span
/// and line are all recoverable from the index against that `Lexed`, and the index is four
/// bytes where the struct is sixteen, on the columns that dominate a frozen file's size.
///
/// An index is a position IN ONE FILE and means nothing against another's tokens, which is
/// why a body spliced from another unit is moved onto the call site as it lands
/// (`Inline.spliceAt`) rather than carrying the index it was compiled with.
///
/// ABSENCE IS THE NEGATIVE SPACE. A token index is non-negative, so "no source anchor" is
/// any negative value and the storage stays a bare `int<token>`: widening the column to a
/// `voption` costs four more bytes per node for a case only a handful of derived nodes ever
/// take. The `voption` lives at the READ instead (`ofColumn`), where it is free, so nothing
/// downstream ever meets a raw negative — one home for the convention, as `BinderNaming`
/// is for the naming column's empty slot.
[<RequireQualifiedAccess>]
module Anchor =

    /// The column value for a node NO SOURCE SPELLS: a lowering's own derived node, an
    /// `.fsi` contract's reconstructed parameter pattern, a declaration's pattern-less
    /// binder key slot.
    [<Literal>]
    let none = -1<token>

    /// THE anchor of a node of a FILE, taken from the token that spells it.
    ///
    /// A VIRTUAL token faults rather than converting: it carries no lexed index, so there
    /// is nothing to store, and nothing that could be resolved back. No anchor rule can
    /// pick one — every `CstKeys.firstTokenOfExpr`/`firstTokenOfPat` arm takes a keyword, a
    /// real operator, a real opening delimiter, or recurses; recovery synthesises only
    /// CLOSING delimiters (`ParsingHelpers.pEnclosed` passes the real `l` through), and the
    /// only virtual-IDENTIFIER producer (`recoverLongIdent`) serves `open`/`namespace`/
    /// `module` headers, for which there is no frozen decl at all. That is N individually
    /// correct choices; this is the one place they are all answerable, so an arm that comes
    /// to pick a virtual token is caught here rather than by whichever consumer first asks
    /// the anchor for text or a position.
    ///
    /// A node that genuinely has no anchor does not come through here — it takes `none`.
    let ofToken (tok: SyntaxToken) : int<token> =
        match tok.Index with
        | TokenIndex.Regular i -> i
        | TokenIndex.Virtual ->
            failwithf "Anchor.ofToken: the VIRTUAL token %A names no place in the source, so it anchors nothing" tok

    /// The anchor a column slot holds, if the node has one. The sole decoder of the
    /// negative-space convention, so no consumer can invent a second reading of it.
    let ofColumn (stored: int<token>) : int<token> voption =
        if stored < 0<token> then ValueNone else ValueSome stored

    /// The column slot for an anchor that may be absent — the inverse of `ofColumn`, and
    /// the only way a node that names no position gets into a column.
    let toColumn (anchor: int<token> voption) : int<token> =
        match anchor with
        | ValueSome i -> i
        | ValueNone -> none

    /// The token a stored anchor widens back to when there is NO `Lexed` to widen it
    /// against: the cross-unit inline wire, whose template was compiled from a different
    /// file entirely (`TastPoolBuilder.declTree`). A producer's index names a different
    /// node in the consumer's file, so the drain hands over no position rather than one
    /// that resolves to the wrong thing.
    ///
    /// VIRTUAL, and that is the guard: `Inline.spliceAt` moves every node of a spliced body
    /// onto the call site, so a body reaching the freeze still carrying this is a splice
    /// path that failed to relocate — which `ofToken` faults on rather than storing.
    let foreignToken: SyntaxToken =
        SyntaxToken.virtualToken (PositionedToken.Create(Token.VirtualApp, 0))
