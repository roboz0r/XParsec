namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// The index of its anchor token in the `Lexed` of the file it belongs to. An index is a
/// position IN ONE FILE and means nothing against another file's tokens.
[<Struct>]
type Anchor =
    private
        {
            /// Negative for `nowhere`; no token index is.
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
    /// reconstructed pattern, a declaration's pattern-less bound variable slot.
    let nowhere: Anchor = { Raw = -1<token> }

    let ofToken (tok: SyntaxToken) : Anchor =
        match tok.Index with
        | TokenIndex.Regular i -> { Raw = i }
        | TokenIndex.Virtual ->
            failwithf "Anchor.ofToken: the VIRTUAL token %A names no place in the source, so it anchors nothing" tok

    /// The wire form; `nowhere` travels as `-1`.
    let toStored (a: Anchor) : int =
        match a.Index with
        | ValueSome i -> int i
        | ValueNone -> -1

    let ofStored (raw: int) : Anchor =
        if raw < 0 then nowhere else { Raw = raw * 1<token> }

/// WHICH FILE a set of `Anchor`s index — the package and the path within it, and no absolute
/// path. It is folded into the per-file input hash.
type OriginPath =
    {
        BucketName: string
        /// Path relative to the package directory, as the manifest names it (`"math/z.fs"`).
        Relative: string
    }

/// A producer file an `Anchor` may be resolved against: which file, plus a hash of the exact
/// text whose `Lexed` those indices address. Without the hash, a producer edited between two
/// builds leaves every index still in range and naming a DIFFERENT token, with no error.
type OriginFile =
    { Path: OriginPath; Content: InputHash }

[<RequireQualifiedAccess>]
module OriginFile =

    /// The identity of a pool that is NOBODY's file, bearing only nodes that anchor
    /// `Anchor.nowhere`. No real origin is spelled `""`.
    let nowhere: OriginFile =
        {
            Path = { BucketName = ""; Relative = "" }
            Content = InputHash.ofBytes [||]
        }

/// A producer file RETAINED past the parse that produced it, so that anchors of a tree unpooled
/// from it stay readable.
type OriginSource =
    {
        File: OriginFile
        Lexed: Lexed
    }

    member this.Input: string = this.Lexed.Input

/// Every producer file whose anchors a compilation may have to resolve. Keyed by PATH rather
/// than by the whole `OriginFile`, so a file retained at DIFFERENT contents is found and
/// faults instead of missing.
type OriginSources =
    private
        {
            ByPath: Map<OriginPath, OriginSource>
        }

[<RequireQualifiedAccess>]
module OriginSources =

    let empty: OriginSources = { ByPath = Map.empty }

    /// Retain one parsed producer file. A later retention of the same path replaces.
    let add (src: OriginSource) (sources: OriginSources) : OriginSources =
        {
            ByPath = Map.add src.File.Path src sources.ByPath
        }

    let ofSeq (srcs: OriginSource seq) : OriginSources =
        Seq.fold (fun acc src -> add src acc) empty srcs

    /// Every source of `added` retained over `sources`; a path in both keeps `added`'s read.
    let addAll (added: OriginSources) (sources: OriginSources) : OriginSources =
        (sources, added.ByPath) ||> Map.fold (fun acc _ src -> add src acc)

    /// Everything retained, in path order.
    let toList (sources: OriginSources) : OriginSource list =
        [ for KeyValue(_, src) in sources.ByPath -> src ]

    /// The token `at` names in `file`, taken from that file's retained `Lexed`.
    let tokenAt (sources: OriginSources) (file: OriginFile) (at: Anchor) : SyntaxToken =
        match at.Index with
        | ValueNone -> SyntaxToken.nowhere
        | ValueSome i ->
            match Map.tryFind file.Path sources.ByPath with
            | None ->
                failwithf
                    "OriginSources: no retained source for %s (package %s), so a tree anchored in it has no readable positions"
                    file.Path.Relative
                    file.Path.BucketName
            | Some src when src.File.Content <> file.Content ->
                failwithf
                    "OriginSources: %s (package %s) has changed since the tree anchored in it was built (anchored against %s, retained %s) — every one of its anchors now names a different token"
                    file.Path.Relative
                    file.Path.BucketName
                    file.Content.Hex
                    src.File.Content.Hex
            | Some src when int i >= src.Lexed.Tokens.Length ->
                failwithf
                    "OriginSources: anchor %d is past the end of %s (%d tokens)"
                    (int i)
                    file.Path.Relative
                    src.Lexed.Tokens.Length
            | Some src -> SyntaxToken.syntaxToken src.Lexed.Tokens.[i] (int i)
