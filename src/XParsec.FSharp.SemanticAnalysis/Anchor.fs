namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// The index of its anchor token in the `Lexed` of the file it belongs
/// to, or NOWHERE for a node no source spells.
///
/// An index is a position IN ONE FILE and means nothing against another's tokens.
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

/// WHICH FILE a set of `Anchor`s index — identity only, with no claim about the file's
/// contents.
///
/// A FILE and not a package: a package has many files, and an anchor indexes exactly one.
///
/// The package and the path WITHIN it, and deliberately nothing else — no absolute path. This
/// is a KEY: it decides which retained source an anchor resolves against, and it is folded into
/// the per-file compile-cache key. Where a driver happens to have mounted the package is not a
/// fact about the file, so admitting it would make one invocation's blobs unreachable to the
/// next (and one retention unfindable from the other) over a difference no tree can observe.
/// WHERE to read the bytes belongs with whoever reads them.
type OriginPath =
    {
        /// The declaring package.
        BucketName: string
        /// Path relative to the package directory, as the manifest names it (`"math/z.fs"`).
        Relative: string
    }

/// A producer file an `Anchor` may be resolved against: which file, plus a hash of the exact
/// text whose `Lexed` those indices address.
///
/// The hash is the load-bearing half, not bookkeeping. An anchor is an integer index into a
/// file the consumer RE-READS on a later build, and the tree carrying it is cached to disk
/// (`FrozenCache`); a producer edited between the two builds leaves every index still in range
/// and pointing at a DIFFERENT token — wrong source maps, wrong diagnostics, and no error.
/// `Hashing.dependencySignatureHash` folds every referenced package's sources into the
/// compilation digest, so the cache key does move today — but that is two subsystems agreeing
/// by coincidence rather than a checked invariant, and it says nothing about a tree reached by
/// any other route. So the claim is recorded here and enforced where it is relied upon, as a
/// hard failure (`OriginSources.tokenAt`).
type OriginFile =
    {
        Path: OriginPath
        /// `Hashing.hashString` of the file's text — the same hash vocabulary the compile cache
        /// keys on, so there is ONE notion of "this file's contents" rather than two that can
        /// drift apart. Minted at `Hashing.originSource`, the sole site that takes it.
        Content: InputHash
    }

[<RequireQualifiedAccess>]
module OriginFile =

    /// The identity of a pool that is NOBODY's file — `FrozenPools.empty`, whose overlay bears
    /// nodes minted from an `.fsi` contract or re-axised by a provider and indexing into no
    /// source at all. Every such node anchors `Anchor.nowhere`, which `OriginSources.tokenAt`
    /// answers before it consults the retention, so this never has to name a retained source.
    ///
    /// Distinguishable from every real origin, which names a path: no file is spelled `""`.
    let nowhere: OriginFile =
        {
            Path = { BucketName = ""; Relative = "" }
            Content = InputHash.ofBytes [||]
        }

[<RequireQualifiedAccess>]
module SourceText =

    /// THE text a file compiles as, taken at the point its bytes become source. A package's
    /// own files are read TWICE on a self-host build — once as the manifest's splice
    /// templates, once as the assembly's sources — and an `OriginSources` keyed by path
    /// holds one text per file, so two reads that differ only in line endings would retain
    /// the same file at two contents and fault a tree anchored against the other read.
    let normalise (raw: string) : string = raw.Replace("\r\n", "\n")

/// A producer file RETAINED past the parse that produced it, so that anchors of a tree unpooled
/// from it stay readable. `Input` rides with the `Lexed` because a token carries offsets into
/// the text and not the text itself, and the text is what a multi-source map publishes.
type OriginSource =
    {
        File: OriginFile
        Input: string
        Lexed: Lexed
    }

/// Every producer file whose anchors a compilation may have to resolve.
///
/// Keyed by PATH rather than by the whole `OriginFile`, so a file retained at DIFFERENT
/// contents is found and faults instead of missing — a miss and a mismatch are different
/// failures and only one of them means "this file was never collected".
type OriginSources =
    private
        {
            ByPath: Map<OriginPath, OriginSource>
        }

[<RequireQualifiedAccess>]
module OriginSources =

    let empty: OriginSources = { ByPath = Map.empty }

    /// Retain one parsed producer file. A later retention of the same path replaces: within one
    /// compilation a file is read once, so two entries for a path are the same read.
    let add (src: OriginSource) (sources: OriginSources) : OriginSources =
        {
            ByPath = Map.add src.File.Path src sources.ByPath
        }

    let ofSeq (srcs: OriginSource seq) : OriginSources =
        Seq.fold (fun acc src -> add src acc) empty srcs

    /// Every source of `added` retained over `sources`. A path in both keeps `added`'s read,
    /// on the same "a file is read once" grounds `add` replaces on.
    let addAll (added: OriginSources) (sources: OriginSources) : OriginSources =
        (sources, added.ByPath) ||> Map.fold (fun acc _ src -> add src acc)

    /// Everything retained, in path order. The enumeration a consumer that must PUBLISH the
    /// producer text — a multi-source map's `sourcesContent` — reads, which is why it yields
    /// whole sources and not just their identities.
    let toList (sources: OriginSources) : OriginSource list =
        [ for KeyValue(_, src) in sources.ByPath -> src ]

    /// THE reading of an anchor: the token `at` names in `file`, taken from that file's retained
    /// `Lexed`. Naming the file is not a convenience — it is the whole of what makes the index
    /// mean anything, so this is where a domain stated on a pool, an entry or a node is finally
    /// cashed in.
    ///
    /// Faults on a file never retained, and — the case the hash exists for — on one whose
    /// retained contents disagree with the contents the tree was anchored against. Both are
    /// silent misattribution otherwise: the index is in range either way, so this is the last
    /// point at which a wrong answer is still distinguishable from a right one.
    let tokenAt (sources: OriginSources) (file: OriginFile) (at: Anchor) : SyntaxToken =
        match at.Index with
        // A node no source spells stays unspelled. Inventing a position would put the node on
        // a token it was never written at, which is the misattribution this guards against.
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
