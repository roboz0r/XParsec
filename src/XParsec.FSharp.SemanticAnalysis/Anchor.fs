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

    /// A node no source spells: a lowering's own derived node, a contract's
    /// reconstructed pattern, a declaration's pattern-less bound variable slot.
    let nowhere: Anchor = { Raw = -1<token> }

    let ofToken (tok: SyntaxToken) : Anchor =
        match tok.Index with
        | TokenIndex.Regular i -> { Raw = i }
        | TokenIndex.Virtual ->
            failwithf "Anchor.ofToken: the VIRTUAL token %A has no place in the source, so it anchors nothing" tok

    /// The wire form; `nowhere` travels as `-1`.
    let toStored (a: Anchor) : int =
        match a.Index with
        | ValueSome i -> int i
        | ValueNone -> -1

    let ofStored (raw: int) : Anchor =
        if raw < 0 then nowhere else { Raw = raw * 1<token> }

/// One file's IDENTITY within its assembly, folded into the per-file input hash and never
/// reopened: `math/z.fs`, relative and `/`-separated with no `.`/`..` left in it, so
/// `D:\work\math\z.fs` cannot key a build to a checkout, nor `math\z.fs` to Windows.
[<Struct>]
type AssemblyFileId =
    private
    | FileId of name: string

    member this.Name =
        match this with
        | FileId n -> n

    override this.ToString() = this.Name

[<RequireQualifiedAccess>]
module AssemblyFileId =

    /// The name of NO file: blank, which is refused of every real one.
    let nowhere: AssemblyFileId = FileId ""

    /// Rooted on ANY OS, not just the host one: `/z.fs`, `\z.fs`, `C:/z.fs`.
    let private isRooted (s: string) =
        s.[0] = '/'
        || s.[0] = '\\'
        || (s.Length >= 2 && s.[1] = ':' && System.Char.IsAsciiLetter s.[0])

    /// `relative` canonicalised, or why it does not name a file within its assembly.
    let tryOfRelative (relative: string) : Result<AssemblyFileId, string> =
        if System.String.IsNullOrWhiteSpace relative then
            Error "a file is named by something, not by blank"
        elif isRooted relative then
            Error(
                sprintf "'%s' is rooted: a file is named WITHIN its assembly, never by where a checkout put it" relative
            )
        else
            // `..` resolves HERE rather than on disk, because nothing reopens this name: the
            // only thing it could escape is the assembly, and that is the error below.
            let segments = ResizeArray<string>()
            let mutable escaped = false

            for segment in relative.Replace('\\', '/').Split('/') do
                match segment with
                | ""
                | "." -> ()
                | ".." ->
                    if segments.Count = 0 then
                        escaped <- true
                    else
                        segments.RemoveAt(segments.Count - 1)
                | s -> segments.Add s

            if escaped then
                Error(sprintf "'%s' climbs out of its assembly" relative)
            else
                match segments.Count with
                | 0 -> Error(sprintf "'%s' does not name a file: nothing is left of it canonicalised" relative)
                | _ -> Ok(FileId(String.concat "/" segments))

    let ofRelative (relative: string) : AssemblyFileId =
        match tryOfRelative relative with
        | Ok id -> id
        | Error why -> failwithf "AssemblyFileId: %s" why

    /// Trust `name` as already canonical: it is being read back from the frozen tree it was
    /// canonicalised into.
    let ofStored (name: string) : AssemblyFileId = FileId name

    /// `relative` named as the FILESYSTEM beneath `root` has it, a segment at a time: where it
    /// is case-insensitive, `foo.fs` and `Foo.fs` open ONE file and must not mint two
    /// identities for it. A segment it does not have is kept as asked, for the read to report.
    let ofPathUnder (root: string) (relative: string) : AssemblyFileId =
        // The one entry answers for `segment` only if it IS that name: a segment holding `?`
        // or `*` is a search pattern to `GetFileSystemEntries` and could match anything.
        let asOnDisk (dir: string) (segment: string) =
            if not (System.IO.Directory.Exists dir) then
                segment
            else
                match System.IO.Directory.GetFileSystemEntries(dir, segment) with
                | [| entry |] ->
                    let actual = System.IO.Path.GetFileName entry

                    if System.String.Equals(actual, segment, System.StringComparison.OrdinalIgnoreCase) then
                        actual
                    else
                        segment
                | _ -> segment

        let onDisk = ResizeArray<string>()
        let mutable dir = root

        for segment in (ofRelative relative).Name.Split('/') do
            let actual = asOnDisk dir segment
            onDisk.Add actual
            dir <- System.IO.Path.Combine(dir, actual)

        FileId(String.concat "/" onDisk)

/// WHICH FILE a set of `Anchor`s index: the package and the name within it. It is folded into
/// the per-file input hash.
type OriginPath =
    {
        BucketName: string
        Relative: AssemblyFileId
    }

/// A producer file an `Anchor` may be resolved against: which file, plus a hash of the exact
/// text whose `Lexed` those indices address. Without the hash, a producer edited between two
/// builds leaves every index still in range and naming a DIFFERENT token, with no error.
type OriginFile =
    { Path: OriginPath; Content: InputHash }

[<RequireQualifiedAccess>]
module OriginFile =

    /// The identity of a pool that is NOBODY's file, bearing only nodes that anchor
    /// `Anchor.nowhere`.
    let nowhere: OriginFile =
        {
            Path =
                {
                    BucketName = ""
                    Relative = AssemblyFileId.nowhere
                }
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
                    file.Path.Relative.Name
                    file.Path.BucketName
            | Some src when src.File.Content <> file.Content ->
                failwithf
                    "OriginSources: %s (package %s) has changed since the tree anchored in it was built (anchored against %s, retained %s) — every one of its anchors now names a different token"
                    file.Path.Relative.Name
                    file.Path.BucketName
                    file.Content.Hex
                    src.File.Content.Hex
            | Some src when int i >= src.Lexed.Tokens.Length ->
                failwithf
                    "OriginSources: anchor %d is past the end of %s (%d tokens)"
                    (int i)
                    file.Path.Relative.Name
                    src.Lexed.Tokens.Length
            | Some src -> SyntaxToken.syntaxToken src.Lexed.Tokens.[i] (int i)
