namespace XParsec.FSharp.SemanticAnalysis

open System.Text
open System.IO.Hashing
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

/// One file's IDENTITY within its assembly, never reopened: `math/z.fs`, relative and
/// `/`-separated with no `.`/`..` left in it, so `D:\work\math\z.fs` cannot key a build to a
/// checkout, nor `math\z.fs` to Windows.
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

    /// `relative` canonicalised, or why it does not identify a file within its assembly.
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
                | 0 -> Error(sprintf "'%s' does not identify a file: nothing is left of it canonicalised" relative)
                | _ -> Ok(FileId(String.concat "/" segments))

    let ofRelative (relative: string) : AssemblyFileId =
        match tryOfRelative relative with
        | Ok id -> id
        | Error why -> failwithf "AssemblyFileId: %s" why

    /// Trust `name` as already canonical: it is being read back from the frozen tree it was
    /// canonicalised into.
    let ofStored (name: string) : AssemblyFileId = FileId name

    /// UTF-8 so the digest is culture- and platform-independent.
    let private contentHex (input: string) : string =
        System.Convert.ToHexString(XxHash128.Hash(Encoding.UTF8.GetBytes input)).ToLowerInvariant()

    /// The name of text handed over with no file behind it: a script fragment, a driver given
    /// a string, a test. Two identical texts get one name.
    let ofText (input: string) : AssemblyFileId =
        ofStored (sprintf "<text:%s>" (contentHex input))

    /// `relative` named as the FILESYSTEM beneath `root` has it, a segment at a time: where it
    /// is case-insensitive, `foo.fs` and `Foo.fs` open ONE file and must not mint two
    /// identities for it. A segment it does not have is kept as asked, for the read to report.
    let ofPathUnder (root: string) (relative: string) : AssemblyFileId =
        // A single match is `segment`'s on-disk spelling only if it IS that name: a segment
        // holding `?` or `*` is a search pattern to `GetFileSystemEntries` and could match
        // anything.
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

/// An assembly's SIMPLE name, with no version, culture or public key.
[<Struct>]
type AssemblyName =
    | AssemblyName of name: string

    member this.Name = let (AssemblyName n) = this in n

    override this.ToString() = this.Name

[<RequireQualifiedAccess>]
module AssemblyName =

    /// The wire and message form; `ValueNone` travels as the empty string, which no assembly
    /// name is.
    let toStored (assembly: AssemblyName voption) : string =
        match assembly with
        | ValueSome a -> a.Name
        | ValueNone -> ""

    let ofStored (raw: string) : AssemblyName voption =
        if System.String.IsNullOrEmpty raw then
            ValueNone
        else
            ValueSome(AssemblyName raw)

/// WHICH FILE a set of `Anchor`s index: the assembly, and the name within it. `Assembly` is
/// `ValueNone` where no assembly claims the file — text handed over with no file behind it,
/// or a file that failed to parse.
type AssemblyFilePath =
    {
        Assembly: AssemblyName voption
        Relative: AssemblyFileId
    }

[<RequireQualifiedAccess>]
module AssemblyFilePath =

    /// The identity of a pool that is NOBODY's file, bearing only nodes that anchor
    /// `Anchor.nowhere`.
    let nowhere: AssemblyFilePath =
        {
            Assembly = ValueNone
            Relative = AssemblyFileId.nowhere
        }

    /// `AssemblyFileId.ofText` under no assembly. Where the assembly IS known, build the path
    /// over that id instead.
    let ofText (input: string) : AssemblyFilePath =
        {
            Assembly = ValueNone
            Relative = AssemblyFileId.ofText input
        }

/// A declaring file RETAINED past the parse that produced it, so that anchors of a tree
/// unpooled from it stay readable.
type LexedFile =
    {
        Path: AssemblyFilePath
        Lexed: Lexed
    }

    member this.Input: string = this.Lexed.Input

[<RequireQualifiedAccess>]
module LexedFile =

    let inFile (path: AssemblyFilePath) (lexed: Lexed) : LexedFile = { Path = path; Lexed = lexed }

    /// The identity every anchor and diagnostic of one file resolves against: the assembly it
    /// is bucketed under, and the name it is known by within it.
    let inAssembly (assembly: AssemblyName) (id: AssemblyFileId) (lexed: Lexed) : LexedFile =
        inFile
            {
                Assembly = ValueSome assembly
                Relative = id
            }
            lexed

    /// A file no assembly claims, named by `id` rather than by its text.
    let unclaimed (id: AssemblyFileId) (lexed: Lexed) : LexedFile =
        inFile { Assembly = ValueNone; Relative = id } lexed

    let ofText (lexed: Lexed) : LexedFile =
        inFile (AssemblyFilePath.ofText lexed.Input) lexed

/// Every declaring file whose anchors a compilation may have to resolve.
type LexedFiles =
    private
        {
            ByPath: Map<AssemblyFilePath, LexedFile>
        }

[<RequireQualifiedAccess>]
module LexedFiles =

    let empty: LexedFiles = { ByPath = Map.empty }

    /// Retain one parsed declaring file. A later retention of the same path replaces.
    let add (file: LexedFile) (files: LexedFiles) : LexedFiles =
        {
            ByPath = Map.add file.Path file files.ByPath
        }

    let ofSeq (files: LexedFile seq) : LexedFiles =
        Seq.fold (fun acc file -> add file acc) empty files

    /// Every file of `added` retained over `files`; a path in both keeps `added`'s read.
    let addAll (added: LexedFiles) (files: LexedFiles) : LexedFiles =
        (files, added.ByPath) ||> Map.fold (fun acc _ file -> add file acc)

    /// Everything retained, in path order.
    let toList (files: LexedFiles) : LexedFile list =
        [ for KeyValue(_, file) in files.ByPath -> file ]

    /// The token `at` names in `path`, taken from that file's retained `Lexed`.
    let tokenAt (files: LexedFiles) (path: AssemblyFilePath) (at: Anchor) : SyntaxToken =
        match at.Index with
        | ValueNone -> SyntaxToken.nowhere
        | ValueSome i ->
            match Map.tryFind path files.ByPath with
            | None ->
                failwithf
                    "LexedFiles: no retained file for %s (assembly %s), so a tree anchored in it has no readable positions"
                    path.Relative.Name
                    (AssemblyName.toStored path.Assembly)
            | Some file when int i >= file.Lexed.Tokens.Length ->
                failwithf
                    "LexedFiles: anchor %d is past the end of %s (%d tokens)"
                    (int i)
                    path.Relative.Name
                    file.Lexed.Tokens.Length
            | Some file -> SyntaxToken.syntaxToken file.Lexed.Tokens.[i] (int i)
