namespace XParsec.FSharp.SemanticAnalysis

open System
open System.IO
open System.IO.Hashing
open System.Text
open XParsec.FSharp.Lexer

/// Cache keys for the per-file compile cache. The obligation: a key changes exactly when a
/// determinant of the file's compiled output changes, and not otherwise.
module Hashing =

    let hashBytes (bytes: byte[]) : InputHash =
        InputHash.ofBytes (XxHash128.Hash bytes)

    /// UTF-8 so the digest is culture- and platform-independent.
    let hashString (s: string) : InputHash = hashBytes (Encoding.UTF8.GetBytes s)

    /// The content hash is taken off the `Lexed`'s own text, not a re-read of the path, which
    /// can already disagree with what the token indices address.
    let originSource (path: OriginPath) (lexed: Lexed) : OriginSource =
        {
            File =
                {
                    Path = path
                    Content = hashString lexed.Input
                }
            Lexed = lexed
        }

    /// The identity of a source handed over as TEXT with no file behind it: a script fragment,
    /// a driver given a string, a test. The content hash stands in for the path.
    let textOriginPath (input: string) : OriginPath =
        {
            BucketName = ""
            Relative = AssemblyFileId.ofRelative (sprintf "<text:%s>" (hashString input).Hex)
        }

    let originSourceOfText (lexed: Lexed) : OriginSource =
        originSource (textOriginPath lexed.Input) lexed

    /// Length-prefixed so a hash of a sequence of runs is injective in the run boundaries:
    /// `"Ab"+"c"` and `"A"+"bc"` hash differently.
    let private appendLengthPrefixed (hasher: XxHash128) (bytes: byte[]) =
        hasher.Append(ReadOnlySpan(BitConverter.GetBytes bytes.Length))
        hasher.Append(ReadOnlySpan bytes)

    /// The marker every OPTIONAL on-disk input carries ahead of its payload: a length prefix
    /// alone gives an absent file and a present empty one the same zero.
    let private appendPresence (hasher: XxHash128) (present: bool) =
        hasher.Append(ReadOnlySpan [| (if present then 1uy else 0uy) |])

    /// The digests fold DEDUPLICATED and SORTED, so the result is a function of the input SET:
    /// pass one digest per INDEPENDENT input, each already covering its own internal ordering.
    let inputHash (source: string) (dependencyHashes: InputHash seq) : InputHash =
        let hasher = XxHash128()
        appendLengthPrefixed hasher (Encoding.UTF8.GetBytes source)

        let sortedDeps =
            dependencyHashes
            |> Seq.map (fun h -> h.Hex)
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toArray

        for hex in sortedDeps do
            hasher.Append(ReadOnlySpan(Encoding.UTF8.GetBytes hex))

        InputHash.ofBytes (hasher.GetCurrentHash())

    /// One referenced package's signature: the manifest's bytes plus the CONTENTS of every
    /// source it lists, `.fs` impls included, whose inline templates splice into a consumer's
    /// tree. Reads all of them. `depends-on` is not followed from here.
    let dependencySignatureHash (manifest: ReferencedProject.Manifest) : InputHash =
        let hasher = XxHash128()

        // NOT redundant with the manifest bytes below: `Name` falls back to the manifest's
        // DIRECTORY name when the file declares none.
        appendLengthPrefixed hasher (Encoding.UTF8.GetBytes manifest.Name)

        // The manifest's OWN bytes: which files it lists, under which key, in which order,
        // plus `depends-on`, none of which is visible in the contents below.
        appendLengthPrefixed hasher (File.ReadAllBytes manifest.Path.Path)

        for rel in ReferencedProject.sourceInputs manifest |> List.sort do
            let abs = Path.Combine(manifest.Dir, rel)
            let exists = File.Exists abs
            appendLengthPrefixed hasher (Encoding.UTF8.GetBytes rel)
            appendPresence hasher exists

            if exists then
                appendLengthPrefixed hasher (File.ReadAllBytes abs)

        InputHash.ofBytes (hasher.GetCurrentHash())

    /// Every input a file's frozen tree is a function of EXCEPT the file's own text. This set
    /// must be COMPLETE: a determinant the front end reads and this omits is a silent stale hit.
    type CompilationInputs =
        {
            /// The assembly name the front end roots minted keys at: a blob frozen under one
            /// home assembly keys its own symbols differently from one frozen under another.
            HomeAssembly: string
            /// The backend target. It SELECTS which `manifest.<t>.toml` each package below
            /// resolves to, and is folded in its own right besides: backend-supplied facts reach
            /// type-checking verdicts even where two targets' manifests agree byte for byte.
            Target: string
            /// The compilation's own reference assemblies: they decide what a BCL name such as
            /// `System.String` resolves to.
            ReferenceAssemblies: string list
            /// The ROOT package DIRECTORIES, resolved against `Target` and closed over
            /// `depends-on` when the digest is folded rather than by the caller.
            Packages: string list
            /// The package this compilation IS, when it is one: inside its own compile a BCL
            /// signature presents that package's own primitives (`System.String` ->
            /// `Vesper.string`). `None` for a compilation that declares no primitives.
            SelfPackage: string option
            /// The symbols the front end resolves every file's `#if` directives against. Two
            /// compilations that differ here parse the same text into different trees.
            CompilationDefines: Set<string>
        }

    /// A folded `CompilationInputs`, the per-compilation half of every file's cache key, paid
    /// for ONCE and then carried.
    [<Struct>]
    type CompilationDigest = | CompilationDigest of InputHash

    /// How files are compiled, minus any file's text and any package's contents. Reference
    /// assemblies fold by IDENTITY (path, presence, length, write stamp), never contents, and
    /// in order, because resolution is first-hit by name.
    let private environmentHash (inputs: CompilationInputs) : InputHash =
        let hasher = XxHash128()
        appendLengthPrefixed hasher (Encoding.UTF8.GetBytes inputs.HomeAssembly)
        appendLengthPrefixed hasher (Encoding.UTF8.GetBytes inputs.Target)

        // The self package's ROLE, its contents being folded elsewhere: that fold is
        // deduplicated, so a package named BOTH as self and as a reference would collapse.
        appendPresence hasher inputs.SelfPackage.IsSome
        appendLengthPrefixed hasher (Encoding.UTF8.GetBytes(defaultArg inputs.SelfPackage ""))

        // Counted, so the boundary with the next field does not depend on the element count.
        // `Set` iterates sorted, so the fold does not depend on how the caller built it.
        hasher.Append(ReadOnlySpan(BitConverter.GetBytes inputs.CompilationDefines.Count))

        for symbol in inputs.CompilationDefines do
            appendLengthPrefixed hasher (Encoding.UTF8.GetBytes symbol)

        for path in inputs.ReferenceAssemblies do
            let info = FileInfo path
            appendLengthPrefixed hasher (Encoding.UTF8.GetBytes path)
            appendPresence hasher info.Exists

            if info.Exists then
                hasher.Append(ReadOnlySpan(BitConverter.GetBytes info.Length))
                hasher.Append(ReadOnlySpan(BitConverter.GetBytes info.LastWriteTimeUtc.Ticks))

        InputHash.ofBytes (hasher.GetCurrentHash())

    /// THE expensive step in a cache key: it reads every source file the closure reaches. The
    /// `depends-on` closure is taken HERE, because inline bodies splice out of
    /// transitively-reached packages too.
    let compilationDigest (inputs: CompilationInputs) : CompilationDigest =
        // A determinant on the SAME footing as a reference: its `.fs` companions carry the
        // `(# … #)` reprs, so editing one moves a BCL signature without touching any `.fsi`.
        let closure =
            ReferencedProject.resolveAll inputs.Target (inputs.Packages @ Option.toList inputs.SelfPackage)
            |> Result.mapError (PackageSetFault.code >> DiagCode.render)
            |> Result.bind (ReferencedProject.buildClosure >> Result.mapError (sprintf "closure:%s"))

        // A package set that does not resolve is keyed on the FAULT, never on the subset that
        // did resolve: keying on the subset would serve a good build's tree as a hit.
        let determinants =
            match closure with
            | Ok manifests -> List.map dependencySignatureHash manifests
            | Error tag ->
                [
                    inputHash
                        "package-set-unresolved"
                        [
                            for text in tag :: inputs.Packages -> InputHash.ofBytes (Encoding.UTF8.GetBytes text)
                        ]
                ]

        CompilationDigest(inputHash "" (environmentHash inputs :: determinants))

    /// A file's identity as ONE digest: handed to the SET-valued fold separately, the two
    /// fields would dedupe when they agree.
    let private originPathHash (p: OriginPath) : InputHash =
        let hasher = XxHash128()
        appendLengthPrefixed hasher (Encoding.UTF8.GetBytes p.BucketName)
        appendLengthPrefixed hasher (Encoding.UTF8.GetBytes p.Relative.Name)
        InputHash.ofBytes (hasher.GetCurrentHash())

    /// This file's text and IDENTITY folded with its compilation's digest. Touches no disk.
    /// The path is a determinant: a frozen tree's nodes carry the `OriginFile` their anchors
    /// index, so the same text under a different path freezes to a different tree.
    let fileInputHash (path: OriginPath) (source: string) (CompilationDigest digest) : InputHash =
        inputHash source [ originPathHash path; digest ]
