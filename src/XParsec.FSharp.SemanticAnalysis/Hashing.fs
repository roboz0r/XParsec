namespace XParsec.FSharp.SemanticAnalysis

open System
open System.IO
open System.IO.Hashing
open System.Text
open XParsec.FSharp.Lexer

/// The Merkle root of the per-file compile cache: a file's key is the hash of its own
/// source folded with a digest of everything ELSE its compile reads — the compilation
/// environment (home assembly, target, reference assemblies) and the full contents of every
/// source file in the referenced-package closure. Because the cache key hashes INPUTS (never
/// the stored blob), a hit is sound iff this value changes exactly when a determinant of the
/// file's compile output changes and stays stable otherwise. That "changes when it must,
/// stable when it can" property is the one correctness obligation here; everything downstream
/// (interning, the wire format) is a size optimization keyed off it.
///
/// **The key is computed in TWO lifetimes, and the types say which is which.** Everything but
/// the file's own text belongs to the COMPILATION (`CompilationInputs` → `compilationDigest`
/// → `CompilationDigest`), and folding it touches the whole dependency closure on disk;
/// the per-file step (`fileInputHash`) touches nothing but the source string. A driver
/// compiling n files folds the digest ONCE. That split is not a convenience: the disk fold is
/// the same order of magnitude as the front end a hit elides (`FrozenCodecBenchmarks`), so
/// doing it per file would spend most of what the cache saves.
///
/// `XxHash128` is a non-cryptographic hash — correct for a build cache (we key on content
/// identity, not adversarial collision resistance) and far cheaper than a crypto digest.
module Hashing =

    /// Raw hash of `bytes`, as the store's lowercase-hex `InputHash`. `XxHash128` produces a
    /// 16-byte digest.
    let hashBytes (bytes: byte[]) : InputHash =
        InputHash.ofBytes (XxHash128.Hash bytes)

    /// Hash of a string's UTF-8 encoding — the text-input entry point (source, identity
    /// strings). UTF-8 so the digest is culture- and platform-independent.
    let hashString (s: string) : InputHash = hashBytes (Encoding.UTF8.GetBytes s)

    /// THE site that mints an `OriginFile`. The content hash is taken off the `Lexed`'s own
    /// text, so it necessarily hashes the very string that was parsed rather than a re-read of
    /// the path that can already disagree with what the indices address. That is what makes the
    /// mismatch check at `OriginSources.tokenAt` mean what it says.
    let originSource (path: OriginPath) (lexed: Lexed) : OriginSource =
        {
            File =
                {
                    Path = path
                    Content = hashString lexed.Input
                }
            Lexed = lexed
        }

    /// The identity of a compiled source handed over as TEXT with no file behind it: a script
    /// fragment, a driver given a string, a test. The content hash stands in for the path, so
    /// two different texts are two different origins and cannot collide in an `OriginSources`.
    ///
    /// Separate from `originSourceOfText` because a cache key is taken BEFORE the parse that
    /// yields a `Lexed` — a driver keying a compile needs the identity without paying for the
    /// front end it is trying to elide.
    let textOriginPath (input: string) : OriginPath =
        {
            BucketName = ""
            Relative = sprintf "<text:%s>" (hashString input).Hex
        }

    let originSourceOfText (lexed: Lexed) : OriginSource =
        originSource (textOriginPath lexed.Input) lexed

    /// Append a variable-length byte run PREFIXED by its length, so a hash built from a
    /// sequence of such runs is injective in the run boundaries: two different splittings of
    /// the same concatenated bytes (`"Ab"+"c"` vs `"A"+"bc"`, or one file vs two whose bytes
    /// abut) hash differently. Without the prefix the boundary is invisible to the hash, and a
    /// cache keyed on it could return a WRONG hit for a distinct signature — a silent
    /// miscompile, the failure this module exists to preclude.
    let private appendLengthPrefixed (hasher: XxHash128) (bytes: byte[]) =
        hasher.Append(ReadOnlySpan(BitConverter.GetBytes bytes.Length))
        hasher.Append(ReadOnlySpan bytes)

    /// The one-byte marker every OPTIONAL on-disk input carries ahead of its payload. Length
    /// prefixes alone cannot separate "the file is absent" from "the file is present and
    /// empty" — both would contribute a zero length — and those are genuinely different
    /// inputs: a compile that finds no `.fs` companion resolves differently from one that
    /// finds an empty one.
    let private appendPresence (hasher: XxHash128) (present: bool) =
        hasher.Append(ReadOnlySpan [| (if present then 1uy else 0uy) |])

    /// The Merkle fold: hash `source` together with the digests of every OTHER input the
    /// file's compile depends on, yielding the file's cache-key input hash.
    ///
    /// The digests are folded DEDUPLICATED and SORTED, so the result is a function of the
    /// input SET, not of enumeration order OR multiplicity. That is the contract a caller
    /// must respect: pass one digest per INDEPENDENT input, each already covering its own
    /// internal ordering. It holds for a dependency set (which projects a file references
    /// determines its output; the order a caller enumerates them in does not, and a repeated
    /// reference is the same reference) and equally for the compilation environment
    /// (`compilationDigest`'s `environmentHash`, which folds its own ordered contents into one
    /// digest before it gets here). Folding order- or multiplicity-sensitively would
    /// spuriously miss on a reordered or duplicated-but-equivalent reference list. The source
    /// is length-prefixed so its boundary with the appended fixed-width digests is unambiguous
    /// (no source suffix can be mistaken for a digest).
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

    /// Derive one referenced project's SIGNATURE hash from its `manifest.toml` — the value
    /// that must change whenever anything about the dependency changes a consumer's compiled
    /// output. A dependency exposes itself through its identity (its assembly name, which the
    /// consumer's minted keys carry), the manifest that says which files play which
    /// role, and the CONTENTS of every source file it names.
    ///
    /// **Contract `.fsi` bytes alone are not enough, and assuming they were is a stale hit.**
    /// This fold used to cover `manifest.Files` only, on the premise that the hash could be
    /// "invariant to its implementation `.fs` bodies (which do not affect a consumer's
    /// resolution)". They do. A dependency's `impl` `.fs` files are re-parsed,
    /// re-analysed and their templates SPLICED INTO the consumer's tree before it is frozen
    /// (`SymbolProviders.inlineBodies`, folded onto the provider entry that owns each key),
    /// and the `.fs` companion beside each `.fsi` is scanned for the intrinsic reprs that
    /// decide what a primitive resolves to. Editing either without touching an `.fsi` left
    /// this hash — and so the cache key — unmoved, and the store served a blob carrying the
    /// OLD inlined body.
    ///
    /// So the fold is over `ReferencedProject.sourceInputs`: the manifest's own bytes plus
    /// every source path it names, target-blind. That is a rule about the manifest's shape
    /// rather than an enumeration of the current reader's appetite, which is what keeps it
    /// true as the reader grows.
    ///
    /// Order is carried by the MANIFEST bytes, where it actually lives (compile order IS the
    /// `files` sequence), so the contents are folded over a SORTED path set — a fold whose
    /// value does not depend on how `sourceInputs` happened to enumerate. Each entry is
    /// path + presence + contents (see `appendPresence`).
    ///
    /// This covers ONE manifest. A dependency's own `depends-on` is not followed here: the
    /// closure is `compilationDigest`'s job, because a root's hash must not silently absorb a
    /// package the caller could also have named directly.
    ///
    /// A manifest that fails to load degrades to hashing its path: still stable and distinct
    /// per dependency, so the cache stays sound (it can only over-invalidate, never wrongly
    /// hit) when a reference is malformed.
    ///
    /// COST: this reads every source file the package names. It is the expensive half of a
    /// cache key and belongs to the COMPILATION, not to a file — see `compilationDigest`.
    let dependencySignatureHash (manifestPath: string) : InputHash =
        match ReferencedProject.loadManifest manifestPath with
        | Error _ -> hashString manifestPath
        | Ok manifest ->
            let dir = Path.GetDirectoryName manifestPath
            let hasher = XxHash128()

            // The package's assembly name, length-prefixed so it cannot be re-partitioned
            // against the bytes that follow (see `appendLengthPrefixed`). NOT redundant with
            // the manifest bytes below: `Name` falls back to the manifest's DIRECTORY name
            // when the file declares none, so it is the one piece of a package's identity
            // that need not appear in the file. The namespaces its symbols live in need no
            // fold of their own — they are `namespace` headers in the source bytes below.
            appendLengthPrefixed hasher (Encoding.UTF8.GetBytes manifest.Name)

            // The manifest's OWN bytes: which files it names, under which key, in which order,
            // plus `depends-on` and `sig-only`. None of that is visible in the file contents
            // below, and all of it moves a consumer's compile. Unconditional — `loadManifest`
            // has already returned `Error` for a manifest that is not there, so a guard here
            // could only ever skip the read on a path that cannot happen and silently weaken
            // the hash if it did.
            appendLengthPrefixed hasher (File.ReadAllBytes manifestPath)

            for rel in ReferencedProject.sourceInputs manifest |> List.sort do
                let abs = Path.Combine(dir, rel)
                let exists = File.Exists abs
                appendLengthPrefixed hasher (Encoding.UTF8.GetBytes rel)
                appendPresence hasher exists

                if exists then
                    appendLengthPrefixed hasher (File.ReadAllBytes abs)

            InputHash.ofBytes (hasher.GetCurrentHash())

    /// Every input the frozen tree of a file is a function of EXCEPT the file's own text —
    /// which is to say, everything shared by every file of one compilation. Named as a record
    /// rather than passed as loose arguments because the cache's whole correctness obligation
    /// is that this set is COMPLETE: a determinant reachable by the front end and absent here
    /// is a stale hit, and the failure is silent. A new front-end input is a new field, and
    /// the compiler then asks every driver what to put in it.
    ///
    /// The frontier is `Pipeline.analyseFor homeAssembly provider origin file`, whose
    /// inputs are the origin, the home assembly the minted keys are rooted at, and the
    /// provider — itself a function of the reference assemblies, the target suffix, and the
    /// manifest set. The origin is the one that varies per file and so is not here; the rest
    /// are. Nothing else in a driver's config qualifies: a backend's output config
    /// (`ProjectInfo.References`, `OutputKind`, …) feeds CODEGEN, which re-runs on a hit.
    type CompilationInputs =
        {
            /// The assembly name the front end roots minted keys at
            /// (`Pipeline.analyseFor`'s first argument). A blob frozen under one home
            /// assembly names its own symbols differently from one frozen under another.
            HomeAssembly: string
            /// The backend target selecting the `[targets.<t>]` manifest lists. Two
            /// targets over one manifest set are two different providers.
            Target: string
            /// The compilation's own reference assemblies — the metadata leaf under the
            /// contract stack, so they decide what a BCL name resolves to.
            ReferenceAssemblies: string list
            /// The ROOT package manifests. Closed over `depends-on` by `compilationDigest`,
            /// not by the caller.
            Manifests: string list
            /// The manifest of the package this compilation IS, when it is one — the source
            /// of the intrinsic reverse axis its own leaf is seeded with, so a BCL signature
            /// presents the package's own primitives (`System.String` -> `Vesper.string`)
            /// inside its own compile. `None` for a compilation that declares no primitives,
            /// which is every consumer.
            ///
            /// A determinant twice over, and both halves are folded: its CONTENTS (the
            /// `(# … #)` reprs live in the `.fs` companions the manifest names) and its ROLE
            /// (the same manifest as a REFERENCE seeds nothing, so path equality alone would
            /// alias two different compilations).
            SelfManifest: string option
        }

    /// A folded `CompilationInputs` — the per-compilation half of every file's cache key,
    /// paid for ONCE and then carried.
    ///
    /// Its own type, not a bare `InputHash`, because the three hashes in this module are not
    /// interchangeable and only one of them is expensive: a `dependencySignatureHash` covers
    /// one package, a `CompilationDigest` covers the whole environment plus every package's
    /// closure, and `fileInputHash`'s result covers a file. Making the middle one distinct is
    /// what lets `fileInputHash` demand the digest — and so makes "compute this once and hoist
    /// it out of the file loop" the shape the types describe rather than advice in a comment.
    [<Struct>]
    type CompilationDigest = | CompilationDigest of InputHash

    /// The compilation ENVIRONMENT digest: everything about HOW files are compiled that is
    /// neither any file's text nor a referenced package's contents. Touches no source.
    ///
    /// Reference assemblies are folded by IDENTITY — path, presence, length, last-write time
    /// — and deliberately not by contents. A ref pack is two orders of magnitude larger than
    /// everything else the key touches, and the cache exists to save a front end measured in
    /// tens of milliseconds; reading it would cost more than the hit saves. The stamp is the
    /// stronger half of the trade: a path alone would miss an assembly replaced in place, and
    /// a stamp only over-invalidates (a restore that rewrites timestamps costs a rebuild).
    /// This is sound precisely because the blob is a LOCAL cache and never published — a
    /// timestamp is not portable, and nothing asks it to be.
    ///
    /// Order is significant and preserved: reference resolution is first-hit by simple name,
    /// so a reordered list is a different resolution, not the same one spelled differently.
    let private environmentHash (inputs: CompilationInputs) : InputHash =
        let hasher = XxHash128()
        appendLengthPrefixed hasher (Encoding.UTF8.GetBytes inputs.HomeAssembly)
        appendLengthPrefixed hasher (Encoding.UTF8.GetBytes inputs.Target)

        // The self manifest's ROLE. Its CONTENTS are folded by `compilationDigest` through
        // the same `dependencySignatureHash` a reference gets — which is why the role has to
        // be recorded here as well: that fold is deduplicated, so a package named BOTH as
        // self and as a reference would otherwise contribute one indistinguishable digest.
        appendPresence hasher inputs.SelfManifest.IsSome
        appendLengthPrefixed hasher (Encoding.UTF8.GetBytes(defaultArg inputs.SelfManifest ""))

        for path in inputs.ReferenceAssemblies do
            let info = FileInfo path
            appendLengthPrefixed hasher (Encoding.UTF8.GetBytes path)
            appendPresence hasher info.Exists

            if info.Exists then
                hasher.Append(ReadOnlySpan(BitConverter.GetBytes info.Length))
                hasher.Append(ReadOnlySpan(BitConverter.GetBytes info.LastWriteTimeUtc.Ticks))

        InputHash.ofBytes (hasher.GetCurrentHash())

    /// Fold a compilation's whole environment and referenced-package closure into one digest.
    /// **THE expensive step in a cache key** — it stats and reads every source file every
    /// package in the closure names — and the reason `CompilationDigest` is a value a driver
    /// holds rather than something `fileInputHash` derives: compute it once per compilation,
    /// then key n files off it for the cost of hashing n source strings.
    ///
    /// The manifest set is closed over `depends-on` HERE. A driver hands over the roots it was
    /// configured with, but the provider build resolves the whole closure
    /// (`ReferencedProject.buildClosureWithDeps`) and splices inline bodies out of every
    /// package in it — so keying on the roots alone leaves a transitively-reached package's
    /// sources invisible, which is the same stale hit `dependencySignatureHash` documents one
    /// level down. A closure that fails to resolve degrades to the roots: the provider build
    /// refuses the same cycle/missing dependency, so no blob is reached either way.
    ///
    /// The environment digest is folded alongside the package digests, through the same
    /// order- and multiplicity-insensitive `inputHash`: it is one INDEPENDENT input covering
    /// its own ordered contents, which is exactly that function's stated contract. The empty
    /// source string carries no information and is not one — a compilation has no text.
    let compilationDigest (inputs: CompilationInputs) : CompilationDigest =
        let manifests =
            match ReferencedProject.buildClosure inputs.Manifests with
            | Ok ordered -> ordered
            | Error _ -> inputs.Manifests

        // The self package's sources are a determinant on the SAME footing as a reference's:
        // its `.fs` companions carry the `(# … #)` reprs the leaf's seed is folded from, and
        // editing one moves what a BCL signature resolves to WITHOUT touching any `.fsi`.
        // Folded through the identical hash, so "a package's bytes matter" is one rule.
        let selfManifests = Option.toList inputs.SelfManifest

        CompilationDigest(
            inputHash
                ""
                (environmentHash inputs
                 :: (manifests @ selfManifests |> List.map dependencySignatureHash))
        )

    /// A file's IDENTITY as ONE digest. Its own fold and not two entries in the list below,
    /// because the fields are a record where `inputHash` takes a SET: handed separately they
    /// would dedupe when the two agree and say nothing about which field held which string.
    let private originPathHash (p: OriginPath) : InputHash =
        let hasher = XxHash128()
        appendLengthPrefixed hasher (Encoding.UTF8.GetBytes p.BucketName)
        appendLengthPrefixed hasher (Encoding.UTF8.GetBytes p.Relative)
        InputHash.ofBytes (hasher.GetCurrentHash())

    /// The per-file compile-cache input hash: this file's text and IDENTITY folded with its
    /// compilation's digest. Touches no disk — the whole cost of a key lives in
    /// `compilationDigest`, which is why that one is hoisted and this one is not.
    ///
    /// The path is a determinant and not decoration: a frozen tree's nodes carry the
    /// `OriginFile` their anchors index, so the same text analysed under a different path
    /// freezes to a different tree, and a key blind to the path would serve the first for the
    /// second. Taking the `OriginPath` rather than deriving one is what makes that checkable:
    /// the caller states the identity it is about to analyse under, once, for both.
    let fileInputHash (path: OriginPath) (source: string) (CompilationDigest digest) : InputHash =
        inputHash source [ originPathHash path; digest ]
