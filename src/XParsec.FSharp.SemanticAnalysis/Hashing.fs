namespace XParsec.FSharp.SemanticAnalysis

open System
open System.IO
open System.IO.Hashing
open System.Text

/// The Merkle spine of the per-file compile cache: a file's `inputHash` is the hash of
/// its own source folded with the signature hashes of every project it references. Because
/// the cache key hashes INPUTS (never the stored blob), a hit is sound iff this value
/// changes exactly when a determinant of the file's compile output changes — its source, or
/// any dependency's exported signature — and stays stable otherwise. That "changes when it
/// must, stable when it can" property is the one correctness obligation here; everything
/// downstream (interning, the wire format) is a size optimization keyed off it.
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

    /// Append a variable-length byte run PREFIXED by its length, so a hash built from a
    /// sequence of such runs is injective in the run boundaries: two different splittings of
    /// the same concatenated bytes (`"Ab"+"c"` vs `"A"+"bc"`, or one file vs two whose bytes
    /// abut) hash differently. Without the prefix the boundary is invisible to the hash, and a
    /// cache keyed on it could return a WRONG hit for a distinct signature — a silent
    /// miscompile, the failure this module exists to preclude.
    let private appendLengthPrefixed (hasher: XxHash128) (bytes: byte[]) =
        hasher.Append(ReadOnlySpan(BitConverter.GetBytes bytes.Length))
        hasher.Append(ReadOnlySpan bytes)

    /// The Merkle fold: hash `source` together with the signature hashes of the file's
    /// dependencies, yielding the file's cache-key input hash.
    ///
    /// The dependency hashes are folded DEDUPLICATED and SORTED, so the result is a function
    /// of the dependency SET, not of enumeration order OR multiplicity. This is correct — and
    /// required — because a file's dependency set IS a set: which projects it references
    /// determines its compile output, but neither the order a caller enumerates them in nor a
    /// repeated reference does. Folding order- or multiplicity-sensitively would spuriously
    /// miss the cache on a reordered/duplicated-but-equivalent reference list. The source is
    /// length-prefixed so its boundary with the appended fixed-width dependency digests is
    /// unambiguous (no source suffix can be mistaken for a dependency digest).
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

    /// Derive one referenced project's SIGNATURE hash from its `manifest.toml` — the artifact
    /// that actually determines cross-file compile correctness. A dependency exposes itself
    /// to a consumer through exactly (a) its identity (assembly name + namespace, which the
    /// consumer's minted keys carry) and (b) its contract `.fsi` files, in compile order —
    /// the target-agnostic signature `FrozenSignature`/the `.fsi` extractor project a file's
    /// exports onto. Hashing those bytes is the cheap, sound choice: it changes whenever the
    /// dependency's exported signature changes and is invariant to its implementation `.fs`
    /// bodies (which do not affect a consumer's resolution). Compile ORDER is significant
    /// (an `.fsi` may reference an earlier one), so the files are folded in `Files` order —
    /// unlike the dependency SET in `inputHash`, this is a sequence.
    ///
    /// A manifest that fails to load degrades to hashing its path: still stable and distinct
    /// per dependency, so the cache stays sound (it can only over-invalidate, never wrongly
    /// hit) when a reference is malformed.
    let dependencySignatureHash (manifestPath: string) : InputHash =
        match ReferencedProject.loadManifest manifestPath with
        | Error _ -> hashString manifestPath
        | Ok manifest ->
            let dir = Path.GetDirectoryName manifestPath
            let hasher = XxHash128()

            // Length-prefixed so the identity strings and the abutting file contents cannot be
            // re-partitioned into a different-but-same-bytes signature (see `appendLengthPrefixed`).
            appendLengthPrefixed hasher (Encoding.UTF8.GetBytes manifest.Name)
            appendLengthPrefixed hasher (Encoding.UTF8.GetBytes manifest.Namespace)

            for rel in manifest.Files do
                let abs = Path.Combine(dir, rel)

                if File.Exists abs then
                    appendLengthPrefixed hasher (File.ReadAllBytes abs)

            InputHash.ofBytes (hasher.GetCurrentHash())

    /// The per-file compile-cache input hash a driver computes at its seam: fold `source`
    /// with the signature hash of every referenced project (named by its `manifest.toml`
    /// path). This is the `hash(source ⊕ dependency-signature-hashes)` the cache keys on.
    /// Exposed for a driver to call once cache lookup lands; the front end does not yet
    /// consult it.
    let fileInputHash (source: string) (manifestPaths: string seq) : InputHash =
        inputHash source (manifestPaths |> Seq.map dependencySignatureHash)
