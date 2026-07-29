module XParsec.FSharp.SemanticAnalysis.Tests.HashingTests

open System.IO
open Expecto
open XParsec.FSharp.SemanticAnalysis

/// A few distinct dependency-signature hashes to fold, built off the pure string hash so the
/// tests need no on-disk manifest for the fold-shape properties.
let private depA = Hashing.hashString "dependency-A"
let private depB = Hashing.hashString "dependency-B"
let private depC = Hashing.hashString "dependency-C"

/// A fresh directory under the repo `./tmp` for the contract-bytes fixture, isolated per test
/// by name and wiped ON ENTRY so a prior run cannot leak stale `.fsi` bytes. The repo keeps
/// scratch out of the system temp.
///
/// Wipe-on-entry is the WHOLE cleanup story, deliberately: a trailing delete in each test is
/// redundant when the test passes and is skipped exactly when it fails — which is the one run
/// whose directory a person wants to look at. Nothing here deletes on the way out.
let private freshRoot (name: string) : string =
    let root =
        Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "tmp", "hashing-tests", name)

    if Directory.Exists root then
        Directory.Delete(root, true)

    Directory.CreateDirectory root |> ignore
    root

/// Stand up a `<root>/<pkg>/manifest.toml` naming `contract.fsi`, plus the `.fsi` bytes, and
/// return the manifest path. The package directory name is the package identity.
let private writePackage (root: string) (pkg: string) (contract: string) : string =
    let dir = Path.Combine(root, pkg)
    Directory.CreateDirectory dir |> ignore
    File.WriteAllText(Path.Combine(dir, "contract.fsi"), contract)
    let manifestPath = Path.Combine(dir, "manifest.toml")
    File.WriteAllText(manifestPath, "[core]\nnamespace = \"Test\"\nfiles = [\"contract.fsi\"]\n")
    manifestPath

/// The flexible sibling of `writePackage`: an explicit manifest body plus an explicit set of
/// `(relative path, contents)` files. The coverage tests below each gate a DIFFERENT manifest
/// key, so each stands up its own package rather than sharing a fixture that would have to
/// name every key at once. `pkg` is the DIRECTORY name, which is the package identity a
/// sibling's `depends-on` resolves against.
let private writePackageFiles
    (root: string)
    (pkg: string)
    (manifestBody: string)
    (files: (string * string) list)
    : string =
    let dir = Path.Combine(root, pkg)
    Directory.CreateDirectory dir |> ignore

    for (rel, contents) in files do
        File.WriteAllText(Path.Combine(dir, rel), contents)

    let manifestPath = Path.Combine(dir, "manifest.toml")
    File.WriteAllText(manifestPath, manifestBody)
    manifestPath

/// The shape every coverage test has: hash, rewrite ONE file of the package, hash again. The
/// test then states only which file it is gating, not the mechanics of gating it.
let private hashAcrossWrite (manifestPath: string) (rel: string) (contents: string) =
    let before = Hashing.dependencySignatureHash manifestPath
    File.WriteAllText(Path.Combine(Path.GetDirectoryName manifestPath, rel), contents)
    struct (before, Hashing.dependencySignatureHash manifestPath)

/// A minimal `CompilationInputs` the coverage tests perturb ONE field of at a time, so each
/// states only the determinant it gates.
let private compilation: Hashing.CompilationInputs =
    {
        HomeAssembly = "Consumer"
        Target = None
        ReferenceAssemblies = []
        Manifests = []
    }

/// The fixture source. Fixed wherever a test is gating a COMPILATION determinant, so a moved
/// key can only be the field the test perturbed.
let private fixtureSource = "let x = 1"

/// A whole cache key, folded as a driver folds one: `compilationDigest` for the compilation,
/// `fileInputHash` for this file's text. The coverage tests below each move one input and
/// compare two of these.
let private keyOf (source: string) (inputs: Hashing.CompilationInputs) : InputHash =
    Hashing.fileInputHash source (Hashing.compilationDigest inputs)

/// `keyOf` at the fixture source, for the tests that vary the compilation and not the text.
let private keyUnder (inputs: Hashing.CompilationInputs) : InputHash = keyOf fixtureSource inputs

[<Tests>]
let tests =
    testList
        "Hashing"
        [
            testList
                "determinism"
                [
                    test "hashString is pure" {
                        Expect.equal (Hashing.hashString "abc") (Hashing.hashString "abc") "same input, same hash"
                    }

                    test "hashString distinguishes inputs" {
                        Expect.notEqual (Hashing.hashString "abc") (Hashing.hashString "abd") "one byte differs"
                    }

                    test "inputHash is pure" {
                        let a = Hashing.inputHash "source" [ depA; depB ]
                        let b = Hashing.inputHash "source" [ depA; depB ]
                        Expect.equal a b "same source + deps, same hash"
                    }
                ]

            testList
                "source-sensitivity"
                [
                    test "a changed source changes the hash with deps fixed" {
                        let a = Hashing.inputHash "let x = 1" [ depA; depB ]
                        let b = Hashing.inputHash "let x = 2" [ depA; depB ]
                        Expect.notEqual a b "source is a determinant"
                    }
                ]

            testList
                "dependency-sensitivity"
                [
                    test "a changed dependency set changes the hash with source fixed" {
                        let a = Hashing.inputHash "source" [ depA; depB ]
                        let b = Hashing.inputHash "source" [ depA; depC ]
                        Expect.notEqual a b "a dependency signature is a determinant"
                    }

                    test "adding a dependency changes the hash" {
                        let a = Hashing.inputHash "source" [ depA; depB ]
                        let b = Hashing.inputHash "source" [ depA; depB; depC ]
                        Expect.notEqual a b "a larger set is a distinct set"
                    }

                    test "removing a dependency changes the hash" {
                        let a = Hashing.inputHash "source" [ depA; depB ]
                        let b = Hashing.inputHash "source" [ depA ]
                        Expect.notEqual a b "a smaller set is a distinct set"
                    }

                    test "the same dependency set in a different order yields the same hash" {
                        let a = Hashing.inputHash "source" [ depA; depB; depC ]
                        let b = Hashing.inputHash "source" [ depC; depA; depB ]
                        Expect.equal a b "a dependency set is a set, not a sequence"
                    }

                    test "a duplicated dependency does not perturb the set hash" {
                        // A set ignores multiplicity: referencing the same project twice is the
                        // same dependency set, so the fold must deduplicate before hashing.
                        let a = Hashing.inputHash "source" [ depA; depB ]
                        let b = Hashing.inputHash "source" [ depA; depB; depA ]
                        Expect.equal a b "a set ignores duplicate members"
                    }
                ]

            testList
                "dependencySignatureHash over contract bytes"
                [
                    test "a changed contract .fsi changes the signature hash" {
                        let root = freshRoot "contract-change"
                        let manifest = writePackage root "Pkg" "type a = extern\n"
                        let before = Hashing.dependencySignatureHash manifest
                        // Rewrite the SAME contract file with different bytes.
                        File.WriteAllText(Path.Combine(root, "Pkg", "contract.fsi"), "type b = extern\n")
                        let after = Hashing.dependencySignatureHash manifest
                        Expect.notEqual before after "the exported contract determines the signature hash"
                    }

                    test "an unrelated file in the package dir does not change the signature hash" {
                        let root = freshRoot "unrelated-change"
                        let manifest = writePackage root "Pkg" "type a = extern\n"
                        let before = Hashing.dependencySignatureHash manifest
                        // A sibling file NOT listed in `[core] files` is not part of the
                        // contract, so it must not perturb the signature hash.
                        File.WriteAllText(Path.Combine(root, "Pkg", "notes.txt"), "irrelevant\n")
                        let after = Hashing.dependencySignatureHash manifest
                        Expect.equal before after "only the listed contract files feed the hash"
                    }

                    test "dependencySignatureHash is deterministic" {
                        let root = freshRoot "determinism"
                        let manifest = writePackage root "Pkg" "type a = extern\n"

                        Expect.equal
                            (Hashing.dependencySignatureHash manifest)
                            (Hashing.dependencySignatureHash manifest)
                            "same contract, same hash"
                    }

                    test "a file's key folds its source with each dependency's signature hash" {
                        let root = freshRoot "file-input"
                        let manifest = writePackage root "Pkg" "type a = extern\n"

                        let withDep =
                            keyUnder
                                { compilation with
                                    Manifests = [ manifest ]
                                }

                        Expect.notEqual withDep (keyUnder compilation) "a referenced package is part of the key"

                        Expect.notEqual
                            withDep
                            (keyOf
                                "let x = 2"
                                { compilation with
                                    Manifests = [ manifest ]
                                })
                            "so is the file's own text"
                    }

                    // The two halves have different lifetimes, and the digest is the reusable
                    // one: a driver folds it once and keys every file of the compilation off
                    // it. That is only sound if it is a pure function of its inputs.
                    test "a compilation digest is a pure function of its inputs" {
                        let root = freshRoot "digest-purity"
                        let manifest = writePackage root "Pkg" "type a = extern\n"

                        let inputs =
                            { compilation with
                                Manifests = [ manifest ]
                            }

                        Expect.equal
                            (Hashing.compilationDigest inputs)
                            (Hashing.compilationDigest inputs)
                            "same compilation, same digest"

                        let digest = Hashing.compilationDigest inputs

                        Expect.equal
                            (Hashing.fileInputHash fixtureSource digest)
                            (keyUnder inputs)
                            "a hoisted digest keys a file exactly as an inline fold does"
                    }
                ]

            // The key is the driver's whole cache guard, so its obligation is COVERAGE: every
            // input the frozen tree is a function of has to move it. The manifest closure is
            // the subtle one — a driver hands over the roots it was configured with, while the
            // provider build resolves and splices from the transitive closure.
            testList
                "the cache key covers every determinant of the frozen tree"
                [
                    test "an edited TRANSITIVE dependency changes the key" {
                        // `Root` names only `Dep` in `depends-on`; the consumer names only
                        // `Root`. The provider build reaches `Dep` through the closure and
                        // splices its inline bodies, so its bytes are a determinant even though
                        // no caller ever spelled its manifest path.
                        let root = freshRoot "transitive-dep"

                        writePackageFiles
                            root
                            "Dep"
                            "[core]\nnamespace = \"Dep\"\nfiles = [\"dep.fsi\"]\n"
                            [ "dep.fsi", "type d = extern\n" ]
                        |> ignore

                        let rootManifest =
                            writePackageFiles
                                root
                                "Root"
                                "[core]\nnamespace = \"Root\"\nfiles = [\"root.fsi\"]\ndepends-on = [\"Dep\"]\n"
                                [ "root.fsi", "type r = extern\n" ]

                        let inputs =
                            { compilation with
                                Manifests = [ rootManifest ]
                            }

                        let before = keyUnder inputs
                        File.WriteAllText(Path.Combine(root, "Dep", "dep.fsi"), "type e = extern\n")

                        Expect.notEqual
                            before
                            (keyUnder inputs)
                            "the key closes over `depends-on`; only the root was named"
                    }

                    test "the home assembly changes the key" {
                        // The front end roots every minted key at it, so a blob frozen under one
                        // home assembly names its own symbols differently from one frozen under
                        // another — identical source or not.
                        Expect.notEqual
                            (keyUnder compilation)
                            (keyUnder
                                { compilation with
                                    HomeAssembly = "Other"
                                })
                            "the home assembly is a determinant of the frozen tree"
                    }

                    test "the target suffix changes the key" {
                        // A target selects the per-target manifest lists (`inline-bodies-js`,
                        // `files-js`), so two targets over one manifest set are two providers.
                        Expect.notEqual
                            (keyUnder compilation)
                            (keyUnder { compilation with Target = Some "js" })
                            "the target is a determinant of the provider, hence of the tree"
                    }

                    test "the reference assembly set changes the key" {
                        // Folded by identity (path + presence + length + mtime), never contents:
                        // a ref pack dwarfs everything else the key touches.
                        let root = freshRoot "reference-assemblies"
                        let refPath = Path.Combine(root, "Ref.dll")

                        let withRef =
                            { compilation with
                                ReferenceAssemblies = [ refPath ]
                            }

                        let absent = keyUnder withRef

                        Expect.notEqual
                            absent
                            (keyUnder compilation)
                            "a named reference is part of the key even when it is missing"

                        File.WriteAllBytes(refPath, [| 1uy; 2uy; 3uy |])
                        Expect.notEqual absent (keyUnder withRef) "a reference that appears is a different environment"
                    }

                    test "reordering the reference assemblies changes the key" {
                        // Resolution is first-hit by simple name, so the order IS the
                        // resolution — unlike the dependency SET, which the fold sorts.
                        let a = Path.Combine("refs", "A.dll")
                        let b = Path.Combine("refs", "B.dll")

                        Expect.notEqual
                            (keyUnder
                                { compilation with
                                    ReferenceAssemblies = [ a; b ]
                                })
                            (keyUnder
                                { compilation with
                                    ReferenceAssemblies = [ b; a ]
                                })
                            "reference order decides which assembly wins a simple name"
                    }

                    test "the dependency set is order- and multiplicity-insensitive" {
                        // The other half of the same contract: what must NOT move the key.
                        let root = freshRoot "dependency-set"
                        let one = writePackage root "One" "type a = extern\n"
                        let two = writePackage root "Two" "type b = extern\n"

                        let under manifests =
                            keyUnder
                                { compilation with
                                    Manifests = manifests
                                }

                        Expect.equal
                            (under [ one; two ])
                            (under [ two; one ])
                            "a reordered reference list is the same set"

                        Expect.equal
                            (under [ one; two ])
                            (under [ one; two; one ])
                            "a repeated reference is one reference"
                    }
                ]

            // The contract `.fsi` set is NOT the whole determinant of a consumer's output, and
            // treating it as one was a stale hit: a dependency's inline bodies are spliced into
            // the consumer's tree before it is frozen, and the `.fs` companions decide what a
            // primitive resolves to. Each test below gates one path in
            // `ReferencedProject.sourceInputs` — the coverage set the fold is defined over.
            testList
                "dependencySignatureHash covers every source the provider build reads"
                [
                    test "an edited cross-package inline body changes the signature hash" {
                        // THE regression. `[core] inline-bodies` bodies are re-analysed and
                        // spliced into the CONSUMER pre-freeze, so editing one changes what the
                        // consumer emits while touching no `.fsi`.
                        let root = freshRoot "inline-bodies-change"

                        let manifest =
                            writePackageFiles
                                root
                                "Pkg"
                                "[core]\nnamespace = \"Test\"\nfiles = [\"contract.fsi\"]\ninline-bodies = [\"ops.fs\"]\n"
                                [
                                    "contract.fsi", "val inline f: int -> int\n"
                                    "ops.fs", "let inline f x = x + 1\n"
                                ]

                        let struct (before, after) =
                            hashAcrossWrite manifest "ops.fs" "let inline f x = x + 2\n"

                        Expect.notEqual before after "an inline body a consumer splices is a compile determinant"
                    }

                    test "an edited impl .fs changes the signature hash" {
                        let root = freshRoot "impl-change"

                        let manifest =
                            writePackageFiles
                                root
                                "Pkg"
                                "[core]\nnamespace = \"Test\"\nfiles = [\"contract.fsi\"]\nimpl = [\"body.fs\"]\n"
                                [ "contract.fsi", "val f: int -> int\n"; "body.fs", "let f x = x\n" ]

                        let struct (before, after) = hashAcrossWrite manifest "body.fs" "let f x = x + 1\n"

                        Expect.notEqual before after "an impl file is a source the build reads"
                    }

                    test "an edited .fs companion beside a contract changes the signature hash" {
                        // Never named by the manifest — DERIVED from each `.fsi`
                        // (`ReferencedProject.companionFs`) and harvested for the intrinsic
                        // reprs a consumer's primitives resolve through.
                        let root = freshRoot "companion-change"

                        let manifest =
                            writePackageFiles
                                root
                                "Pkg"
                                "[core]\nnamespace = \"Test\"\nfiles = [\"contract.fsi\"]\n"
                                [ "contract.fsi", "type a = extern\n"; "contract.fs", "type a = (# \"A\" #)\n" ]

                        let struct (before, after) =
                            hashAcrossWrite manifest "contract.fs" "type a = (# \"B\" #)\n"

                        Expect.notEqual before after "an intrinsic-repr companion moves a consumer's resolution"
                    }

                    test "an edited per-target extra contract changes the signature hash" {
                        // `files-<t>` APPENDS to the contract surface, so it is contract the
                        // base `files` fold never saw.
                        let root = freshRoot "files-target-change"

                        let manifest =
                            writePackageFiles
                                root
                                "Pkg"
                                "[core]\nnamespace = \"Test\"\nfiles = [\"contract.fsi\"]\nfiles-js = [\"shim.js.fsi\"]\n"
                                [ "contract.fsi", "type a = extern\n"; "shim.js.fsi", "type b = extern\n" ]

                        let struct (before, after) =
                            hashAcrossWrite manifest "shim.js.fsi" "type c = extern\n"

                        Expect.notEqual before after "a per-target extra contract is contract surface"
                    }

                    test "an edited per-target companion changes the signature hash" {
                        // The suffix comes from `files-js`; `contract.js.fs` is named by NEITHER
                        // list — it is the target companion of `contract.fsi`, which is why
                        // `targetSuffixes` has to sweep every per-target key.
                        let root = freshRoot "target-companion-change"

                        let manifest =
                            writePackageFiles
                                root
                                "Pkg"
                                "[core]\nnamespace = \"Test\"\nfiles = [\"contract.fsi\"]\nfiles-js = [\"shim.js.fsi\"]\n"
                                [
                                    "contract.fsi", "type a = extern\n"
                                    "shim.js.fsi", "type b = extern\n"
                                    "contract.js.fs", "type a = (# \"number\" #)\n"
                                ]

                        let struct (before, after) =
                            hashAcrossWrite manifest "contract.js.fs" "type a = (# \"bigint\" #)\n"

                        Expect.notEqual before after "a target companion is a source the build reads"
                    }

                    test "reordering the manifest's file list changes the signature hash" {
                        // Compile order is a determinant and lives in the manifest, not in any
                        // file's contents — so it is the manifest's own bytes that carry it.
                        let root = freshRoot "manifest-order-change"

                        let manifest =
                            writePackageFiles
                                root
                                "Pkg"
                                "[core]\nnamespace = \"Test\"\nfiles = [\"a.fsi\", \"b.fsi\"]\n"
                                [ "a.fsi", "type a = extern\n"; "b.fsi", "type b = extern\n" ]

                        let struct (before, after) =
                            hashAcrossWrite
                                manifest
                                "manifest.toml"
                                "[core]\nnamespace = \"Test\"\nfiles = [\"b.fsi\", \"a.fsi\"]\n"

                        Expect.notEqual before after "compile order is part of the signature"
                    }

                    test "creating a file the manifest already named changes the signature hash" {
                        // Absent and present-but-empty must differ: length prefixes alone cannot
                        // say which, so the fold writes a presence byte per named path.
                        let root = freshRoot "absent-to-empty"

                        let manifest =
                            writePackageFiles
                                root
                                "Pkg"
                                "[core]\nnamespace = \"Test\"\nfiles = [\"contract.fsi\"]\nimpl = [\"body.fs\"]\n"
                                [ "contract.fsi", "type a = extern\n" ]

                        let struct (before, after) = hashAcrossWrite manifest "body.fs" ""

                        Expect.notEqual before after "an absent source is distinct from an empty one"
                    }
                ]
        ]
