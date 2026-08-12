module XParsec.FSharp.SemanticAnalysis.Tests.HashingTests

open System.IO
open Expecto
open XParsec.FSharp.SemanticAnalysis

/// Stand-in dependency signatures, so the fold-shape tests need no on-disk manifest.
let private depA = Hashing.hashString "dependency-A"
let private depB = Hashing.hashString "dependency-B"
let private depC = Hashing.hashString "dependency-C"

/// A per-test scratch directory under the repo `./tmp`, wiped ON ENTRY and never on exit: a
/// failing run's files survive for inspection.
let private freshRoot (name: string) : string =
    let root =
        Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "tmp", "hashing-tests", name)

    if Directory.Exists root then
        Directory.Delete(root, true)

    Directory.CreateDirectory root |> ignore
    root

/// Write `<root>/<pkg>/manifest.<target>.toml` with `manifestBody`, plus each
/// `(relative path, contents)` file, and return the manifest path. The directory name is the
/// package identity.
let private writePackageFilesFor
    (root: string)
    (pkg: string)
    (target: string)
    (manifestBody: string)
    (files: (string * string) list)
    : ReferencedProject.ManifestPath =
    let dir = Path.Combine(root, pkg)
    Directory.CreateDirectory dir |> ignore

    for (rel, contents) in files do
        File.WriteAllText(Path.Combine(dir, rel), contents)

    File.WriteAllText(Path.Combine(dir, "manifest." + target + ".toml"), manifestBody)

    match ReferencedProject.resolveManifest target dir with
    | Result.Ok mp -> mp
    | Result.Error e -> failwithf "resolveManifest: %s" e

/// `writePackageFilesFor` at the clr target, which `compilation` below names.
let private writePackageFiles (root: string) (pkg: string) (manifestBody: string) (files: (string * string) list) =
    writePackageFilesFor root pkg "clr" manifestBody files

/// A one-contract clr package: `contract.fsi` carrying `contract`, and nothing else.
let private writePackage (root: string) (pkg: string) (contract: string) : ReferencedProject.ManifestPath =
    writePackageFiles root pkg "[core]\nfiles = [\"contract.fsi\"]\n" [ "contract.fsi", contract ]

/// The package's signature AS OF NOW: re-parsed each call, so a test that rewrites the
/// manifest itself is hashing what it just wrote.
let private signatureHash (mp: ReferencedProject.ManifestPath) : InputHash =
    match ReferencedProject.loadManifest mp with
    | Result.Ok m -> Hashing.dependencySignatureHash m
    | Result.Error e -> failwithf "loadManifest: %s" e

/// Hash, rewrite ONE file of the package, hash again.
let private hashAcrossWrite (mp: ReferencedProject.ManifestPath) (rel: string) (contents: string) =
    let before = signatureHash mp
    File.WriteAllText(Path.Combine(mp.PackageDir, rel), contents)
    struct (before, signatureHash mp)

/// A minimal `CompilationInputs` the tests below perturb ONE field of at a time.
let private compilation: Hashing.CompilationInputs =
    {
        HomeAssembly = "Consumer"
        Target = "clr"
        ReferenceAssemblies = []
        Packages = []
        SelfPackage = None
    }

/// Held fixed wherever a test gates a COMPILATION determinant, so a moved key can only be
/// the field the test perturbed.
let private fixtureSource = "let x = 1"

let private keyOf (source: string) (inputs: Hashing.CompilationInputs) : InputHash =
    Hashing.fileInputHash (Hashing.textOriginPath source) source (Hashing.compilationDigest inputs)

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
                        // Referencing the same project twice is the same dependency set.
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
                        let before = signatureHash manifest
                        File.WriteAllText(Path.Combine(root, "Pkg", "contract.fsi"), "type b = extern\n")
                        let after = signatureHash manifest
                        Expect.notEqual before after "the exported contract determines the signature hash"
                    }

                    test "an unrelated file in the package dir does not change the signature hash" {
                        let root = freshRoot "unrelated-change"
                        let manifest = writePackage root "Pkg" "type a = extern\n"
                        let before = signatureHash manifest
                        File.WriteAllText(Path.Combine(root, "Pkg", "notes.txt"), "irrelevant\n")
                        let after = signatureHash manifest
                        Expect.equal before after "only the listed contract files feed the hash"
                    }

                    test "dependencySignatureHash is deterministic" {
                        let root = freshRoot "determinism"
                        let manifest = writePackage root "Pkg" "type a = extern\n"

                        Expect.equal (signatureHash manifest) (signatureHash manifest) "same contract, same hash"
                    }

                    test "a file's key folds its source with each dependency's signature hash" {
                        let root = freshRoot "file-input"
                        let manifest = writePackage root "Pkg" "type a = extern\n"

                        let withDep =
                            keyUnder
                                { compilation with
                                    Packages = [ manifest.PackageDir ]
                                }

                        Expect.notEqual withDep (keyUnder compilation) "a referenced package is part of the key"

                        Expect.notEqual
                            withDep
                            (keyOf
                                "let x = 2"
                                { compilation with
                                    Packages = [ manifest.PackageDir ]
                                })
                            "so is the file's own text"
                    }

                    test "moving ONLY the file's path moves its key" {
                        let digest = Hashing.compilationDigest compilation

                        let under (path: OriginPath) =
                            Hashing.fileInputHash path fixtureSource digest

                        let a =
                            {
                                BucketName = "Pkg"
                                Relative = "a.fs"
                            }

                        Expect.notEqual
                            (under a)
                            (under { a with Relative = "b.fs" })
                            "the relative path is a key input"

                        Expect.notEqual (under a) (under { a with BucketName = "Other" }) "…and so is the bucket"

                        // Bucket and relative path are a record, not a set: `("x", "y")` is a
                        // different file from `("y", "x")`.
                        Expect.notEqual
                            (under
                                { a with
                                    BucketName = "x"
                                    Relative = "y"
                                })
                            (under
                                { a with
                                    BucketName = "y"
                                    Relative = "x"
                                })
                            "transposing two fields is a different file, not the same one"
                    }

                    // A driver folds the digest once and keys every file of the compilation
                    // off it, which is sound only if it is pure.
                    test "a compilation digest is a pure function of its inputs" {
                        let root = freshRoot "digest-purity"
                        let manifest = writePackage root "Pkg" "type a = extern\n"

                        let inputs =
                            { compilation with
                                Packages = [ manifest.PackageDir ]
                            }

                        Expect.equal
                            (Hashing.compilationDigest inputs)
                            (Hashing.compilationDigest inputs)
                            "same compilation, same digest"

                        let digest = Hashing.compilationDigest inputs

                        Expect.equal
                            (Hashing.fileInputHash (Hashing.textOriginPath fixtureSource) fixtureSource digest)
                            (keyUnder inputs)
                            "a hoisted digest keys a file exactly as an inline fold does"
                    }
                ]

            // Every input the frozen tree is a function of has to move the key. The manifest
            // closure is the subtle one: a driver hands over ROOTS, the provider build reads
            // the transitive closure.
            testList
                "the cache key covers every determinant of the frozen tree"
                [
                    test "an edited TRANSITIVE dependency changes the key" {
                        // `Root` names only `Dep` in `depends-on` and the consumer only `Root`,
                        // so no caller ever spells `Dep`'s manifest path.
                        let root = freshRoot "transitive-dep"

                        writePackageFiles
                            root
                            "Dep"
                            "[core]\nfiles = [\"dep.fsi\"]\n"
                            [ "dep.fsi", "type d = extern\n" ]
                        |> ignore

                        let rootManifest =
                            writePackageFiles
                                root
                                "Root"
                                "[core]\nfiles = [\"root.fsi\"]\ndepends-on = [\"Dep\"]\n"
                                [ "root.fsi", "type r = extern\n" ]

                        let inputs =
                            { compilation with
                                Packages = [ rootManifest.PackageDir ]
                            }

                        let before = keyUnder inputs
                        File.WriteAllText(Path.Combine(root, "Dep", "dep.fsi"), "type e = extern\n")

                        Expect.notEqual
                            before
                            (keyUnder inputs)
                            "the key closes over `depends-on`; only the root was named"
                    }

                    test "the self manifest changes the key" {
                        // Naming a package as SELF and as a REFERENCE is the same bytes under a
                        // different provider, so the two must not share a blob.
                        let root = freshRoot "self-manifest"
                        let manifest = writePackage root "Pkg" "type a = extern\n"

                        let asSelf =
                            { compilation with
                                SelfPackage = Some manifest.PackageDir
                            }

                        let asReference =
                            { compilation with
                                Packages = [ manifest.PackageDir ]
                            }

                        Expect.notEqual (keyUnder asSelf) (keyUnder compilation) "a self package is part of the key"

                        Expect.notEqual
                            (keyUnder asSelf)
                            (keyUnder asReference)
                            "compiling a package is not the same as referencing it"
                    }

                    test "an edited self package changes the key" {
                        let root = freshRoot "self-manifest-edit"
                        let manifest = writePackage root "Pkg" "type a = extern\n"

                        let inputs =
                            { compilation with
                                SelfPackage = Some manifest.PackageDir
                            }

                        let before = keyUnder inputs
                        File.WriteAllText(Path.Combine(root, "Pkg", "contract.fsi"), "type b = extern\n")

                        Expect.notEqual before (keyUnder inputs) "the self package's contents are a determinant"
                    }

                    test "the home assembly changes the key" {
                        Expect.notEqual
                            (keyUnder compilation)
                            (keyUnder
                                { compilation with
                                    HomeAssembly = "Other"
                                })
                            "the home assembly is a determinant of the frozen tree"
                    }

                    test "the target changes the key" {
                        // Two targets over one manifest set are two providers.
                        Expect.notEqual
                            (keyUnder compilation)
                            (keyUnder { compilation with Target = "js" })
                            "the target is a determinant of the provider, hence of the tree"
                    }

                    test "the reference assembly set changes the key" {
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
                        // The dependency SET, by contrast, is sorted before folding.
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
                        let root = freshRoot "dependency-set"
                        let one = writePackage root "One" "type a = extern\n"
                        let two = writePackage root "Two" "type b = extern\n"

                        let under manifests =
                            keyUnder
                                { compilation with
                                    Packages =
                                        manifests |> List.map (fun (m: ReferencedProject.ManifestPath) -> m.PackageDir)
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

            // The contract `.fsi` set is NOT the whole determinant of a consumer's output: a
            // dependency's inline bodies splice into the consumer's tree before it is frozen,
            // and the `.fs` companions decide what a primitive resolves to.
            testList
                "dependencySignatureHash covers every source the provider build reads"
                [
                    test "an edited cross-package inline body changes the signature hash" {
                        // Editing `let inline f x = x + 1` changes what the consumer emits
                        // while touching no `.fsi`.
                        let root = freshRoot "inline-bodies-change"

                        let manifest =
                            writePackageFiles
                                root
                                "Pkg"
                                "[core]\nfiles = [\"contract.fsi\"]\nimpl = [\"ops.fs\"]\n"
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
                                "[core]\nfiles = [\"contract.fsi\"]\nimpl = [\"body.fs\"]\n"
                                [ "contract.fsi", "val f: int -> int\n"; "body.fs", "let f x = x\n" ]

                        let struct (before, after) = hashAcrossWrite manifest "body.fs" "let f x = x + 1\n"

                        Expect.notEqual before after "an impl file is a source the build reads"
                    }

                    test "an edited intrinsic-repr body changes the signature hash" {
                        // `type a = (# "A" #)` is what a consumer's `a` resolves through, and it
                        // moves with no `.fsi` touched.
                        let root = freshRoot "companion-change"

                        let manifest =
                            writePackageFiles
                                root
                                "Pkg"
                                "[core]\nfiles = [\"contract.fsi\"]\nimpl = [\"contract.fs\"]\n"
                                [ "contract.fsi", "type a = extern\n"; "contract.fs", "type a = (# \"A\" #)\n" ]

                        let struct (before, after) =
                            hashAcrossWrite manifest "contract.fs" "type a = (# \"B\" #)\n"

                        Expect.notEqual before after "an intrinsic-repr companion moves a consumer's resolution"
                    }

                    test "an edited extra contract changes the signature hash" {
                        // A second `files` entry is contract surface too, not just the first.
                        let root = freshRoot "extra-contract-change"

                        let manifest =
                            writePackageFiles
                                root
                                "Pkg"
                                "[core]\nfiles = [\"contract.fsi\", \"shim.fsi\"]\n"
                                [ "contract.fsi", "type a = extern\n"; "shim.fsi", "type b = extern\n" ]

                        let struct (before, after) = hashAcrossWrite manifest "shim.fsi" "type c = extern\n"

                        Expect.notEqual before after "every contract a manifest names is contract surface"
                    }

                    test "a body only the js manifest names leaves the clr signature hash alone" {
                        // The digest is PER TARGET: each manifest folds the sources IT names, so
                        // a JS-only body edit cannot invalidate a CLR consumer's frozen cache.
                        let root = freshRoot "per-target-digest"

                        let clrManifest =
                            writePackageFilesFor
                                root
                                "Pkg"
                                "clr"
                                "[core]\nfiles = [\"contract.fsi\"]\nimpl = [\"contract.clr.fs\"]\n"
                                [
                                    "contract.fsi", "type a = extern\n"
                                    "contract.clr.fs", "type a = (# \"System.Int32\" #)\n"
                                ]

                        let jsManifest =
                            writePackageFilesFor
                                root
                                "Pkg"
                                "js"
                                "[core]\nfiles = [\"contract.fsi\"]\nimpl = [\"contract.js.fs\"]\n"
                                [ "contract.js.fs", "type a = (# \"number\" #)\n" ]

                        let clrBefore = signatureHash clrManifest

                        let struct (jsBefore, jsAfter) =
                            hashAcrossWrite jsManifest "contract.js.fs" "type a = (# \"bigint\" #)\n"

                        Expect.notEqual jsBefore jsAfter "the js digest moves with the body it names"

                        Expect.equal
                            clrBefore
                            (signatureHash clrManifest)
                            "and the clr digest, which names no js body, does not"
                    }

                    test "reordering the manifest's file list changes the signature hash" {
                        // Compile order lives in the manifest, not in any file's contents.
                        let root = freshRoot "manifest-order-change"

                        let manifest =
                            writePackageFiles
                                root
                                "Pkg"
                                "[core]\nfiles = [\"a.fsi\", \"b.fsi\"]\n"
                                [ "a.fsi", "type a = extern\n"; "b.fsi", "type b = extern\n" ]

                        let struct (before, after) =
                            hashAcrossWrite manifest "manifest.clr.toml" "[core]\nfiles = [\"b.fsi\", \"a.fsi\"]\n"

                        Expect.notEqual before after "compile order is part of the signature"
                    }

                    test "creating a file the manifest already named changes the signature hash" {
                        // Absent and present-but-empty both length-prefix as zero.
                        let root = freshRoot "absent-to-empty"

                        let manifest =
                            writePackageFiles
                                root
                                "Pkg"
                                "[core]\nfiles = [\"contract.fsi\"]\nimpl = [\"body.fs\"]\n"
                                [ "contract.fsi", "type a = extern\n" ]

                        let struct (before, after) = hashAcrossWrite manifest "body.fs" ""

                        Expect.notEqual before after "an absent source is distinct from an empty one"
                    }
                ]
        ]
