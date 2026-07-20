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
/// by name and wiped on entry so a prior run cannot leak stale `.fsi` bytes. The repo keeps
/// scratch out of the system temp.
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
                        Directory.Delete(root, true)
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
                        Directory.Delete(root, true)
                    }

                    test "dependencySignatureHash is deterministic" {
                        let root = freshRoot "determinism"
                        let manifest = writePackage root "Pkg" "type a = extern\n"
                        Expect.equal
                            (Hashing.dependencySignatureHash manifest)
                            (Hashing.dependencySignatureHash manifest)
                            "same contract, same hash"
                        Directory.Delete(root, true)
                    }

                    test "fileInputHash folds source with the dependency signature hashes" {
                        let root = freshRoot "file-input"
                        let manifest = writePackage root "Pkg" "type a = extern\n"
                        let viaSeam = Hashing.fileInputHash "let x = 1" [ manifest ]
                        let viaParts =
                            Hashing.inputHash "let x = 1" [ Hashing.dependencySignatureHash manifest ]
                        Expect.equal viaSeam viaParts "the seam function is the documented fold"
                        Directory.Delete(root, true)
                    }
                ]
        ]
