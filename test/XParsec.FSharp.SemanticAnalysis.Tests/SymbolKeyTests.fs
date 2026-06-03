module XParsec.FSharp.SemanticAnalysis.Tests.SymbolKeyTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// Guards the "asm agrees across every mint path" invariant. Unification compares
// nominal `SemType`s by *full* `SymbolKey` equality,
// including the home assembly `asm` — so if one producer mints `asm = None` and
// another `asm = Some "X"` for the same type they stop unifying with no diagnostic.
// Every external-type producer routes through `SymbolKeyOps.externalTypeKey`
// (origin in hand) or `qualifiedTypeKeyOf` (explicit asm + qualified name); the
// local-def path is `LocalSymbolKey.ofType`, whose output for a well-known type IS
// the canonical `RuntimeNames.*Key` literal. These assert all three agree, asm and
// all, for the well-known singletons whose key the rest of the pipeline matches on.
[<Tests>]
let tests =
    testList
        "SymbolKey asm invariant"
        [
            test "cons-list: external-ref mint paths equal the local-def canonical key" {
                // External ref resolved from a `SymbolOrigin` (the consumer path:
                // `mkNominal` / `Translate` / `InferResolve`).
                let viaOrigin =
                    SymbolKeyOps.externalTypeKey
                        {
                            Assembly = Some "Vesper.List"
                            Namespace = "Vesper.Collections"
                            DeclaringType = None
                        }
                        "Vesper.Collections.List"
                        1

                // External ref minted from an explicit home assembly + qualified name
                // (the asm-aware codegen / metadata path).
                let viaQualified =
                    SymbolKeyOps.qualifiedTypeKeyOf (Some "Vesper.List") "Vesper.Collections.List" 1

                // The local-def path (`LocalSymbolKey.ofType (Some "Vesper.List")
                // "Vesper.Collections" "List" 1`) produces exactly this literal.
                Expect.equal viaOrigin RuntimeNames.vesperListKey "origin mint = canonical (incl. asm)"
                Expect.equal viaQualified RuntimeNames.vesperListKey "qualified mint = canonical (incl. asm)"
                Expect.equal viaOrigin viaQualified "both external mint paths agree"
            }

            test "ref cell: external-ref mint paths equal the local-def canonical key" {
                let viaOrigin =
                    SymbolKeyOps.externalTypeKey
                        {
                            Assembly = Some "Vesper.Core"
                            Namespace = "Vesper"
                            DeclaringType = None
                        }
                        "Vesper.Ref"
                        1

                let viaQualified =
                    SymbolKeyOps.qualifiedTypeKeyOf (Some "Vesper.Core") "Vesper.Ref" 1

                Expect.equal viaOrigin RuntimeNames.vesperRefKey "origin mint = canonical (incl. asm)"
                Expect.equal viaQualified RuntimeNames.vesperRefKey "qualified mint = canonical (incl. asm)"
                Expect.equal viaOrigin viaQualified "both external mint paths agree"
            }

            test "asm is load-bearing: same (ns, name, arity) but differing home assembly are distinct keys" {
                // The failure mode the unify guard catches: two mints that agree on
                // everything but `asm` must NOT compare equal (else a drifting mint
                // path silently unifies / fails to unify with no diagnostic).
                let homeA =
                    SymbolKeyOps.qualifiedTypeKeyOf (Some "Vesper.List") "Vesper.Collections.List" 1

                let homeB =
                    SymbolKeyOps.qualifiedTypeKeyOf (Some "Other.Asm") "Vesper.Collections.List" 1

                let asmless = SymbolKeyOps.qualifiedTypeKey "Vesper.Collections.List" 1

                Expect.notEqual homeA homeB "differing home assembly ⇒ distinct key"
                Expect.notEqual homeA asmless "asm = None ⇒ distinct from asm = Some home"

                Expect.equal
                    (SymbolKeyOps.qualifiedName homeA)
                    (SymbolKeyOps.qualifiedName homeB)
                    "yet they project to the same qualified name — exactly the silent-mismatch shape"
            }
        ]
