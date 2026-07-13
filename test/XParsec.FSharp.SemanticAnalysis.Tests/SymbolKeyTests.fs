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
                            Namespace = SymbolKeyOps.namespaceKey (Some "Vesper.List") "Vesper.Collections"
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
                            Namespace = SymbolKeyOps.namespaceKey (Some "Vesper.Core") "Vesper"
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

            // A package origin is a BLANKET fact (`Vesper`), but a type in that package can
            // live DEEPER (`Vesper.Collections.seq`). Deriving the namespace by stripping the
            // blanket origin off the compiled name mis-cuts it into ns=`Vesper` /
            // name=`Collections.seq`, which then fails to match the capability's canonical
            // key. The namespace must come from the name the type belongs to.
            test "blanket package origin does not mis-cut a deeper compiled name" {
                let blanket =
                    {
                        Namespace = SymbolKeyOps.namespaceKey (Some "Vesper.Core") "Vesper"
                    }

                let viaOrigin = SymbolKeyOps.externalTypeKeyOf blanket "Vesper.Collections.seq" 1

                let viaQualified =
                    SymbolKeyOps.qualifiedTypeKeyOfT (Some "Vesper.Core") "Vesper.Collections.seq" 1

                Expect.equal viaOrigin viaQualified "both external mint paths agree"

                Expect.equal
                    (List.ofSeq viaOrigin.Namespace.Path.Underlying)
                    [ "Vesper"; "Collections" ]
                    "the namespace is segmented from the name, not cut at the blanket origin"

                Expect.equal viaOrigin.Name "seq`1" "the simple name is the last segment, arity-suffixed"
            }

            // `typeMetaName` is THE renderer and `typeKeyOf` THE parser for the `+`-mangled
            // reflection display name of a CLR nested type. They must invert each other, and
            // the nesting must land in the holder chain — not survive inside a key's `Name`.
            test "nested type: the `+` chain becomes holders, and renders back unchanged" {
                let k =
                    SymbolKeyOps.typeKeyOf
                        (Some "System.Private.CoreLib")
                        "System.Collections.Generic"
                        "List`1+Enumerator"

                match k.Holder with
                | TypeHolder.InType outer ->
                    Expect.equal outer.Name "List`1" "the outer keeps its arity suffix"
                    Expect.equal k.Name "Enumerator" "the inner Name is the bare segment, not `+`-mangled"
                | other -> failtestf "expected InType, got %A" other

                Expect.equal
                    (SymbolKeyOps.typeMetaName k)
                    "System.Collections.Generic.List`1+Enumerator"
                    "the renderer inverts the parser"

                Expect.equal
                    (SymbolKeyOps.typeNs k)
                    "System.Collections.Generic"
                    "a nested type reports its OUTER's namespace, as the CLR does"

                // A nested type's arity is carried by its OUTER, so requesting arity 1 must not
                // re-suffix the inner segment.
                Expect.equal
                    (SymbolKeyOps.qualifiedTypeKeyOfT (Some "A") "N.List`1+Enumerator" 1
                     |> SymbolKeyOps.typeMetaName)
                    "N.List`1+Enumerator"
                    "the arity is already spelled by the outer; it is not re-appended to the inner"
            }

            // `moduleKeyOf` is a last-dot split; its only correctness argument is that
            // `moduleFullName` inverts it exactly.
            test "module full name round-trips through moduleKeyOf" {
                Expect.equal
                    (SymbolKeyOps.moduleFullName (SymbolKeyOps.moduleKeyOf (Some "A") "Vesper.Unchecked"))
                    "Vesper.Unchecked"
                    "namespace-qualified module round-trips"

                Expect.equal
                    (SymbolKeyOps.moduleFullName (SymbolKeyOps.moduleKeyOf (Some "A") "Util"))
                    "Util"
                    "a module in the global namespace round-trips"
            }

            // The UNQUALIFIED binding — a flat package's export / a global extern. Its holder
            // is the global namespace, NOT an absent one: it still carries the home assembly,
            // which is what a `ModuleKey voption` could not have done.
            test "unqualified binding: holder is the global namespace, home assembly survives" {
                let b = SymbolKeyOps.bindingKeyOf (Some "pkg") "" "f"

                match b.Decl with
                | ModuleHolder.InNamespace ns ->
                    Expect.isTrue ns.Path.IsEmpty "the global namespace is an EMPTY path, not a sentinel"

                    Expect.equal
                        ns.Origin
                        (Origin.InAssembly(AssemblyName "pkg"))
                        "the home assembly is reachable through the holder chain"
                | other -> failtestf "expected InNamespace, got %A" other

                Expect.equal (SymbolKeyOps.holderFullName b.Decl) "" "an empty holder renders empty"

                Expect.equal
                    (SymbolKeyOps.qualifiedName (SymbolKey.Binding b))
                    "f"
                    "qualifiedName drops the empty holder rather than emitting a leading dot"
            }
        ]
