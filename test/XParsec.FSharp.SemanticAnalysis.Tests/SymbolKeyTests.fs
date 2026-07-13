module XParsec.FSharp.SemanticAnalysis.Tests.SymbolKeyTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

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

                // The local-def path (`LocalSymbolKey.ofType` over the same containment —
                // namespace `Vesper.Collections`, home `Vesper.List`) produces exactly
                // this literal.
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

            // `moduleFullName` is the ONE rendering of a module's containment. Nothing parses
            // it back, so its correctness argument is that it renders each holder shape whole.
            test "module full name renders the holder chain" {
                Expect.equal
                    (SymbolKeyOps.moduleFullName (SymbolKeyOps.moduleInNamespace (Some "A") "Vesper" "Unchecked"))
                    "Vesper.Unchecked"
                    "namespace-qualified module"

                Expect.equal
                    (SymbolKeyOps.moduleFullName (SymbolKeyOps.moduleInNamespace (Some "A") "" "Util"))
                    "Util"
                    "a module in the global namespace"
            }

            // A NESTED module: the holder chain no producer could mint while `moduleKeyOf`
            // took a dotted name (its last segment became the module and the rest the
            // namespace, so `Inner` and `Outer` both flattened into the namespace path).
            // `Outer` must be a MODULE here, not a namespace segment.
            test "nested module: the chain nests, and the namespace stops where it stops" {
                let outer = SymbolKeyOps.moduleInNamespace (Some "A") "Vesper" "Outer"

                let inner = SymbolKeyOps.moduleKeyOf (ModuleHolder.InModule outer) "Inner"

                Expect.equal (SymbolKeyOps.moduleFullName inner) "Vesper.Outer.Inner" "the whole chain renders"

                Expect.equal
                    (List.ofSeq inner.Namespace.Path.Underlying)
                    [ "Vesper" ]
                    "the namespace is `Vesper` alone — `Outer` is a module, not a namespace segment"

                Expect.equal
                    inner.Origin
                    (Origin.InAssembly(AssemblyName "A"))
                    "the home assembly is reachable through the nested chain"

                let b = SymbolKeyOps.bindingKeyOf (ModuleHolder.InModule inner) "f"

                Expect.equal
                    (SymbolKeyOps.qualifiedName (SymbolKey.Binding b))
                    "Vesper.Outer.Inner.f"
                    "a binding in a nested module qualifies through the whole chain"
            }

            // The UNQUALIFIED binding — a flat package's export / a global extern. Its holder
            // is the global namespace, NOT an absent one: it still carries the home assembly,
            // which is what a `ModuleKey voption` could not have done.
            test "unqualified binding: holder is the global namespace, home assembly survives" {
                let b = SymbolKeyOps.bindingKeyOf (SymbolKeyOps.inNamespace (Some "pkg") "") "f"

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

            // `reroot` is how a provider stack re-homes the keys its inner leaf minted before
            // the package's assembly was known. It rewrites the `Origin` at the ROOT of the
            // chain and nothing else — in particular it does not re-derive the chain from a
            // rendered name, which is what used to flatten a nested module away.
            test "reroot rewrites the home assembly and preserves the containment chain" {
                let inner =
                    SymbolKeyOps.moduleKeyOf
                        (ModuleHolder.InModule(SymbolKeyOps.moduleInNamespace None "Vesper" "Outer"))
                        "Inner"

                let k =
                    SymbolKey.Binding(SymbolKeyOps.bindingKeyOf (ModuleHolder.InModule inner) "f")

                let homed = SymbolKeyOps.reroot (Origin.InAssembly(AssemblyName "Vesper.Core")) k

                Expect.equal
                    homed.Origin
                    (Origin.InAssembly(AssemblyName "Vesper.Core"))
                    "the root carries the new home"

                Expect.equal
                    (SymbolKeyOps.qualifiedName homed)
                    (SymbolKeyOps.qualifiedName k)
                    "the containment chain is untouched"
            }
        ]

// The DECLARING containment of a project-local type, as minted by
// `NameResolutionTypeRegistration.stampLocalTypeKey`. A type declared inside a `module`
// is held by that module (`TypeHolder.InModule`), not by the namespace the module sits in
// — the module name is neither folded into the namespace path nor dropped.
//
// The `(name, arity)` CLAIM table stays namespace- and module-blind, so this does not yet
// admit two same-named sibling-module types (see `Codegen.Clr.Tests.LocalModuleTests`),
// and the emitted metadata name is unchanged: `typeMetaName` renders an `InModule` key
// exactly as the namespace-held key it replaces.
module private Local =

    /// The `TypeKey` NameResolution minted for the local type `name` at `arity`.
    let typeKeyOf (arity: int) (name: string) (src: string) : TypeKey =
        let ctx, _ = analyseNameRes (realProvider.Force()) src

        match TypeRegistry.tryTypeClaim ctx.Types SourcePos.unbounded name arity with
        | ValueSome id -> id.Key
        | ValueNone -> failtestf "no type claim for %s`%d" name arity

    let source (lines: string list) : string = String.concat "\n" lines

[<Tests>]
let localTypeContainment =
    testList
        "SymbolKey local type containment"
        [
            test "a type declared directly in a namespace is held by the NAMESPACE" {
                let k =
                    Local.typeKeyOf 0 "T" (Local.source [ "namespace N"; ""; "type T = { x: int }" ])

                match k.Holder with
                | TypeHolder.InNamespace ns ->
                    Expect.equal (List.ofSeq ns.Path.Underlying) [ "N" ] "the declaring namespace, segmented"
                | other -> failtestf "expected InNamespace, got %A" other
            }

            test "a type declared inside a module is held by the MODULE" {
                let k =
                    Local.typeKeyOf 0 "T" (Local.source [ "namespace N"; ""; "module M ="; "    type T = { x: int }" ])

                match k.Holder with
                | TypeHolder.InModule m ->
                    Expect.equal m.Name "M" "the enclosing module, by its compiled holder name"

                    match m.Holder with
                    | ModuleHolder.InNamespace ns ->
                        Expect.equal
                            (List.ofSeq ns.Path.Underlying)
                            [ "N" ]
                            "the namespace at the root of the chain — `M` is NOT a namespace segment"
                    | other -> failtestf "expected the module to sit in a namespace, got %A" other
                | other -> failtestf "expected InModule, got %A" other

                Expect.equal
                    (List.ofSeq k.Namespace.Path.Underlying)
                    [ "N" ]
                    "`TypeKey.Namespace` walks the chain to its root"

                // The behaviour freeze: the module rides in the KEY, and nowhere else. The
                // metadata name a module-held type renders (and emits) as is unchanged.
                Expect.equal (SymbolKeyOps.typeMetaName k) "N.T" "the rendered metadata name does not move"
            }

            test "a NESTED module produces a nested InModule chain" {
                let k =
                    Local.typeKeyOf
                        0
                        "T"
                        (Local.source
                            [
                                "namespace N"
                                ""
                                "module A ="
                                "    module B ="
                                "        type T = { x: int }"
                            ])

                match k.Holder with
                | TypeHolder.InModule b ->
                    Expect.equal b.Name "B" "held by the INNERMOST module"

                    match b.Holder with
                    | ModuleHolder.InModule a ->
                        Expect.equal a.Name "A" "which is itself held by the outer module"

                        Expect.equal
                            (List.ofSeq a.Namespace.Path.Underlying)
                            [ "N" ]
                            "and the outer module by the namespace — neither module is a namespace segment"
                    | other -> failtestf "expected B's holder to be module A, got %A" other
                | other -> failtestf "expected InModule, got %A" other

                Expect.equal (SymbolKeyOps.typeMetaName k) "N.T" "the rendered metadata name is still flat"
            }

            // `ModuleKey.Name` carries the COMPILED holder name — the static class the module
            // compiles to — which is what a `ModuleKey` means at every other mint (the
            // contract face bakes the suffix in at mint time too). The rule has ONE
            // implementation (`moduleHolderName`), so the key and the emitted holder cannot
            // disagree about which class holds what.
            test "the module's key carries its COMPILED holder name (…Module on a type collision)" {
                let k =
                    Local.typeKeyOf
                        0
                        "T"
                        (Local.source
                            [
                                "namespace N"
                                ""
                                "type M = { a: int }"
                                ""
                                "module M ="
                                "    type T = { x: int }"
                            ])

                match k.Holder with
                | TypeHolder.InModule m ->
                    Expect.equal m.Name "MModule" "the module collides with `type M`, so its holder class is suffixed"
                | other -> failtestf "expected InModule, got %A" other
            }
        ]
