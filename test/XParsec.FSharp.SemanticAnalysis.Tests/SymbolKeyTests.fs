module XParsec.FSharp.SemanticAnalysis.Tests.SymbolKeyTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// The home assembly is a physical LOCATION, not part of a nominal identity: it lives
/// on a resolved shape's `SymbolOrigin`, never in the key. So the same type minted via
/// ANY path — from a resolved external origin (`mkNominal` / `Translate` /
/// `InferResolve`), from a bare fully-qualified compiled name (the ten string-fed sites
/// that have no assembly in hand at all), or by local definition (`LocalSymbolKey.ofType`,
/// whose output for a well-known type IS the `RuntimeNames.*Key` literal) — must compare
/// EQUAL, by construction rather than by assertion. Unification compares nominal
/// `SemType`s by full `SymbolKey` equality, so a mint path that disagreed would silently
/// fail to unify with no diagnostic. These pin the agreement for the well-known
/// singletons the rest of the pipeline matches on, INCLUDING across differing homes.
let private originIn (asm: string) (ns: string) : SymbolOrigin =
    {
        Home = Origin.InAssembly(AssemblyName asm)
        Namespace = SymbolKeyOps.namespaceKey ns
    }

[<Tests>]
let tests =
    testList
        "SymbolKey mint-path invariant"
        [
            test "cons-list: every mint path yields the canonical key, whatever the home" {
                // External ref resolved from a `SymbolOrigin` (the consumer path:
                // `mkNominal` / `Translate` / `InferResolve`).
                let viaOrigin =
                    SymbolKeyOps.externalTypeKey
                        (originIn "Vesper.List" "Vesper.Collections")
                        "Vesper.Collections.List"
                        1

                // External ref minted from a qualified compiled name alone — no assembly
                // in hand (the string-fed codegen / metadata path).
                let viaQualified = SymbolKeyOps.qualifiedTypeKey "Vesper.Collections.List" 1

                // The SAME type resolved from a shape homed in a DIFFERENT assembly. The
                // home is not part of the identity, so this is the same key — the fact the
                // deleted "asm is load-bearing" test asserted the negation of.
                let viaOtherHome =
                    SymbolKeyOps.externalTypeKey (originIn "Other.Asm" "Vesper.Collections") "Vesper.Collections.List" 1

                // The local-def path (`LocalSymbolKey.ofType` over the same containment —
                // namespace `Vesper.Collections`) produces exactly this literal.
                Expect.equal viaOrigin RuntimeNames.vesperListKey "origin mint = canonical"
                Expect.equal viaQualified RuntimeNames.vesperListKey "qualified mint = canonical"
                Expect.equal viaOrigin viaQualified "both external mint paths agree"
                Expect.equal viaOtherHome viaOrigin "a differing home assembly does NOT change the identity"
            }

            test "ref cell: every mint path yields the canonical key, whatever the home" {
                let viaOrigin =
                    SymbolKeyOps.externalTypeKey (originIn "Vesper.Core" "Vesper") "Vesper.Ref" 1

                let viaQualified = SymbolKeyOps.qualifiedTypeKey "Vesper.Ref" 1

                let viaOtherHome =
                    SymbolKeyOps.externalTypeKey (originIn "Other.Asm" "Vesper") "Vesper.Ref" 1

                Expect.equal viaOrigin RuntimeNames.vesperRefKey "origin mint = canonical"
                Expect.equal viaQualified RuntimeNames.vesperRefKey "qualified mint = canonical"
                Expect.equal viaOrigin viaQualified "both external mint paths agree"
                Expect.equal viaOtherHome viaOrigin "a differing home assembly does NOT change the identity"
            }

            // A package origin is a BLANKET fact (`Vesper`), but a type in that package can
            // live DEEPER (`Vesper.Collections.seq`). Deriving the namespace by stripping the
            // blanket origin off the compiled name mis-cuts it into ns=`Vesper` /
            // name=`Collections.seq`, which then fails to match the capability's canonical
            // key. The namespace must come from the name the type belongs to.
            test "blanket package origin does not mis-cut a deeper compiled name" {
                let blanket = originIn "Vesper.Core" "Vesper"

                let viaOrigin = SymbolKeyOps.externalTypeKeyOf blanket "Vesper.Collections.seq" 1

                let viaQualified = SymbolKeyOps.qualifiedTypeKeyOfT "Vesper.Collections.seq" 1

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
                let k = SymbolKeyOps.typeKeyOf "System.Collections.Generic" "List`1+Enumerator"

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
                    (SymbolKeyOps.qualifiedTypeKeyOfT "N.List`1+Enumerator" 1
                     |> SymbolKeyOps.typeMetaName)
                    "N.List`1+Enumerator"
                    "the arity is already spelled by the outer; it is not re-appended to the inner"
            }

            // `moduleFullName` is the ONE rendering of a module's containment. Nothing parses
            // it back, so its correctness argument is that it renders each holder shape whole.
            test "module full name renders the holder chain" {
                Expect.equal
                    (SymbolKeyOps.moduleFullName (SymbolKeyOps.moduleInNamespace "Vesper" "Unchecked"))
                    "Vesper.Unchecked"
                    "namespace-qualified module"

                Expect.equal
                    (SymbolKeyOps.moduleFullName (SymbolKeyOps.moduleInNamespace "" "Util"))
                    "Util"
                    "a module in the global namespace"
            }

            // A NESTED module: the holder chain no producer could mint while `moduleKeyOf`
            // took a dotted name (its last segment became the module and the rest the
            // namespace, so `Inner` and `Outer` both flattened into the namespace path).
            // `Outer` must be a MODULE here, not a namespace segment.
            test "nested module: the chain nests, and the namespace stops where it stops" {
                let outer = SymbolKeyOps.moduleInNamespace "Vesper" "Outer"

                let inner = SymbolKeyOps.moduleKeyOf (ModuleHolder.InModule outer) "Inner"

                Expect.equal (SymbolKeyOps.moduleFullName inner) "Vesper.Outer.Inner" "the whole chain renders"

                Expect.equal
                    (List.ofSeq inner.Namespace.Path.Underlying)
                    [ "Vesper" ]
                    "the namespace is `Vesper` alone — `Outer` is a module, not a namespace segment"

                let b = SymbolKeyOps.bindingKeyOf (ModuleHolder.InModule inner) "f"

                Expect.equal
                    (SymbolKeyOps.qualifiedName (SymbolKey.Binding b))
                    "Vesper.Outer.Inner.f"
                    "a binding in a nested module qualifies through the whole chain"
            }

            // The UNQUALIFIED binding — a flat package's export / a global extern. Its holder
            // is the GLOBAL NAMESPACE, not an absent one: the empty path is a real value, so
            // every holder shape is inhabited and no site has to model "no holder".
            test "unqualified binding: the holder is the global namespace, not a sentinel" {
                let b = SymbolKeyOps.bindingKeyOf (SymbolKeyOps.inNamespace "") "f"

                match b.Decl with
                | ModuleHolder.InNamespace ns ->
                    Expect.isTrue ns.Path.IsEmpty "the global namespace is an EMPTY path, not a sentinel"
                | other -> failtestf "expected InNamespace, got %A" other

                Expect.equal (SymbolKeyOps.holderFullName b.Decl) "" "an empty holder renders empty"

                Expect.equal
                    (SymbolKeyOps.qualifiedName (SymbolKey.Binding b))
                    "f"
                    "qualifiedName drops the empty holder rather than emitting a leading dot"
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
