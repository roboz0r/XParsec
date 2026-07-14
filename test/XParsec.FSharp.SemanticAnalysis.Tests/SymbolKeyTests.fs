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

                Expect.equal viaOrigin.Name "seq" "the simple name is the last segment, PLAIN — no `` `N ``"
                Expect.equal viaOrigin.Arity 1 "the arity is an int field, not a suffix in the name"
            }

            // `typeMetaName` is THE renderer and `typeKeyOf` THE parser for the `+`-mangled,
            // arity-suffixed reflection display name of a CLR nested type. They must invert
            // each other; the nesting lands in the holder chain and the arity in `Arity` —
            // neither survives inside a key's `Name`.
            test "nested type: the `+` chain becomes holders, and renders back unchanged" {
                let k = SymbolKeyOps.typeKeyOf "System.Collections.Generic" "List`1+Enumerator"

                match k.Holder with
                | TypeHolder.InType outer ->
                    Expect.equal outer.Name "List" "the outer's Name is plain"
                    Expect.equal outer.Arity 1 "the outer owns the typar"
                    Expect.equal k.Name "Enumerator" "the inner Name is the bare segment, not `+`-mangled"
                    Expect.equal k.Arity 0 "the enumerator declares no typar of its own"
                | other -> failtestf "expected InType, got %A" other

                Expect.equal
                    (SymbolKeyOps.typeMetaName k)
                    "System.Collections.Generic.List`1+Enumerator"
                    "the renderer inverts the parser"

                Expect.equal
                    (SymbolKeyOps.typeNs k)
                    "System.Collections.Generic"
                    "a nested type reports its OUTER's namespace, as the CLR does"

                // A nested type's arity is spelled by the segment that OWNS it, so a caller's
                // single `arity` — the shape's total — must not be re-appended to the inner.
                Expect.equal
                    (SymbolKeyOps.qualifiedTypeKeyOfT "N.List`1+Enumerator" 1
                     |> SymbolKeyOps.typeMetaName)
                    "N.List`1+Enumerator"
                    "the arity is already spelled by the outer; it is not re-appended to the inner"
            }

            // Each CLR metadata segment's `` `N `` is that segment's OWN typar count, so a
            // generic type nested in a generic type spells BOTH (`` Outer`1+Inner`1 ``). With
            // the arity mangled into `Name` this shape was unwritable — a producer could not
            // suffix an inner whose outer already carried a backtick. Per-segment `Arity: int`
            // makes it fall out.
            test "renderer and parser are inverses, per segment" {
                let roundTrip (ns: string) (name: string) =
                    let k = SymbolKeyOps.typeKeyOf ns name

                    // NAME round-trip: render ∘ parse = id on the metadata spelling.
                    Expect.equal
                        (SymbolKeyOps.typeMetaName k)
                        (if ns = "" then name else ns + "." + name)
                        (sprintf "render(parse(%s)) = %s" name name)

                    // KEY round-trip: parse ∘ render = id on the key.
                    Expect.equal
                        (SymbolKeyOps.typeKeyOf ns (SymbolKeyOps.typeNestedName k))
                        k
                        (sprintf "parse(render(k)) = k for %s" name)

                    k

                Expect.equal (roundTrip "N" "Plain").Arity 0 "non-generic ⇒ arity 0, no suffix rendered"
                Expect.equal (roundTrip "N" "List`1").Arity 1 "a generic's suffix parses to its arity"
                Expect.equal (roundTrip "" "Global`2").Arity 2 "the global namespace round-trips too"

                let nested = roundTrip "N" "Outer`1+Inner`1"

                Expect.equal nested.Arity 1 "the INNER declares one typar of its own"

                match nested.Holder with
                | TypeHolder.InType outer -> Expect.equal outer.Arity 1 "the OUTER declares one of its own"
                | other -> failtestf "expected InType, got %A" other

                // The array's source spelling is BACKTICK-ESCAPED (F# requires it — `[]` is not
                // a bare identifier). Those backticks are an escape, NOT an arity, so the name
                // must survive the parser whole and render back verbatim.
                let arr = roundTrip "Vesper" RuntimeNames.arrayContractName

                Expect.equal arr.Name RuntimeNames.arrayContractName "the escape is not mangled into a name + arity"

                Expect.equal arr.Arity 0 "a backtick ESCAPE is not a `` `N ``"

                // ...and a mint that is HANDED an arity for it must not invent a suffix the
                // renderer cannot spell, or the key would stop equalling the contract's.
                Expect.equal
                    (SymbolKeyOps.qualifiedTypeKeyOfT ("Vesper." + RuntimeNames.arrayContractName) 1)
                    arr
                    "an escaped name takes no arity, however it is minted"
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

        match TypeRegistry.tryTypeClaim ctx.Types UseSite.unbounded name arity with
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

                // A module compiles to a static class, so a type it holds is a class NESTED
                // in it: the metadata name `+`-joins the holder, and the namespace column is
                // the outermost holder's — exactly what the CLR does with a nested type.
                Expect.equal
                    (SymbolKeyOps.typeMetaName k)
                    "N.M+T"
                    "the module's holder class is the type's enclosing class"
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

                // EVERY module in the chain is a holder class, so the rendering nests as far
                // as the source does. This is what makes the renderer INJECTIVE: `N.A.T` and
                // `N.B.T` no longer collapse onto one name.
                Expect.equal (SymbolKeyOps.typeMetaName k) "N.A+B+T" "the whole module chain nests, outermost first"
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
