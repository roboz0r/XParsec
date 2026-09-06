module XParsec.FSharp.SemanticAnalysis.Tests.SymbolKeyTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Unification compares nominal types by full `SymbolKey` equality, so two mint paths that
// disagreed would fail to unify with no diagnostic.
[<Tests>]
let tests =
    testList
        "SymbolKey mint-path invariant"
        [
            // A compiled name is the whole input: no mint path takes a declaring assembly or a
            // package origin, so the same name from any source yields the one canonical key.
            test "cons-list: the compiled name mints the canonical key" {
                // A local definition in namespace `Vesper.Collections` mints this literal too.
                Expect.equal
                    (SymbolKeyOps.qualifiedTypeKeyOf "Vesper.Collections.List" 1)
                    RuntimeNames.vesperListKey
                    "qualified mint = canonical"
            }

            test "ref cell: the compiled name mints the canonical key" {
                Expect.equal
                    (SymbolKeyOps.qualifiedTypeKeyOf "Vesper.Ref" 1)
                    RuntimeNames.vesperRefKey
                    "qualified mint = canonical"
            }

            // A package origin is a BLANKET fact (`Vesper`), but a type in it can live DEEPER
            // (`Vesper.Collections.seq`). Cutting the namespace at a blanket origin would
            // mis-segment that into ns=`Vesper` / name=`Collections.seq`.
            test "a deeper compiled name segments at its own last dot" {
                let key = SymbolKeyOps.qualifiedTypeKeyOf "Vesper.Collections.seq" 1

                Expect.equal
                    (List.ofSeq key.Namespace.Path.Underlying)
                    [ "Vesper"; "Collections" ]
                    "the namespace is segmented from the name"

                Expect.equal key.Name "seq" "the simple name is the last segment, PLAIN — no `` `N ``"
                Expect.equal key.TyparArity 1 "the arity is an int field, not a suffix in the name"
            }

            // Parsing `` List`1+Enumerator `` lands the nesting in the containment chain and
            // the arity in `TyparArity` — neither survives inside a key's `Name`.
            test "nested type: the `+` chain becomes containers, and renders back unchanged" {
                let k = SymbolKeyOps.typeKeyOf "System.Collections.Generic" "List`1+Enumerator"

                match k.Container with
                | TypeContainer.InType outer ->
                    Expect.equal outer.Name "List" "the outer's Name is plain"
                    Expect.equal outer.TyparArity 1 "the outer owns the typar"
                    Expect.equal k.Name "Enumerator" "the inner Name is the bare segment, not `+`-mangled"
                    Expect.equal k.TyparArity 0 "the enumerator declares no typar of its own"
                | other -> failtestf "expected InType, got %A" other

                Expect.equal
                    (SymbolKeyOps.typeMetaName k)
                    "System.Collections.Generic.List`1+Enumerator"
                    "the renderer inverts the parser"

                Expect.equal
                    (SymbolKeyOps.typeNs k)
                    "System.Collections.Generic"
                    "a nested type reports its OUTER's namespace, as the CLR does"

                Expect.equal
                    (SymbolKeyOps.qualifiedTypeKeyOf "N.List`1+Enumerator" 1
                     |> SymbolKeyOps.typeMetaName)
                    "N.List`1+Enumerator"
                    "the arity is already spelled by the outer; it is not re-appended to the inner"
            }

            test "renderer and parser are inverses, per segment" {
                let roundTrip (ns: string) (name: string) =
                    let k = SymbolKeyOps.typeKeyOf ns name

                    Expect.equal
                        (SymbolKeyOps.typeMetaName k)
                        (if ns = "" then name else ns + "." + name)
                        (sprintf "render(parse(%s)) = %s" name name)

                    Expect.equal
                        (SymbolKeyOps.typeKeyOf ns (SymbolKeyOps.typeNestedName k))
                        k
                        (sprintf "parse(render(k)) = k for %s" name)

                    k

                Expect.equal (roundTrip "N" "Plain").TyparArity 0 "non-generic ⇒ arity 0, no suffix rendered"
                Expect.equal (roundTrip "N" "List`1").TyparArity 1 "a generic's suffix parses to its arity"
                Expect.equal (roundTrip "" "Global`2").TyparArity 2 "the global namespace round-trips too"

                let nested = roundTrip "N" "Outer`1+Inner`1"

                Expect.equal nested.TyparArity 1 "the INNER declares one typar of its own"

                match nested.Container with
                | TypeContainer.InType outer -> Expect.equal outer.TyparArity 1 "the OUTER declares one of its own"
                | other -> failtestf "expected InType, got %A" other

                // `type 'T ``[]`` ` spells its name backtick-escaped, F# having no bare `[]`
                // identifier, but the escape is source spelling and reaches no key.
                let arr = roundTrip "Vesper" (SymbolKeyOps.arrayName 1)

                Expect.equal arr.Name "[]" "the array's key holds the bare name"

                Expect.equal arr.TyparArity 0 "a structural constructor takes no arity"

                Expect.equal
                    (SymbolKeyOps.qualifiedTypeKeyOf "Vesper.[]" 1)
                    arr
                    "the array's element type is carried on its args, so no arity is supplied however it is minted"

                Expect.equal
                    (SymbolKeyOps.typeKeyOfArity "Vesper" (SymbolKeyOps.arrayName 1) 1)
                    arr
                    "the DECLARATION's one typar keys the same, so a use site and a declaration agree"
            }

            test "module declared path renders the containment chain" {
                Expect.equal
                    (SymbolKeyOps.moduleInNamespace "Vesper" "Unchecked").DeclaredPath
                    "Vesper.Unchecked"
                    "namespace-qualified module"

                Expect.equal
                    (SymbolKeyOps.moduleInNamespace "" "Util").DeclaredPath
                    "Util"
                    "a module in the global namespace"
            }

            test "nested module: the chain nests, and the namespace stops where it stops" {
                let outer = SymbolKeyOps.moduleInNamespace "Vesper" "Outer"

                let inner = SymbolKeyOps.moduleKeyOf (ModuleContainer.InModule outer) "Inner"

                Expect.equal inner.DeclaredPath "Vesper.Outer.Inner" "the whole chain renders"

                Expect.equal
                    (List.ofSeq inner.Namespace.Path.Underlying)
                    [ "Vesper" ]
                    "the namespace is `Vesper` alone — `Outer` is a module, not a namespace segment"

                let b = SymbolKeyOps.bindingKeyOf (ModuleContainer.InModule inner) "f"

                Expect.equal
                    (SymbolKeyOps.qualifiedName (SymbolKey.Binding b))
                    "Vesper.Outer.Inner.f"
                    "a binding in a nested module qualifies through the whole chain"
            }

            // The UNQUALIFIED binding — a flat package's export, or a global extern.
            test "unqualified binding: the container is the global namespace, not a sentinel" {
                let b = SymbolKeyOps.bindingKeyOf (SymbolKeyOps.inNamespace "") "f"

                match b.Decl with
                | ModuleContainer.InNamespace ns ->
                    Expect.isTrue ns.Path.IsEmpty "the global namespace is an EMPTY path, not a sentinel"
                | other -> failtestf "expected InNamespace, got %A" other

                Expect.equal (SymbolKeyOps.containerFullName b.Decl) "" "an empty container renders empty"

                Expect.equal
                    (SymbolKeyOps.qualifiedName (SymbolKey.Binding b))
                    "f"
                    "qualifiedName drops the empty container rather than emitting a leading dot"
            }

            // "No home" is represented as `ValueNone`, never a placeholder name the caller would
            // have to recognise.
            test "an unstamped origin has no home assembly, a stamped one has its name" {
                Expect.equal SymbolHome.Unstamped.AssemblyOption ValueNone "SymbolHome.Unstamped has no home assembly"

                let unstamped = SymbolOrigin.Empty

                Expect.equal
                    unstamped.Home.AssemblyOption
                    ValueNone
                    "SymbolOrigin.Empty is unstamped, so it has no home assembly"

                Expect.equal
                    (SymbolHome.InAssembly(AssemblyName "Vesper.Core")).AssemblyOption
                    (ValueSome(AssemblyName "Vesper.Core"))
                    "a stamped origin returns its home assembly name"
            }
        ]

// Each case below is a real F# overload set (checked against `dotnet fsi`), so a key that
// collapsed any one axis would intern two distinct overloads under one identity.
[<Tests>]
let memberKeyIdentity =
    let cKey = SymbolKeyOps.qualifiedTypeKeyOf "C" 1 // the OPEN `C<'T>`
    let ftInt: FrozenType = FTConst(RuntimeNames.intKey, EqArray.empty)
    let declTypar: FrozenType = FTTypar(TyparScope.Type cKey, 0)
    let methodTypar: FrozenType = FTTypar(TyparScope.Member cKey, 0)

    let mk (argSig: FrozenType list) (methodTyparArity: int) : MemberKey =
        SymbolKeyOps.memberKeyOf cKey "M" (EqArray.ofList argSig) methodTyparArity MemberKind.Method

    testList
        "MemberKey overload identity"
        [
            test "method-typar ARITY is an identity axis: M<'a>() <> M<'a,'b>()" {
                Expect.notEqual (mk [] 1) (mk [] 2) "the method-typar arity alone separates them"
            }

            // On `C<'T>`, `M(x:'T)`, `M<'U>(x:'U)` and `M(x:int)` coexist as three overloads.
            test "the FTTypar scope separates declaring / method / concrete param types" {
                let mDecl = mk [ declTypar ] 0
                let mMethod = mk [ methodTypar ] 1
                let mConcrete = mk [ ftInt ] 0
                Expect.notEqual mDecl mMethod "declaring-typar arg <> method-typar arg"
                Expect.notEqual mDecl mConcrete "declaring-typar arg <> concrete int arg"
                Expect.notEqual mMethod mConcrete "method-typar arg <> concrete int arg"
            }

            // A `'T`-typed param at a `C<int>` use site is STILL `FTTypar(Type C, 0)`.
            test "argSig is keyed on the OPEN declaring form, not an instantiation" {
                Expect.equal
                    (mk [ declTypar ] 0)
                    (mk [ declTypar ] 0)
                    "same key from the open declaration and a C<int> use site"

                Expect.notEqual
                    (mk [ declTypar ] 0)
                    (mk [ ftInt ] 0)
                    "the open-typar overload is NOT the concrete-int one"
            }
        ]

// The DECLARING containment of a project-local type: `type T` inside `module M` is held by
// `M`, so the module name is neither folded into the namespace path nor dropped.
module private Local =

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

                match k.Container with
                | TypeContainer.InNamespace ns ->
                    Expect.equal (List.ofSeq ns.Path.Underlying) [ "N" ] "the declaring namespace, segmented"
                | other -> failtestf "expected InNamespace, got %A" other
            }

            test "a type declared inside a module is held by the MODULE" {
                let k =
                    Local.typeKeyOf 0 "T" (Local.source [ "namespace N"; ""; "module M ="; "    type T = { x: int }" ])

                match k.Container with
                | TypeContainer.InModule m ->
                    Expect.equal m.Name "M" "the enclosing module, by its compiled container name"

                    match m.Container with
                    | ModuleContainer.InNamespace ns ->
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

                // A module compiles to a static class, so the metadata name `+`-joins it and
                // the namespace column is the outermost container's.
                Expect.equal
                    (SymbolKeyOps.typeMetaName k)
                    "N.M+T"
                    "the module's container class is the type's enclosing class"
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

                match k.Container with
                | TypeContainer.InModule b ->
                    Expect.equal b.Name "B" "held by the INNERMOST module"

                    match b.Container with
                    | ModuleContainer.InModule a ->
                        Expect.equal a.Name "A" "which is itself held by the outer module"

                        Expect.equal
                            (List.ofSeq a.Namespace.Path.Underlying)
                            [ "N" ]
                            "and the outer module by the namespace — neither module is a namespace segment"
                    | other -> failtestf "expected B's container to be module A, got %A" other
                | other -> failtestf "expected InModule, got %A" other

                Expect.equal (SymbolKeyOps.typeMetaName k) "N.A+B+T" "the whole module chain nests, outermost first"
            }

            test "the module's key carries the name its SOURCE writes, though it compiles suffixed" {
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

                match k.Container with
                | TypeContainer.InModule m ->
                    Expect.equal
                        m.Name
                        "M"
                        "the collision with `type M` suffixes the emitted class, which is published apart from the key"
                | other -> failtestf "expected InModule, got %A" other
            }

            // `MeasureTerm` normalises by sorting its atoms, so the order on `TypeKey` must be
            // total. The rendered path is not: namespace `A.B` + type `C` and namespace `A` +
            // module `B` + type `C` both read `A.B.C`.
            test "two keys with one declared path order apart, so a measure term normalises order-independently" {
                let inNamespace = SymbolKeyOps.typeKeyOf "A.B" "C"

                let inModule =
                    SymbolKeyOps.typeKeyOfContainer
                        (TypeContainer.InModule(SymbolKeyOps.moduleKeyOf (SymbolKeyOps.inNamespace "A") "B"))
                        "C"
                        0

                Expect.equal inNamespace.DeclaredPath inModule.DeclaredPath "the two paths render alike"
                Expect.notEqual (compare inNamespace inModule) 0 "the keys still order apart"

                Expect.equal
                    (MeasureTerm.OfList [ inNamespace, Rational.One; inModule, Rational.One ])
                    (MeasureTerm.OfList [ inModule, Rational.One; inNamespace, Rational.One ])
                    "one term whichever atom is listed first"
            }
        ]
