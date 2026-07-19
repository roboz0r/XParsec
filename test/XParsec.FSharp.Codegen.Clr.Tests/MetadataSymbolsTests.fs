module XParsec.FSharp.Codegen.Clr.Tests.MetadataSymbolsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr

// The metadata leaf canonicalizes BCL primitives (`System.Int32 → int`) through the
// harvested `{ platform → [canon] }` reverse map — the reverse face of Vesper.Core's
// `type int = (# "System.Int32" #)`. Seed the test leaf with the REAL harvested map
// (not a static table) so `String.Length`/`List.get_Item` present `int`, etc.
let private reverseCanon =
    (ClrSymbolProviders.buildContract [ TestHelpers.vesperCoreManifest ]).IntrinsicReverseCanon

let private provider =
    MetadataSymbols.createWith reverseCanon (MetadataSymbols.runtimeAssemblyPaths ())

let private eqComparer = "System.Collections.Generic.EqualityComparer`1"

// The resolver's by-name type face answers the identity WITH the shape; these tests are
// about what the metadata scrape MODELS, so they read the shape half. The identity half
// has its own test below.
let private typeShape (name: string) =
    provider.TryLookupType name |> ExternalSymbols.typeShapeOf

[<Tests>]
let tests =
    testList
        "MetadataSymbols"
        [
            test "EqualityComparer`1 resolves as a non-interface Class with an origin" {
                match typeShape eqComparer with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.equal info.TyparArity 1 "one declared typar"
                    Expect.isFalse info.IsInterface "a class, not an interface"
                    Expect.equal info.Origin.Namespace.Dotted "System.Collections.Generic" "origin namespace"

                    Expect.isTrue
                        (match info.Origin.Home with
                         | Origin.InAssembly _ -> true
                         | Origin.Unstamped -> false)
                        "origin carries the defining assembly"
                | other -> failtestf "expected a Class shape, got %A" other
            }

            test "a generic interface resolves with isInterface = true" {
                match typeShape "System.Collections.Generic.IEnumerable`1" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.equal info.TyparArity 1 "one typar"
                    Expect.isTrue info.IsInterface "IEnumerable`1 is an interface"
                | other -> failtestf "expected an interface Class shape, got %A" other
            }

            test "a non-generic type resolves with arity 0" {
                match typeShape "System.Object" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.equal info.TyparArity 0 "System.Object is non-generic"
                    Expect.isFalse info.IsInterface "System.Object is a class"
                | other -> failtestf "expected a Class shape, got %A" other
            }

            // The metadata layer fills the rich class shape — members, interfaces,
            // base type, flags — so B-1/B-2 don't have to retry through `TryLookupMember` per name.
            test "the Class shape eagerly publishes the type's members" {
                match typeShape eqComparer with
                | ValueSome(ExternalTypeShape.Class info) ->
                    let names = info.Members |> Array.map (fun m -> m.Name) |> Set.ofArray
                    Expect.isTrue (Set.contains "Default" names) "Default property is enumerated"
                    Expect.isTrue (Set.contains "GetHashCode" names) "GetHashCode method is enumerated"

                    // Accessor methods are folded into the property — without the
                    // IsSpecialName filter, `Default` would surface twice.
                    Expect.isFalse (Set.contains "get_Default" names) "property accessors are not duplicated as methods"
                | other -> failtestf "expected a Class shape, got %A" other
            }

            test "the Class shape exposes the type's declared interfaces" {
                match typeShape "System.Collections.Generic.List`1" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    let impls =
                        ExternalSymbols.instantiateInterfaces info [| TyConst(RuntimeNames.intKey, EqArray.empty) |]
                        |> Array.map fst
                        |> Set.ofArray

                    Expect.isTrue (Set.contains "System.Collections.Generic.IList`1" impls) "List<T> declares IList<T>"

                    Expect.isTrue (Set.contains "System.Collections.IList" impls) "List<T> declares non-generic IList"
                | other -> failtestf "expected List`1 as a Class shape, got %A" other
            }

            test "the Class shape carries the declared base type (or ValueNone on an interface)" {
                match typeShape "System.IO.StringWriter" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    match ExternalSymbols.instantiateBaseType info [||] with
                    | ValueSome(TyClass(k, args)) when
                        SymbolKeyOps.typeMetaName k = "System.IO.TextWriter" && args.IsEmpty
                        ->
                        ()
                    | ValueSome other -> failtestf "expected StringWriter base = TextWriter, got %A" other
                    | ValueNone -> failtest "expected StringWriter to record a base type"
                | other -> failtestf "expected StringWriter as a Class shape, got %A" other

                match typeShape "System.Collections.Generic.IEnumerable`1" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.isTrue info.FrozenBaseType.IsNone "interfaces carry no base type"
                | other -> failtestf "expected IEnumerable`1 as a Class shape, got %A" other
            }

            test "the Class shape decodes sealed / abstract flags from TypeAttributes" {
                match typeShape "System.String" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.isTrue info.Flags.IsSealed "System.String is sealed"
                    Expect.isFalse info.Flags.IsAbstract "System.String is not abstract"
                | other -> failtestf "expected System.String as a Class shape, got %A" other

                match typeShape "System.IO.Stream" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.isTrue info.Flags.IsAbstract "System.IO.Stream is abstract"
                    Expect.isFalse info.Flags.IsSealed "System.IO.Stream is not sealed"
                | other -> failtestf "expected System.IO.Stream as a Class shape, got %A" other
            }

            test "Default resolves as a static property typed EqualityComparer<'T>" {
                match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey eqComparer 0, "Default") with
                | ValueSome m ->
                    Expect.isTrue m.IsStatic "Default is static"
                    Expect.equal m.Storage MemberStorage.Property "Default is a property"
                    // The declaring type is carried by the KEY's containment chain, typed —
                    // not by a string beside it.
                    Expect.equal
                        (SymbolKeyOps.typeMetaName m.Key.Decl)
                        eqComparer
                        "member key's declaring TypeKey names the declaring type"

                    // Instantiated at `'T = int`, the property type is
                    // `EqualityComparer<int>` (the §7.3 per-use substitution).
                    match
                        ExternalSymbols.instantiateSignature
                            (TypeStore())
                            m
                            [| TyConst(RuntimeNames.intKey, EqArray.empty) |]
                            0
                    with
                    | TyClass(key, args) when
                        args.Length = 1
                        && (
                            match args.[0] with
                            | TyConst(k, _) -> SymbolKeyOps.simpleName k = DisplayName "int"
                            | _ -> false
                        )
                        ->
                        Expect.equal (SymbolKeyOps.typeMetaName key) eqComparer "Default : EqualityComparer<int>"
                    | other -> failtestf "unexpected Default signature %A" other
                | ValueNone -> failtest "Default did not resolve"
            }

            test "GetHashCode resolves as an instance method typed 'T -> int" {
                match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey eqComparer 0, "GetHashCode") with
                | ValueSome m ->
                    Expect.isFalse m.IsStatic "GetHashCode(T) is an instance method"
                    Expect.equal m.Storage MemberStorage.Method "a method, not a property"

                    // Instantiated at `'T = int`: `int -> int`.
                    match
                        ExternalSymbols.instantiateSignature
                            (TypeStore())
                            m
                            [| TyConst(RuntimeNames.intKey, EqArray.empty) |]
                            0
                    with
                    | TyFun(TyConst(k1, _), TyConst(k2, _)) when
                        SymbolKeyOps.simpleName k1 = DisplayName "int"
                        && SymbolKeyOps.simpleName k2 = DisplayName "int"
                        ->
                        ()
                    | other -> failtestf "expected int -> int, got %A" other
                | ValueNone -> failtest "GetHashCode did not resolve"
            }

            test "String.Empty resolves as a genuine static FIELD (not a property)" {
                // A real public field — invisible to the property/method walks before the
                // `GetFields` pass. It must carry `Storage = Field` so emission lowers it to
                // `ldsfld` (a `call get_Empty` would `MissingMethodException` — String has no
                // such accessor).
                match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey "System.String" 0, "Empty") with
                | ValueSome m ->
                    Expect.equal m.Storage MemberStorage.Field "Empty is a field"
                    Expect.isTrue m.IsStatic "Empty is static"

                    match ExternalSymbols.instantiateSignature (TypeStore()) m [||] 0 with
                    | TyConst(key, _) when SymbolKeyOps.simpleName key = DisplayName "string" -> ()
                    | other -> failtestf "Empty should be typed string, got %A" other
                | ValueNone -> failtest "String.Empty did not resolve as a field"
            }

            test "metadata templates instantiate to the expected use-site types" {
                // The eager (reflection-backed) provider freezes its `FrozenType`
                // templates at construction (its descriptors are total and
                // registry-independent, unlike the contract layer's). Spot-check that
                // the templates realise correctly through the `instantiate*` helpers
                // for a generic class: declaring typars substituted, every member's
                // signature instantiates without throwing.
                match typeShape "System.Collections.Generic.List`1" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    let intArg = [| TyConst(RuntimeNames.intKey, EqArray.empty) |]

                    // Interfaces: `IEnumerable<int>` once `'T := int` is substituted.
                    match
                        ExternalSymbols.instantiateInterfaces info intArg
                        |> Array.tryFind (fun (n, _) -> n = "System.Collections.Generic.IEnumerable`1")
                    with
                    | Some(_, args) ->
                        Expect.equal
                            args
                            [| TyConst(RuntimeNames.intKey, EqArray.empty) |]
                            "IEnumerable<int> after 'T := int"
                    | None -> failtest "List<int> should implement IEnumerable<int>"

                    // Base type: `List<'T> : Object`, surfaced EAGERLY as the canon
                    // `obj` identity (`TyConst`), not the BCL-nominal `TyClass` —
                    // metadata canonicalizes the subtype roots at surfacing.
                    match ExternalSymbols.instantiateBaseType info intArg with
                    | ValueSome(TyConst(k, _)) ->
                        Expect.equal (SymbolKeyOps.qualifiedName k) "Vesper.obj" "List bases on the canon obj root"
                    | other -> failtestf "expected List base = canon obj, got %A" other

                    // Every member's signature instantiates without throwing — the
                    // declaring typar resolves and any method typar freshens.
                    for m in info.Members do
                        ExternalSymbols.instantiateSignature (TypeStore()) m intArg 0 |> ignore
                | other -> failtestf "expected List`1 as a Class shape, got %A" other
            }

            test "an unknown type misses" {
                Expect.isTrue (provider.TryLookupType "No.Such.Type`9" |> ValueOption.isNone) "unknown type miss"
            }

            // Bare IL has no module chains, so this leaf's name IS its identity — which is
            // why it can answer the identity at all rather than leave a use site to re-cut
            // one from the spelling. The two directions must invert each other: the key the
            // by-name face reports must be the key whose `qualifiedName` rendering is the
            // name that was asked for, ARITY INCLUDED (`EqualityComparer`1` is arity 1, not
            // an arity-0 type whose name happens to end in a backtick).
            test "the by-name face answers the identity its own name index round-trips to" {
                match provider.TryLookupType eqComparer with
                | ValueSome(struct (key, _)) ->
                    Expect.equal (SymbolKeyOps.typeMetaName key) eqComparer "key renders back to the name asked for"
                    Expect.equal key.TyparArity 1 "the `1 suffix is the key's arity, not part of its name"

                    Expect.equal
                        (provider.TryLookupType eqComparer |> ExternalSymbols.typeShapeOf)
                        (provider.TryLookupType(SymbolKey.Type key))
                        "the by-name and by-key faces answer the same type"
                | ValueNone -> failtestf "expected %s to resolve" eqComparer
            }

            test "the metadata layer resolves no values" {
                // F#-style module values / operators are not a metadata surface;
                // they fall through to lower-priority sources in the composite.
                Expect.isTrue (provider.TryLookup "op_Addition" |> ValueOption.isNone) "no value surface"
            }

            test "the path-taking leaf resolves identically to the host-TPA leaf" {
                // Sourced from the SAME host TPA the singleton reflects, so the
                // path-taking leaf must resolve a known BCL type identically.
                let leaf =
                    ClrSymbolProviders.bclMetaTailWith (MetadataSymbols.runtimeAssemblyPaths ()) reverseCanon
                    |> List.exactlyOne

                match leaf.TryLookupType eqComparer |> ExternalSymbols.typeShapeOf, typeShape eqComparer with
                | ValueSome(ExternalTypeShape.Class a), ValueSome(ExternalTypeShape.Class b) ->
                    Expect.equal a.TyparArity b.TyparArity "same arity"
                    Expect.equal a.IsInterface b.IsInterface "same interface-ness"
                    Expect.equal a.Origin.Home.AssemblyOption b.Origin.Home.AssemblyOption "same origin assembly"
                | other -> failtestf "expected both leaves to resolve %s as a Class, got %A" eqComparer other
            }

            test "buildContractWithRefs does not alias distinct ref sets under one manifest" {
                // Enumerable lives in System.Linq (never CoreLib): the full TPA resolves
                // it, the full TPA with System.Linq.dll removed does not (a core assembly
                // stays present, so MLC still constructs). If the cache keyed on
                // `cacheTag|target|manifests` ignored the path set, the second build would
                // alias the first and both would agree — this proves it does not.
                let full = MetadataSymbols.runtimeAssemblyPaths ()

                let withoutLinq =
                    full |> List.filter (fun p -> System.IO.Path.GetFileName p <> "System.Linq.dll")

                Expect.isTrue (List.length withoutLinq < List.length full) "host TPA carries System.Linq.dll"

                let fullProvider =
                    ClrSymbolProviders.buildContractWithRefs full None [ TestHelpers.vesperCoreManifest ]

                let limited =
                    ClrSymbolProviders.buildContractWithRefs withoutLinq None [ TestHelpers.vesperCoreManifest ]

                Expect.isTrue
                    (fullProvider.TryLookupType "System.Linq.Enumerable" |> ValueOption.isSome)
                    "full ref set resolves System.Linq.Enumerable"

                Expect.isTrue
                    (limited.TryLookupType "System.Linq.Enumerable" |> ValueOption.isNone)
                    "ref set without System.Linq.dll does not resolve System.Linq.Enumerable"
            }

            test "RefPack.resolve net8.0 finds the installed ref pack" {
                match RefPack.resolve "net8.0" with
                | Ok paths ->
                    Expect.isNonEmpty paths "ref pack has assemblies"

                    Expect.isTrue
                        (paths
                         |> List.exists (fun p -> System.IO.Path.GetFileName p = "System.Runtime.dll"))
                        "ref pack contains System.Runtime.dll"
                | Error e -> failtestf "expected net8.0 ref pack to resolve: %s" e
            }

            test "a ref-pack-backed leaf binds the REF assembly identity" {
                match RefPack.resolve "net8.0" with
                | Error e -> failtestf "net8.0 ref pack did not resolve: %s" e
                | Ok refPaths ->
                    let leaf =
                        ClrSymbolProviders.bclMetaTailWith refPaths reverseCanon |> List.exactlyOne

                    match leaf.TryLookupType "System.Text.StringBuilder" |> ExternalSymbols.typeShapeOf with
                    | ValueSome(ExternalTypeShape.Class info) ->
                        // In the ref pack StringBuilder lives in System.Runtime (the ref
                        // facade), not System.Private.CoreLib (the runtime impl). This also
                        // proves MetadataLoadContext finds a core assembly when System.Object
                        // lives in System.Runtime.dll rather than System.Private.CoreLib.
                        Expect.equal
                            info.Origin.Home.AssemblyOption
                            (ValueSome "System.Runtime")
                            "REF identity, not the impl"
                    | other -> failtestf "expected StringBuilder as a Class shape, got %A" other
            }
        ]
