module XParsec.FSharp.Codegen.Clr.Tests.MetadataSymbolsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr

// The metadata reader canonicalizes BCL primitives (`System.Int32` → `int`) through the
// intrinsic axis. Seed it from the real Vesper.Core contract, not a static table, so
// `String.Length` presents `int`.
let private intrinsics =
    (ClrSymbolProviders.buildContract [ TestHelpers.vesperCorePackage ]).IntrinsicTypeMap

let private provider =
    MetadataSymbols.createWith intrinsics (MetadataSymbols.runtimeAssemblyPaths ())

let private eqComparer = "System.Collections.Generic.EqualityComparer`1"

// A metadata RENDERING read through the store, by parsed key.
let private typeShape (name: string) =
    ExternalSymbols.tryMetaType provider name

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

                    Expect.isTrue info.Origin.Home.AssemblyOption.IsSome "origin carries the defining assembly"
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

            // Members, interfaces, base type and flags are all filled at lookup, so a
            // consumer never retries through `TryLookupMember` per name.
            test "the Class shape eagerly publishes the type's members" {
                match typeShape eqComparer with
                | ValueSome(ExternalTypeShape.Class info) ->
                    let names = info.Members |> EqArray.map (fun m -> m.Name) |> Set.ofSeq
                    Expect.isTrue (Set.contains "Default" names) "Default property is enumerated"
                    Expect.isTrue (Set.contains "GetHashCode" names) "GetHashCode method is enumerated"

                    // Accessor methods are folded into the property, so without the
                    // `IsSpecialName` filter `Default` would surface twice.
                    Expect.isFalse (Set.contains "get_Default" names) "property accessors are not duplicated as methods"
                | other -> failtestf "expected a Class shape, got %A" other
            }

            test "the Class shape exposes the type's declared interfaces" {
                match typeShape "System.Collections.Generic.List`1" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    let impls =
                        ExternalSymbols.instantiateInterfaces info [| TyConst(RuntimeNames.intKey, EqArray.empty) |]
                        |> Array.choose (fun ty ->
                            match RuntimeNames.interfaceNominal ty with
                            | ValueSome(struct (k, _)) -> Some(SymbolKeyOps.typeMetaName k)
                            | ValueNone -> None
                        )
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
                    Expect.isTrue info.Flags.Declared.IsSealed "System.String is sealed"
                    Expect.isFalse info.Flags.Declared.IsAbstract "System.String is not abstract"
                | other -> failtestf "expected System.String as a Class shape, got %A" other

                match typeShape "System.IO.Stream" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.isTrue info.Flags.Declared.IsAbstract "System.IO.Stream is abstract"
                    Expect.isFalse info.Flags.Declared.IsSealed "System.IO.Stream is not sealed"
                | other -> failtestf "expected System.IO.Stream as a Class shape, got %A" other
            }

            test "Default resolves as a static property typed EqualityComparer<'T>" {
                match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKeyOf eqComparer 0, "Default") with
                | ValueSome m ->
                    Expect.isTrue m.IsStatic "Default is static"
                    Expect.equal m.Storage MemberStorage.Property "Default is a property"
                    // The declaring type is carried on the key's containment chain, typed,
                    // rather than as a string beside it.
                    Expect.equal
                        (SymbolKeyOps.typeMetaName m.Key.Decl)
                        eqComparer
                        "member key's declaring TypeKey names the declaring type"

                    // Instantiated at `'T = int`: `EqualityComparer<int>`.
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
                            | TyConst(k, _) -> SymbolKeyOps.typeSimpleName k = DisplayName "int"
                            | _ -> false
                        )
                        ->
                        Expect.equal (SymbolKeyOps.typeMetaName key) eqComparer "Default : EqualityComparer<int>"
                    | other -> failtestf "unexpected Default signature %A" other
                | ValueNone -> failtest "Default did not resolve"
            }

            test "GetHashCode resolves as an instance method typed 'T -> int" {
                match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKeyOf eqComparer 0, "GetHashCode") with
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
                        SymbolKeyOps.typeSimpleName k1 = DisplayName "int"
                        && SymbolKeyOps.typeSimpleName k2 = DisplayName "int"
                        ->
                        ()
                    | other -> failtestf "expected int -> int, got %A" other
                | ValueNone -> failtest "GetHashCode did not resolve"
            }

            test "String.Empty resolves as a genuine static FIELD (not a property)" {
                // `Storage = Field` so emission lowers it to `ldsfld`: `String` has no
                // `get_Empty`, so a property lowering would `MissingMethodException`.
                match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKeyOf "System.String" 0, "Empty") with
                | ValueSome m ->
                    Expect.equal m.Storage MemberStorage.Field "Empty is a field"
                    Expect.isTrue m.IsStatic "Empty is static"

                    match ExternalSymbols.instantiateSignature (TypeStore()) m [||] 0 with
                    | TyConst(key, _) when SymbolKeyOps.typeSimpleName key = DisplayName "string" -> ()
                    | other -> failtestf "Empty should be typed string, got %A" other
                | ValueNone -> failtest "String.Empty did not resolve as a field"
            }

            test "metadata templates instantiate to the expected use-site types" {
                // The reader freezes its templates at construction; check they instantiate
                // through the `instantiate*` helpers: declaring typars substituted, and
                // every member's signature instantiating without throwing.
                match typeShape "System.Collections.Generic.List`1" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    let intArg = [| TyConst(RuntimeNames.intKey, EqArray.empty) |]

                    // Interfaces: `IEnumerable<int>` once `'T := int` is substituted.
                    let enumerableKey =
                        SymbolKeyOps.qualifiedTypeKeyOf "System.Collections.Generic.IEnumerable`1" 1

                    match
                        ExternalSymbols.instantiateInterfaces info intArg
                        |> Array.tryPick (fun ty ->
                            match RuntimeNames.interfaceNominal ty with
                            | ValueSome(struct (k, args)) when k = enumerableKey -> Some args
                            | _ -> None
                        )
                    with
                    | Some args ->
                        Expect.equal
                            (EqArray.toArray args)
                            [| TyConst(RuntimeNames.intKey, EqArray.empty) |]
                            "IEnumerable<int> after 'T := int"
                    | None -> failtest "List<int> should implement IEnumerable<int>"

                    // Base type: `List<'T> : Object` surfaces as the canon `obj` identity
                    // (`TyConst`), not a BCL-nominal `TyClass`, because the reader
                    // canonicalizes the subtype roots.
                    match ExternalSymbols.instantiateBaseType info intArg with
                    | ValueSome(TyConst(k, _)) ->
                        Expect.equal (SymbolKeyOps.typeMetaName k) "Vesper.obj" "List bases on the canon obj root"
                    | other -> failtestf "expected List base = canon obj, got %A" other

                    // The declaring typar resolves and any method typar freshens.
                    for m in info.Members do
                        ExternalSymbols.instantiateSignature (TypeStore()) m intArg 0 |> ignore
                | other -> failtestf "expected List`1 as a Class shape, got %A" other
            }

            test "an unknown type misses" {
                Expect.isTrue (typeShape "No.Such.Type`9" |> ValueOption.isNone) "unknown type miss"
            }

            test "a rendering's parsed key round-trips, arity included" {
                match ExternalSymbols.tryMetaTypeAt provider eqComparer 0 with
                | ValueSome(struct (key, shape)) ->
                    Expect.equal (SymbolKeyOps.typeMetaName key) eqComparer "key renders back to the name asked for"
                    Expect.equal key.TyparArity 1 "the `1 suffix is the key's arity, not part of its name"
                    Expect.equal (ValueSome shape) (provider.TryLookupType key) "the parsed key reaches the same type"
                | ValueNone -> failtestf "expected %s to resolve" eqComparer
            }

            test "the metadata layer resolves no values" {
                // Module values / operators are not a metadata surface at all.
                Expect.isTrue
                    (ScopeContents.tryValueAt provider.Scope "op_Addition" |> ValueOption.isNone)
                    "no value surface"
            }

            test "the path-taking reader resolves identically to the host-TPA one" {
                // Sourced from the SAME host TPA the singleton reflects, so the
                // path-taking reader must resolve a known BCL type identically.
                let reader =
                    ClrSymbolProviders.dotnetMetadataWith (MetadataSymbols.runtimeAssemblyPaths ()) intrinsics
                    |> List.exactlyOne

                match ExternalSymbols.tryMetaType reader eqComparer, typeShape eqComparer with
                | ValueSome(ExternalTypeShape.Class a), ValueSome(ExternalTypeShape.Class b) ->
                    Expect.equal a.TyparArity b.TyparArity "same arity"
                    Expect.equal a.IsInterface b.IsInterface "same interface-ness"
                    Expect.equal a.Origin.Home.AssemblyOption b.Origin.Home.AssemblyOption "same origin assembly"
                | other -> failtestf "expected both to resolve %s as a Class, got %A" eqComparer other
            }

            test "buildContractWithRefs does not alias distinct ref sets under one manifest" {
                // `Enumerable` lives in System.Linq, never CoreLib, so dropping
                // System.Linq.dll from the TPA must lose it. If the reader ignored the
                // path set the second build would alias the first and still resolve it.
                let full = MetadataSymbols.runtimeAssemblyPaths ()

                let withoutLinq =
                    full |> List.filter (fun p -> System.IO.Path.GetFileName p <> "System.Linq.dll")

                Expect.isTrue (List.length withoutLinq < List.length full) "host TPA carries System.Linq.dll"

                let fullProvider =
                    ClrSymbolProviders.buildContractWithRefs None full [ TestHelpers.vesperCorePackage ]

                let limited =
                    ClrSymbolProviders.buildContractWithRefs None withoutLinq [ TestHelpers.vesperCorePackage ]

                Expect.isTrue
                    (ExternalSymbols.tryMetaType fullProvider "System.Linq.Enumerable"
                     |> ValueOption.isSome)
                    "full ref set resolves System.Linq.Enumerable"

                Expect.isTrue
                    (ExternalSymbols.tryMetaType limited "System.Linq.Enumerable"
                     |> ValueOption.isNone)
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

            test "a ref-pack-backed reader binds the REF assembly identity" {
                match RefPack.resolve "net8.0" with
                | Error e -> failtestf "net8.0 ref pack did not resolve: %s" e
                | Ok refPaths ->
                    let reader =
                        ClrSymbolProviders.dotnetMetadataWith refPaths intrinsics |> List.exactlyOne

                    match ExternalSymbols.tryMetaType reader "System.Text.StringBuilder" with
                    | ValueSome(ExternalTypeShape.Class info) ->
                        // In the ref pack `StringBuilder` lives in System.Runtime (the
                        // facade), not System.Private.CoreLib, which also proves the load
                        // context finds a core assembly with `System.Object` in the facade.
                        Expect.equal
                            info.Origin.Home.AssemblyOption
                            (ValueSome(AssemblyName "System.Runtime"))
                            "REF identity, not the impl"
                    | other -> failtestf "expected StringBuilder as a Class shape, got %A" other
            }

            test "the scope resolves a declared namespace and each dotted prefix to a container" {
                let scope = provider.Scope

                for path in [ "System"; "System.Collections"; "System.Collections.Generic" ] do
                    match scope.TryContainer path with
                    | ValueSome(ModuleContainer.InNamespace ns) -> Expect.equal ns.Dotted path "container namespace"
                    | other -> failtestf "expected a namespace container for %s, got %A" path other

                Expect.isTrue (scope.TryContainer "No.Such.Namespace").IsNone "an undeclared path misses"
            }

            test "a name declared at several arities resolves ascending, narrowest first" {
                let scope = provider.Scope

                match scope.TryContainer "System" with
                | ValueNone -> failtest "System is a container"
                | ValueSome c ->
                    let arities =
                        [
                            for struct (k, shape) in (scope.TypesNamed(c, "Action")).Underlying do
                                Expect.equal shape.TyparArity k.TyparArity "key and shape agree on arity"
                                k.TyparArity
                        ]

                    Expect.isTrue (arities.Length > 2) "Action is declared at several arities"
                    Expect.sequenceEqual arities (List.sort arities) "ascending by arity"
                    Expect.equal (List.head arities) 0 "the narrowest is the delegate with no args"
            }

            // `System.Environment.SpecialFolder.Desktop` compiles in `dotnet fsi`. Reflection
            // spells nesting `+`, so the directory files top-level types alone and
            // `asm.GetType "System.Environment.SpecialFolder"` misses; `inType`'s arms are union
            // case, enum case and static member, which leaves a nested type reachable by no
            // written route. Closing this publishes the declaring type as a container, with
            // `MembersByKey` for what it nests.
            ptest "GAP a nested type is reachable by its written dotted name" {
                let scope = provider.Scope

                match scope.TryContainer "System.Environment" with
                | ValueNone -> failtest "a type that nests others is a container"
                | ValueSome c -> Expect.equal (scope.TypesNamed(c, "SpecialFolder")).Length 1 "the nested enum resolves"
            }

            test "the scope publishes no values and no union cases" {
                let scope = provider.Scope

                match scope.TryContainer "System" with
                | ValueNone -> failtest "System is a container"
                | ValueSome c ->
                    Expect.isTrue (scope.TryValue(c, "Console")).IsNone "IL declares no free value"
                    Expect.equal (scope.UnionCasesNamed(c, "Some")).Length 0 "IL declares no union case"
            }

            // FSharp.Core carries 11 assembly-level `AutoOpen` rows (`Microsoft.FSharp.Core`,
            // `Microsoft.FSharp.Collections`, …). This layer reports none, so an fsc-built
            // reference assembly loses its prelude and a bare `List.map` stays unresolved.
            ptest "GAP: a reference assembly's [<assembly: AutoOpen>] rows reach ImplicitOpens" {
                let fsharpCore = typeof<int list>.Assembly.Location

                let withCore =
                    MetadataSymbols.create (fsharpCore :: MetadataSymbols.runtimeAssemblyPaths ())

                Expect.isNonEmpty withCore.ImplicitOpens "FSharp.Core's assembly-level AutoOpen rows are published"
            }
        ]
