module XParsec.FSharp.Codegen.Clr.Tests.MetadataSymbolsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr

let private provider = MetadataSymbols.provider

let private eqComparer = "System.Collections.Generic.EqualityComparer`1"

[<Tests>]
let tests =
    testList
        "MetadataSymbols"
        [
            test "EqualityComparer`1 resolves as a non-interface Class with an origin" {
                match provider.TryLookupType eqComparer with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.equal info.Arity 1 "one declared typar"
                    Expect.isFalse info.IsInterface "a class, not an interface"
                    Expect.equal info.Origin.Namespace "System.Collections.Generic" "origin namespace"
                    Expect.isTrue info.Origin.Assembly.IsSome "origin carries the defining assembly"
                | other -> failtestf "expected a Class shape, got %A" other
            }

            test "a generic interface resolves with isInterface = true" {
                match provider.TryLookupType "System.Collections.Generic.IEnumerable`1" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.equal info.Arity 1 "one typar"
                    Expect.isTrue info.IsInterface "IEnumerable`1 is an interface"
                | other -> failtestf "expected an interface Class shape, got %A" other
            }

            test "a non-generic type resolves with arity 0" {
                match provider.TryLookupType "System.Object" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.equal info.Arity 0 "System.Object is non-generic"
                    Expect.isFalse info.IsInterface "System.Object is a class"
                | other -> failtestf "expected a Class shape, got %A" other
            }

            // H2 (pre-sprint-recommendations): the metadata layer fills the rich
            // class shape — members, interfaces, base type, flags — so the sprint's
            // B-1/B-2 don't have to retry through `TryLookupMember` per name.
            test "the Class shape eagerly publishes the type's members" {
                match provider.TryLookupType eqComparer with
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
                match provider.TryLookupType "System.Collections.Generic.List`1" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    let impls =
                        info.Interfaces [| TyConst("int", EqArray.empty) |]
                        |> Array.map fst
                        |> Set.ofArray

                    Expect.isTrue (Set.contains "System.Collections.Generic.IList`1" impls) "List<T> declares IList<T>"

                    Expect.isTrue (Set.contains "System.Collections.IList" impls) "List<T> declares non-generic IList"
                | other -> failtestf "expected List`1 as a Class shape, got %A" other
            }

            test "the Class shape carries the declared base type (or ValueNone on an interface)" {
                match provider.TryLookupType "System.IO.StringWriter" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    match info.BaseType with
                    | ValueSome build ->
                        match build [||] with
                        | TyClass(k, args) when SymbolKeyOps.qualifiedName k = "System.IO.TextWriter" && args.IsEmpty ->
                            ()
                        | other -> failtestf "expected StringWriter base = TextWriter, got %A" other
                    | ValueNone -> failtest "expected StringWriter to record a base type"
                | other -> failtestf "expected StringWriter as a Class shape, got %A" other

                match provider.TryLookupType "System.Collections.Generic.IEnumerable`1" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.isTrue info.BaseType.IsNone "interfaces carry no base type"
                | other -> failtestf "expected IEnumerable`1 as a Class shape, got %A" other
            }

            test "the Class shape decodes sealed / abstract flags from TypeAttributes" {
                match provider.TryLookupType "System.String" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.isTrue info.Flags.IsSealed "System.String is sealed"
                    Expect.isFalse info.Flags.IsAbstract "System.String is not abstract"
                | other -> failtestf "expected System.String as a Class shape, got %A" other

                match provider.TryLookupType "System.IO.Stream" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.isTrue info.Flags.IsAbstract "System.IO.Stream is abstract"
                    Expect.isFalse info.Flags.IsSealed "System.IO.Stream is not sealed"
                | other -> failtestf "expected System.IO.Stream as a Class shape, got %A" other
            }

            test "Default resolves as a static property typed EqualityComparer<'T>" {
                match provider.TryLookupMember(eqComparer, "Default") with
                | ValueSome m ->
                    Expect.isTrue m.IsStatic "Default is static"
                    Expect.isTrue m.IsProperty "Default is a property"
                    Expect.equal m.Origin.DeclaringType (Some eqComparer) "member origin names the declaring type"

                    // Instantiated at `'T = int`, the property type is
                    // `EqualityComparer<int>` (the §7.3 per-use substitution).
                    match m.BuildSignature [| TyConst("int", EqArray.empty) |] with
                    | TyClass(key, args) when
                        args.Length = 1
                        && (
                            match args.[0] with
                            | TyConst("int", _) -> true
                            | _ -> false
                        )
                        ->
                        Expect.equal (SymbolKeyOps.qualifiedName key) eqComparer "Default : EqualityComparer<int>"
                    | other -> failtestf "unexpected Default signature %A" other
                | ValueNone -> failtest "Default did not resolve"
            }

            test "GetHashCode resolves as an instance method typed 'T -> int" {
                match provider.TryLookupMember(eqComparer, "GetHashCode") with
                | ValueSome m ->
                    Expect.isFalse m.IsStatic "GetHashCode(T) is an instance method"
                    Expect.isFalse m.IsProperty "a method, not a property"

                    // Instantiated at `'T = int`: `int -> int`.
                    match m.BuildSignature [| TyConst("int", EqArray.empty) |] with
                    | TyFun(TyConst("int", _), TyConst("int", _)) -> ()
                    | other -> failtestf "expected int -> int, got %A" other
                | ValueNone -> failtest "GetHashCode did not resolve"
            }

            test "external-signature oracle: eager templates match their closures (metadata path)" {
                // The step-1 oracle on the *eager* (reflection-backed) provider —
                // `MetadataSymbols` freezes at construction (its closures are total
                // and registry-independent, unlike the contract layer's). For a
                // member: `Signature` round-trips its `BuildSignature` —
                // `instantiateSignature ≡ BuildSignature` on ground args (the method
                // axis is checked structurally where present). For a class:
                // `FrozenInterfaces` / `FrozenBaseType` match the closures on the
                // declaring-typar markers.
                let checkMember (label: string) (m: ExternalMember) =
                    let args =
                        Array.init m.Signature.DeclaringArity (fun i -> TyConst(sprintf "g%d" i, EqArray.empty))

                    if m.MethodArity = 0 then
                        Expect.equal
                            (ExternalSymbols.instantiateSignature m args 0)
                            (m.BuildSignature args)
                            (sprintf "instantiateSignature ≡ BuildSignature: %s" label)
                    else
                        // Generic method: the closure bakes `TempTypar(Method,_)`; the
                        // template freezes it to `FTTypar(Method,_)` and freshens on
                        // instantiate. Assert the frozen template equals the freeze of
                        // the closure on markers (law 1), which is axis-exact.
                        Expect.equal
                            m.Signature.Return
                            (match toFrozen (m.BuildSignature(declaringMarkers m.Signature.DeclaringArity)) with
                             | FTFun(_, r) -> r
                             | other -> other)
                            (sprintf "law 1 (generic-method return): %s" label)

                match provider.TryLookupMember(eqComparer, "Default") with
                | ValueSome m -> checkMember "EqualityComparer.Default" m
                | ValueNone -> failtest "Default did not resolve"

                match provider.TryLookupMember(eqComparer, "GetHashCode") with
                | ValueSome m -> checkMember "EqualityComparer.GetHashCode" m
                | ValueNone -> failtest "GetHashCode did not resolve"

                // A class with interfaces + base type: `List`1` implements
                // `IEnumerable<'T>` etc. and bases on `Object`.
                match provider.TryLookupType "System.Collections.Generic.List`1" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    let markers = declaringMarkers info.Arity

                    let expectedInterfaces =
                        info.Interfaces markers
                        |> Array.map (fun (n, args) -> n, Array.map toFrozen args)

                    Expect.equal info.FrozenInterfaces expectedInterfaces "law 1 (class interfaces)"

                    let expectedBase = info.BaseType |> ValueOption.map (fun f -> toFrozen (f markers))
                    Expect.equal info.FrozenBaseType expectedBase "law 1 (class base type)"

                    for m in info.Members do
                        checkMember (sprintf "List.%s" m.Name) m
                | other -> failtestf "expected List`1 as a Class shape, got %A" other
            }

            test "an unknown type misses" {
                Expect.isTrue (provider.TryLookupType "No.Such.Type`9" |> ValueOption.isNone) "unknown type miss"
            }

            test "the metadata layer resolves no values" {
                // F#-style module values / operators are not a metadata surface;
                // they fall through to lower-priority sources in the composite.
                Expect.isTrue (provider.TryLookup "op_Addition" |> ValueOption.isNone) "no value surface"
            }
        ]
