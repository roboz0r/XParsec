module XParsec.FSharp.Codegen.Js.Tests.ExternalNominalClassTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// A manifest `Interface`/`Class` name freezes to `FTClass`, so a value whose type comes
// from a function or member RETURN admits `.member` access. Negative gates: an `int`
// return stays `FTConst`, and a `TypeAlias` name stays a transparent `Abbrev`.

// ─── Hand-built manifest (no JSON round-trip) ──────────────────────────────

let private named n = Schema.TypeRef.Named(n, [])
let private intT = named "int"
let private unitT = named "unit"

let private sig0 (ret: Schema.TypeRef) : Schema.Signature =
    {
        TypeParams = 0
        TypeParamBounds = []
        Params = []
        Returns = ret
    }

let private sig1 (pname: string) (pty: Schema.TypeRef) (ret: Schema.TypeRef) : Schema.Signature =
    {
        TypeParams = 0
        TypeParamBounds = []
        Params =
            [
                {
                    Name = pname
                    Type = pty
                    Optional = false
                    Rest = false
                }
            ]
        Returns = ret
    }

let private method' (name: string) (sg: Schema.Signature) : Schema.Member =
    {
        Name = name
        Kind = Schema.MemberKind.Method
        Type = None
        Signatures = [ sg ]
        Static = false
        Optional = false
    }

/// `boxlib`: a non-generic `Box { get(): int; set(x: int): unit }`, free functions whose
/// RETURN freezes (`makeBox(): Box` → `FTClass`; `wantInt(): int` → `FTConst`), a
/// transparent alias `type Count = int`, and a namespaced `NS.Inner`.
let private boxManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "boxlib"
        Version = None
        Exports =
            [
                Schema.Export.Interface(
                    "Box",
                    0,
                    [ method' "get" (sig0 intT); method' "set" (sig1 "x" intT unitT) ],
                    [],
                    []
                )
                Schema.Export.Function("makeBox", [ sig0 (named "Box") ], Schema.ImportShape.Named)
                Schema.Export.Function("wantInt", [ sig0 intT ], Schema.ImportShape.Named)
                // A generic `Wrap<T> { value(): T }` plus a factory returning `Wrap<int>`:
                // the provider registers `Wrap` under its arity-suffixed name `` Wrap`1 ``,
                // the same spelling a `Wrap<int>` annotation resolves to.
                Schema.Export.Interface("Wrap", 1, [ method' "value" (sig0 (Schema.TypeRef.Typar 0)) ], [], [])
                Schema.Export.Function(
                    "makeIntWrap",
                    [ sig0 (Schema.TypeRef.Named("Wrap", [ intT ])) ],
                    Schema.ImportShape.Named
                )
                Schema.Export.TypeAlias("Count", 0, intT)
                Schema.Export.Namespace(
                    "NS",
                    [
                        Schema.Export.Interface("Inner", 0, [ method' "get" (sig0 intT) ], [], [])
                        // The wire spells a cross/namespaced reference by its QUALIFIED name.
                        Schema.Export.Function("makeInner", [ sig0 (named "NS.Inner") ], Schema.ImportShape.Named)
                    ]
                )
            ]
        Diagnostics = []
        Refs = []
    }

let private boxProviderRaw: IExternalSymbolProvider =
    TsManifestProvider.providerOfManifest boxManifest

/// Layered over the JS provider so `int`/`unit` resolve. The ambient must be aggregated
/// from the sources: `Vesper.unit`/`Vesper.int` are reached only through the `Vesper`
/// open-prefix, which a dropped ambient would hide.
let private boxProvider: IExternalSymbolProvider =
    stackWithAmbient [ boxProviderRaw; jsProvider.Value ]

/// The frozen RETURN of a manifest free function (its `Scheme` is `FTFun(_, ret)`).
let private returnOf (name: string) : FrozenType =
    match boxProviderRaw.TryLookup name with
    | ValueSome sym ->
        match sym.Scheme with
        | FTFun(_, ret) -> ret
        | other -> failwithf "'%s' scheme is not a function type: %A" name other
    | ValueNone -> failwithf "'%s' did not resolve as a value symbol" name

let private analyse (input: string) : Diagnostic list =
    let lexed, file = parseFile input

    let tast = Pipeline.analyseSem boxProvider (Hashing.originSourceOfText lexed) file

    tast.Diagnostics |> Diagnostic.errors

[<Tests>]
let tests =
    testList
        "ExternalNominalClass"
        [
            test "a value from a manifest-interface-returning function admits .member access" {
                // `b`'s type comes from `makeBox`'s return, freezing to `FTClass`, so
                // `.get`/`.set` resolve through the provider. Analysis only; emission is
                // the member-call path's concern.
                let program =
                    String.concat "\n" [ "let b = makeBox()"; "let n = b.get()"; "b.set(n)"; "" ]

                let errors = analyse program

                Expect.isEmpty
                    errors
                    (sprintf "expected no analysis errors, got:\n%A" (errors |> List.map (fun d -> d.Message)))
            }

            test "manifest interface return freezes to FTClass with the identity-equation key" {
                match returnOf "makeBox" with
                | FTClass(key, args) ->
                    Expect.equal
                        (SymbolKeyOps.typeMetaName key)
                        "Box"
                        "top-level key qualifiedName must equal the map key"

                    Expect.isEmpty (args |> EqArray.toList) "Box is non-generic"

                    Expect.isTrue
                        (match boxProviderRaw.TryLookupMember(key, "get") with
                         | ValueSome _ -> true
                         | ValueNone -> false)
                        "the member must resolve by the frozen key (identity equation)"
                | other -> failtestf "makeBox return should be FTClass, got %A" other
            }

            test "namespaced interface return freezes to FTClass keyed by its qualified name" {
                match returnOf "NS.makeInner" with
                | FTClass(key, _) ->
                    Expect.equal
                        (SymbolKeyOps.typeMetaName key)
                        "NS.Inner"
                        "namespaced key qualifiedName must equal the dotted map key"

                    Expect.isTrue
                        (match boxProviderRaw.TryLookupMember(key, "get") with
                         | ValueSome _ -> true
                         | ValueNone -> false)
                        "the namespaced member must resolve under the key's qualifiedName"
                | other -> failtestf "NS.makeInner return should be FTClass, got %A" other
            }

            test "GENERIC manifest interface return freezes to FTClass keyed by its arity-suffixed name" {
                match returnOf "makeIntWrap" with
                | FTClass(key, args) ->
                    Expect.equal
                        (SymbolKeyOps.typeMetaName key)
                        "Wrap`1"
                        "generic key qualifiedName must be arity-suffixed"

                    Expect.equal args.Length 1 "Wrap<int> applies one type arg"

                    Expect.isTrue
                        (match boxProviderRaw.TryLookupType "Wrap`1" with
                         | ValueSome _ -> true
                         | ValueNone -> false)
                        "the generic type must resolve under its arity-suffixed name"
                | other -> failtestf "makeIntWrap return should be FTClass, got %A" other
            }

            test "GENERIC manifest annotation type-checks against the arity-suffixed key" {
                // The annotation `Wrap<int>` and the frozen return of `makeIntWrap()` must
                // reach the same arity-suffixed spelling `` Wrap`1 ``, or the binding fails
                // to unify.
                let errors = analyse "let w : Wrap<int> = makeIntWrap()\n"

                Expect.isEmpty
                    errors
                    (sprintf "expected no analysis errors, got:\n%A" (errors |> List.map (fun d -> d.Message)))
            }

            test "NEGATIVE: a primitive return stays FTConst (not FTClass)" {
                match returnOf "wantInt" with
                | FTConst(key, _) ->
                    Expect.equal
                        (SymbolKeyOps.typeSimpleName key)
                        (DisplayName "int")
                        "primitive must stay a bare FTConst"
                | other -> failtestf "wantInt return should be FTConst int, got %A" other
            }

            test "NEGATIVE: a type-alias name stays a transparent Abbrev (never FTClass)" {
                // The alias must resolve as `Abbrev` so its use sites expand to the target.
                match boxProviderRaw.TryLookupType "Count" |> ExternalSymbols.typeShapeOf with
                | ValueSome(ExternalTypeShape.Abbrev(arity, target)) ->
                    Expect.equal arity 0 "Count is non-generic"

                    match target with
                    | FTConst(key, _) ->
                        Expect.equal
                            (SymbolKeyOps.typeSimpleName key)
                            (DisplayName "int")
                            "the alias target stays FTConst"
                    | other -> failtestf "Count target should be FTConst int, got %A" other
                | other -> failtestf "Count should resolve as ExternalTypeShape.Abbrev, got %A" other
            }
        ]
