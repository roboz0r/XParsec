module XParsec.FSharp.Codegen.Js.Tests.ExternalNominalClassTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// TS provider: a manifest `Interface`/`Class` name resolves through
// `TsManifestProvider.toFrozen` as `FTClass` (→ front-end `TyClass`), so a receiver
// whose type FLOWS FROM A SIGNATURE (a function/member return — the path that goes
// through `toFrozen`, unlike a direct annotation, which Translate.fs already minted as
// `TyClass`) admits `.member` access via `resolveFieldStep`'s external-`TyClass` arm.
//
// The negative gates are asserted at the provider level: a primitive (`int`) return
// stays `FTConst`, and a `TypeAlias` name stays a transparent `Abbrev` (its target
// frozen, its NAME never minted as `FTClass`) — so alias expansion still fires.

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

/// `boxlib`: a NON-GENERIC interface `Box { get(): int; set(x: int): unit }`, two free
/// functions whose RETURN types force the `toFrozen` path (`makeBox(): Box` → `FTClass`;
/// `wantInt(): int` → `FTConst`), a transparent alias (`type Count = int`), and a
/// namespaced interface `NS.Inner` reached by its qualified name — exercising the
/// nsPath="" AND namespaced legs of the identity equation.
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
                // A GENERIC interface `Wrap<T> { value(): T }` + a factory returning
                // `Wrap<int>`, to pin THE LAW (`SymbolKeyOps.arityName`): the provider must
                // register/mint `Wrap` under its arity-suffixed compiled name `` Wrap`1 `` so a
                // `Wrap<int>` annotation (resolved by `TypeTranslate` as `arityName "Wrap" 1`)
                // and the `toFrozen` return key AGREE — the exact mitt wall-2 shape.
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

/// Layered over the JS provider so `int`/`unit` resolve in the front-end test.
/// Ambient AGGREGATED from the sources (not `stackJs []`): the intrinsic resolver
/// reaches `Vesper.unit`/`Vesper.int` (carried by `jsProvider`'s Vesper.Core) only
/// through the `Vesper` open-prefix, which a dropped ambient would hide.
let private boxProvider: IExternalSymbolProvider =
    stackWithAmbient [ boxProviderRaw; jsProvider.Value ]

/// The frozen RETURN of a manifest free function (its `Scheme` is `FTFun(_, ret)`).
let private returnOf (name: string) : FrozenType =
    match boxProviderRaw.TryLookup name with
    | ValueSome sym ->
        match sym.Scheme with
        | FTFun(_, ret) -> ret
        | other -> failwithf "'%s' scheme is not a function arrow: %A" name other
    | ValueNone -> failwithf "'%s' did not resolve as a value symbol" name

let private analyse (input: string) : Diagnostic list =
    let lexed, file = parseFile input

    let tast =
        Pipeline.analyseSemForSelfHost boxProvider (Hashing.originSourceOfText input lexed) file

    tast.Diagnostics |> Diagnostic.errors

[<Tests>]
let tests =
    testList
        "ExternalNominalClass"
        [
            test "a value from a manifest-interface-returning function admits .member access" {
                // `b`'s type flows from `makeBox`'s return (the `toFrozen` path). Without the
                // external-class freeze it would be `FTConst`→`TyConst` and `.get`/`.set` would
                // fall to the "non-record non-class" catch-all; with it, `b` is
                // `FTClass`→`TyClass` and the member resolves through the provider. Analysis-only
                // — no JS emission (that is the member-call path's concern).
                let program =
                    String.concat "\n" [ "let b = makeBox()"; "let n = b.get()"; "b.set(n)"; "" ]

                let errors = analyse program

                Expect.isEmpty
                    errors
                    (sprintf "expected no analysis errors, got:\n%A" (errors |> List.map (fun d -> d.Message)))
            }

            test "manifest interface return freezes to FTClass with the identity-equation key" {
                // The frozen `FTClass` key must be servable by the provider's store
                // face, and its `qualifiedName` must equal the manifest's map key —
                // the producer-side invariant the provider build (and the store
                // face's internal name projection) rely on. Prove it by reading the
                // key back and confirming the key-addressed member lookup hits.
                match returnOf "makeBox" with
                | FTClass(key, args) ->
                    Expect.equal
                        (SymbolKeyOps.typeMetaName key)
                        "Box"
                        "top-level key qualifiedName must equal the map key"

                    Expect.isEmpty (args |> EqArray.toList) "Box is non-generic"

                    Expect.isTrue
                        (match boxProviderRaw.TryLookupMember(SymbolKey.Type key, "get") with
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
                        (match boxProviderRaw.TryLookupMember(SymbolKey.Type key, "get") with
                         | ValueSome _ -> true
                         | ValueNone -> false)
                        "the namespaced member must resolve under the key's qualifiedName"
                | other -> failtestf "NS.makeInner return should be FTClass, got %A" other
            }

            test "GENERIC manifest interface return freezes to FTClass keyed by its arity-suffixed name" {
                // THE LAW (`SymbolKeyOps.arityName`): `Wrap<T>` is nominal `` Wrap`1 ``. The
                // frozen return of `makeIntWrap(): Wrap<int>` must be `FTClass` whose key
                // `qualifiedName` is `` "Wrap`1" `` — NOT the bare `Wrap` — and the type must
                // resolve under that suffixed string.
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

            test "GENERIC manifest annotation type-checks (mitt wall-2 shape, minus mitt's other walls)" {
                // `let w : Wrap<int> = makeIntWrap()`: the annotation resolves through
                // `TypeTranslate` as `arityName "Wrap" 1` = `` Wrap`1 `` while the RHS return
                // freezes to `FTClass(` Wrap`1 `)`. Pre-fix the two spellings diverged
                // (`TyClass(Wrap)` vs `TyClass(Wrap`1)`) — the mitt wall #2 mismatch — and the
                // annotation failed to unify. With the provider now speaking THE LAW, they agree.
                let errors = analyse "let w : Wrap<int> = makeIntWrap()\n"

                Expect.isEmpty
                    errors
                    (sprintf "expected no analysis errors, got:\n%A" (errors |> List.map (fun d -> d.Message)))
            }

            test "NEGATIVE: a primitive return stays FTConst (not FTClass)" {
                match returnOf "wantInt" with
                | FTConst(key, _) ->
                    Expect.equal (SymbolKeyOps.simpleName key) (DisplayName "int") "primitive must stay a bare FTConst"
                | other -> failtestf "wantInt return should be FTConst int, got %A" other
            }

            test "NEGATIVE: a type-alias name stays a transparent Abbrev (never FTClass)" {
                // The alias must resolve as `Abbrev` so its use sites expand to the target;
                // minting `FTClass` for the alias NAME would break that expansion.
                match boxProviderRaw.TryLookupType "Count" |> ExternalSymbols.typeShapeOf with
                | ValueSome(ExternalTypeShape.Abbrev(arity, target)) ->
                    Expect.equal arity 0 "Count is non-generic"

                    match target with
                    | FTConst(key, _) ->
                        Expect.equal (SymbolKeyOps.simpleName key) (DisplayName "int") "the alias target stays FTConst"
                    | other -> failtestf "Count target should be FTConst int, got %A" other
                | other -> failtestf "Count should resolve as ExternalTypeShape.Abbrev, got %A" other
            }
        ]
