module XParsec.FSharp.Codegen.Js.Tests.ExternalNominalClassTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// R1 (ts-provider plan): a manifest `Interface`/`Class` name resolves through
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
        Params = []
        Returns = ret
    }

let private sig1 (pname: string) (pty: Schema.TypeRef) (ret: Schema.TypeRef) : Schema.Signature =
    {
        TypeParams = 0
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
                    []
                )
                Schema.Export.Function("makeBox", [ sig0 (named "Box") ], Schema.ImportShape.Named)
                Schema.Export.Function("wantInt", [ sig0 intT ], Schema.ImportShape.Named)
                Schema.Export.TypeAlias("Count", 0, intT)
                Schema.Export.Namespace(
                    "NS",
                    [
                        Schema.Export.Interface("Inner", 0, [ method' "get" (sig0 intT) ], [])
                        // The wire spells a cross/namespaced reference by its QUALIFIED name.
                        Schema.Export.Function("makeInner", [ sig0 (named "NS.Inner") ], Schema.ImportShape.Named)
                    ]
                )
            ]
        Diagnostics = []
    }

let private boxProviderRaw: IExternalSymbolProvider =
    TsManifestProvider.providerOfManifest boxManifest

/// Layered over the JS provider so `int`/`unit` resolve in the front-end test.
let private boxProvider: IExternalSymbolProvider =
    ExternalSymbols.stack ValueNone [] [ boxProviderRaw; jsProvider.Value ]

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
    let tast = Pipeline.analyseSemForSelfHost boxProvider input lexed file
    tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

[<Tests>]
let tests =
    testList
        "ExternalNominalClass"
        [
            test "a value from a manifest-interface-returning function admits .member access" {
                // `b`'s type flows from `makeBox`'s return (the `toFrozen` path). Pre-R1 it
                // was `FTConst`→`TyConst` and `.get`/`.set` fell to the "non-record
                // non-class" catch-all; post-R1 it is `FTClass`→`TyClass` and the member
                // resolves through the provider. Analysis-only — no JS emission (that is R2).
                let program =
                    String.concat "\n" [ "let b = makeBox()"; "let n = b.get()"; "b.set(n)"; "" ]

                let errors = analyse program

                Expect.isEmpty
                    errors
                    (sprintf "expected no analysis errors, got:\n%A" (errors |> List.map (fun d -> d.Message)))
            }

            test "manifest interface return freezes to FTClass with the identity-equation key" {
                // The minted key's `qualifiedName` MUST equal the provider's map key — the
                // exact string `resolveFieldStep` hands to `TryLookupMember`. Prove it by
                // reading the key back and confirming the member lookup hits under it.
                match returnOf "makeBox" with
                | FTClass(key, args) ->
                    Expect.equal
                        (SymbolKeyOps.qualifiedName key)
                        "Box"
                        "top-level key qualifiedName must equal the map key"

                    Expect.isEmpty (args |> EqArray.toList) "Box is non-generic"

                    Expect.isTrue
                        (match boxProviderRaw.TryLookupMember(SymbolKeyOps.qualifiedName key, "get") with
                         | ValueSome _ -> true
                         | ValueNone -> false)
                        "the member must resolve under the key's qualifiedName (identity equation)"
                | other -> failtestf "makeBox return should be FTClass, got %A" other
            }

            test "namespaced interface return freezes to FTClass keyed by its qualified name" {
                match returnOf "NS.makeInner" with
                | FTClass(key, _) ->
                    Expect.equal
                        (SymbolKeyOps.qualifiedName key)
                        "NS.Inner"
                        "namespaced key qualifiedName must equal the dotted map key"

                    Expect.isTrue
                        (match boxProviderRaw.TryLookupMember(SymbolKeyOps.qualifiedName key, "get") with
                         | ValueSome _ -> true
                         | ValueNone -> false)
                        "the namespaced member must resolve under the key's qualifiedName"
                | other -> failtestf "NS.makeInner return should be FTClass, got %A" other
            }

            test "NEGATIVE: a primitive return stays FTConst (not FTClass)" {
                match returnOf "wantInt" with
                | FTConst(name, _) -> Expect.equal name "int" "primitive must stay a bare FTConst"
                | other -> failtestf "wantInt return should be FTConst int, got %A" other
            }

            test "NEGATIVE: a type-alias name stays a transparent Abbrev (never FTClass)" {
                // The alias must resolve as `Abbrev` so its use sites expand to the target;
                // minting `FTClass` for the alias NAME would break that expansion.
                match boxProviderRaw.TryLookupType "Count" with
                | ValueSome(ExternalTypeShape.Abbrev(arity, target)) ->
                    Expect.equal arity 0 "Count is non-generic"

                    match target with
                    | FTConst(name, _) -> Expect.equal name "int" "the alias target stays FTConst"
                    | other -> failtestf "Count target should be FTConst int, got %A" other
                | other -> failtestf "Count should resolve as ExternalTypeShape.Abbrev, got %A" other
            }
        ]
