module XParsec.FSharp.Codegen.Clr.Tests.ExternalMemberTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// P3 gate: front-end member access on an
// *external* type. `EqualityComparer<int>.Default.GetHashCode 5` type-checks and
// freezes through the metadata-backed provider (P2), each member-access node
// carrying its resolved `SymbolKey` (§7.2) — the first front-end consumer of the
// provider's member surface. (Emission is P4; this asserts typing + the key only.)

let private eqComparer = "System.Collections.Generic.EqualityComparer`1"

/// Analyse a source string through the layered provider (`composite [ metadata ;
/// MockBuiltins ]`), returning the frozen `TastFile`.
let private analyseWith (provider: IExternalSymbolProvider) (input: string) : TastFile =
    let lexed, file = parseFile input
    Pipeline.analyseSem provider input lexed file

let private errors (tast: TastFile) : Diagnostic list =
    tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

[<Tests>]
let tests =
    testList
        "ExternalMember"
        [
            test "EqualityComparer<int>.Default.GetHashCode 5 type-checks + freezes carrying its key" {
                let provider = SymbolProviders.build []

                let tast =
                    analyseWith
                        provider
                        "let h = System.Collections.Generic.EqualityComparer<int>.Default.GetHashCode 5"

                Expect.isEmpty (errors tast) "no type errors through the metadata-backed provider"

                // `GetHashCode 5` is `App(ExternalMember(GetHashCode), 5)`; the
                // GetHashCode access's receiver is the `Default` static access.
                let value =
                    match tast.Decls with
                    | EqList [ TDecl.Let(value = v) ] -> v
                    | _ -> failtestf "expected a single let binding, got %A" tast.Decls

                match value with
                | TExpr.App(TExpr.ExternalMember(ValueSome inner, ghKey, "GetHashCode", false, ghTy, _),
                            TExpr.Const(TConstValue.Int 5, _, _),
                            resultTy,
                            _) ->
                    // The instance access is a method value `int -> int`; applying
                    // `5` yields `int`.
                    match Unification.zonk ghTy with
                    | TyFun(TyConst("int", _), TyConst("int", _)) -> ()
                    | other -> failtestf "GetHashCode should be typed int -> int, got %A" other

                    match Unification.zonk resultTy with
                    | TyConst("int", _) -> ()
                    | other -> failtestf "the application should be typed int, got %A" other

                    // GetHashCode(T) — an instance method on the open type, its
                    // argSig the declaring typar `!0`.
                    match ghKey with
                    | SymbolKey.MemberKey(SymbolKey.TypeKey(asm, ns, name), "GetHashCode", argSig, MemberKind.Method) ->
                        Expect.isTrue asm.IsSome "GetHashCode decl carries the defining assembly"
                        Expect.equal ns "System.Collections.Generic" "GetHashCode decl namespace"
                        Expect.equal name "EqualityComparer`1" "GetHashCode decl type name (arity-suffixed)"
                        Expect.equal (EqArray.toList argSig) [ "!0" ] "GetHashCode(T) argSig is the declaring typar"
                    | other -> failtestf "unexpected GetHashCode key %A" other

                    // The `Default` static property — receiver dropped (ValueNone),
                    // typed EqualityComparer<int>, empty argSig.
                    match inner with
                    | TExpr.ExternalMember(ValueNone, defKey, "Default", true, defTy, _) ->
                        match Unification.zonk defTy with
                        | TyClass(name, args) when
                            args.Length = 1
                            && (
                                match args.[0] with
                                | TyConst("int", _) -> true
                                | _ -> false
                            )
                            ->
                            Expect.equal name eqComparer "Default : EqualityComparer<int>"
                        | other -> failtestf "Default should be typed EqualityComparer<int>, got %A" other

                        match defKey with
                        | SymbolKey.MemberKey(SymbolKey.TypeKey(_, ns, name), "Default", argSig, MemberKind.Property) ->
                            Expect.equal ns "System.Collections.Generic" "Default decl namespace"
                            Expect.equal name "EqualityComparer`1" "Default decl type name"
                            Expect.isTrue argSig.IsEmpty "Default is a property: empty argSig"
                        | other -> failtestf "unexpected Default key %A" other
                    | other -> failtestf "expected a static `Default` ExternalMember receiver, got %A" other
                | other -> failtestf "expected App(ExternalMember GetHashCode, 5), got %A" other
            }

            test "the frozen key matches the provider's own resolved member key" {
                // The node's interned key must equal what the provider resolves the
                // member to directly — Freeze stamps the resolver's verdict, it does
                // not re-derive a key.
                let provider = SymbolProviders.build []

                let expected =
                    match provider.TryLookupMember(eqComparer, "GetHashCode") with
                    | ValueSome m -> m.Key
                    | ValueNone -> failtest "provider did not resolve GetHashCode"

                let tast =
                    analyseWith
                        provider
                        "let h = System.Collections.Generic.EqualityComparer<int>.Default.GetHashCode 5"

                let frozen =
                    match tast.Decls with
                    | EqList [ TDecl.Let(value = TExpr.App(TExpr.ExternalMember(key = k), _, _, _)) ] -> k
                    | _ -> failtestf "expected App(ExternalMember …), got %A" tast.Decls

                Expect.equal frozen expected "frozen key = provider's resolved key"
            }

            // O2 gate: the *short* name under its `open`
            // type-checks and freezes the same keyed node as the fully-qualified
            // form — short-name resolution flows through `OpenScope.tryQualify` in
            // both NameResolution and Unification.
            test "short name under `open` type-checks + freezes carrying its key" {
                let provider = SymbolProviders.build []

                let tast =
                    analyseWith
                        provider
                        "open System.Collections.Generic\nlet h = EqualityComparer<int>.Default.GetHashCode 5"

                Expect.isEmpty (errors tast) "no type errors for the short-name form under its open"

                let value =
                    tast.Decls
                    |> EqArray.tryFind (
                        function
                        | TDecl.Let _ -> true
                        | _ -> false
                    )
                    |> ValueOption.map (
                        function
                        | TDecl.Let(value = v) -> v
                        | _ -> failwith "unreachable"
                    )

                match value with
                | ValueSome(TExpr.App(TExpr.ExternalMember(ValueSome inner, ghKey, "GetHashCode", false, ghTy, _),
                                      TExpr.Const(TConstValue.Int 5, _, _),
                                      resultTy,
                                      _)) ->
                    match Unification.zonk ghTy with
                    | TyFun(TyConst("int", _), TyConst("int", _)) -> ()
                    | other -> failtestf "GetHashCode should be typed int -> int, got %A" other

                    match Unification.zonk resultTy with
                    | TyConst("int", _) -> ()
                    | other -> failtestf "the application should be typed int, got %A" other

                    match ghKey with
                    | SymbolKey.MemberKey(SymbolKey.TypeKey(asm, ns, name), "GetHashCode", argSig, MemberKind.Method) ->
                        Expect.isTrue asm.IsSome "GetHashCode decl carries the defining assembly"
                        Expect.equal ns "System.Collections.Generic" "GetHashCode decl namespace"
                        Expect.equal name "EqualityComparer`1" "GetHashCode decl type name (arity-suffixed)"
                        Expect.equal (EqArray.toList argSig) [ "!0" ] "GetHashCode(T) argSig is the declaring typar"
                    | other -> failtestf "unexpected GetHashCode key %A" other

                    match inner with
                    | TExpr.ExternalMember(ValueNone, _, "Default", true, _, _) -> ()
                    | other -> failtestf "expected a static `Default` ExternalMember receiver, got %A" other
                | other -> failtestf "expected App(ExternalMember GetHashCode, 5), got %A" other
            }

            test "short-name key equals the fully-qualified form's resolved key" {
                let provider = SymbolProviders.build []

                let expected =
                    match provider.TryLookupMember(eqComparer, "GetHashCode") with
                    | ValueSome m -> m.Key
                    | ValueNone -> failtest "provider did not resolve GetHashCode"

                let tast =
                    analyseWith
                        provider
                        "open System.Collections.Generic\nlet h = EqualityComparer<int>.Default.GetHashCode 5"

                let frozen =
                    tast.Decls
                    |> EqArray.tryFind (
                        function
                        | TDecl.Let(value = TExpr.App(TExpr.ExternalMember _, _, _, _)) -> true
                        | _ -> false
                    )
                    |> ValueOption.map (
                        function
                        | TDecl.Let(value = TExpr.App(TExpr.ExternalMember(key = k), _, _, _)) -> k
                        | _ -> failwith "unreachable"
                    )

                Expect.equal frozen (ValueSome expected) "frozen key (short name) = provider's resolved key"
            }

            // The codegen identity
            // bridge. The frozen `TExpr.ExternalMember` nodes (a static property
            // `Default`, an instance method `GetHashCode`) are emitted from their
            // interned `SymbolKey` through `ClrProvider.ExternalMemberRef` — the
            // `Default` getter + `GetHashCode(!0)` member refs are minted on an
            // `EqualityComparer`1<int>` `TypeSpec`, no per-member hand-coding. An
            // emitted call runs: `Int32.GetHashCode` is the identity, so
            // `EqualityComparer<int>.Default.GetHashCode 5 = 5`. (`compileSource`'s
            // provider is `composite [ metadata ; MockBuiltins ]`, so the member
            // access resolves to the keyed node.
            test "an emitted call to a metadata-resolved member runs (EqualityComparer<int>.Default.GetHashCode 5 = 5)" {
                let src =
                    "printfn \"%d\" (System.Collections.Generic.EqualityComparer<int>.Default.GetHashCode 5)"

                let _, artifact = compileSource "P4ExternalMemberFq" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "5" "GetHashCode of int 5 is 5"

                // The BCL comparer pins no FSharp.Core dependency (it rides
                // `System.Private.CoreLib`, like the `hash` stopgap).
                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "BCL member call pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)
            }

            // `translateType` external-type resolution: a *type annotation* naming an external type used to land as
            // an opaque `TyConst` (single-segment, args dropped) or a fresh `TyVar`
            // (multi-segment) — only static-member *receivers* resolved
            // (`tryExternalTypeReceiver`). Now `translateType` probes the provider too,
            // so the annotated type is the same external `TyClass` the receiver carries
            // and the two unify.
            test "a type annotation resolves an external type — short form unifies with the receiver" {
                let provider = SymbolProviders.build []

                // The annotation `EqualityComparer<int>` must unify with the resolved
                // `Default` receiver type. Before the fix the single-segment annotation
                // dropped its args to `TyConst("EqualityComparer", _)`, which clashes with
                // the receiver's `TyClass` → a spurious type error; an empty error list
                // is the decisive observable.
                let tast =
                    analyseWith
                        provider
                        "open System.Collections.Generic\nlet d : EqualityComparer<int> = EqualityComparer<int>.Default"

                Expect.isEmpty (errors tast) "the annotated external type unifies with the resolved Default receiver"
            }

            test "a fully-qualified type annotation resolves to the external TyClass (not a fresh TyVar)" {
                let provider = SymbolProviders.build []

                // Before the fix a multi-segment annotation fell to a fresh `TyVar`,
                // which unifies silently with `5 : int` (no error). Now it resolves to
                // the external `TyClass`, so the `int` RHS is a reported mismatch — and
                // the message names the resolved type, proving `translateType` resolved
                // it rather than handing back an anonymous variable.
                let tast =
                    analyseWith provider "let d : System.Collections.Generic.EqualityComparer<int> = 5"

                let errs = errors tast
                Expect.isNonEmpty errs "the resolved external annotation rejects the int RHS"

                Expect.isTrue
                    (errs |> List.exists (fun d -> d.Message.Contains "EqualityComparer"))
                    (sprintf
                        "the mismatch names the resolved external type, got %A"
                        (errs |> List.map (fun d -> d.Message)))
            }

            // Tupled-member regression (member-emit + recoverTypeArgs): a 2-arg
            // external *instance* method.
            // `EqualityComparer<int>.Default.Equals(x, y)` is the first
            // arity-≥2 external method to flow through `buildExpr` + `externalMemberRef`
            // (the DU triple hand-rolls its IL and bypasses this path). The member is
            // modelled tupled (`(int*int)→bool`), so the front-end `unify`/`recoverTypeArgs`
            // recover the declaring typar from the element (not the whole tuple), and
            // emit pushes the literal `(x, y)` tuple element-wise — no `splitAt` crash,
            // no `EqualityComparer<int*int>` mis-encoding.
            test "a 2-arg external instance method (EqualityComparer<int>.Default.Equals) emits + runs" {
                let src =
                    String.concat
                        "\n"
                        [
                            "open System.Collections.Generic"
                            "printfn \"%d\" (if EqualityComparer<int>.Default.Equals(1, 1) then 1 else 0)" // 1
                            "printfn \"%d\" (if EqualityComparer<int>.Default.Equals(1, 2) then 1 else 0)" // 0
                        ]

                let _, artifact = compileSource "P4ExternalEquals2Arg" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n0" "Equals(1,1)=true, Equals(1,2)=false"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "2-arg BCL member call pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)
            }

            // Overload-resolution regression: a 2-arg external *static*
            // method with overloads. `System.String.Concat` has many overloads
            // (`(string,string)`, `(object,object)`, `(ReadOnlySpan<char>,…)`, …);
            // the call-site resolver filters by arity (2), then applicability
            // (string args rule out the `ReadOnlySpan` pair), then betterness
            // (`(string,string)` beats `(object,object)`). The old eager single-pick
            // ("most params wins") chose a 4-param overload and mis-typed the call.
            test "a 2-arg external static method with overloads (String.Concat) resolves + runs" {
                let _, artifact =
                    compileSource "P4ExternalConcat2Arg" "printfn \"%s\" (System.String.Concat(\"a\", \"b\"))"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "ab" "String.Concat(\"a\", \"b\") = \"ab\""
            }

            // The short-name form (under its `open`) emits and runs identically —
            // open-resolution (P3.5) feeds the same keyed node into P4.
            test "the short-name form under `open` emits and runs" {
                let src =
                    "open System.Collections.Generic\nprintfn \"%d\" (EqualityComparer<int>.Default.GetHashCode 42)"

                let _, artifact = compileSource "P4ExternalMemberShort" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "GetHashCode of int 42 is 42"
            }

            // Non-generic external static access. `System.Console.Out` folds into a single LongIdent (no `<>` to
            // keep a `TypeApp` receiver), so the generic DotLookup arm never sees it;
            // `tryExternalStaticLongIdent` recovers the type-prefix / static-member
            // split, types it, and freezes a keyed `TExpr.ExternalMember`.
            test "non-generic external static property resolves + freezes carrying its key" {
                let provider = SymbolProviders.build []
                let tast = analyseWith provider "let w = System.Console.Out"

                Expect.isEmpty (errors tast) "System.Console.Out resolves through the metadata provider"

                let value =
                    match tast.Decls with
                    | EqList [ TDecl.Let(value = v) ] -> v
                    | _ -> failtestf "expected a single let binding, got %A" tast.Decls

                match value with
                | TExpr.ExternalMember(ValueNone, key, "Out", true, ty, _) ->
                    match Unification.zonk ty with
                    | TyClass("System.IO.TextWriter", args) when args.IsEmpty -> ()
                    | other -> failtestf "Out should be typed System.IO.TextWriter, got %A" other

                    match key with
                    | SymbolKey.MemberKey(SymbolKey.TypeKey(asm, ns, name), "Out", argSig, MemberKind.Property) ->
                        Expect.isTrue asm.IsSome "Out decl carries the defining assembly"
                        Expect.equal ns "System" "Out decl namespace"
                        Expect.equal name "Console" "Out decl type name (non-generic, no arity suffix)"
                        Expect.isTrue argSig.IsEmpty "Out is a property: empty argSig"
                    | other -> failtestf "unexpected Out key %A" other
                | other -> failtestf "expected a static `Out` ExternalMember, got %A" other
            }

            test "the short form under `open` resolves the same non-generic static member" {
                let provider = SymbolProviders.build []
                let tast = analyseWith provider "open System\nlet w = Console.Out"

                Expect.isEmpty (errors tast) "Console.Out resolves under `open System`"

                let value =
                    tast.Decls
                    |> EqArray.tryFind (
                        function
                        | TDecl.Let _ -> true
                        | _ -> false
                    )
                    |> ValueOption.map (
                        function
                        | TDecl.Let(value = v) -> v
                        | _ -> failwith "unreachable"
                    )

                match value with
                | ValueSome(TExpr.ExternalMember(ValueNone,
                                                 SymbolKey.MemberKey(SymbolKey.TypeKey(_, "System", "Console"),
                                                                     "Out",
                                                                     EqList [],
                                                                     MemberKind.Property),
                                                 "Out",
                                                 true,
                                                 _,
                                                 _)) -> ()
                | other -> failtestf "expected the same keyed Console.Out ExternalMember, got %A" other
            }

            // A resolved type prefix whose final segment is NOT an accessible static
            // member (here `PI`, a const field, not modelled yet) falls through
            // without a spurious "no accessible member" error — it's valid F#, just
            // unsupported.
            test "a non-member tail on a resolved external type does not error" {
                let provider = SymbolProviders.build []
                let tast = analyseWith provider "let p = System.Math.PI"
                Expect.isEmpty (errors tast) "System.Math.PI (a field) falls through silently, no false error"
            }

            // End-to-end: a non-generic static *property* returning a primitive emits
            // and runs through the P4 `ExternalMember` bridge (same path as `Default`).
            test "a non-generic external static property emits and runs" {
                let src = "printfn \"%d\" System.Environment.ProcessorCount"
                let _, artifact = compileSource "NonGenericStaticProp" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                let n = output.Replace("\r", "").Trim()

                match System.Int32.TryParse n with
                | true, v -> Expect.isTrue (v > 0) (sprintf "ProcessorCount is a positive int, got %d" v)
                | false, _ -> failtestf "expected a numeric ProcessorCount, got %A" n
            }
        ]
