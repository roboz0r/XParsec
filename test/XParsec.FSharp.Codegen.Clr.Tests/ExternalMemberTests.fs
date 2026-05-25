module XParsec.FSharp.Codegen.Clr.Tests.ExternalMemberTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// P3 gate (symbol-resolution-plan §8 / handoff): front-end member access on an
// *external* type. `EqualityComparer<int>.Default.GetHashCode 5` type-checks and
// freezes through the metadata-backed provider (P2), each member-access node
// carrying its resolved `SymbolKey` (§7.2) — the first front-end consumer of the
// provider's member surface. (Emission is P4; this asserts typing + the key only.)

let private eqComparer = "System.Collections.Generic.EqualityComparer`1"

/// Analyse a source string through the layered provider (`composite [ metadata ;
/// MockBuiltins ]`), returning the frozen `TastFile`.
let private analyseWith (provider: IExternalSymbolProvider) (input: string) : TastFile =
    let lexed, file = parseFile input
    Pipeline.analyse provider input lexed file

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
                    | [ TDecl.Let(value = v) ] -> v
                    | other -> failtestf "expected a single let binding, got %A" other

                match value with
                | TExpr.App(TExpr.ExternalMember(ValueSome inner, ghKey, "GetHashCode", false, ghTy),
                            TExpr.Const(TConstValue.Int 5, _),
                            resultTy) ->
                    // The instance access is a method value `int -> int`; applying
                    // `5` yields `int`.
                    match Unification.zonk ghTy with
                    | TyFun(TyConst "int", TyConst "int") -> ()
                    | other -> failtestf "GetHashCode should be typed int -> int, got %A" other

                    match Unification.zonk resultTy with
                    | TyConst "int" -> ()
                    | other -> failtestf "the application should be typed int, got %A" other

                    // GetHashCode(T) — an instance method on the open type, its
                    // argSig the declaring typar `!0` (symbol-resolution-plan §7.3).
                    match ghKey with
                    | SymbolKey.MemberKey(SymbolKey.TypeKey(asm, ns, name), "GetHashCode", argSig) ->
                        Expect.isTrue asm.IsSome "GetHashCode decl carries the defining assembly"
                        Expect.equal ns "System.Collections.Generic" "GetHashCode decl namespace"
                        Expect.equal name "EqualityComparer`1" "GetHashCode decl type name (arity-suffixed)"
                        Expect.equal argSig [ "!0" ] "GetHashCode(T) argSig is the declaring typar"
                    | other -> failtestf "unexpected GetHashCode key %A" other

                    // The `Default` static property — receiver dropped (ValueNone),
                    // typed EqualityComparer<int>, empty argSig.
                    match inner with
                    | TExpr.ExternalMember(ValueNone, defKey, "Default", true, defTy) ->
                        match Unification.zonk defTy with
                        | TyClass(name, [ TyConst "int" ]) ->
                            Expect.equal name eqComparer "Default : EqualityComparer<int>"
                        | other -> failtestf "Default should be typed EqualityComparer<int>, got %A" other

                        match defKey with
                        | SymbolKey.MemberKey(SymbolKey.TypeKey(_, ns, name), "Default", argSig) ->
                            Expect.equal ns "System.Collections.Generic" "Default decl namespace"
                            Expect.equal name "EqualityComparer`1" "Default decl type name"
                            Expect.equal argSig [] "Default is a property: empty argSig"
                        | other -> failtestf "unexpected Default key %A" other
                    | other -> failtestf "expected a static `Default` ExternalMember receiver, got %A" other
                | other -> failtestf "expected App(ExternalMember GetHashCode, 5), got %A" other
            }

            test "the frozen key matches the provider's own resolved member key" {
                // The node's interned key must equal what the provider resolves the
                // member to directly — Freeze stamps the resolver's verdict, it does
                // not re-derive a key (symbol-resolution-plan §7.2).
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
                    | [ TDecl.Let(value = TExpr.App(TExpr.ExternalMember(key = k), _, _)) ] -> k
                    | other -> failtestf "expected App(ExternalMember …), got %A" other

                Expect.equal frozen expected "frozen key = provider's resolved key"
            }

            // O2 gate (symbol-resolution-handoff.md, open-resolution): the *short* name under its `open`
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
                    |> List.tryPick (
                        function
                        | TDecl.Let(value = v) -> Some v
                        | _ -> None
                    )

                match value with
                | Some(TExpr.App(TExpr.ExternalMember(ValueSome inner, ghKey, "GetHashCode", false, ghTy),
                                 TExpr.Const(TConstValue.Int 5, _),
                                 resultTy)) ->
                    match Unification.zonk ghTy with
                    | TyFun(TyConst "int", TyConst "int") -> ()
                    | other -> failtestf "GetHashCode should be typed int -> int, got %A" other

                    match Unification.zonk resultTy with
                    | TyConst "int" -> ()
                    | other -> failtestf "the application should be typed int, got %A" other

                    match ghKey with
                    | SymbolKey.MemberKey(SymbolKey.TypeKey(asm, ns, name), "GetHashCode", argSig) ->
                        Expect.isTrue asm.IsSome "GetHashCode decl carries the defining assembly"
                        Expect.equal ns "System.Collections.Generic" "GetHashCode decl namespace"
                        Expect.equal name "EqualityComparer`1" "GetHashCode decl type name (arity-suffixed)"
                        Expect.equal argSig [ "!0" ] "GetHashCode(T) argSig is the declaring typar"
                    | other -> failtestf "unexpected GetHashCode key %A" other

                    match inner with
                    | TExpr.ExternalMember(ValueNone, _, "Default", true, _) -> ()
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
                    |> List.tryPick (
                        function
                        | TDecl.Let(value = TExpr.App(TExpr.ExternalMember(key = k), _, _)) -> Some k
                        | _ -> None
                    )

                Expect.equal frozen (Some expected) "frozen key (short name) = provider's resolved key"
            }
        ]
