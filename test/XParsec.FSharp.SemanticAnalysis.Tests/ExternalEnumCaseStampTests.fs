module XParsec.FSharp.SemanticAnalysis.Tests.ExternalEnumCaseStampTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// NameResolution — the one resolve-once layer — recognises an external enum-case access
// `E.C1` in BOTH expression and pattern position: `E` is qualified opens-aware to an
// external `ExternalTypeShape.Enum` declaring `C1`, and the enum's nominal `SymbolKey`
// is stamped under the head's `NodeKey` (`Resolution.ExternalEnumCaseStamp`).
// Unification's `InferIdentExpr` / `InferPat` enum arms READ that stamp and type the node
// `TyEnum key`, instead of re-recognising the spelling through the resolver-face
// `TryLookupType(string)`. These tests assert the stamp is present at a representative
// expression and pattern site, absent for an unknown case, and gated on the enum's
// namespace being open (short-name type resolution is opens-sensitive).

/// A provider that knows two external enums: `Tests.Direction` (cases `Up`/`Down`) whose
/// namespace `Tests` is AUTO-OPENED via `AmbientOpenPrefixes`, and `Other.Mode` (case
/// `On`) whose namespace `Other` is NOT ambient, so its short name `Mode` resolves only
/// under an explicit `open Other`. Only the resolver-face `TryLookupType(string)` is
/// exercised (NameResolution stamps on enum-case resolution); the store face is unused.
let private provider: IExternalSymbolProvider =
    let enum names =
        ExternalTypeShape.Enum(
            names
            |> List.mapi (fun i n ->
                {
                    Name = n
                    Value = ExternalEnumCaseValue.IntVal(int64 i)
                }
            )
            |> List.toArray,
            SymbolOrigin.Empty
        )

    { new IExternalSymbolProvider

      interface IExternalSymbolResolver with
          member _.TryLookup _ = ValueNone

          member _.TryLookupType(n: string) =
              match n with
              | "Tests.Direction" -> ValueSome(enum [ "Up"; "Down" ])
              | "Other.Mode" -> ValueSome(enum [ "On" ])
              | _ -> ValueNone

          member _.TryLookupUnionCase _ = ValueNone
          member _.AmbientOpenPrefixes = [ "Tests" ]
      interface IExternalSymbolStore with
          member _.TryLookupType(_: SymbolKey) = ValueNone
          member _.TryLookupMember(_, _) = ValueNone
          member _.TryLookupMembers(_, _) = [||]
          member _.TryLookupIndexSignature _ = []
          member _.TryLookupInlineBody _ = ValueNone
          member _.IntrinsicReverseCanon = Map.empty
          member _.IntrinsicForwardRepr = ExternalSymbols.emptyForwardRepr
    }

let private analyse (input: string) : PassContext * ImplementationFile<SyntaxToken> =
    let lexed, file = parseFile input
    let ctx = PassContext(provider, input, lexed)
    Desugar.run ctx file
    NameResolution.run ctx file
    ctx, file

/// The RHS expression of the file's first `let` binding.
let private firstBindingExpr (file: ImplementationFile<SyntaxToken>) : Expr<SyntaxToken> =
    CstWalk.implFileElems file
    |> Seq.pick (fun m ->
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) when bindings.Length > 0 ->
            Some bindings.[0].expr
        | _ -> None
    )

/// Every match-arm pattern reachable in `file`.
let private matchArmPats (file: ImplementationFile<SyntaxToken>) : Pat<SyntaxToken> list =
    let acc = ResizeArray<Pat<SyntaxToken>>()

    let walker: CstWalk.ExprWalker<unit> =
        {
            Visit = fun () _ -> ()
            EnterFun = fun () _ -> ()
            EnterBindingRhs = fun () _ _ _ -> ()
            EnterLetBody = fun () _ -> ()
            EnterForTo = fun () _ -> ()
            EnterForIn = fun () _ -> ()
            EnterMatchArm = fun () p -> acc.Add p
        }

    for m in CstWalk.implFileElems file do
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            for b in bindings do
                CstWalk.iterExpr walker () b.expr
        | ModuleElem.Expression e -> CstWalk.iterExpr walker () e
        | _ -> ()

    List.ofSeq acc

/// A `Pat.Named` whose last segment is `Up` (the `Direction.Up` enum-case pattern head).
let private isDirectionUp (ctx: PassContext) (p: Pat<SyntaxToken>) : bool =
    match p with
    | Pat.Named(longIdent = li) when li.Idents.Length >= 1 -> ctx.NameOf li.Idents.[li.Idents.Length - 1] = "Up"
    | _ -> false

[<Tests>]
let tests =
    testList
        "ExternalEnumCaseStamp"
        [
            // Expression position: `Direction.Up` (head `Direction` qualifies to the
            // auto-opened `Tests.Direction`) is stamped with the enum nominal key so
            // `InferIdentExpr` types it `TyEnum key`.
            test "external enum-case access is stamped (expression)" {
                let ctx, file = analyse "let x = Direction.Up"
                let e = firstBindingExpr file

                Expect.isTrue
                    (ctx.Resolution.ExternalEnumCaseStamp.ContainsKey(CstKeys.ofExpr e))
                    "Direction.Up enum key stamped in expression position"
            }

            // Pattern position: `| Direction.Up` is stamped so `InferPat`'s enum arm types
            // the pattern `TyEnum key` and the scrutinee unifies.
            test "external enum-case pattern is stamped" {
                let ctx, file = analyse "let f (o: obj) = match o with | Direction.Up -> 1 | _ -> 0"
                let heads = matchArmPats file |> List.filter (isDirectionUp ctx)

                Expect.equal heads.Length 1 "exactly one Direction.Up match arm"

                for h in heads do
                    Expect.isTrue
                        (ctx.Resolution.ExternalEnumCaseStamp.ContainsKey(CstKeys.ofPat h))
                        "Direction.Up enum key stamped in pattern position"
            }

            // An unknown case name on a known enum resolves to nothing — not stamped.
            test "unknown enum case is not stamped" {
                let ctx, file = analyse "let x = Direction.Sideways"
                let e = firstBindingExpr file

                Expect.isFalse
                    (ctx.Resolution.ExternalEnumCaseStamp.ContainsKey(CstKeys.ofExpr e))
                    "Direction.Sideways (no such case) is not stamped"
            }

            // The opens gate: `Other.Mode`'s namespace `Other` is not auto-opened, so the
            // short enum name `Mode` does not resolve — `Mode.On` is not stamped.
            test "enum case whose namespace is not opened is not stamped" {
                let ctx, file = analyse "let x = Mode.On"
                let e = firstBindingExpr file

                Expect.isFalse
                    (ctx.Resolution.ExternalEnumCaseStamp.ContainsKey(CstKeys.ofExpr e))
                    "Mode.On is not stamped with Other unopened"
            }

            // Positive: the SAME access stamps once its namespace is explicitly opened.
            test "enum case is stamped once its namespace is opened" {
                let ctx, file = analyse "open Other\nlet x = Mode.On"
                let e = firstBindingExpr file

                Expect.isTrue
                    (ctx.Resolution.ExternalEnumCaseStamp.ContainsKey(CstKeys.ofExpr e))
                    "Mode.On is stamped under open Other"
            }
        ]
