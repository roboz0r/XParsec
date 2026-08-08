module XParsec.FSharp.SemanticAnalysis.Tests.ExternalEnumCaseStampTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// `Tests.Direction` (cases `Up`/`Down`) sits in the ambient namespace `Tests`;
/// `Other.Mode` (case `On`) does not, so `Mode` resolves only under `open Other`.
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

    ExternalSymbolProviders.ofNamedLeaf
        { ExternalSymbolProviders.NamedLeaf.empty with
            TryLookupType =
                fun n ->
                    match n with
                    | "Tests.Direction" -> ValueSome(enum [ "Up"; "Down" ])
                    | "Other.Mode" -> ValueSome(enum [ "On" ])
                    | _ -> ValueNone
            AmbientOpenPrefixes = [ "Tests" ]
        }

let private analyse (input: string) = analyseNameRes provider input

/// Every match-arm pattern reachable in `file`.
let private matchArmPats (file: ImplementationFile<SyntaxToken>) : Pat<SyntaxToken> list =
    let acc = ResizeArray<Pat<SyntaxToken>>()

    let walker =
        { CstWalk.identityExprWalker with
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

/// A `Pat.Named` whose last segment is `Up` (the `Direction.Up` enum-case pattern).
let private isDirectionUp (ctx: PassContext) (p: Pat<SyntaxToken>) : bool =
    match p with
    | Pat.Named(longIdent = li) when li.Idents.Length >= 1 -> ctx.NameOf li.Idents.[li.Idents.Length - 1] = "Up"
    | _ -> false

[<Tests>]
let tests =
    testList
        "ExternalEnumCaseStamp"
        [
            // The stamp is the enum's nominal key, held under the anchor `Direction`.
            test "external enum-case access is stamped (expression)" {
                let ctx, file = analyse "let x = Direction.Up"
                let e = firstBindingExpr file

                Expect.isTrue
                    (ctx.Resolution.ExternalEnumCaseStamp.ContainsKey(CstKeys.ofExpr e))
                    "Direction.Up enum key stamped in expression position"
            }

            test "external enum-case pattern is stamped" {
                let ctx, file = analyse "let f (o: obj) = match o with | Direction.Up -> 1 | _ -> 0"
                let arms = matchArmPats file |> List.filter (isDirectionUp ctx)

                Expect.equal arms.Length 1 "exactly one Direction.Up match arm"

                for h in arms do
                    Expect.isTrue
                        (ctx.Resolution.ExternalEnumCaseStamp.ContainsKey(CstKeys.ofPat h))
                        "Direction.Up enum key stamped in pattern position"
            }

            // The enum resolves but does not declare `Sideways`, so nothing is stamped.
            test "unknown enum case is not stamped" {
                let ctx, file = analyse "let x = Direction.Sideways"
                let e = firstBindingExpr file

                Expect.isFalse
                    (ctx.Resolution.ExternalEnumCaseStamp.ContainsKey(CstKeys.ofExpr e))
                    "Direction.Sideways (no such case) is not stamped"
            }

            // Short-name type resolution is opens-sensitive: `Mode` does not resolve at all.
            test "enum case whose namespace is not opened is not stamped" {
                let ctx, file = analyse "let x = Mode.On"
                let e = firstBindingExpr file

                Expect.isFalse
                    (ctx.Resolution.ExternalEnumCaseStamp.ContainsKey(CstKeys.ofExpr e))
                    "Mode.On is not stamped with Other unopened"
            }

            test "enum case is stamped once its namespace is opened" {
                let ctx, file = analyse "open Other\nlet x = Mode.On"
                let e = firstBindingExpr file

                Expect.isTrue
                    (ctx.Resolution.ExternalEnumCaseStamp.ContainsKey(CstKeys.ofExpr e))
                    "Mode.On is stamped under open Other"
            }
        ]
