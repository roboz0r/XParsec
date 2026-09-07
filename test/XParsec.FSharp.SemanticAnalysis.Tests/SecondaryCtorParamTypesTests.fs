module XParsec.FSharp.SemanticAnalysis.Tests.SecondaryCtorParamTypesTests

open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSemFor testCompiling realProvider.Value (LexedFile.ofText lexed) file

let private diagnostics (tast: TastFile) =
    tast.Diagnostics |> Seq.map (fun d -> d.Message) |> List.ofSeq

/// The declared types of a class's single secondary ctor, in parameter order.
let private secondaryCtorParamTypes (tast: TastFile) : SemType list =
    match diagnostics tast with
    | [] -> ()
    | diags -> failtestf "expected clean analysis, got diagnostics: %A" diags

    let typeDecl =
        tast.Decls
        |> Block.toList
        |> List.tryPick (
            function
            | TDecl.Type t -> Some t
            | _ -> None
        )
        |> Option.defaultWith (fun () -> failwithf "expected a TDecl.Type, got %A" tast.Decls)

    match typeDecl.Kind with
    | TTypeKind.Class c ->
        match Block.toList c.SecondaryCtors with
        | [ sc ] -> sc.Params |> Block.toList |> List.map snd
        | other -> failwithf "expected exactly one secondary ctor, got %d" other.Length
    | other -> failtestf "expected TTypeKind.Class, got %A" other

let private isCtorArgShapeError (tast: TastFile) =
    tast.Diagnostics
    |> Seq.exists (fun d ->
        match d.Kind with
        | Kind.NotYetSupported feature -> feature.Contains "constructor argument pattern"
        | _ -> false
    )

// Primary and `new(...)` ctor params share one walker: it adds one entry per param, in
// order, and handles only a bare identifier with an optional annotation (`x`, `(x: T)`).
// Richer shapes are reported as not-yet-supported rather than silently dropped.
[<Tests>]
let tests =
    testList
        "SecondaryCtorParamTypes"
        [
            test "annotated secondary-ctor params link to their own param, in order" {
                let tast =
                    analyse "type C(x: int) =\n    new(a: int, b: string) = C(a)\n    member this.X = x"

                Expect.equal
                    (secondaryCtorParamTypes tast)
                    [ BuiltinTypes.tyInt; BuiltinTypes.tyString ]
                    "a : int, b : string"
            }

            test "attributed param is rejected on a primary ctor" {
                let tast = analyse "type C([<System.Obsolete>] x: int) =\n    member this.X = x"
                Expect.isTrue (isCtorArgShapeError tast) "attributed primary-ctor arg is rejected"
            }

            test "attributed param is rejected on a secondary ctor" {
                let tast =
                    analyse "type C(x: int) =\n    new([<System.Obsolete>] a: int) = C(a)\n    member this.X = x"

                Expect.isTrue (isCtorArgShapeError tast) "attributed secondary-ctor arg is rejected"
            }

            test "parenthesised annotated param is rejected on a secondary ctor" {
                let tast =
                    analyse "type C(x: int) =\n    new((a): int, b: string) = C(a)\n    member this.X = x"

                Expect.isTrue (isCtorArgShapeError tast) "non-simple secondary-ctor arg pattern is rejected"
            }

            // The TAST keeps only the constructor chain; a statement it would drop is
            // diagnosed as unsupported rather than silently lost.

            // `do printfn "hi"` (explicit `do`) parses as `Expr.ControlFlow`, which
            // `CstKeys.firstTokenOfExpr` cannot token-ize yet, so the statement form here
            // is the bare one fsi equally accepts.
            test "a statement before the ctor chain is diagnosed, not dropped" {
                let tast =
                    analyse
                        "type C(x: int) =\n    member this.X = x\n    new(a: int) =\n        printfn \"hi\"\n        C(a)"

                Expect.isTrue
                    (tast.Diagnostics
                     |> Seq.exists (fun d ->
                         match d.Kind with
                         | Kind.NotYetSupported feature ->
                             feature.Contains "secondary constructor of 'C'"
                             && feature.Contains "before the constructor chain"
                         | _ -> false
                     ))
                    (sprintf "expected a dropped-statement diagnostic naming C, got %A" (diagnostics tast))
            }

            test "a `then` statement after the ctor chain is diagnosed, not dropped" {
                let tast =
                    analyse
                        "type C(x: int) =\n    member this.X = x\n    new(a: int) =\n        C(a)\n        then printfn \"made\""

                Expect.isTrue
                    (tast.Diagnostics
                     |> Seq.exists (fun d ->
                         match d.Kind with
                         | Kind.NotYetSupported feature ->
                             feature.Contains "secondary constructor of 'C'"
                             && feature.Contains "after the constructor chain"
                         | _ -> false
                     ))
                    (sprintf "expected a dropped-then diagnostic naming C, got %A" (diagnostics tast))
            }

            test "a conditional ctor chain is diagnosed (condition and else branch dropped)" {
                let tast =
                    analyse
                        "type C(x: int) =\n    member this.X = x\n    new(a: int, b: int) =\n        if a > b then C(a) else C(b)"

                Expect.isTrue
                    (tast.Diagnostics
                     |> Seq.exists (fun d ->
                         match d.Kind with
                         | Kind.NotYetSupported feature ->
                             feature.Contains "secondary constructor of 'C'"
                             && feature.Contains "conditional"
                         | _ -> false
                     ))
                    (sprintf "expected a conditional-chain diagnostic naming C, got %A" (diagnostics tast))
            }
        ]
