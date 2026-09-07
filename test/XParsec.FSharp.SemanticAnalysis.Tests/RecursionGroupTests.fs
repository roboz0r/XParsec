module XParsec.FSharp.SemanticAnalysis.Tests.RecursionGroupTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// `V260` reports a `let rec` shape that overstates its recursion: a `let rec … and …` group
// covering more than one strongly connected component, and a `rec` binding whose value
// references only outer names. F# publishes no equivalent code, so both the code and the
// wording are this compiler's own.

/// The `V260` diagnostics `src` reports. Analysis must run to completion with no error, so an
/// empty result means the shape was accepted rather than the program refused.
let private v260 (src: string) : Diagnostic list =
    let tast = analyseSem src
    expectCleanTast tast
    tast.Diagnostics |> List.filter (fun d -> d.Code = DiagCode.Vesper "V260")

let private theOne (src: string) : Diagnostic =
    match v260 src with
    | [ d ] -> d
    | ds -> failtestf "expected one V260 diagnostic, got %A" (ds |> List.map (fun d -> d.Message))

[<Tests>]
let tests =
    testList
        "Recursion groups"
        [
            test "a group covering several components warns and names the member to declare outside it" {
                // `a` and `b` are a cycle. `c` references `a`, and the cycle references `c`
                // nowhere, so `c` is a component of its own and belongs after the group as a
                // plain `let`.
                let d =
                    theOne (
                        "let rec a n = if n = 0 then 0 else b (n - 1)\n"
                        + "and b n = a n\n"
                        + "and c n = a n\n"
                    )

                Expect.equal d.Severity Severity.Warning "the program still compiles"
                Expect.stringContains d.Message "'c'" "names the member that can leave the group"
            }

            test "a `let rec` binding whose value references only outer names warns" {
                let d = theOne "let helper x = x + 1\nlet rec unused x = helper x\n"

                Expect.equal d.Severity Severity.Warning "the program still compiles"
                Expect.stringContains d.Message "'unused'" "names the binding the `rec` keyword adds nothing to"
            }

            test "a genuine cycle is silent" {
                let src =
                    "let rec isEven n = if n = 0 then true else isOdd (n - 1)\n"
                    + "and isOdd n = if n = 0 then false else isEven (n - 1)\n"

                Expect.isEmpty (v260 src) "one component covers both members"
            }

            test "a self-referencing `let rec` singleton is silent" {
                Expect.isEmpty
                    (v260 "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)\n")
                    "a self-edge makes the singleton a recursive component"
            }

            test "a cycle through a type-annotated value member is silent" {
                // `f : int -> int` binds through a return annotation, so its binding site is
                // the same key a reference to it resolves to. `dotnet fsi` accepts this program.
                let src = "let rec f : int -> int = fun x -> g x\nand g x = f x\n"
                Expect.isEmpty (v260 src) "one component covers both members"
            }

            test "a cycle through a type-annotated value member stays monomorphic" {
                // Both members share one component, so `g` is typed at `int -> int` and
                // `dotnet fsi` rejects `g "str"` with FS0001.
                let errors =
                    errorMessages
                        (analyseSem "let rec f : int -> int = fun x -> g x\nand g x = f x\nlet r = g \"str\"\n")
                            .Diagnostics

                Expect.isNonEmpty errors "the string argument is a type error"
            }

            test "a group written without `rec` is silent" {
                // Siblings are out of each other's scope without `rec`, so the group has no
                // edges and no `rec` keyword to report on.
                Expect.isEmpty (v260 "let a = 1\nlet b = a\n") "no rec keyword"
            }

            test "`inline` on a group member is an error under F#'s code" {
                // `dotnet fsi` reports FS1114 on each inline member of a `let rec … and …`
                // group. A lone `let rec inline` is accepted here, its expansion bounded by the
                // specialization table, so the report is the group's alone.
                let tast = analyseSem "let rec inline f x = g x\nand g x = f x\n"

                Expect.equal
                    [ for d in tast.Diagnostics -> d.Code, d.Severity ]
                    [ DiagCode.FSharp 1114, Severity.Error ]
                    "one FS1114 on the inline member and no V260"

                Expect.stringContains (List.head tast.Diagnostics).Message "'f'" "names the inline member"
            }
        ]
