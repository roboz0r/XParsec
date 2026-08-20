module XParsec.FSharp.SemanticAnalysis.Tests.IntrinsicKeyStampTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// A desugared `External` node (`a + b` → `op_Addition`) carries the key its cross-package
// inline body is spliced by. Under the served contract the splice happens during analysis,
// so the stamp's observable is the specialization entry minted under the operator's key.

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value (LexedFile.ofText lexed) file

let private assertStamped (name: string) (input: string) =
    let tast = analyse input
    Expect.isEmpty tast.Diagnostics (sprintf "no diagnostics for: %s" input)

    let specialized =
        tast.Specializations
        |> Seq.exists (fun s -> SymbolKeyOps.simpleName s.Key.Template = DisplayName name)

    if not specialized then
        failtestf
            "no `%s` specialization entry — the `IntrinsicKey` stamp is missing, so the served inline body cannot splice by key: %s"
            name
            input

[<Tests>]
let tests =
    testList
        "IntrinsicKeyStamp"
        [
            test "infix `+` stamps op_Addition" { assertStamped "op_Addition" "let f (a: int) (b: int) : int = a + b" }

            test "prefix `-` stamps op_UnaryNegation" { assertStamped "op_UnaryNegation" "let f (a: int) : int = -a" }
        ]
