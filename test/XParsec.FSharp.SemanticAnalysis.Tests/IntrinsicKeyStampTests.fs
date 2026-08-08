module XParsec.FSharp.SemanticAnalysis.Tests.IntrinsicKeyStampTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// A desugared `External` node (`a + b` → `op_Addition`, `arr.[i]` → `GetArray`) carries
// the key its cross-package inline body is spliced by. This harness resolves contracts
// from `.fsi` alone, so no body is served and the node survives unspliced, key readable.

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value (Hashing.originSourceOfText lexed) file

/// The `key` of the first `TExpr.External` named `name` in the lowered decls (`None` if
/// none survives). Diagnostics are checked empty first: an unresolved intrinsic stamps nothing.
let private externalKey (name: string) (input: string) : SymbolKey voption option =
    let tast = analyse input
    Expect.isEmpty tast.Diagnostics (sprintf "no diagnostics for: %s" input)

    let mutable found: SymbolKey voption option = None

    let mapper: TastWalk.Mapper =
        { TastWalk.identityMapper with
            OverrideExpr =
                fun _ e ->
                    match e with
                    | TExpr.External(n, k, _, _) when n = name && found.IsNone -> found <- Some k
                    | _ -> ()

                    ValueNone
        }

    for d in tast.Decls do
        match d with
        | TDecl.Let(_, value, _, _) -> TastWalk.mapExpr mapper value |> ignore
        | _ -> ()

    found

let private assertStamped (name: string) (input: string) =
    match externalKey name input with
    | Some(ValueSome _) -> ()
    | Some ValueNone ->
        failtestf
            "`%s` node minted with a ValueNone key — the `IntrinsicKey` stamp is missing, so InlineExpansion cannot splice its inline body by key: %s"
            name
            input
    | None -> failtestf "no `%s` External node found in the lowered TAST of: %s" name input

[<Tests>]
let tests =
    testList
        "IntrinsicKeyStamp"
        [
            test "infix `+` stamps op_Addition" { assertStamped "op_Addition" "let f (a: int) (b: int) : int = a + b" }

            test "prefix `-` stamps op_UnaryNegation" { assertStamped "op_UnaryNegation" "let f (a: int) : int = -a" }

            test "array index read `arr.[i]` stamps GetArray" {
                assertStamped "GetArray" "let f (a: int[]) (i: int) : int = a.[i]"
            }

            test "array length `arr.Length` (LongIdent chain) stamps GetArrayLength" {
                assertStamped "GetArrayLength" "let f (a: int[]) : int = a.Length"
            }

            test "array length on a non-ident object argument (DotLookup) stamps GetArrayLength" {
                assertStamped "GetArrayLength" "let f () : int = [| 1; 2; 3 |].Length"
            }

            test "array index write `arr.[i] <- v` stamps SetArray" {
                assertStamped "SetArray" "let f (a: int[]) (i: int) (v: int) : unit = a.[i] <- v"
            }
        ]
