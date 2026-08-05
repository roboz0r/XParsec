module XParsec.FSharp.SemanticAnalysis.Tests.IntrinsicKeyStampTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// The desugared-operator and synthesised-intrinsic `External` heads Elaborate mints
// (`a + b` → `op_Addition`, `arr.[i]` → `GetArray`, `arr.Length` → `GetArrayLength`,
// `arr.[i] <- v` → `SetArray`, …) MUST carry the resolved `SymbolKey` Unification
// stamped in `Resolution.IntrinsicKey`. That key is the SOLE channel by which
// `InlineExpansion` splices the cross-package `let inline` body (the by-name inline
// channel is gone): a `ValueNone` key here is a silent mis-splice / phantom `call`
// downstream, not a graceful miss — exactly the failure mode the boundary plan warns
// of. These are the front-end shape twins of the codegen end-to-end splice suites
// (`OpsPlatform*Tests`, array/dynamic tests): this harness resolves the `Vesper.Core`
// contract from its `.fsi` alone, so no body is served and the `External` head
// SURVIVES `InlineExpansion` unspliced — letting us assert the KEY is present on the
// head that a body-serving stack would splice by.

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value (Hashing.originSourceOfText lexed) file

/// The `key` field of the first `TExpr.External` named `name` anywhere in the
/// program's lowered decls (`None` if no such head survives). Asserts the program
/// froze without diagnostics first — a diagnostic means the intrinsic never resolved,
/// so there would be nothing to stamp.
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
            "`%s` head minted with a ValueNone key — the `IntrinsicKey` stamp is missing, so InlineExpansion cannot splice its inline body by key: %s"
            name
            input
    | None -> failtestf "no `%s` External head found in the lowered TAST of: %s" name input

[<Tests>]
let tests =
    testList
        "IntrinsicKeyStamp"
        [
            // Group 1 — desugared operators (`inferInfix` / `inferPrefix` stamp).
            test "infix `+` stamps op_Addition" { assertStamped "op_Addition" "let f (a: int) (b: int) : int = a + b" }

            test "prefix `-` stamps op_UnaryNegation" { assertStamped "op_UnaryNegation" "let f (a: int) : int = -a" }

            // Group 2 — synthesised element/length intrinsics (`inferIndexedLookup`,
            // `resolveFieldStep`, `inferAssignment` stamp).
            test "array index read `arr.[i]` stamps GetArray" {
                assertStamped "GetArray" "let f (a: int[]) (i: int) : int = a.[i]"
            }

            test "array length `arr.Length` (LongIdent chain) stamps GetArrayLength" {
                assertStamped "GetArrayLength" "let f (a: int[]) : int = a.Length"
            }

            test "array length on a non-ident receiver (DotLookup) stamps GetArrayLength" {
                assertStamped "GetArrayLength" "let f () : int = [| 1; 2; 3 |].Length"
            }

            test "array index write `arr.[i] <- v` stamps SetArray" {
                assertStamped "SetArray" "let f (a: int[]) (i: int) (v: int) : unit = a.[i] <- v"
            }
        ]
