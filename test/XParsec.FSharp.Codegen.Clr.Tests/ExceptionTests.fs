module XParsec.FSharp.Codegen.Clr.Tests.ExceptionTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Exception construction + `raise` / `failwith`. The chosen mechanism is *not* a
// dedicated `TExpr.Raise` TAST node — `raise` / `failwith` / `invalidArg` are real cross-package inline
// operators in `Vesper.Core/ops-platform.clr.fs` whose bodies splice to a
// `TExpr.ILIntrinsic "throw"` (the terminal `throw` arm in `Emit`). These tests
// pin the runtime behaviour: the thrown CLR exception's *type* and message.

let private lines xs = String.concat "\n" xs

/// Compile a source that defines a single top-level `let f … = raise …`,
/// reflect its emitted static method, invoke it, and return the CLR exception it
/// throws (unwrapped from `TargetInvocationException`). The function is given one
/// `int` parameter it ignores so it emits as a plain static method we can invoke
/// with a dummy argument.
let private thrownBy (assemblyName: string) (src: string) : exn =
    let tast, artifact = compileSource assemblyName src

    Expect.isEmpty tast.Diagnostics (sprintf "no diagnostics: %A" (tast.Diagnostics |> List.map (fun d -> d.Message)))

    let fn = programHolderMethods (Codegen.toBytes artifact) |> Array.exactlyOne

    try
        fn.Invoke(null, [| box 0 |]) |> ignore
        failwith "expected the emitted function to throw, but it returned"
    with :? TargetInvocationException as e when not (isNull e.InnerException) ->
        e.InnerException

[<Tests>]
let tests =
    testList
        "Exceptions"
        [
            test "failwith throws System.Exception with the given message" {
                let ex =
                    thrownBy "ExnFailwith" (lines [ "let boom (n: int) : int = failwith \"boom\"" ])

                Expect.equal (ex.GetType()) typeof<Exception> "failwith constructs a plain System.Exception"
                Expect.equal ex.Message "boom" "the message round-trips through the ctor"
            }

            test "raise of a constructed System.Exception throws it" {
                let ex =
                    thrownBy
                        "ExnRaiseBase"
                        (lines [ "let boom (n: int) : int = raise (new System.Exception(\"boom\"))" ])

                Expect.equal (ex.GetType()) typeof<Exception> "the raised exception is the one we constructed"
                Expect.equal ex.Message "boom" "message preserved"
            }

            test "raise of a derived exception (InvalidOperationException) throws the derived type" {
                // `raise : System.Exception -> 'T`, but the argument is an
                // `InvalidOperationException` (a subtype). This exercises argument
                // subsumption at the call site — the set.clr.fs `raise (InvalidOperationException …)`
                // enumeration-guard sites.
                let ex =
                    thrownBy
                        "ExnRaiseDerived"
                        (lines
                            [
                                "let boom (n: int) : int = raise (new System.InvalidOperationException(\"bad state\"))"
                            ])

                Expect.equal
                    (ex.GetType())
                    typeof<InvalidOperationException>
                    "the concrete derived exception type is preserved through `raise`"

                Expect.equal ex.Message "bad state" "message preserved"
            }

            test "raise of a non-exception is rejected by the :> exn constraint" {
                // `raise : 'e -> 'a when 'e :> exn` — passing an `int` must fail the
                // coercion constraint at type-check: the v1 compromise dropped this
                // bound; the constraint chain restores it via `subsumes` + the
                // prim-types-exn.clr.fs `exn ≡ System.Exception` identity). Compile only
                // (no run): we assert a diagnostic, not a throw.
                let tast, _ =
                    compileSource "ExnRaiseBadArg" (lines [ "let boom (n: int) : int = raise 42" ])

                let msgs = tast.Diagnostics |> List.map (fun d -> d.Message)

                Expect.isNonEmpty msgs "raising a non-exception must produce a diagnostic"

                Expect.exists
                    msgs
                    (fun m -> m.Contains "subtype of" || m.Contains "constraint")
                    (sprintf "expected a coercion-constraint diagnostic, got: %A" msgs)
            }

            test "invalidArg throws ArgumentException with ParamName and message" {
                let ex =
                    thrownBy
                        "ExnInvalidArg"
                        (lines [ "let boom (n: int) : int = invalidArg \"x\" \"must be positive\"" ])

                Expect.equal (ex.GetType()) typeof<ArgumentException> "invalidArg constructs a System.ArgumentException"

                let argEx = ex :?> ArgumentException
                Expect.equal argEx.ParamName "x" "the argument name is carried as ParamName"
                Expect.stringContains argEx.Message "must be positive" "the message is carried through"
            }

            // --- `exn`/`obj` as contract-sourced heritable roots ---
            //   * `inherit exn(msg)` downstream: the provider publishes `exn` as an
            //     `IntrinsicClass` (contract `inherit obj` + `new:` ctors); codegen chains the
            //     parameterized external base ctor (`System.Exception::.ctor(string)`), so the
            //     message must round-trip, never be dropped.
            //   * upcast to the roots: metadata surfaces `System.Exception`/`System.Object` as
            //     the canon `exn`/`obj` identities, and the annotation seam admits a concrete
            //     nominal subtype into a supertype annotation via the subtype walk.

            test "a user type inheriting exn raises as its own type, a subclass of System.Exception" {
                let ex =
                    thrownBy
                        "ExnInheritUser"
                        (lines
                            [
                                "type MyErr(m: string) ="
                                "    inherit exn(m)"
                                "let boom (n: int) : int = raise (MyErr \"boom\")"
                            ])

                Expect.equal (ex.GetType().Name) "MyErr" "the raised value keeps its own runtime type"
                Expect.isTrue (typeof<Exception>.IsAssignableFrom(ex.GetType())) "MyErr is a System.Exception subclass"
                Expect.equal ex.Message "boom" "the message chains through the exn(msg) base ctor, not dropped"
            }

            test "a BCL exception upcasts to exn and to obj without diagnostics" {
                let tast, _ =
                    compileSource
                        "ExnUpcastRoots"
                        (lines
                            [
                                "let toExn (e: System.InvalidOperationException) : exn = e"
                                "let toObj (e: System.InvalidOperationException) : obj = e"
                            ])

                let msgs = tast.Diagnostics |> List.map (fun d -> d.Message)

                Expect.isEmpty msgs (sprintf "upcasting a BCL exception to the exn / obj roots type-checks: %A" msgs)
            }
        ]
