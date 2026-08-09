module XParsec.FSharp.Codegen.Clr.Tests.ExceptionTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `raise` / `failwith` / `invalidArg` are cross-package inline operators in
// `Vesper.Core/ops-platform.clr.fs`, not TAST nodes: `raise e` splices to
// `(# "throw" e #)`, and the other two are `raise (new …)` over it.

let private lines xs = String.concat "\n" xs

/// Returns the CLR exception `src`'s sole emitted method throws, unwrapped from
/// `TargetInvocationException`. Every `src` here writes `let boom (n: int) : int = …`:
/// the ignored `int` parameter is what makes it a plain static method to invoke.
let private thrownBy (assemblyName: string) (src: string) : exn =
    let tast, artifact = compileSource assemblyName src

    Expect.isEmpty tast.Diagnostics (sprintf "no diagnostics: %A" (tast.Diagnostics |> List.map (fun d -> d.Message)))

    let fn = programClassMethods (Codegen.toBytes artifact) |> Array.exactlyOne

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
                // The `'TException :> exn` bound admits a subtype, so the concrete
                // `InvalidOperationException` must survive to the throw rather than
                // widening to `System.Exception`.
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
                // An `int` argument must fail the coercion constraint on
                // `raise: exn: 'TException -> 'T when 'TException :> exn`. Compile only:
                // the assertion is a diagnostic, not a throw.
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

            // `exn` and `obj` are contract-declared types a user program can inherit
            // from and upcast to; both must resolve to `System.Exception` /
            // `System.Object` in metadata for the two tests below to hold.

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
