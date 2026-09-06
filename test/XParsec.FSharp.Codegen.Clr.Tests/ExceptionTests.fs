module XParsec.FSharp.Codegen.Clr.Tests.ExceptionTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection
open XParsec.FSharp.Codegen.Clr.Tests.ReflectionHarness

// `raise` / `failwith` / `invalidArg` are cross-package inline operators declared in
// `src/Vesper.Core/ops-platform.fsi`, not TAST nodes: `raise e` splices to
// `(# "throw" e #)`, and the other two are `raise (new …)` over it.

/// The CLR exception thrown by the top-level function `fnName` of `src`, compiled
/// under `assemblyName` and invoked with `args`, unwrapped from
/// `TargetInvocationException`.
let private thrownBy (assemblyName: string) (fnName: string) (args: obj[]) (src: string) : exn =
    let fn = programFunction fnName (Codegen.toBytes (compileSource assemblyName src))

    try
        fn.Invoke(null, args) |> ignore
        failtestf "expected `%s` to throw, but it returned" fnName
    with :? TargetInvocationException as e when not (isNull e.InnerException) ->
        e.InnerException

/// `thrownBy` for the fixtures below, each of which declares `let boom (n: int) : int`
/// and ignores `n`.
let private boomThrows (assemblyName: string) (src: string list) : exn =
    thrownBy assemblyName "boom" [| box 0 |] (lines src)

[<Tests>]
let tests =
    testList
        "Exceptions"
        [
            test "failwith throws System.Exception with the given message" {
                let ex = boomThrows "ExnFailwith" [ "let boom (n: int) : int = failwith \"boom\"" ]

                Expect.equal (ex.GetType()) typeof<Exception> "failwith constructs a plain System.Exception"
                Expect.equal ex.Message "boom" "the message round-trips through the ctor"
            }

            test "raise of a constructed System.Exception throws it" {
                let ex =
                    boomThrows "ExnRaiseBase" [ "let boom (n: int) : int = raise (new System.Exception(\"boom\"))" ]

                Expect.equal (ex.GetType()) typeof<Exception> "the raised exception is the one we constructed"
                Expect.equal ex.Message "boom" "message preserved"
            }

            test "raise of a derived exception (InvalidOperationException) throws the derived type" {
                // `raise` admits a subtype of `exn`, so the concrete
                // `InvalidOperationException` must survive to the throw rather than
                // widening to `System.Exception`.
                let ex =
                    boomThrows
                        "ExnRaiseDerived"
                        [
                            "let boom (n: int) : int = raise (new System.InvalidOperationException(\"bad state\"))"
                        ]

                Expect.equal
                    (ex.GetType())
                    typeof<InvalidOperationException>
                    "the concrete derived exception type is preserved through `raise`"

                Expect.equal ex.Message "bad state" "message preserved"
            }

            test "raise of a non-exception is rejected by the :> exn constraint" {
                // An `int` argument fails `raise`'s coercion constraint. Front end only:
                // the assertion is a diagnostic, not a throw.
                let msgs =
                    diagnoseSource "ExnRaiseBadArg" (lines [ "let boom (n: int) : int = raise 42" ])
                    |> diagnosticMessages

                Expect.isNonEmpty msgs "raising a non-exception must produce a diagnostic"

                Expect.exists
                    msgs
                    (fun m -> m.Contains "subtype of" || m.Contains "constraint")
                    (sprintf "expected a coercion-constraint diagnostic, got: %A" msgs)
            }

            test "invalidArg throws ArgumentException with ParamName and message" {
                let ex =
                    boomThrows "ExnInvalidArg" [ "let boom (n: int) : int = invalidArg \"x\" \"must be positive\"" ]

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
                    boomThrows
                        "ExnInheritUser"
                        [
                            "type MyErr(m: string) ="
                            "    inherit exn(m)"
                            "let boom (n: int) : int = raise (MyErr \"boom\")"
                        ]

                Expect.equal (ex.GetType().Name) "MyErr" "the raised value keeps its own runtime type"
                Expect.isTrue (typeof<Exception>.IsAssignableFrom(ex.GetType())) "MyErr is a System.Exception subclass"
                Expect.equal ex.Message "boom" "the message chains through the exn(msg) base ctor, not dropped"
            }

            test "a BCL exception upcasts to exn and to obj without diagnostics" {
                // The clean compile IS the assertion: both upcasts type-check.
                compileSource
                    "ExnUpcastRoots"
                    (lines
                        [
                            "let toExn (e: System.InvalidOperationException) : exn = e"
                            "let toObj (e: System.InvalidOperationException) : obj = e"
                        ])
                |> ignore
            }
        ]
