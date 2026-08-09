module XParsec.FSharp.Codegen.Clr.Tests.ResultTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Reflection over the built `Vesper.Result.dll` for the pure-data surface; driver
// programs for the combinators, whose `Vesper.Fun` argument a lambda builds naturally.

// `Ok` / `Error` each constrain one of the two parameters, so a standalone `Ok 5` is
// `Result<int, '_>`; every such value is annotated `: Result<int, string>` to pin both.
// `Result` has no instance members: results read back through a module call or `match`.

/// The built `Vesper.Result.dll` (cached). Every type below is reflected from *this*
/// assembly, so identities line up across `Invoke`s.
let private resultAsm: Lazy<Assembly> =
    lazy (fst (buildPackage "Vesper.Result").Value)

let private intTy = typeof<int>
let private strTy = typeof<string>

/// `Vesper.Result`2` closed over <int, string>, the object-argument type for the case
/// factories. The contract's `[<CompiledName("FSharpResult`2")>]` is not applied to the
/// emitted type name, so the metadata name stays `Result`2`.
let private resultIntStr: Lazy<Type> =
    lazy (resultAsm.Value.GetType("Vesper.Result`2").MakeGenericType(intTy, strTy))

/// `Ok (v: int) : Result<int, string>` via the emitted static `Ok` factory.
let private okIS (v: int) : obj =
    resultIntStr.Value.GetMethod("Ok").Invoke(null, [| box v |])

/// `Error (e: string) : Result<int, string>` via the emitted static `Error` factory.
let private errIS (e: string) : obj =
    resultIntStr.Value.GetMethod("Error").Invoke(null, [| box e |])

/// Invoke a `Vesper.ResultModule` static, instantiating it at the given type
/// argument(s) when it is generic.
let private callModule (name: string) (typeArgs: Type[]) (args: obj[]) : obj =
    let m = resultAsm.Value.GetType("Vesper.ResultModule").GetMethod(name)

    let m =
        if m.IsGenericMethodDefinition then
            m.MakeGenericMethod typeArgs
        else
            m

    m.Invoke(null, args)

let private asBool (o: obj) : bool = o :?> bool
let private asInt (o: obj) : int = o :?> int

let private isTy = [| intTy; strTy |]

[<Tests>]
let tests =
    testList
        "Result"
        [
            test "Ok / Error construct distinct values" {
                Expect.isNotNull (okIS 3) "Ok 3 constructs"
                Expect.isNotNull (errIS "boom") "Error \"boom\" constructs"
            }

            test "isOk: Ok -> true, Error -> false" {
                Expect.isTrue (asBool (callModule "isOk" isTy [| okIS 3 |])) "isOk (Ok 3)"
                Expect.isFalse (asBool (callModule "isOk" isTy [| errIS "e" |])) "isOk (Error e)"
            }

            test "isError: Error -> true, Ok -> false" {
                Expect.isTrue (asBool (callModule "isError" isTy [| errIS "e" |])) "isError (Error e)"
                Expect.isFalse (asBool (callModule "isError" isTy [| okIS 3 |])) "isError (Ok 3)"
            }

            test "count: Ok -> 1, Error -> 0" {
                Expect.equal (asInt (callModule "count" isTy [| okIS 3 |])) 1 "count (Ok 3)"
                Expect.equal (asInt (callModule "count" isTy [| errIS "e" |])) 0 "count (Error e)"
            }

            test "defaultValue: Error -> default, Ok -> value" {
                Expect.equal (asInt (callModule "defaultValue" isTy [| box 99; errIS "e" |])) 99 "default on Error"
                Expect.equal (asInt (callModule "defaultValue" isTy [| box 99; okIS 3 |])) 3 "value on Ok"
            }

            // `map`/`mapError`/`bind`/`fold`/… take a `Vesper.Fun` reflection cannot mint;
            // the driver programs below build one from a lambda.
            test "higher-order combinators covered by ResultModuleCallRuntime" { () }
        ]

// Construction + pattern matching of `Result`'s cases across the package boundary
// (`open Vesper`). Both cases carry a field and there are no instance members, so the
// constructed value is read back through a `match`.
[<Tests>]
let ctorAndMatchRuntime =
    testList
        "ResultCtorRuntime"
        [
            // Emit smoke: both case factories produce valid IL and the program runs.
            test "Ok and Error construct and run (Layer B emit smoke)" {
                runsResult
                    "ok"
                    ("open Vesper\n"
                     + "let a : Result<int, string> = Ok 5\n"
                     + "let b : Result<int, string> = Error \"boom\"\n"
                     + "printfn \"%s\" \"ok\"")
            }

            // `Ok` is declared first: tag 0, `int` field.
            test "match extracts Ok payload, defaults on Error" {
                runsResultLines
                    [ "7"; "0" ]
                    ("open Vesper\n"
                     + "let describe (r: Result<int, string>) =\n    match r with\n    | Ok x -> x\n    | Error _ -> 0\n"
                     + "printfn \"%d\" (describe (Ok 7))\n"
                     + "printfn \"%d\" (describe (Error \"boom\"))")
            }

            // The second case's `Error_0` extract: tag 1, `string` field.
            test "match binds the Error payload" {
                runsResultLines
                    [ "ok"; "boom" ]
                    ("open Vesper\n"
                     + "let msg (r: Result<int, string>) =\n    match r with\n    | Ok _ -> \"ok\"\n    | Error e -> e\n"
                     + "printfn \"%s\" (msg (Ok 1))\n"
                     + "printfn \"%s\" (msg (Error \"boom\"))")
            }
        ]

// External module-function calls over every `Result` combinator. Driver programs
// `open Vesper` and call the module functions directly; a combinator's result is
// observed through a second module call or a `match`.
[<Tests>]
let moduleCallRuntime =
    testList
        "ResultModuleCallRuntime"
        [
            // A pure-data module call, generic over <'T, 'TError>.
            test "Result.defaultValue: value on Ok, default on Error" {
                runsResultLines
                    [ "7"; "9" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Result.defaultValue 9 (Ok 7: Result<int, string>))\n"
                     + "printfn \"%d\" (Result.defaultValue 9 (Error \"e\": Result<int, string>))")
            }

            test "Result.isOk / isError / count through the module" {
                runsResultLines
                    [ "true"; "false"; "1"; "0" ]
                    ("open Vesper\n"
                     + "printfn \"%b\" (Result.isOk (Ok 5: Result<int, string>))\n"
                     + "printfn \"%b\" (Result.isOk (Error \"e\": Result<int, string>))\n"
                     + "printfn \"%d\" (Result.count (Ok 5: Result<int, string>))\n"
                     + "printfn \"%d\" (Result.count (Error \"e\": Result<int, string>))")
            }

            // `map : ('T -> 'U) -> Result<'T,'TError> -> Result<'U,'TError>`. Three method
            // typars; read back through `defaultValue` / `isError`.
            test "Result.map transforms Ok, preserves Error" {
                runsResultLines
                    [ "5"; "true" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Result.defaultValue 0 (Result.map (fun x -> x + 1) (Ok 4: Result<int, string>)))\n"
                     + "printfn \"%b\" (Result.isError (Result.map (fun x -> x + 1) (Error \"e\": Result<int, string>)))")
            }

            // `mapError : ('TError -> 'U) -> Result<'T,'TError> -> Result<'T,'U>`. Read back
            // through a `match` that extracts the mapped error, and through `isOk`.
            test "Result.mapError transforms Error, preserves Ok" {
                runsResultLines
                    [ "11"; "true" ]
                    ("open Vesper\n"
                     + "let errOr (r: Result<int, int>) =\n    match r with\n    | Ok _ -> -1\n    | Error e -> e\n"
                     + "printfn \"%d\" (errOr (Result.mapError (fun e -> e + 1) (Error 10: Result<int, int>)))\n"
                     + "printfn \"%b\" (Result.isOk (Result.mapError (fun e -> e + 1) (Ok 5: Result<int, int>)))")
            }

            // `bind : ('T -> Result<'U,'TError>) -> Result<'T,'TError> -> Result<'U,'TError>`.
            // `f` returns a `Result`, constructed cross-package inside its own body.
            test "Result.bind chains a Result-returning function" {
                runsResultLines
                    [ "11"; "0" ]
                    ("open Vesper\n"
                     + "let f x = if x > 0 then Ok (x + 1) else Error \"neg\"\n"
                     + "printfn \"%d\" (Result.defaultValue 0 (Result.bind f (Ok 10: Result<int, string>)))\n"
                     + "printfn \"%d\" (Result.defaultValue 0 (Result.bind f (Error \"e\": Result<int, string>)))")
            }

            // `fold : ('State -> 'T -> 'State) -> 'State -> Result<'T,'TError> -> 'State`.
            // The folder is curried: `translatePat` does not lower `fun s x -> …`.
            test "Result.fold accumulates over Ok, returns state on Error" {
                runsResultLines
                    [ "13"; "3" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Result.fold (fun s -> fun x -> s + x) 3 (Ok 10: Result<int, string>))\n"
                     + "printfn \"%d\" (Result.fold (fun s -> fun x -> s + x) 3 (Error \"e\": Result<int, string>))")
            }

            // `foldBack : ('T -> 'State -> 'State) -> Result<'T,'TError> -> 'State -> 'State`.
            // The result argument comes before the state.
            test "Result.foldBack accumulates over Ok, returns state on Error" {
                runsResultLines
                    [ "13"; "3" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Result.foldBack (fun x -> fun s -> s + x) (Ok 10: Result<int, string>) 3)\n"
                     + "printfn \"%d\" (Result.foldBack (fun x -> fun s -> s + x) (Error \"e\": Result<int, string>) 3)")
            }

            // `('T -> bool) -> Result<'T,'TError> -> bool`. On `Error`, `exists` is false
            // and `forall` is vacuously true.
            test "Result.exists / forall over Ok and Error" {
                runsResultLines
                    [ "true"; "false"; "false"; "true" ]
                    ("open Vesper\n"
                     + "printfn \"%b\" (Result.exists (fun x -> x > 5) (Ok 7: Result<int, string>))\n"
                     + "printfn \"%b\" (Result.exists (fun x -> x > 5) (Ok 1: Result<int, string>))\n"
                     + "printfn \"%b\" (Result.exists (fun x -> x > 5) (Error \"e\": Result<int, string>))\n"
                     + "printfn \"%b\" (Result.forall (fun x -> x > 5) (Error \"e\": Result<int, string>))")
            }

            // `defaultWith : ('TError -> 'T) -> Result<'T,'TError> -> 'T`. The recovery
            // function is applied to the error value.
            test "Result.defaultWith runs the recovery only on Error" {
                runsResultLines
                    [ "5"; "-1" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Result.defaultWith (fun e -> -1) (Ok 5: Result<int, string>))\n"
                     + "printfn \"%d\" (Result.defaultWith (fun e -> -1) (Error \"e\": Result<int, string>))")
            }

            // `iter : ('T -> unit) -> Result<'T,'TError> -> unit`. The action is observed by
            // its `printfn` side effect, so only the `Ok` line is expected.
            test "Result.iter runs the action only on Ok" {
                runsResult
                    "5"
                    ("open Vesper\n"
                     + "Result.iter (fun x -> printfn \"%d\" x) (Ok 5: Result<int, string>)\n"
                     + "Result.iter (fun x -> printfn \"%d\" x) (Error \"e\": Result<int, string>)")
            }
        ]

// Analysis only: the cross-package Result surface resolves through the provider's
// open scope.
[<Tests>]
let frontEndTests =
    testList
        "ResultFrontEnd"
        [
            test "Ok 5 types as Result<int, string> (annotated)" {
                typeChecksResult "let x : Result<int, string> = Ok 5"
            }

            test "Error \"boom\" types as Result<int, string> (annotated)" {
                typeChecksResult "let x : Result<int, string> = Error \"boom\""
            }

            test "Result.Ok / Result.Error (qualified) type-check" {
                typeChecksResult
                    "let x : Result<int, string> = Result.Ok 5\nlet y : Result<int, string> = Result.Error \"e\""
            }

            // Each bound variable picks up the scrutinee's instantiation.
            test "match Ok x binds x : int; Error e binds e : string" {
                typeChecksResult
                    "let f (r: Result<int, string>) : int =\n    match r with\n    | Ok x -> x\n    | Error _ -> 0"

                typeChecksResult
                    "let f (r: Result<int, string>) : string =\n    match r with\n    | Ok _ -> \"\"\n    | Error e -> e"
            }

            test "Result.isOk / defaultValue type-check" {
                typeChecksResult "let f (r: Result<int, string>) : bool = Result.isOk r"
                typeChecksResult "let f (r: Result<int, string>) : int = Result.defaultValue 0 r"
            }

            test "Result.map / mapError type-check (HOF, three typars)" {
                typeChecksResult "let f (r: Result<int, string>) : Result<int, string> = Result.map (fun x -> x + 1) r"
                typeChecksResult "let f (r: Result<int, string>) : Result<int, int> = Result.mapError (fun e -> 0) r"
            }

            test "Result.fold type-checks (curried folder)" {
                typeChecksResult "let f (r: Result<int, string>) : int = Result.fold (fun s -> fun x -> s + x) 0 r"
            }

            // A qualified reference to a non-existent module member is an unresolved-member
            // error, not a silently-accepted fresh TyVar.
            test "an unknown Result function is unresolved" {
                failsWithResult "Nope" "let f (r: Result<int, string>) : int = Result.Nope r"
            }
        ]
