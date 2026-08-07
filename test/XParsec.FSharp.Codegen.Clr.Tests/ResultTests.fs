module XParsec.FSharp.Codegen.Clr.Tests.ResultTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The behavioral runtime suite for `Vesper.Result`,
// the read-across from `Vesper.Option`: same shape (a BCL-only struct union + a
// `ModuleSuffix` module), so the same two routes apply.
//
//   * REFLECTION-INVOKE for the pure-data surface (`isOk` / `isError` / `count` /
//     `defaultValue` + construction). `buildPackage "Vesper.Result"` emits a real
//     `Vesper.Result.dll`; we construct `Ok`/`Error` through the union's emitted
//     static case factories and invoke the `Vesper.ResultModule` statics.
//   * DRIVER PROGRAMS for the higher-order combinators (`map`/`mapError`/`bind`/
//     `fold`/…), whose `Vesper.Fun` argument the lambda builds naturally, through
//     the now-general cross-package machinery (Gap 2 Layers B/C/D).
//
// Two differences from Option drive the test shape:
//   1. `Result<'T, 'TError>` has *two* type parameters, and `Ok` / `Error` each
//      constrain only one — the other stays free at a construction site (`Ok 5` is
//      `Result<int, '_>`; F# accepts that and generalizes, so this is *not* the
//      value restriction). Every standalone value is annotated `: Result<int,
//      string>` to pin both parameters, keeping each row's instantiation explicit
//      and aligned with the reflection helpers (closed over <int, string>). Inside
//      a `match`/module-call argument the annotated function parameter already pins
//      both, so those literals carry no annotation.
//   2. Result has **no instance members** (no `.Value`/`.IsOk`). A combinator's
//      `Result` *result* is therefore observed by feeding it back through a module
//      function (`Result.defaultValue` / `isOk`) or a `match` (Layer C), not a
//      property getter.

/// The built `Vesper.Result.dll` (cached). `buildPackage` loads it into its own
/// ALC and returns the loaded assembly; every type/value below is reflected from
/// *this* assembly so identities line up across `Invoke`s.
let private resultAsm: Lazy<Assembly> =
    lazy (fst (buildPackage "Vesper.Result").Value)

let private intTy = typeof<int>
let private strTy = typeof<string>

/// `Vesper.Result`2` closed over <int, string> — the receiver type for the case
/// factories. (The `[<CompiledName("FSharpResult`2")>]` on the contract is a
/// C#-interop alias the backend does not apply to the emitted type name, same as
/// `Vesper.Option`1` carries `FSharpOption`1`.)
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
            // ---- construction round-trips through the case factories ----------
            test "Ok / Error construct distinct values" {
                Expect.isNotNull (okIS 3) "Ok 3 constructs"
                Expect.isNotNull (errIS "boom") "Error \"boom\" constructs"
            }

            // ---- discriminators -----------------------------------------------
            test "isOk: Ok -> true, Error -> false" {
                Expect.isTrue (asBool (callModule "isOk" isTy [| okIS 3 |])) "isOk (Ok 3)"
                Expect.isFalse (asBool (callModule "isOk" isTy [| errIS "e" |])) "isOk (Error e)"
            }

            test "isError: Error -> true, Ok -> false" {
                Expect.isTrue (asBool (callModule "isError" isTy [| errIS "e" |])) "isError (Error e)"
                Expect.isFalse (asBool (callModule "isError" isTy [| okIS 3 |])) "isError (Ok 3)"
            }

            // ---- count --------------------------------------------------------
            test "count: Ok -> 1, Error -> 0" {
                Expect.equal (asInt (callModule "count" isTy [| okIS 3 |])) 1 "count (Ok 3)"
                Expect.equal (asInt (callModule "count" isTy [| errIS "e" |])) 0 "count (Error e)"
            }

            // ---- defaultValue -------------------------------------------------
            test "defaultValue: Error -> default, Ok -> value" {
                Expect.equal (asInt (callModule "defaultValue" isTy [| box 99; errIS "e" |])) 99 "default on Error"
                Expect.equal (asInt (callModule "defaultValue" isTy [| box 99; okIS 3 |])) 3 "value on Ok"
            }

            // ---- higher-order combinators -------------------------------------
            // `map`/`mapError`/`bind`/`fold`/… take a `Vesper.Fun` argument that
            // reflection can't readily mint; they are exercised through the driver-
            // program route in `ResultModuleCallRuntime` below.
            test "higher-order combinators covered by ResultModuleCallRuntime" { () }
        ]

// Construction (Layer B) + pattern matching (Layer C) of `Result`'s cases across
// the package boundary (`open Vesper`). Result has no nullary case (both `Ok` and
// `Error` carry a field) and no instance members, so the constructed value is read
// back through a `match` — which also drives the `Error` arm's field extract (tag
// 1, the second declaration-order case).
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

            // `match` extracts the `Ok` payload (tag 0, int field) and defaults on
            // `Error`; the case-pattern type is driven by the annotated parameter.
            test "match extracts Ok payload, defaults on Error" {
                runsResultLines
                    [ "7"; "0" ]
                    ("open Vesper\n"
                     + "let describe (r: Result<int, string>) =\n    match r with\n    | Ok x -> x\n    | Error _ -> 0\n"
                     + "printfn \"%d\" (describe (Ok 7))\n"
                     + "printfn \"%d\" (describe (Error \"boom\"))")
            }

            // `match` binding the *Error* field (tag 1, string field) — the second
            // case's `<Error>_0` extract, distinct from the `Ok` arm above.
            test "match binds the Error payload" {
                runsResultLines
                    [ "ok"; "boom" ]
                    ("open Vesper\n"
                     + "let msg (r: Result<int, string>) =\n    match r with\n    | Ok _ -> \"ok\"\n    | Error e -> e\n"
                     + "printfn \"%s\" (msg (Ok 1))\n"
                     + "printfn \"%s\" (msg (Error \"boom\"))")
            }
        ]

// The general external module-function call (Gap 2 Layer D) over every `Result`
// combinator. Driver programs `open Vesper` and call the module functions
// directly; combinator results are observed through a second module call or a
// `match` (Result has no instance members).
[<Tests>]
let moduleCallRuntime =
    testList
        "ResultModuleCallRuntime"
        [
            // Pure-data module calls, generic over <'T, 'TError>.
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

            // `map : ('T -> 'U) -> Result<'T,'TError> -> Result<'U,'TError>` — three
            // method typars (`'T`, `'U`, `'TError`); the `Ok` is transformed, the
            // `Error` is preserved. Result read back through `defaultValue` / `isError`.
            test "Result.map transforms Ok, preserves Error" {
                runsResultLines
                    [ "5"; "true" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Result.defaultValue 0 (Result.map (fun x -> x + 1) (Ok 4: Result<int, string>)))\n"
                     + "printfn \"%b\" (Result.isError (Result.map (fun x -> x + 1) (Error \"e\": Result<int, string>)))")
            }

            // `mapError : ('TError -> 'U) -> Result<'T,'TError> -> Result<'T,'U>` —
            // the mirror of `map`; the `Error` is transformed, the `Ok` preserved.
            // Observed through a `match` (extracting the mapped error) and `isOk`.
            test "Result.mapError transforms Error, preserves Ok" {
                runsResultLines
                    [ "11"; "true" ]
                    ("open Vesper\n"
                     + "let errOr (r: Result<int, int>) =\n    match r with\n    | Ok _ -> -1\n    | Error e -> e\n"
                     + "printfn \"%d\" (errOr (Result.mapError (fun e -> e + 1) (Error 10: Result<int, int>)))\n"
                     + "printfn \"%b\" (Result.isOk (Result.mapError (fun e -> e + 1) (Ok 5: Result<int, int>)))")
            }

            // `bind : ('T -> Result<'U,'TError>) -> Result<'T,'TError> -> Result<'U,'TError>`
            // — the binder itself returns a `Result`, constructed cross-package
            // inside the lambda (both `Ok` and `Error` arms).
            test "Result.bind chains a Result-returning function" {
                runsResultLines
                    [ "11"; "0" ]
                    ("open Vesper\n"
                     + "let f x = if x > 0 then Ok (x + 1) else Error \"neg\"\n"
                     + "printfn \"%d\" (Result.defaultValue 0 (Result.bind f (Ok 10: Result<int, string>)))\n"
                     + "printfn \"%d\" (Result.defaultValue 0 (Result.bind f (Error \"e\": Result<int, string>)))")
            }

            // `fold : ('State -> 'T -> 'State) -> 'State -> Result<'T,'TError> -> 'State`
            // — two state-bearing typars. The folder is written curried
            // (`fun s -> fun x -> …`): a multi-arg lambda (`fun s x -> …`) hits the
            // pre-existing Elaborate `Pat.Named` gap, orthogonal to this layer.
            test "Result.fold accumulates over Ok, returns state on Error" {
                runsResultLines
                    [ "13"; "3" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Result.fold (fun s -> fun x -> s + x) 3 (Ok 10: Result<int, string>))\n"
                     + "printfn \"%d\" (Result.fold (fun s -> fun x -> s + x) 3 (Error \"e\": Result<int, string>))")
            }

            // `foldBack : ('T -> 'State -> 'State) -> Result<'T,'TError> -> 'State -> 'State`
            // — the argument order flips (result before state).
            test "Result.foldBack accumulates over Ok, returns state on Error" {
                runsResultLines
                    [ "13"; "3" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Result.foldBack (fun x -> fun s -> s + x) (Ok 10: Result<int, string>) 3)\n"
                     + "printfn \"%d\" (Result.foldBack (fun x -> fun s -> s + x) (Error \"e\": Result<int, string>) 3)")
            }

            // `exists` / `forall` — `('T -> bool) -> Result<'T,'TError> -> bool`. On
            // `Error`, `exists` is false and `forall` is vacuously true.
            test "Result.exists / forall over Ok and Error" {
                runsResultLines
                    [ "true"; "false"; "false"; "true" ]
                    ("open Vesper\n"
                     + "printfn \"%b\" (Result.exists (fun x -> x > 5) (Ok 7: Result<int, string>))\n"
                     + "printfn \"%b\" (Result.exists (fun x -> x > 5) (Ok 1: Result<int, string>))\n"
                     + "printfn \"%b\" (Result.exists (fun x -> x > 5) (Error \"e\": Result<int, string>))\n"
                     + "printfn \"%b\" (Result.forall (fun x -> x > 5) (Error \"e\": Result<int, string>))")
            }

            // `defaultWith : ('TError -> 'T) -> Result<'T,'TError> -> 'T` — the
            // recovery function runs only on `Error`, applied to the error value.
            test "Result.defaultWith runs the recovery only on Error" {
                runsResultLines
                    [ "5"; "-1" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Result.defaultWith (fun e -> -1) (Ok 5: Result<int, string>))\n"
                     + "printfn \"%d\" (Result.defaultWith (fun e -> -1) (Error \"e\": Result<int, string>))")
            }

            // `iter : ('T -> unit) -> Result<'T,'TError> -> unit` — a `unit`-domain
            // action that runs on `Ok` and is skipped on `Error`; observed by its
            // `printfn` side effect (only the `Ok` line is emitted).
            test "Result.iter runs the action only on Ok" {
                runsResult
                    "5"
                    ("open Vesper\n"
                     + "Result.iter (fun x -> printfn \"%d\" x) (Ok 5: Result<int, string>)\n"
                     + "Result.iter (fun x -> printfn \"%d\" x) (Error \"e\": Result<int, string>)")
            }
        ]

// Front-end regression guard (analysis only): the cross-package Result surface
// type-checks through the contract provider's ambient open scope.
[<Tests>]
let frontEndTests =
    testList
        "ResultFrontEnd"
        [
            // Construction resolves through the reverse case index (open `Vesper`).
            test "Ok 5 types as Result<int, string> (annotated)" {
                typeChecksResult "let x : Result<int, string> = Ok 5"
            }

            test "Error \"boom\" types as Result<int, string> (annotated)" {
                typeChecksResult "let x : Result<int, string> = Error \"boom\""
            }

            // Qualified case forms `Result.Ok` / `Result.Error`.
            test "Result.Ok / Result.Error (qualified) type-check" {
                typeChecksResult
                    "let x : Result<int, string> = Result.Ok 5\nlet y : Result<int, string> = Result.Error \"e\""
            }

            // `match` binds each case's field at the receiver's instantiation.
            test "match Ok x binds x : int; Error e binds e : string" {
                typeChecksResult
                    "let f (r: Result<int, string>) : int =\n    match r with\n    | Ok x -> x\n    | Error _ -> 0"

                typeChecksResult
                    "let f (r: Result<int, string>) : string =\n    match r with\n    | Ok _ -> \"\"\n    | Error e -> e"
            }

            // Pure-data + higher-order module calls.
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

            // Negative: a qualified reference to a non-existent module member is an
            // unresolved-member error, not a silently-accepted fresh TyVar
            // (vesper-result-handoff.md). The bare-ident path already errors
            // cleanly; the qualified external-module path now matches.
            test "an unknown Result function is unresolved" {
                failsWithResult "Nope" "let f (r: Result<int, string>) : int = Result.Nope r"
            }
        ]
