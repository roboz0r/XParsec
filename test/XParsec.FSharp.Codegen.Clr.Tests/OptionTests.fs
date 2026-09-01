module XParsec.FSharp.Codegen.Clr.Tests.OptionTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PackageHarness

// Reflection over the built `Vesper.Option.dll`: values come from the union's emitted
// static case factories, and results are asserted as BCL types. Combinators taking a
// `Vesper.Fun` cannot be minted by reflection, so they run as driver programs below.

/// The built `Vesper.Option.dll` (cached). Every type below is reflected from *this*
/// assembly, so identities line up across `Invoke`s.
let private optionAsm: Lazy<Assembly> =
    lazy (fst (buildPackage "Vesper.Option").Value)

let private intTy = typeof<int>

/// `Vesper.Option`1` closed over `int`, the object-argument type for the case factories
/// and instance members.
let private optionOfInt: Lazy<Type> =
    lazy (optionAsm.Value.GetType("Vesper.Option`1").MakeGenericType(intTy))

/// `Some (v: int)` via the emitted static `Some` factory.
let private someInt (v: int) : obj =
    optionOfInt.Value.GetMethod("Some").Invoke(null, [| box v |])

/// `None : int option` via the emitted static `None` factory.
let private noneInt: Lazy<obj> =
    lazy (optionOfInt.Value.GetMethod("None").Invoke(null, [||]))

/// Invoke a `Vesper.OptionModule` static, instantiating it at the given element
/// type(s) when it is generic.
let private callModule (name: string) (typeArgs: Type[]) (args: obj[]) : obj =
    let m = optionAsm.Value.GetType("Vesper.OptionModule").GetMethod(name)

    let m =
        if m.IsGenericMethodDefinition then
            m.MakeGenericMethod typeArgs
        else
            m

    m.Invoke(null, args)

/// Read an instance member (`get_Value` / `get_IsSome` / `get_IsNone`) off an option.
let private instanceGet (name: string) (objArg: obj) : obj =
    optionOfInt.Value.GetMethod(name).Invoke(objArg, [||])

let private asBool (o: obj) : bool = o :?> bool
let private asInt (o: obj) : int = o :?> int

[<Tests>]
let tests =
    testList
        "Option"
        [
            test "Some / None construct distinct values" {
                Expect.isNotNull (someInt 3) "Some 3 constructs"
                Expect.isNotNull noneInt.Value "None constructs"
            }

            test "isSome: Some -> true, None -> false" {
                Expect.isTrue (asBool (callModule "isSome" [| intTy |] [| someInt 3 |])) "isSome (Some 3)"
                Expect.isFalse (asBool (callModule "isSome" [| intTy |] [| noneInt.Value |])) "isSome None"
            }

            test "isNone: None -> true, Some -> false" {
                Expect.isTrue (asBool (callModule "isNone" [| intTy |] [| noneInt.Value |])) "isNone None"
                Expect.isFalse (asBool (callModule "isNone" [| intTy |] [| someInt 3 |])) "isNone (Some 3)"
            }

            test "count: Some -> 1, None -> 0" {
                Expect.equal (asInt (callModule "count" [| intTy |] [| someInt 3 |])) 1 "count (Some 3)"
                Expect.equal (asInt (callModule "count" [| intTy |] [| noneInt.Value |])) 0 "count None"
            }

            test "get (Some 7) returns 7" {
                Expect.equal (asInt (callModule "get" [| intTy |] [| someInt 7 |])) 7 "get (Some 7)"
            }

            test "get None raises InvalidOperationException" {
                // `MethodBase.Invoke` wraps the user exception in a
                // `TargetInvocationException`; unwrap and inspect the inner.
                let inner =
                    try
                        callModule "get" [| intTy |] [| noneInt.Value |] |> ignore
                        None
                    with :? TargetInvocationException as e ->
                        Some e.InnerException

                match inner with
                | Some(:? InvalidOperationException) -> ()
                | other -> failtestf "expected an inner InvalidOperationException, got %A" other
            }

            test "defaultValue: None -> default, Some -> value" {
                Expect.equal
                    (asInt (callModule "defaultValue" [| intTy |] [| box 99; noneInt.Value |]))
                    99
                    "default on None"

                Expect.equal (asInt (callModule "defaultValue" [| intTy |] [| box 99; someInt 3 |])) 3 "value on Some"
            }

            test "flatten: Some (Some 5) -> Some 5, None -> None" {
                let optionOfOption =
                    optionAsm.Value.GetType("Vesper.Option`1").MakeGenericType(optionOfInt.Value)

                let someSome = optionOfOption.GetMethod("Some").Invoke(null, [| someInt 5 |])
                let flattened = callModule "flatten" [| intTy |] [| someSome |]
                Expect.equal (asInt (callModule "get" [| intTy |] [| flattened |])) 5 "flatten (Some (Some 5))"

                let outerNone = optionOfOption.GetMethod("None").Invoke(null, [||])
                let flatNone = callModule "flatten" [| intTy |] [| outerNone |]
                Expect.isTrue (asBool (callModule "isNone" [| intTy |] [| flatNone |])) "flatten None is None"
            }

            test "orElse: None -> ifNone, Some -> self" {
                let fallback = someInt 42
                let onNone = callModule "orElse" [| intTy |] [| fallback; noneInt.Value |]
                Expect.equal (asInt (callModule "get" [| intTy |] [| onNone |])) 42 "orElse picks ifNone on None"

                let onSome = callModule "orElse" [| intTy |] [| fallback; someInt 3 |]
                Expect.equal (asInt (callModule "get" [| intTy |] [| onSome |])) 3 "orElse keeps the Some"
            }

            test "instance IsSome / IsNone / Value" {
                Expect.isTrue (asBool (instanceGet "get_IsSome" (someInt 3))) "(Some 3).IsSome"
                Expect.isFalse (asBool (instanceGet "get_IsNone" (someInt 3))) "(Some 3).IsNone"
                Expect.isTrue (asBool (instanceGet "get_IsNone" noneInt.Value)) "None.IsNone"
                Expect.equal (asInt (instanceGet "get_Value" (someInt 8))) 8 "(Some 8).Value"
            }

            // `map`/`bind`/`fold`/… take a `Vesper.Fun`, which reflection cannot mint:
            // closures are synthesised per call site, not exposed as a constructible
            // delegate. The driver programs below build one naturally from a lambda.
            test "higher-order combinators covered by OptionModuleCallRuntime" { () }
        ]

// Analysis only, no codegen. Regression guarded: the `'T option` abbreviation dealiased
// to a mis-kinded `TyRecord("Vesper.Option", …)`, so `o.IsSome` routed to the record
// registry and failed with `Unknown record type 'Vesper.Option'`.
[<Tests>]
let frontEndTests =
    testList
        "OptionFrontEnd"
        [
            test "int option dealiases to the union; o.IsSome : bool" {
                typeChecksOption "let f (o: int option) : bool = o.IsSome"
            }

            test "int option; o.IsNone : bool" { typeChecksOption "let f (o: int option) : bool = o.IsNone" }

            test "int option; o.Value substitutes the type arg (: int)" {
                typeChecksOption "let f (o: int option) : int = o.Value"
            }

            test "Vesper.Option<int> direct ref; o.IsSome : bool" {
                typeChecksOption "let f (o: Vesper.Option<int>) : bool = o.IsSome"
            }

            // An absent member errors as a member miss, not `Unknown record type`.
            test "absent member on int option is a clean instance-member error" {
                failsWithOption "has no instance member" "let f (o: int option) : bool = o.Nope"
            }

            // The module-qualified twin: an unknown `Option.X` is an unresolved-member
            // error, not a silently-accepted fresh TyVar.
            test "an unknown Option function is unresolved" {
                failsWithOption "Nope" "let f (o: int option) : int = Option.Nope o"
            }
        ]

// Cross-package construction of an external union's cases (`Some` / `None`, in scope via
// `open Vesper`): the bare or qualified case name resolves through the provider's reverse
// case index, lowers to `TExpr.UnionCons`, and emits a `call` to the static case factory.
[<Tests>]
let optionCtorFrontEnd =
    testList
        "OptionCtorFrontEnd"
        [
            test "Some 5 types as int option (annotated)" { typeChecksOption "let x : int option = Some 5" }

            test "None types as int option (nullary, annotated)" { typeChecksOption "let x : int option = None" }

            // Unannotated, `Some 5` infers `int option`; the value restriction pins it.
            test "Some 5 resolves with no annotation" { typeChecksOption "let x = Some 5" }

            test "Option.Some 5 (qualified) types as int option" {
                typeChecksOption "let x : int option = Option.Some 5"
            }

            test "nested Some in a tuple" { typeChecksOption "let x : int option * int option = Some 1, None" }
        ]

[<Tests>]
let optionCtorRuntime =
    testList
        "OptionCtorRuntime"
        [
            // Emit smoke: both the n-ary (`Some 5`) and nullary (`None`) factories.
            test "Some and None construct and run" {
                runsOption "ok" "open Vesper\nlet a = Some 5\nlet b : int option = None\nprintfn \"%s\" \"ok\""
            }

            // Construction feeding instance-member access: `(Some 5).IsSome` emits a
            // `callvirt get_IsSome` on the external `Vesper.Option`1<int>` `TypeSpec`.
            test "(Some 5).IsSome is true; None.IsSome is false" {
                runsOptionLines
                    [ "true"; "false" ]
                    "open Vesper\nprintfn \"%b\" (Some 5).IsSome\nprintfn \"%b\" (None: int option).IsSome"
            }

            test "None.IsNone is true; (Some 5).IsNone is false" {
                runsOptionLines
                    [ "true"; "false" ]
                    "open Vesper\nprintfn \"%b\" (None: int option).IsNone\nprintfn \"%b\" (Some 5).IsNone"
            }

            // `member Value: 'T`'s getter returns `!0` = `int` at this instantiation.
            test "(Some 5).Value reads the payload" { runsOption "5" "open Vesper\nprintfn \"%d\" (Some 5).Value" }
        ]

// Cross-package pattern matching on an external union's cases. The case pattern lowers
// to `TPat.Union` as a local arm does; `Option` is type-tested, so the backend `isinst`s
// the case's nested type and `ldfld`s the payload off that case's `TypeSpec`.
[<Tests>]
let optionMatchFrontEnd =
    testList
        "OptionMatchFrontEnd"
        [
            // The bound variable picks up the union's instantiation: `'T` = `int`.
            test "match Some x binds x : int" {
                typeChecksOption "let f (o: int option) : int =\n    match o with\n    | Some x -> x\n    | None -> 0"
            }

            // Nullary `None` resolves as a (zero-field) case pattern, not a bound variable.
            test "match None arm type-checks" {
                typeChecksOption
                    "let f (o: int option) : bool =\n    match o with\n    | None -> true\n    | Some _ -> false"
            }

            test "qualified Option.Some pattern type-checks" {
                typeChecksOption
                    "let f (o: int option) : int =\n    match o with\n    | Option.Some x -> x\n    | Option.None -> 0"
            }
        ]

[<Tests>]
let optionMatchRuntime =
    testList
        "OptionMatchRuntime"
        [
            // `None` is declared first, so it is the tag-0 arm.
            test "match extracts Some payload, defaults on None" {
                runsOptionLines
                    [ "7"; "0" ]
                    ("open Vesper\n"
                     + "let describe (o: int option) =\n    match o with\n    | Some x -> x\n    | None -> 0\n"
                     + "printfn \"%d\" (describe (Some 7))\n"
                     + "printfn \"%d\" (describe None)")
            }

            // `Some _` binds nothing, so the arm drives only the `_tag` compare.
            test "match discriminates Some vs None" {
                runsOptionLines
                    [ "true"; "false" ]
                    ("open Vesper\n"
                     + "let isSome (o: int option) =\n    match o with\n    | Some _ -> true\n    | None -> false\n"
                     + "printfn \"%b\" (isSome (Some 1))\n"
                     + "printfn \"%b\" (isSome (None: int option))")
            }

            // Construction feeding a match end-to-end. `option` is a `[<Struct>]` union in
            // a referenced package, so the arm extracts through the `Get_Some_0` reader
            // and mints no `MemberRef` to the `Some_0` field.
            test "match round-trips a constructed Some through the case getter" {
                let (exitCode, output), bytes =
                    runPackagesInspect
                        [ "Vesper.Option" ]
                        ("open Vesper\n"
                         + "let unwrap (o: int option) =\n    match o with\n    | Some x -> x\n    | None -> -1\n"
                         + "printfn \"%d\" (unwrap (Some 42))")

                Expect.equal exitCode 0 "the driver ran"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "the payload came back through the getter"

                Expect.equal (MetadataStructure.memberRefRowCount bytes "Get_Some_0") 1 "one MemberRef to the getter"
                Expect.equal (MetadataStructure.memberRefRowCount bytes "Some_0") 0 "no MemberRef to the payload field"
            }
        ]

// External module-function calls (`Option.defaultValue 0 o`, `Option.map f o`). The
// backend mints a `TypeRef` for `Vesper.OptionModule`, recovers the use-site type args
// from the call type, and `call`s a `MethodSpec` (a bare `MemberRef` when monomorphic).
[<Tests>]
let optionModuleCallRuntime =
    testList
        "OptionModuleCallRuntime"
        [
            // A pure-data module call, generic over `'T`.
            test "Option.defaultValue: value on Some, default on None" {
                runsOptionLines
                    [ "7"; "9" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Option.defaultValue 9 (Some 7))\n"
                     + "printfn \"%d\" (Option.defaultValue 9 (None: int option))")
            }

            test "Option.get / count / isSome through the module" {
                runsOptionLines
                    [ "7"; "1"; "true"; "false" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Option.get (Some 7))\n"
                     + "printfn \"%d\" (Option.count (Some 5))\n"
                     + "printfn \"%b\" (Option.isSome (Some 1))\n"
                     + "printfn \"%b\" (Option.isNone (Some 1))")
            }

            // `map : ('T -> 'U) -> 'T option -> 'U option`. The lambda becomes a synthesised
            // `Vesper.Fun`, and the result is read back through `.Value`.
            test "Option.map applies the function under Some" {
                runsOption "5" "open Vesper\nprintfn \"%d\" (Option.map (fun x -> x + 1) (Some 4)).Value"
            }

            test "Option.map on None stays None" {
                runsOption "true" "open Vesper\nprintfn \"%b\" (Option.map (fun x -> x + 1) (None: int option)).IsNone"
            }

            // `bind : ('T -> 'U option) -> 'T option -> 'U option`. `f` returns an option,
            // constructed cross-package inside its own body.
            test "Option.bind chains an option-returning function" {
                runsOption
                    "11"
                    ("open Vesper\n"
                     + "let f x = if x > 0 then Some (x + 1) else None\n"
                     + "printfn \"%d\" (Option.bind f (Some 10)).Value")
            }

            // `fold : ('State -> 'T -> 'State) -> 'State -> 'T option -> 'State`. Two method
            // typars, so a multi-typar `MethodSpec`. The folder is curried
            // (`fun s -> fun x -> …`): `translatePat` does not lower `fun s x -> …`.
            test "Option.fold accumulates over Some, returns state on None" {
                runsOptionLines
                    [ "13"; "3" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Option.fold (fun s -> fun x -> s + x) 3 (Some 10))\n"
                     + "printfn \"%d\" (Option.fold (fun s -> fun x -> s + x) 3 (None: int option))")
            }

            // Both are `('T -> bool) -> 'T option -> bool`.
            test "Option.exists / forall over Some and None" {
                runsOptionLines
                    [ "true"; "false"; "true" ]
                    ("open Vesper\n"
                     + "printfn \"%b\" (Option.exists (fun x -> x > 5) (Some 7))\n"
                     + "printfn \"%b\" (Option.exists (fun x -> x > 5) (Some 1))\n"
                     + "printfn \"%b\" (Option.forall (fun x -> x > 5) (None: int option))")
            }

            // `defaultWith : (unit -> 'T) -> 'T option -> 'T` — a unit-domain `Vesper.Fun`.
            test "Option.defaultWith runs the thunk only on None" {
                runsOptionLines
                    [ "5"; "99" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Option.defaultWith (fun () -> 99) (Some 5))\n"
                     + "printfn \"%d\" (Option.defaultWith (fun () -> 99) (None: int option))")
            }

            // `filter : ('T -> bool) -> 'T option -> 'T option`.
            test "Option.filter keeps on pass, drops on fail" {
                runsOptionLines
                    [ "true"; "true" ]
                    ("open Vesper\n"
                     + "printfn \"%b\" (Option.filter (fun x -> x > 5) (Some 7)).IsSome\n"
                     + "printfn \"%b\" (Option.filter (fun x -> x > 5) (Some 1)).IsNone")
            }
        ]

[<Tests>]
let optionModuleCallFrontEnd =
    testList
        "OptionModuleCallFrontEnd"
        [
            // Analysis only: the module calls resolve through the provider's open scope.
            test "Option.defaultValue type-checks" {
                typeChecksOption "let f (o: int option) : int = Option.defaultValue 0 o"
            }

            test "Option.map type-checks (HOF)" {
                typeChecksOption "let f (o: int option) : int option = Option.map (fun x -> x + 1) o"
            }

            test "Option.fold type-checks (two method typars)" {
                typeChecksOption "let f (o: int option) : int = Option.fold (fun s -> fun x -> s + x) 0 o"
            }
        ]
