module XParsec.FSharp.Codegen.Clr.Tests.OptionTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The behavioral runtime suite for `Vesper.Option`.
//
// Route: REFLECTION-INVOKE over pure data, not driver programs. The
// front-end now compiles `option.fs` end-to-end (the external-ctor-without-`new`
// fix lets `Option.get` / `member Value`'s `raise (InvalidOperationException …)`
// type-check), so `buildPackage "Vesper.Option"` emits a real `Vesper.Option.dll`.
// We reflect over that DLL: construct `Some`/`None` through the union's emitted
// static case factories (as `UnionTests` does via `consM.Invoke`) and invoke the
// `Vesper.OptionModule` statics, asserting the BCL-typed results.
//
// The driver-program route (a Vesper program that `open`s the package and *calls*
// a module function) is now also available: cross-package external module-function
// calls emit (Gap 2 Layer D, `ClrProvider.TryEmitCall` → `EmitExternalCall`),
// alongside construction (Layer B), instance-member access (Layer A backend), and
// `match` (Layer C). The higher-order combinators — which both need a `Vesper.Fun`
// argument impractical to build by reflection *and* reach the package through a
// module call — are covered through that route in `OptionModuleCallRuntime` below.
// This reflection suite keeps the pure-data surface (construction, the
// discriminators, `count`, `get`, `defaultValue`, `flatten`, `orElse`, and the
// instance members) covered without standing up a driver program per row.

/// The built `Vesper.Option.dll` (cached). `buildPackage` loads it into its own
/// ALC and returns the loaded assembly; every type/value below is reflected from
/// *this* assembly so identities line up across `Invoke`s.
let private optionAsm: Lazy<Assembly> =
    lazy (fst (buildPackage "Vesper.Option").Value)

let private intTy = typeof<int>

/// `Vesper.Option`1` closed over `int` — the receiver type for the case factories
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

/// Read an instance member (`get_Value` / `get_IsSome` / `get_IsNone`) off an
/// option value.
let private instanceGet (name: string) (receiver: obj) : obj =
    optionOfInt.Value.GetMethod(name).Invoke(receiver, [||])

let private asBool (o: obj) : bool = o :?> bool
let private asInt (o: obj) : int = o :?> int

[<Tests>]
let tests =
    testList
        "Option"
        [
            // ---- construction round-trips through the case factories ----------
            test "Some / None construct distinct values" {
                Expect.isNotNull (someInt 3) "Some 3 constructs"
                Expect.isNotNull noneInt.Value "None constructs"
            }

            // ---- discriminators -----------------------------------------------
            test "isSome: Some -> true, None -> false" {
                Expect.isTrue (asBool (callModule "isSome" [| intTy |] [| someInt 3 |])) "isSome (Some 3)"
                Expect.isFalse (asBool (callModule "isSome" [| intTy |] [| noneInt.Value |])) "isSome None"
            }

            test "isNone: None -> true, Some -> false" {
                Expect.isTrue (asBool (callModule "isNone" [| intTy |] [| noneInt.Value |])) "isNone None"
                Expect.isFalse (asBool (callModule "isNone" [| intTy |] [| someInt 3 |])) "isNone (Some 3)"
            }

            // ---- count --------------------------------------------------------
            test "count: Some -> 1, None -> 0" {
                Expect.equal (asInt (callModule "count" [| intTy |] [| someInt 3 |])) 1 "count (Some 3)"
                Expect.equal (asInt (callModule "count" [| intTy |] [| noneInt.Value |])) 0 "count None"
            }

            // ---- get (and its None failure mode) ------------------------------
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

            // ---- defaultValue -------------------------------------------------
            test "defaultValue: None -> default, Some -> value" {
                Expect.equal
                    (asInt (callModule "defaultValue" [| intTy |] [| box 99; noneInt.Value |]))
                    99
                    "default on None"

                Expect.equal (asInt (callModule "defaultValue" [| intTy |] [| box 99; someInt 3 |])) 3 "value on Some"
            }

            // ---- flatten ------------------------------------------------------
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

            // ---- orElse -------------------------------------------------------
            test "orElse: None -> ifNone, Some -> self" {
                let fallback = someInt 42
                let onNone = callModule "orElse" [| intTy |] [| fallback; noneInt.Value |]
                Expect.equal (asInt (callModule "get" [| intTy |] [| onNone |])) 42 "orElse picks ifNone on None"

                let onSome = callModule "orElse" [| intTy |] [| fallback; someInt 3 |]
                Expect.equal (asInt (callModule "get" [| intTy |] [| onSome |])) 3 "orElse keeps the Some"
            }

            // ---- instance members ---------------------------------------------
            test "instance IsSome / IsNone / Value" {
                Expect.isTrue (asBool (instanceGet "get_IsSome" (someInt 3))) "(Some 3).IsSome"
                Expect.isFalse (asBool (instanceGet "get_IsNone" (someInt 3))) "(Some 3).IsNone"
                Expect.isTrue (asBool (instanceGet "get_IsNone" noneInt.Value)) "None.IsNone"
                Expect.equal (asInt (instanceGet "get_Value" (someInt 8))) 8 "(Some 8).Value"
            }

            // ---- higher-order combinators -------------------------------------
            // The `map`/`bind`/`fold`/… combinators take a `Vesper.Fun` argument.
            // Reflection can't readily mint one (closures are synthesised per call
            // site, not exposed as a constructible delegate), so these are exercised
            // through the *driver-program* route — `Vesper.Fun` is built naturally by
            // the lambda, and the cross-package module call now emits (Gap 2 Layer D,
            // `ClrProvider.TryEmitCall` → `EmitExternalCall`). See `OptionModuleCall*`
            // below for that suite; this anchor stays as a pointer.
            test "higher-order combinators covered by OptionModuleCallRuntime (Gap 2 Layer D)" { () }
        ]

// Cross-package external-union type resolution + instance-member access, the
// front-end half (analysis only, no codegen). The regression guarded: a driver
// that `open`s `Vesper` and refers to `option` used to dealias the `'T option`
// abbreviation to a mis-kinded `TyRecord("Vesper.Option", …)` (the extractor
// bakes every nominal head as a record), so `o.IsSome` routed to the record
// registry and failed with `Unknown record type 'Vesper.Option'`. The fix
// re-kinds the dealiased type constructor to the referent's actual shape (`TyUnion`) and
// resolves the union's augmentation members (`IsSome`/`IsNone`/`Value`) through
// the contract provider's now-published member surface.
[<Tests>]
let frontEndTests =
    testList
        "OptionFrontEnd"
        [
            // The abbreviation `'T option = Option<'T>` now dealiases to the union,
            // so `o.IsSome` resolves to `bool` instead of `Unknown record type`.
            test "int option dealiases to the union; o.IsSome : bool" {
                typeChecksOption "let f (o: int option) : bool = o.IsSome"
            }

            test "int option; o.IsNone : bool" { typeChecksOption "let f (o: int option) : bool = o.IsNone" }

            // `member Value: 'T` substitutes the receiver's arg, so `o.Value : int`.
            test "int option; o.Value substitutes the type arg (: int)" {
                typeChecksOption "let f (o: int option) : int = o.Value"
            }

            // The *direct* generic reference (no abbreviation) already resolved as
            // a union before the fix — pin it so a regression in either path shows.
            test "Vesper.Option<int> direct ref; o.IsSome : bool" {
                typeChecksOption "let f (o: Vesper.Option<int>) : bool = o.IsSome"
            }

            // A genuinely-absent member on the (now correctly-kinded) union still
            // errors — and as a member miss, not `Unknown record type`.
            test "absent member on int option is a clean instance-member error" {
                failsWithOption "has no instance member" "let f (o: int option) : bool = o.Nope"
            }

            // The module-qualified twin: an unknown `Option.X` function is an
            // unresolved-member error, not a silently-accepted fresh TyVar — the
            // qualified external-module path matching the bare-ident path
            // (vesper-result-handoff.md).
            test "an unknown Option function is unresolved" {
                failsWithOption "Nope" "let f (o: int option) : int = Option.Nope o"
            }
        ]

// Cross-package construction of an
// external union's cases (`Some` / `None` from a referenced package, in scope via
// `open Vesper`). The front end resolves the bare/qualified case name through the
// provider's reverse case index and types it as a ctor; Elaborate lowers the
// application to `TExpr.UnionCons`; the backend emits a `call` to the union's
// emitted static case factory on the instantiated `TypeSpec`.
//
// Scope: this layer covers *construction* only. Reading the constructed value
// back — `(Some 5).IsSome` (instance member, the Layer A *backend* half),
// `match o with Some x -> …` (Layer C), `Option.defaultValue` (Layer D) — is
// still gated, so the runtime smoke below constructs `Some`/`None` and exits 0
// without inspecting them, and the discriminating assertions stay in
// `OptionTests` (reflection over the factories directly).
[<Tests>]
let layerBFrontEnd =
    testList
        "OptionCtorFrontEnd"
        [
            // Bare case names resolve through the reverse index (open `Vesper`).
            test "Some 5 types as int option (annotated)" { typeChecksOption "let x : int option = Some 5" }

            test "None types as int option (nullary, annotated)" { typeChecksOption "let x : int option = None" }

            // Unannotated: `Some 5` infers `int option` (value restriction keeps it
            // monomorphic); no annotation needed for the ctor itself to resolve.
            test "Some 5 resolves with no annotation" { typeChecksOption "let x = Some 5" }

            // Qualified form `Option.Some` — the anchor is the external union, guarded by
            // the union short-name match.
            test "Option.Some 5 (qualified) types as int option" {
                typeChecksOption "let x : int option = Option.Some 5"
            }

            // A bare ctor application still flows through `inferApp` correctly when
            // nested (the argument of another call).
            test "nested Some in a tuple" { typeChecksOption "let x : int option * int option = Some 1, None" }
        ]

[<Tests>]
let layerBRuntime =
    testList
        "OptionCtorRuntime"
        [
            // Emit smoke: the external case-factory `call` path produces valid IL
            // for both the n-ary (`Some 5`) and nullary (`None`) factories and runs
            // to completion.
            test "Some and None construct and run (Layer B emit smoke)" {
                runsOption "ok" "open Vesper\nlet a = Some 5\nlet b : int option = None\nprintfn \"%s\" \"ok\""
            }

            // Construction (Layer B) feeding instance-member access (Layer A
            // *backend*): `(Some 5).IsSome` constructs the union then reads its
            // augmentation property — a `callvirt get_IsSome` on the external
            // `Vesper.Option`1<int>` `TypeSpec`, routed through `ExternalMemberRefOn`
            // (the recover-by-signature `ExternalMemberRef` can't, since the bare
            // contract name carries no arity and `externalClassRef` skips a union).
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

            // `member Value: 'T` substitutes the receiver's type arg — the property
            // getter returns `!0` = `int`, so the value reads back as `5`.
            test "(Some 5).Value reads the payload" { runsOption "5" "open Vesper\nprintfn \"%d\" (Some 5).Value" }
        ]

// Cross-package *pattern matching* on an
// external union's cases (`match o with Some x -> … | None -> …`). The front end
// types the case pattern through the provider's reverse case index
// (`tryExternalCasePattern`, unifying sub-patterns against the case's declared
// field types); Elaborate lowers it to `TPat.Union` exactly as the local arm does;
// the backend reads `scrut._tag` against the case's declaration-order index and
// `ldfld`s the `<case>_<i>` fields off the instantiated external `TypeSpec`
// (`ExternalUnionTag` / `ExternalUnionCaseField` on the provider). This was the
// doc's named gap #1 (`Elaborate.translatePat: TODO Named`).
[<Tests>]
let layerCFrontEnd =
    testList
        "OptionMatchFrontEnd"
        [
            // The field binder picks up the union's instantiation (`'T` = int), so
            // the match arm types as `int` with no annotation noise.
            test "match Some x binds x : int" {
                typeChecksOption "let f (o: int option) : int =\n    match o with\n    | Some x -> x\n    | None -> 0"
            }

            // Nullary `None` resolves as a (zero-field) case pattern, not a binder.
            test "match None arm type-checks" {
                typeChecksOption
                    "let f (o: int option) : bool =\n    match o with\n    | None -> true\n    | Some _ -> false"
            }

            // Qualified case pattern `Option.Some` — the short-name guard accepts it.
            test "qualified Option.Some pattern type-checks" {
                typeChecksOption
                    "let f (o: int option) : int =\n    match o with\n    | Option.Some x -> x\n    | Option.None -> 0"
            }
        ]

[<Tests>]
let layerCRuntime =
    testList
        "OptionMatchRuntime"
        [
            // Extract the payload through a `Some x` binder; the `None` arm is the
            // declaration-order tag-0 case.
            test "match extracts Some payload, defaults on None" {
                runsOptionLines
                    [ "7"; "0" ]
                    ("open Vesper\n"
                     + "let describe (o: int option) =\n    match o with\n    | Some x -> x\n    | None -> 0\n"
                     + "printfn \"%d\" (describe (Some 7))\n"
                     + "printfn \"%d\" (describe None)")
            }

            // Discriminate without binding (`Some _`), driving only the `_tag`
            // compare — no field extract.
            test "match discriminates Some vs None" {
                runsOptionLines
                    [ "true"; "false" ]
                    ("open Vesper\n"
                     + "let isSome (o: int option) =\n    match o with\n    | Some _ -> true\n    | None -> false\n"
                     + "printfn \"%b\" (isSome (Some 1))\n"
                     + "printfn \"%b\" (isSome (None: int option))")
            }

            // Construction (Layer B) feeding a match (Layer C) end-to-end, with the
            // bound value flowing back out through a second construction.
            test "match round-trips a constructed Some" {
                runsOption
                    "42"
                    ("open Vesper\n"
                     + "let unwrap (o: int option) =\n    match o with\n    | Some x -> x\n    | None -> -1\n"
                     + "printfn \"%d\" (unwrap (Some 42))")
            }
        ]

// A general external *module-function*
// call (`Option.defaultValue 0 o`, `Option.map (fun x -> x + 1) o`). The front end
// already resolved these through the contract provider's ambient open scope; the
// only gap was the backend: `ClrProvider.TryEmitCall` hard-coded `List.fold` +
// `printfn`, so any other external module call fell to `Emit: no call recipe for
// external '…'`. `EmitExternalCall` (generalised from `emitFold`) closes it: it
// reads the symbol's open signature from `Instantiate` (re-kinding the contract's
// `TyRecord` type constructors to `TyUnion`), installs the method's free typars as the ambient
// `!!i` set to encode the member-ref signature, mints the declaring module's
// `TypeRef` (`Vesper.OptionModule`) from the key's `ns` + `Origin`, recovers the
// use-site type args by matching the open signature against the call type, and
// `call`s a `MethodSpec` (or the bare `MemberRef` when monomorphic).
//
// Driver-program route: these `open Vesper` and call the module functions directly,
// so a `Vesper.Fun` argument is built naturally by the lambda (no reflection mint).
[<Tests>]
let layerDRuntime =
    testList
        "OptionModuleCallRuntime"
        [
            // The doc's named reproduction: a pure-data module call, generic over `'T`.
            test "Option.defaultValue: value on Some, default on None" {
                runsOptionLines
                    [ "7"; "9" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Option.defaultValue 9 (Some 7))\n"
                     + "printfn \"%d\" (Option.defaultValue 9 (None: int option))")
            }

            // A module call whose result feeds another (`get` ∘ a constructed Some).
            test "Option.get / count / isSome through the module" {
                runsOptionLines
                    [ "7"; "1"; "true"; "false" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Option.get (Some 7))\n"
                     + "printfn \"%d\" (Option.count (Some 5))\n"
                     + "printfn \"%b\" (Option.isSome (Some 1))\n"
                     + "printfn \"%b\" (Option.isNone (Some 1))")
            }

            // Higher-order: a `('T -> 'U) -> 'T option -> 'U option` combinator — the
            // lambda becomes a synthesised `Vesper.Fun` argument; the `'U option`
            // result is read back through the Layer A instance member.
            test "Option.map applies the function under Some" {
                runsOption "5" "open Vesper\nprintfn \"%d\" (Option.map (fun x -> x + 1) (Some 4)).Value"
            }

            // `Option.map` over `None` short-circuits — the result is `None`.
            test "Option.map on None stays None" {
                runsOption "true" "open Vesper\nprintfn \"%b\" (Option.map (fun x -> x + 1) (None: int option)).IsNone"
            }

            // `bind : ('T -> 'U option) -> 'T option -> 'U option` — the binder itself
            // returns an option (constructed cross-package inside the lambda).
            test "Option.bind chains an option-returning function" {
                runsOption
                    "11"
                    ("open Vesper\n"
                     + "let f x = if x > 0 then Some (x + 1) else None\n"
                     + "printfn \"%d\" (Option.bind f (Some 10)).Value")
            }

            // `fold : ('State -> 'T -> 'State) -> 'State -> 'T option -> 'State` — two
            // method typars (`'State`, `'T`), exercising the multi-typar `MethodSpec`
            // (the appearance-order typar collection must match the producer's). The
            // folder is written curried (`fun s -> fun x -> …`), not as a multi-arg
            // lambda (`fun s x -> …`): the latter parses to a lowercase-anchored
            // `Pat.Named` applicative pattern Elaborate doesn't yet lower — a pre-existing
            // gap orthogonal to this layer.
            test "Option.fold accumulates over Some, returns state on None" {
                runsOptionLines
                    [ "13"; "3" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Option.fold (fun s -> fun x -> s + x) 3 (Some 10))\n"
                     + "printfn \"%d\" (Option.fold (fun s -> fun x -> s + x) 3 (None: int option))")
            }

            // `exists` / `forall` — `('T -> bool) -> 'T option -> bool`.
            test "Option.exists / forall over Some and None" {
                runsOptionLines
                    [ "true"; "false"; "true" ]
                    ("open Vesper\n"
                     + "printfn \"%b\" (Option.exists (fun x -> x > 5) (Some 7))\n"
                     + "printfn \"%b\" (Option.exists (fun x -> x > 5) (Some 1))\n"
                     + "printfn \"%b\" (Option.forall (fun x -> x > 5) (None: int option))")
            }

            // `defaultWith : (unit -> 'T) -> 'T option -> 'T` — a thunk argument
            // (`unit -> 'T`), the unit-domain `Vesper.Fun` shape.
            test "Option.defaultWith runs the thunk only on None" {
                runsOptionLines
                    [ "5"; "99" ]
                    ("open Vesper\n"
                     + "printfn \"%d\" (Option.defaultWith (fun () -> 99) (Some 5))\n"
                     + "printfn \"%d\" (Option.defaultWith (fun () -> 99) (None: int option))")
            }

            // `filter : ('T -> bool) -> 'T option -> 'T option` — predicate that keeps
            // or drops the value.
            test "Option.filter keeps on pass, drops on fail" {
                runsOptionLines
                    [ "true"; "true" ]
                    ("open Vesper\n"
                     + "printfn \"%b\" (Option.filter (fun x -> x > 5) (Some 7)).IsSome\n"
                     + "printfn \"%b\" (Option.filter (fun x -> x > 5) (Some 1)).IsNone")
            }
        ]

[<Tests>]
let layerDFrontEnd =
    testList
        "OptionModuleCallFrontEnd"
        [
            // Front-end regression guard (analysis only): the module calls type-check
            // through the contract provider's ambient open scope.
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
