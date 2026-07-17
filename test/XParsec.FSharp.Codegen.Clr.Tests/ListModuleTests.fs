module XParsec.FSharp.Codegen.Clr.Tests.ListModuleTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The behavioral runtime suite for `Vesper.List`,
// scoped to *what the compiled impl actually contains*. Post-cutover the package's
// manifest builds `list.fs`: the verbatim `[]`/`::` operator-case cons-list +
// `ListModule` (fold + the proven "grow" set), now buildable because the front end
// lowers cons patterns/construction. The `[]`/`::` cases compile to FSharpList's
// shape — `[]` → a static `Empty` factory, `(::)` → a static `Cons` factory +
// `Cons_0`/`Cons_1` payload fields — so the surface under test is the type's
// `IsEmpty`/`Head`/`Tail` members + the module functions.
//
// Two routes, mirroring `OptionTests` / `ChoiceTests`:
//   * REFLECTION-INVOKE over `buildPackage "Vesper.List"` for the pure-data surface
//     (construction through the emitted `Cons`/`Empty` factories + the instance
//     members). `fold` is NOT reflected: its `folder` is a `Vesper.Fun` impractical
//     to mint by reflection (the `OptionTests` rationale), so it rides the driver.
//   * DRIVER PROGRAMS for `fold` (the HOF): `Vesper.List` is already in
//     `defaultManifests` + `withCore`, so the plain `runs` helper has the contract
//     and the DLL in scope — no opt-in harness clone needed (unlike Option/Choice,
//     whose case names would shadow the global stack). List literals `[1; 2; 3]`
//     bind to the cons-list by arity (nullary terminator + binary cons), the path
//     `SelfHostTests`' canonical sample already proves with the `(+)` operator
//     section; the rows below add an explicit *curried* folder lambda and the
//     empty-list base case.

// ---- reflection over the built Vesper.List.dll (pure-data surface) -----------

/// The built `Vesper.List.dll` (cached). Built from `list.fs`, so the type is
/// `Vesper.Collections.List`1` with the `[]`/`::` operator cases — compiled to the
/// `Empty` (nullary) / `Cons` (binary) factories (package name != namespace: the
/// package is `Vesper.List` but contributes `List` into `Vesper.Collections`).
let private listAsm: Lazy<Assembly> = lazy (fst (buildPackage "Vesper.List").Value)

let private intTy = typeof<int>

/// `Vesper.Collections.List`1` closed over `int`.
let private listOfInt: Lazy<Type> =
    lazy (listAsm.Value.GetType("Vesper.Collections.List`1").MakeGenericType(intTy))

/// `[] : int list` via the emitted static nullary case factory (`[]` → `Empty`).
let private nilInt: Lazy<obj> =
    lazy (listOfInt.Value.GetMethod("Empty").Invoke(null, [||]))

/// `Cons (h, t) : int list` via the emitted static binary case factory.
let private consInt (h: int) (t: obj) : obj =
    listOfInt.Value.GetMethod("Cons").Invoke(null, [| box h; t |])

/// Read an instance member (`get_IsEmpty` / `get_Head` / `get_Tail`) off a list.
let private instanceGet (name: string) (receiver: obj) : obj =
    listOfInt.Value.GetMethod(name).Invoke(receiver, [||])

let private asBool (o: obj) : bool = o :?> bool
let private asInt (o: obj) : int = o :?> int

[<Tests>]
let tests =
    testList
        "List"
        [
            // ---- construction round-trips through the case factories ----------
            test "Empty / Cons construct distinct values" {
                Expect.isNotNull nilInt.Value "[] (Empty) constructs"
                Expect.isNotNull (consInt 1 nilInt.Value) "1 :: [] (Cons) constructs"
            }

            // ---- IsEmpty discriminates the two cases --------------------------
            test "IsEmpty: [] -> true, Cons -> false" {
                Expect.isTrue (asBool (instanceGet "get_IsEmpty" nilInt.Value)) "[].IsEmpty"
                Expect.isFalse (asBool (instanceGet "get_IsEmpty" (consInt 1 nilInt.Value))) "(1 :: []).IsEmpty"
            }

            // ---- Head / Tail walk the spine -----------------------------------
            test "Head / Tail read the first element and the rest" {
                // [1; 2] = Cons (1, Cons (2, Nil))
                let twoElems = consInt 1 (consInt 2 nilInt.Value)
                Expect.equal (asInt (instanceGet "get_Head" twoElems)) 1 "head of [1; 2] is 1"

                let tail = instanceGet "get_Tail" twoElems
                Expect.equal (asInt (instanceGet "get_Head" tail)) 2 "head of the tail is 2"

                Expect.isTrue
                    (asBool (instanceGet "get_IsEmpty" (instanceGet "get_Tail" tail)))
                    "tail of the tail is Nil"
            }

            // ---- Head / Tail on the empty list raise --------------------------
            // `list.fs`'s `Head`/`Tail` use `failwith`, which the backend lowers to
            // a *plain* BCL `System.Exception` (so the list type references no
            // `FSharp.Core`). Asserting the message keeps the empty-list contract
            // pinned without depending on the exception subtype.
            test "Head on [] raises (empty-list message)" {
                let inner =
                    try
                        instanceGet "get_Head" nilInt.Value |> ignore
                        None
                    with :? TargetInvocationException as e ->
                        Some e.InnerException

                match inner with
                | Some ex -> Expect.stringContains ex.Message "list was empty" "Head Nil message"
                | None -> failtest "expected Head on Nil to raise"
            }

            test "Tail on [] raises (empty-list message)" {
                let inner =
                    try
                        instanceGet "get_Tail" nilInt.Value |> ignore
                        None
                    with :? TargetInvocationException as e ->
                        Some e.InnerException

                match inner with
                | Some ex -> Expect.stringContains ex.Message "list was empty" "Tail Nil message"
                | None -> failtest "expected Tail on Nil to raise"
            }

            // ---- fold is covered by the driver route below --------------------
            // `ListModule.fold`'s `folder` is a `Vesper.Fun` (impractical to mint by
            // reflection — the `OptionTests` HOF rationale), so it is exercised
            // through driver programs in `ListModuleRuntime`, where the lambda builds
            // the `Vesper.Fun` naturally. This anchor stays as the pointer.
            test "fold covered by ListModuleRuntime (Vesper.Fun via driver)" { () }
        ]

// ---- driver programs: ListModule.fold (the HOF) ------------------------------
// `Vesper.List` is in the default stack, so `runs` resolves `List.fold` + binds
// `[…]` literals to the cons-list directly (no opt-in harness). The `(+)` operator
// section is already proven by SelfHostTests' canonical sample; these add the
// explicit folder lambda (curried — `fun s -> fun x -> …` — because Elaborate does not
// lower the multi-arg `fun s x -> …` applicative pattern, the plan's known gap) and
// the empty-list base case.

[<Tests>]
let runtimeTests =
    testList
        "ListModuleRuntime"
        [
            // The canonical-sample shape, restated here as this suite's smoke: the
            // operator-section folder over a non-empty literal.
            test "List.fold (+) sums a list literal" { runs "15" "printfn \"%d\" (List.fold (+) 0 [1; 2; 3; 4; 5])" }

            // An explicit *curried* folder lambda — the new surface this suite
            // settles (the operator section hid whether a synthesised `Vesper.Fun`
            // from a lambda folds correctly cross-package).
            test "List.fold with a curried lambda folder" {
                runs "6" "printfn \"%d\" (List.fold (fun s -> fun x -> s + x) 0 [1; 2; 3])"
            }

            // The base case: folding the empty list returns the seed untouched (the
            // `Nil` arm of `fold`), driving the nullary case factory at the literal.
            test "List.fold over [] returns the initial state" {
                runs "42" "printfn \"%d\" (List.fold (fun s -> fun x -> s + x) 42 [])"
            }
        ]

// ---- the "grow" set: length/isEmpty/head/tail/map/filter/append/rev ----------
// Added to `list-min.fs` + `list.fsi` (vesper-lib-test-plan Phase 2 "then grow").
// Each row composes a new function with an already-proven one (`fold`/`head`) so
// the result prints as a scalar — no list-printing surface is needed.

[<Tests>]
let growRuntime =
    testList
        "ListGrowRuntime"
        [
            // The empty literal is annotated `([] : int list)`: a bare `[]` leaves
            // its element type a free typar, which can't be encoded at the external
            // call site (the non-empty literals pin it by their elements).
            test "List.length counts elements; [] is 0" {
                runsLines
                    [ "3"; "0" ]
                    "printfn \"%d\" (List.length [1; 2; 3])\nprintfn \"%d\" (List.length ([]: int list))"
            }

            test "List.isEmpty: [] true, [1] false" {
                runsLines
                    [ "true"; "false" ]
                    "printfn \"%b\" (List.isEmpty ([]: int list))\nprintfn \"%b\" (List.isEmpty [1])"
            }

            test "List.head returns the first element" { runs "1" "printfn \"%d\" (List.head [1; 2; 3])" }

            test "List.tail drops the first element" { runs "2" "printfn \"%d\" (List.head (List.tail [1; 2; 3]))" }

            // `map` over a sum: 1²+2²+3² = 14 — the mapping is applied to each element.
            test "List.map applies the mapping to each element" {
                runs "14" "printfn \"%d\" (List.fold (+) 0 (List.map (fun x -> x * x) [1; 2; 3]))"
            }

            // `filter` keeps 3 and 4 from [1;2;3;4]; their sum is 7.
            test "List.filter keeps the passing elements" {
                runs "7" "printfn \"%d\" (List.fold (+) 0 (List.filter (fun x -> x > 2) [1; 2; 3; 4]))"
            }

            // `append [1;2] [3;4;5]` has length 5 and sums to 15.
            test "List.append concatenates the two lists" {
                runsLines
                    [ "5"; "15" ]
                    ("printfn \"%d\" (List.length (List.append [1; 2] [3; 4; 5]))\n"
                     + "printfn \"%d\" (List.fold (+) 0 (List.append [1; 2] [3; 4; 5]))")
            }

            // `rev [1;2;3]` is `[3;2;1]`, whose head is 3.
            test "List.rev reverses the order" { runs "3" "printfn \"%d\" (List.head (List.rev [1; 2; 3]))" }
        ]

// ---- external cons-list MATCH (consumer side) --------------------------------
// `match xs with [] -> … | h :: t -> …` against the *referenced* `Vesper.List`
// cons-union, whose `[]`/`::` cases live in `Vesper.List.dll` metadata, not in
// this assembly. The match compiler reads the discriminator + per-case fields off
// the emitted layout (`_tag`; `Empty` tag 0 / `Cons` tag 1; `Cons_0`/`Cons_1`)
// through the provider's cons-list special-case (`ExternalUnionTag` /
// `ExternalUnionCaseField`) — the sibling of construction's `TryEmitUnionCons`.
// Before this special-case the consumer match failed at emit ("no emitted union
// for match on 'Vesper.Collections.List`1'") because the extracted contract keeps
// the cons-list's op-form case names (`op_Nil`/`op_ColonColon`), so it never
// resolves through the generic external-union path.
[<Tests>]
let externalMatchRuntime =
    testList
        "ListExternalMatchRuntime"
        [
            // The empty-vs-cons discriminator (`_tag`) AND the `Cons_1` (tail) field:
            // counts the spine by recursion over the external list's `[]` / `_ :: t`
            // cases (`_` skips `Cons_0`, `t` binds `Cons_1`).
            test "match over the external cons-list discriminates [] from :: (tail recursion)" {
                runs
                    "3"
                    ("let rec len (xs: int list) : int = match xs with | [] -> 0 | _ :: t -> 1 + len t\n"
                     + "printfn \"%d\" (len [1; 2; 3])")
            }

            // The `Cons_0` (head) field extract: binds and returns the first element.
            test "match binds the head field (Cons_0) of the external cons-list" {
                runs
                    "1"
                    ("let hd (xs: int list) : int = match xs with | [] -> 0 | h :: _ -> h\n"
                     + "printfn \"%d\" (hd [1; 2; 3])")
            }

            // G2: fixed-length list-literal patterns `[a]` / `[a; b]` / `[a; b; c]`
            // desugar to cons chains (`a :: []`, `a :: b :: []`, …) terminated by the
            // empty case, so they run through the same external cons-list match path
            // as `h :: t`. A literal arm hits only at its exact length; other lengths
            // fall to the wildcard.
            test "single-element list-literal pattern [a] hits length 1, misses otherwise" {
                runsLines
                    [ "5"; "0" ]
                    ("let f (xs: int list) : int = match xs with | [a] -> a | _ -> 0\n"
                     + "printfn \"%d\" (f [5])\n"
                     + "printfn \"%d\" (f [5; 6])")
            }

            test "two-element list-literal pattern [a; b] binds both elements" {
                runsLines
                    [ "7"; "-1" ]
                    ("let f (xs: int list) : int = match xs with | [a; b] -> a + b | _ -> -1\n"
                     + "printfn \"%d\" (f [3; 4])\n"
                     + "printfn \"%d\" (f [3])")
            }

            test "three-element list-literal pattern [a; b; c] binds all three" {
                runs
                    "6"
                    ("let f (xs: int list) : int = match xs with | [a; b; c] -> a + b + c | _ -> 0\n"
                     + "printfn \"%d\" (f [1; 2; 3])")
            }
        ]

// ---- front-end regression guard (analysis only) ------------------------------
// The cheap probe the plan calls for: `List.fold` type-checks through the default
// contract stack without running it.
[<Tests>]
let frontEndTests =
    testList
        "ListFrontEnd"
        [
            test "List.fold with an operator section type-checks" {
                typeChecks "let sum (xs: int list) : int = List.fold (+) 0 xs"
            }

            test "List.fold with a curried lambda type-checks" {
                typeChecks "let sum (xs: int list) : int = List.fold (fun s -> fun x -> s + x) 0 xs"
            }

            test "List.map / filter / append / rev / length type-check" {
                typeChecks "let f (xs: int list) : int list = List.map (fun x -> x + 1) xs"
                typeChecks "let f (xs: int list) : int list = List.filter (fun x -> x > 0) xs"
                typeChecks "let f (xs: int list) (ys: int list) : int list = List.append xs ys"
                typeChecks "let f (xs: int list) : int list = List.rev xs"
                typeChecks "let f (xs: int list) : int = List.length xs"
            }

            // The cons-pattern/construction wiring the cutover added, guarded at the
            // front-end (analysis-only). The empty-list pattern (`[]`), the cons
            // pattern (`h :: t`), and cons construction (`x :: xs`) all resolve and
            // type-check against the Vesper list (pinned by `int list` annotations).
            test "cons pattern + empty-list pattern type-check" {
                typeChecks "let rec len (xs: int list) : int = match xs with | [] -> 0 | _ :: t -> 1 + len t"
                typeChecks "let hd (xs: int list) : int = match xs with | [] -> 0 | h :: _ -> h"
            }

            test "cons construction type-checks" {
                typeChecks "let cons (x: int) (xs: int list) : int list = x :: xs"
                typeChecks "let two (xs: int list) : int list = 1 :: 2 :: xs"
            }

            // G2: fixed-length list-literal patterns (`[a]`, `[a; b]`, `[a; b; c]`)
            // type-check against the Vesper list — each element shares the element
            // type and the whole pattern is `int list`.
            test "list-literal patterns type-check" {
                typeChecks "let f (xs: int list) : int = match xs with | [a] -> a | _ -> 0"
                typeChecks "let f (xs: int list) : int = match xs with | [a; b] -> a + b | _ -> 0"
                typeChecks "let f (xs: int list) : int = match xs with | [a; b; c] -> a + b + c | _ -> 0"
            }
        ]
