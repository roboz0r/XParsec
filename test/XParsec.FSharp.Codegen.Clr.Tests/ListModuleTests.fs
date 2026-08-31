module XParsec.FSharp.Codegen.Clr.Tests.ListModuleTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PackageHarness

// `list.fs`'s `[]`/`::` operator cases compile to FSharpList's shape: `[]` → a static
// `Empty` factory, `(::)` → a static `Cons` factory plus `Cons_0`/`Cons_1` payload
// fields. The surface under test is `IsEmpty`/`Head`/`Tail` and the module functions.

// Reflection covers the pure-data surface; `fold` is exercised by driver programs, since its
// `folder` is a `Vesper.Fun` reflection cannot mint. `Vesper.List` is in
// `defaultPackages` + `withCore`, so the plain `runs` helper already has it in scope.

/// The built `Vesper.List.dll` (cached). The package is named `Vesper.List` but
/// contributes `List` into `Vesper.Collections`, so the type is
/// `Vesper.Collections.List`1`.
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
let private instanceGet (name: string) (objArg: obj) : obj =
    listOfInt.Value.GetMethod(name).Invoke(objArg, [||])

let private asBool (o: obj) : bool = o :?> bool
let private asInt (o: obj) : int = o :?> int

[<Tests>]
let tests =
    testList
        "List"
        [
            test "Empty / Cons construct distinct values" {
                Expect.isNotNull nilInt.Value "[] (Empty) constructs"
                Expect.isNotNull (consInt 1 nilInt.Value) "1 :: [] (Cons) constructs"
            }

            test "IsEmpty: [] -> true, Cons -> false" {
                Expect.isTrue (asBool (instanceGet "get_IsEmpty" nilInt.Value)) "[].IsEmpty"
                Expect.isFalse (asBool (instanceGet "get_IsEmpty" (consInt 1 nilInt.Value))) "(1 :: []).IsEmpty"
            }

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

            // `list.fs`'s `Head`/`Tail` raise through `failwith`; the message, not the
            // exception subtype, is what these pin.
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

            // `fold`'s `folder` is a `Vesper.Fun` reflection cannot mint; the driver
            // programs below build one from a lambda.
            test "fold covered by ListModuleRuntime (Vesper.Fun via driver)" { () }
        ]

// `[…]` literals bind to the cons-list by arity: a nullary terminator and a binary cons.
// Folders are curried (`fun s -> fun x -> …`) because `translatePat` does not lower the
// multi-arg `fun s x -> …` form.

[<Tests>]
let runtimeTests =
    testList
        "ListModuleRuntime"
        [
            // An operator-section folder over a non-empty literal.
            test "List.fold (+) sums a list literal" { runs "15" "printfn \"%d\" (List.fold (+) 0 [1; 2; 3; 4; 5])" }

            // A lambda folder, so a synthesised `Vesper.Fun` rather than an eta-reified
            // operator section.
            test "List.fold with a curried lambda folder" {
                runs "6" "printfn \"%d\" (List.fold (fun s -> fun x -> s + x) 0 [1; 2; 3])"
            }

            // The empty literal drives the nullary case factory.
            test "List.fold over [] returns the initial state" {
                runs "42" "printfn \"%d\" (List.fold (fun s -> fun x -> s + x) 42 [])"
            }
        ]

// length/isEmpty/head/tail/map/filter/append/rev. Each row composes the function under
// test with `fold` or `head` so the result prints as a scalar; there is no
// list-printing surface.

[<Tests>]
let growRuntime =
    testList
        "ListGrowRuntime"
        [
            // A bare `[]` leaves its element type a free typar, which cannot be encoded at
            // the external call site, hence `([] : int list)`.
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

            // 1² + 2² + 3² = 14.
            test "List.map applies the mapping to each element" {
                runs "14" "printfn \"%d\" (List.fold (+) 0 (List.map (fun x -> x * x) [1; 2; 3]))"
            }

            // Keeps 3 and 4 from [1; 2; 3; 4]; their sum is 7.
            test "List.filter keeps the passing elements" {
                runs "7" "printfn \"%d\" (List.fold (+) 0 (List.filter (fun x -> x > 2) [1; 2; 3; 4]))"
            }

            // `[1; 2] @ [3; 4; 5]` has length 5 and sums to 15.
            test "List.append concatenates the two lists" {
                runsLines
                    [ "5"; "15" ]
                    ("printfn \"%d\" (List.length (List.append [1; 2] [3; 4; 5]))\n"
                     + "printfn \"%d\" (List.fold (+) 0 (List.append [1; 2] [3; 4; 5]))")
            }

            // `rev [1; 2; 3]` is `[3; 2; 1]`, whose head is 3.
            test "List.rev reverses the order" { runs "3" "printfn \"%d\" (List.head (List.rev [1; 2; 3]))" }
        ]

// `match xs with [] -> … | h :: t -> …` against the referenced `Vesper.List` cons-union,
// whose cases live in `Vesper.List.dll` metadata. The cons-list is type-tested, so the
// match `isinst`s the nested `Empty` / `Cons` types and reads the payload off `Cons`.

// The extracted contract keeps the cons-list's op-form case names (`op_Nil` /
// `op_ColonColon`), so these do not resolve through the generic external-union path;
// the provider special-cases the cons-list instead.
[<Tests>]
let externalMatchRuntime =
    testList
        "ListExternalMatchRuntime"
        [
            // `_` skips the head, `t` binds the tail, so both the case test and the
            // tail extract run.
            test "match over the external cons-list discriminates [] from :: (tail recursion)" {
                runs
                    "3"
                    ("let rec len (xs: int list) : int = match xs with | [] -> 0 | _ :: t -> 1 + len t\n"
                     + "printfn \"%d\" (len [1; 2; 3])")
            }

            // `h` binds `Cons_0`, the head field.
            test "match binds the head field (Cons_0) of the external cons-list" {
                runs
                    "1"
                    ("let hd (xs: int list) : int = match xs with | [] -> 0 | h :: _ -> h\n"
                     + "printfn \"%d\" (hd [1; 2; 3])")
            }

            // `[a]` / `[a; b]` / `[a; b; c]` desugar to cons chains (`a :: []`,
            // `a :: b :: []`, …), so they run the same match path as `h :: t`. A literal
            // arm hits only at its exact length; other lengths fall to the wildcard.
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

// Analysis only: the List surface resolves through the default contract stack.
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

            // `[]` and `h :: t` resolve against the Vesper list, pinned by the `int list`
            // annotations.
            test "cons pattern + empty-list pattern type-check" {
                typeChecks "let rec len (xs: int list) : int = match xs with | [] -> 0 | _ :: t -> 1 + len t"
                typeChecks "let hd (xs: int list) : int = match xs with | [] -> 0 | h :: _ -> h"
            }

            test "cons construction type-checks" {
                typeChecks "let cons (x: int) (xs: int list) : int list = x :: xs"
                typeChecks "let two (xs: int list) : int list = 1 :: 2 :: xs"
            }

            // Every element of a literal pattern shares one element type, and the whole
            // pattern is `int list`.
            test "list-literal patterns type-check" {
                typeChecks "let f (xs: int list) : int = match xs with | [a] -> a | _ -> 0"
                typeChecks "let f (xs: int list) : int = match xs with | [a; b] -> a + b | _ -> 0"
                typeChecks "let f (xs: int list) : int = match xs with | [a; b; c] -> a + b + c | _ -> 0"
            }
        ]
