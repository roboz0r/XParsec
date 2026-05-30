module XParsec.FSharp.Codegen.Clr.Tests.SelfHostR3Tests

open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The bare-program list/fold cutover onto the
// Vesper `List`. The first capability it rests on is a *generic* top-level
// function emitted as a generic static method (`fold<'State,'T>`) — the method's
// type parameters are the free `TypeVar`s of its signature, encoded `!!i`, its
// recursive self-call a `MethodSpec` over its own typars, and any `List<!!i>`
// member ref inside its body instantiated with the method's typars.

let private lines xs = String.concat "\n" xs

[<Tests>]
let tests =
    testList
        "SelfHostR3"
        [
            test "Vesper.List.dll exports List`1 (Cons/Nil + IsEmpty/Head/Tail) and ListModule::fold (its own package)" {
                // The cons-list is its own package now (package-split-plan PS2):
                // forcing the lazy compiles `src/Vesper.List/list-min.fs` into a
                // standalone Vesper.List.dll and loads it.
                let listPath = vesperListDll.Value
                let listAsm = System.Reflection.Assembly.LoadFrom listPath

                let listTy =
                    listAsm.GetTypes()
                    |> Array.tryFind (fun t -> t.FullName = "Vesper.Collections.List`1")

                match listTy with
                | None -> failtest "Vesper.List.dll has no Vesper.Collections.List`1"
                | Some t ->
                    Expect.isTrue t.IsGenericTypeDefinition "List`1 is a generic type definition"
                    Expect.isNotNull (t.GetMethod "Cons") "List`1 has a static Cons factory"
                    Expect.isNotNull (t.GetMethod "Nil") "List`1 has a static Nil factory"
                    Expect.isNotNull (t.GetMethod "get_IsEmpty") "List`1 has an instance get_IsEmpty"
                    Expect.isNotNull (t.GetMethod "get_Head") "List`1 has an instance get_Head"
                    Expect.isNotNull (t.GetMethod "get_Tail") "List`1 has an instance get_Tail"

                // R3 deferred: `module List` compiles to a `Vesper.Collections.ListModule`
                // static class holding the public `fold` (a 2-typar generic static method).
                let listModule =
                    listAsm.GetTypes()
                    |> Array.tryFind (fun t -> t.FullName = "Vesper.Collections.ListModule")

                match listModule with
                | None -> failtest "Vesper.List.dll has no Vesper.Collections.ListModule"
                | Some m ->
                    let fold = m.GetMethod "fold"
                    Expect.isNotNull fold "ListModule has a static fold"
                    Expect.isTrue fold.IsStatic "fold is static"
                    Expect.isTrue fold.IsGenericMethodDefinition "fold is a generic method definition"
                    Expect.equal (fold.GetGenericArguments().Length) 2 "fold has two generic parameters"

                let refs = listAsm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                // The list type itself uses no FSharp.Core (its `Head`/`Tail` use
                // `failwith` → BCL `System.Exception`); `fold` uses `Vesper.Fun`, so
                // the DLL now references `Vesper.Core` (it had no ref while only the
                // minimal list shipped). Still no FSharp.Core ("pay for what you use").
                Expect.isFalse
                    (refs |> Array.contains "FSharp.Core")
                    (sprintf "Vesper.List.dll is FSharp.Core-free (refs: %A)" refs)

                Expect.isTrue
                    (refs |> Array.contains "Vesper.Core")
                    (sprintf "Vesper.List.dll references Vesper.Core (fold's folder is a Vesper.Fun) (refs: %A)" refs)
            }

            // A generic match over a generic union (no recursion / no function
            // param): the `!!`-typed tag + field member refs inside a generic
            // static method body.
            let hdSrc =
                lines
                    [
                        "type Lst<'T> ="
                        "    | Nil"
                        "    | Cons of 'T * Lst<'T>"
                        "let hd (xs: Lst<'T>) (dflt: 'T) : 'T ="
                        "    match xs with"
                        "    | Nil -> dflt"
                        "    | Cons(h, t) -> h"
                        "printfn \"%d\" (hd (Cons(7, Nil)) 0)"
                    ]

            test "a generic match over a generic union returns the head (prints 7)" {
                let tast, artifact = compileSource "R3GenericHd" hdSrc
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "7" "Cons(7, Nil) head is 7 via a generic static method"
            }

            // A recursive generic function (`MethodSpec` self-call) with no
            // function parameter.
            let lastOrSrc =
                lines
                    [
                        "type Lst<'T> ="
                        "    | Nil"
                        "    | Cons of 'T * Lst<'T>"
                        "let rec lastOr (xs: Lst<'T>) (dflt: 'T) : 'T ="
                        "    match xs with"
                        "    | Nil -> dflt"
                        "    | Cons(h, t) -> lastOr t h"
                        "printfn \"%d\" (lastOr (Cons(1, Cons(2, Cons(3, Nil)))) 0)"
                    ]

            test "a recursive generic function self-calls via MethodSpec (prints 3)" {
                let tast, artifact = compileSource "R3GenericLastOr" lastOrSrc
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "3" "lastOr recurses through the list to the last element"
            }

            // The full generic `foldl<'State,'T>` over a generic union, used at
            // `<int,int>`: generic in *both* the accumulator and the element, with
            // a `Vesper.Fun` folder parameter applied (`f acc h`) inside the body.
            let foldlSrc =
                lines
                    [
                        "type Lst<'T> ="
                        "    | Nil"
                        "    | Cons of 'T * Lst<'T>"
                        "let rec foldl (f: 'State -> 'T -> 'State) (acc: 'State) (xs: Lst<'T>) : 'State ="
                        "    match xs with"
                        "    | Nil -> acc"
                        "    | Cons(h, t) -> foldl f (f acc h) t"
                        "printfn \"%d\" (foldl (+) 0 (Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))))"
                    ]

            test "a generic recursive `foldl<'State,'T>` over a generic union folds to 15 (generic static method)" {
                let tast, artifact = compileSource "R3GenericFoldl" foldlSrc
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Trim())
                    "15"
                    "foldl (+) 0 [1..5] = 15 over our own generic Lst<'T>, via a generic static method"
            }

            test "the generic static method reflects as a 2-typar generic method" {
                let _, artifact = compileSource "R3GenericFoldlShape" foldlSrc
                let asm = loadAssembly (Codegen.toBytes artifact)

                let foldl =
                    asm.GetTypes()
                    |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.Static))
                    |> Array.tryFind (fun m -> m.IsGenericMethodDefinition && m.GetGenericArguments().Length = 2)

                match foldl with
                | None -> failtest "no 2-typar generic static method was emitted"
                | Some m -> Expect.equal (m.GetGenericArguments().Length) 2 "foldl has two generic parameters"
            }
        ]
