module XParsec.FSharp.Codegen.Clr.Tests.UnionTests

open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Our own (discriminated) unions: construction, `match` deconstruction,
// recursion over them, augmentation members, and the generic `Lst<'T>` /
// `List<'T>` forms — plus the BCL-only library-DLL shape (static factories,
// generic instance members) the reflection round-trips pin. These were the
// `Rung2`/`SelfHostR3` milestone anchors; they live here under the capability
// they exercise.

let private lines xs = String.concat "\n" xs

let private unionSrc =
    "type Lst =\n    | Nil\n    | Cons of int * Lst\nlet head =\n    match Cons(7, Nil) with\n    | Nil -> 0\n    | Cons(h, _) -> h\nprintfn \"%d\" head"

let private recSrc =
    "type Lst =\n    | Nil\n    | Cons of int * Lst\nlet rec sumList xs =\n    match xs with\n    | Nil -> 0\n    | Cons(h, t) -> h + sumList t\nprintfn \"%d\" (sumList (Cons(1, Cons(2, Cons(3, Nil)))))"

let private memberUnionSrc =
    lines
        [
            "type Lst ="
            "    | Nil"
            "    | Cons of int * Lst"
            ""
            "    member this.IsEmpty ="
            "        match this with"
            "        | Nil -> true"
            "        | Cons(_, _) -> false"
            ""
            "    member this.Head ="
            "        match this with"
            "        | Cons(h, _) -> h"
            "        | Nil -> failwith \"empty\""
            ""
            "    member this.Length ="
            "        match this with"
            "        | Nil -> 0"
            "        | Cons(_, t) -> 1 + t.Length"
            ""
            "    static member Empty = Nil"
            "    static member Single x = Cons(x, Nil)"
        ]

let private genUnionSrc =
    lines
        [
            "type Lst<'T> ="
            "    | Nil"
            "    | Cons of 'T * Lst<'T>"
            "let rec sumList xs ="
            "    match xs with"
            "    | Nil -> 0"
            "    | Cons(h, t) -> h + sumList t"
            "printfn \"%d\" (sumList (Cons(1, Cons(2, Cons(3, Nil)))))"
        ]

let private genMemberSrc =
    lines
        [
            "type Lst<'T> ="
            "    | Nil"
            "    | Cons of 'T * Lst<'T>"
            ""
            "    member this.IsEmpty ="
            "        match this with"
            "        | Nil -> true"
            "        | Cons(_, _) -> false"
            ""
            "    member this.Head ="
            "        match this with"
            "        | Cons(h, _) -> h"
            "        | Nil -> failwith \"empty\""
            ""
            "    member this.Tail ="
            "        match this with"
            "        | Cons(_, t) -> t"
            "        | Nil -> failwith \"empty\""
            ""
            "    member this.Length ="
            "        match this with"
            "        | Nil -> 0"
            "        | Cons(_, t) -> 1 + t.Length"
        ]

// Generic match / recursion / fold over a generic union (SelfHostR3 anchors).
let private hdSrc =
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

let private lastOrSrc =
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

let private foldlSrc =
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

[<Tests>]
let tests =
    testList
        "Unions"
        [
            test "monomorphic union + match analyses clean and surfaces TTypeKind.Union" {
                let tast = analyse unionSrc
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let unions =
                    let acc = ResizeArray<string * EqArray<TUnionCase>>()

                    for d in tast.Decls do
                        match d with
                        | TDecl.Type td ->
                            match td.Kind with
                            | TTypeKind.Union(cs, _) -> acc.Add(td.Name, cs)
                            | _ -> ()
                        | _ -> ()

                    List.ofSeq acc

                match unions with
                | [ ("Lst", EqList [ c0; c1 ]) ] ->
                    Expect.equal (c0: TUnionCase).Name "Nil" "first case is Nil"
                    Expect.isTrue c0.Fields.IsEmpty "Nil is nullary"
                    Expect.equal (c1: TUnionCase).Name "Cons" "second case is Cons"
                    Expect.equal c1.Fields.Length 2 "Cons has two fields"
                | other -> failtestf "unexpected unions: %A" other
            }

            test "construct `Cons(7, Nil)` and read its head via match (prints 7)" {
                let _, artifact = compileSource "UnionHead" unionSrc
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "7" "Cons head extracted by the `| Cons(h, _)` arm"
            }

            test "a `match` arm selects the Nil case of an emitted union (prints 0)" {
                let src =
                    "type Lst =\n    | Nil\n    | Cons of int * Lst\nlet n =\n    match Nil with\n    | Nil -> 0\n    | Cons(h, _) -> h\nprintfn \"%d\" n"

                let _, artifact = compileSource "UnionNil" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "0" "the Nil arm matched on a constructed Nil"
            }

            test "recursive `sumList` analyses clean and self-references its binding" {
                let tast = analyse recSrc
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let fnKey =
                    tast.Decls
                    |> EqArray.tryFind (fun d ->
                        match d with
                        | TDecl.Let(TPat.NamedSimple _, TExpr.Lambda _, _, _) -> true
                        | _ -> false
                    )
                    |> ValueOption.map (fun d ->
                        match d with
                        | TDecl.Let(TPat.NamedSimple(k, _), TExpr.Lambda _, _, _) -> k
                        | _ -> failwith "unreachable"
                    )

                Expect.isTrue fnKey.IsSome "sumList binds a lambda value"
            }

            test "recursive `sumList` folds a 3-element list (prints 6)" {
                let _, artifact = compileSource "UnionRecursion" recSrc
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "6" "sumList [1;2;3] = 6 via self-recursive static call"
            }

            test "nested Cons deconstructs the tail (prints the second element)" {
                let src =
                    "type Lst =\n    | Nil\n    | Cons of int * Lst\nlet second =\n    match Cons(10, Cons(20, Nil)) with\n    | Cons(_, Cons(y, _)) -> y\n    | _ -> -1\nprintfn \"%d\" second"

                let _, artifact = compileSource "UnionNested" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "20" "the nested `Cons(_, Cons(y, _))` bound the tail's head"
            }

            // P3c: emit a union + module as a *library* DLL (no Main) via the
            // converged assembler. The fold here is the concrete `sum` rather than
            // a higher-order `fold`: a function parameter would be an `FSharpFunc`2`,
            // pinning FSharp.Core and breaking "BCL-only".
            test "a union + recursive module fold ships as a BCL-only library DLL (P3c)" {
                let src =
                    "namespace Vesper.Collections\n\ntype IntList =\n    | Empty\n    | Cons of int * IntList\n\nlet rec sum xs =\n    match xs with\n    | Empty -> 0\n    | Cons(h, t) -> h + sum t"

                let tast = analyse src
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let project = ProjectInfo.library "Vesper.Collections"
                let artifact = compileSourceTo project src

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    "the union + concrete fold reference no FSharp.Core construct"

                // Every reflected member + constructed value below must come from
                // this single `asm` — a second load of the same bytes is a *different*
                // assembly, so a cross-`Invoke` would mix two type identities.
                let asm = loadAssembly (Codegen.toBytes artifact)

                Expect.isNull asm.EntryPoint "a library DLL has no entry point"

                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                Expect.isFalse
                    (refs |> Array.contains "FSharp.Core")
                    (sprintf "no FSharp.Core reference (refs: %A)" refs)

                let listTy = asm.GetType("Vesper.Collections.IntList")
                Expect.isNotNull listTy "the DLL contains Vesper.Collections.IntList"

                let emptyM = listTy.GetMethod("Empty")
                let consM = listTy.GetMethod("Cons")
                Expect.isNotNull emptyM "IntList has a static Empty factory"
                Expect.isNotNull consM "IntList has a static Cons factory"

                let sumM =
                    asm.GetTypes()
                    |> Array.collect (fun t ->
                        t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
                    )
                    |> Array.filter (fun m -> m.Name.StartsWith "fn$")

                match sumM with
                | [| sumM |] ->
                    let empty = emptyM.Invoke(null, [||])
                    let l1 = consM.Invoke(null, [| box 3; empty |])
                    let l2 = consM.Invoke(null, [| box 2; l1 |])
                    let l3 = consM.Invoke(null, [| box 1; l2 |])
                    let result = sumM.Invoke(null, [| l3 |])
                    Expect.equal (result :?> int) 6 "sum [1;2;3] = 6 via the emitted static fold"
                | other -> failtestf "expected one static fold method, got %A" (other |> Array.map (fun m -> m.Name))
            }

            test "union augmentation members surface on TTypeKind.Union (P3d.3)" {
                let tast = analyse memberUnionSrc
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let members =
                    tast.Decls
                    |> EqArray.tryFind (fun d ->
                        match d with
                        | TDecl.Type { Kind = TTypeKind.Union _ } -> true
                        | _ -> false
                    )
                    |> ValueOption.bind (fun d ->
                        match d with
                        | TDecl.Type { Kind = TTypeKind.Union(_, ms) } -> ValueSome ms
                        | _ -> ValueNone
                    )

                match members with
                | ValueSome ms ->
                    let byName = [ for (m: TTypeMember) in ms -> m.Name, m.IsStatic, m.Kind ]
                    Expect.contains byName ("IsEmpty", false, TMemberKind.Property) "IsEmpty is an instance property"
                    Expect.contains byName ("Head", false, TMemberKind.Property) "Head is an instance property"
                    Expect.contains byName ("Length", false, TMemberKind.Property) "Length is an instance property"
                    Expect.contains byName ("Empty", true, TMemberKind.Property) "Empty is a static property"
                    Expect.contains byName ("Single", true, TMemberKind.Method) "Single is a static method"
                | ValueNone -> failtest "no union surfaced"
            }

            test "consume union members at runtime: properties, recursion, statics (P3d.3)" {
                let src =
                    memberUnionSrc
                    + "\n"
                    + lines
                        [
                            "let xs = Cons(10, Cons(20, Cons(30, Nil)))"
                            "printfn \"%b\" xs.IsEmpty"
                            "printfn \"%d\" xs.Head"
                            "printfn \"%d\" xs.Length"
                            "let e = Lst.Empty"
                            "printfn \"%b\" e.IsEmpty"
                            "let s = Lst.Single 7"
                            "printfn \"%d\" s.Head"
                        ]

                let tast, artifact = compileSource "UnionMembers" src

                Expect.isEmpty
                    tast.Diagnostics
                    (sprintf "no diagnostics: %A" (tast.Diagnostics |> List.map (fun d -> d.Message)))

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                let outLines =
                    output.Split('\n')
                    |> Array.map (fun s -> s.Trim())
                    |> Array.filter (fun s -> s.Length > 0)

                Expect.equal
                    outLines
                    [| "false"; "10"; "3"; "true"; "7" |]
                    "xs.IsEmpty=false, xs.Head=10, xs.Length=3, Lst.Empty.IsEmpty=true, (Lst.Single 7).Head=7"
            }

            test "an instance member is emitted as a real method (get_IsEmpty) on the union (P3d.3)" {
                let _, artifact = compileSource "UnionMemberMeta" memberUnionSrc
                let asm = loadAssembly (Codegen.toBytes artifact)
                let listTy = asm.GetType "Lst"
                Expect.isNotNull listTy "the assembly contains the union type Lst"

                let getIsEmpty =
                    listTy.GetMethod("get_IsEmpty", BindingFlags.Public ||| BindingFlags.Instance)

                Expect.isNotNull getIsEmpty "IsEmpty is emitted as an instance get_IsEmpty method"

                let single = listTy.GetMethod("Single", BindingFlags.Public ||| BindingFlags.Static)

                Expect.isNotNull single "Single is emitted as a static method"
                Expect.equal (single.GetParameters().Length) 1 "Single has one real Param row"
            }

            test "a generic union constructs + match-deconstructs at runtime via a recursive fold (prints 6)" {
                let tast, artifact = compileSource "GenUnion" genUnionSrc

                Expect.isEmpty
                    tast.Diagnostics
                    (sprintf "no diagnostics: %A" (tast.Diagnostics |> List.map (fun d -> d.Message)))

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Trim())
                    "6"
                    "Cons(1, Cons(2, Cons(3, Nil))) folds to 6 over our own generic Lst<'T>"
            }

            test "a generic List<'T> union compiles to a BCL-only library DLL with generic factories (P3d.4)" {
                let src =
                    lines
                        [
                            "namespace Vesper.Collections"
                            ""
                            "type List<'T> ="
                            "    | ([]): List<'T>"
                            "    | (::): Head: 'T * Tail: List<'T> -> List<'T>"
                        ]

                let tast = analyse src
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let project = ProjectInfo.library "Vesper.Collections"
                let artifact = compileSourceTo project src

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    "the generic union (int / 'T / self fields) references no FSharp.Core construct"

                let asm = loadAssembly (Codegen.toBytes artifact)
                Expect.isNull asm.EntryPoint "a library DLL has no entry point"

                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                Expect.isFalse
                    (refs |> Array.contains "FSharp.Core")
                    (sprintf "no FSharp.Core reference (refs: %A)" refs)

                let listTy = asm.GetType "Vesper.Collections.List`1"
                Expect.isNotNull listTy "the DLL contains Vesper.Collections.List`1"
                Expect.isTrue listTy.IsGenericTypeDefinition "List`1 is a generic type definition"
                Expect.equal (listTy.GetGenericArguments().Length) 1 "List`1 has one type parameter"

                let listOfInt = listTy.MakeGenericType(typeof<int>)
                let emptyM = listOfInt.GetMethod "Empty"
                let consM = listOfInt.GetMethod "Cons"
                Expect.isNotNull emptyM "List<int> has a static Empty factory"
                Expect.isNotNull consM "List<int> has a static Cons factory"

                let empty = emptyM.Invoke(null, [||])
                let one = consM.Invoke(null, [| box 1; empty |])
                Expect.isNotNull one "Cons(1, Empty) constructs a List<int>"

                // The head field is the type's `!0`, so on `List<int>` it is `int`.
                let headField = listOfInt.GetField "Cons_0"
                Expect.isNotNull headField "List<int> has the Cons_0 (Head) field"
                Expect.equal (headField.GetValue one :?> int) 1 "Cons_0 holds the head value 1"
            }

            // R2: instance augmentation members on a *generic* union. The member
            // signatures + bodies carry the declaring-typar marker (`!0`), and every
            // member access on `Lst<int>` goes through a `MemberRef` on the
            // instantiated `TypeSpec` (`Lst<int>::get_Head`).
            test "generic union instance members run at runtime: chained Head/Tail, recursive Length (R2)" {
                let src =
                    genMemberSrc
                    + "\n"
                    + lines
                        [
                            "let xs = Cons(10, Cons(20, Cons(30, Nil)))"
                            "printfn \"%b\" xs.IsEmpty"
                            "printfn \"%d\" xs.Head"
                            "printfn \"%d\" xs.Length"
                            "printfn \"%d\" xs.Tail.Head"
                        ]

                let tast, artifact = compileSource "GenMembers" src

                Expect.isEmpty
                    tast.Diagnostics
                    (sprintf "no diagnostics: %A" (tast.Diagnostics |> List.map (fun d -> d.Message)))

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                let outLines =
                    output.Split('\n')
                    |> Array.map (fun s -> s.Trim())
                    |> Array.filter (fun s -> s.Length > 0)

                Expect.equal
                    outLines
                    [| "false"; "10"; "3"; "20" |]
                    "xs.IsEmpty=false, xs.Head=10, xs.Length=3, xs.Tail.Head=20"
            }

            test "a generic union with members compiles to a BCL-only library DLL; members reflect + run (R2)" {
                let src = "namespace Vesper.Collections\n\n" + genMemberSrc

                let tast = analyse src
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let project = ProjectInfo.library "Vesper.Collections.GenMembers"
                let artifact = compileSourceTo project src

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    "the generic union + instance members reference no FSharp.Core construct"

                let asm = loadAssembly (Codegen.toBytes artifact)
                Expect.isNull asm.EntryPoint "a library DLL has no entry point"

                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                Expect.isFalse
                    (refs |> Array.contains "FSharp.Core")
                    (sprintf "no FSharp.Core reference (refs: %A)" refs)

                let listTy = asm.GetType "Vesper.Collections.Lst`1"
                Expect.isNotNull listTy "the DLL contains Vesper.Collections.Lst`1"

                let listOfInt = listTy.MakeGenericType(typeof<int>)
                let consM = listOfInt.GetMethod "Cons"
                let nilM = listOfInt.GetMethod "Nil"
                Expect.isNotNull consM "Lst<int> has a static Cons factory"
                Expect.isNotNull nilM "Lst<int> has a static Nil factory"

                let nil = nilM.Invoke(null, [||])
                let l1 = consM.Invoke(null, [| box 7; nil |])
                let l2 = consM.Invoke(null, [| box 42; l1 |])

                let getHead =
                    listOfInt.GetMethod("get_Head", BindingFlags.Public ||| BindingFlags.Instance)

                let getLength =
                    listOfInt.GetMethod("get_Length", BindingFlags.Public ||| BindingFlags.Instance)

                let getTail =
                    listOfInt.GetMethod("get_Tail", BindingFlags.Public ||| BindingFlags.Instance)

                Expect.isNotNull getHead "Lst<int> has an instance get_Head"
                Expect.equal (getHead.ReturnType) typeof<int> "get_Head returns the type arg int (!0)"
                Expect.equal (getHead.Invoke(l2, [||]) :?> int) 42 "l2.Head = 42"
                Expect.equal (getLength.Invoke(l2, [||]) :?> int) 2 "l2.Length = 2 via recursive get_Length"

                let tail = getTail.Invoke(l2, [||])
                Expect.equal (getHead.Invoke(tail, [||]) :?> int) 7 "l2.Tail.Head = 7"
            }

            // Generic match / recursion / fold over a generic union, emitted as
            // generic static methods (`!!`-typed tag + field member refs, a
            // `MethodSpec` self-call, a `Vesper.Fun` folder parameter).
            test "a generic match over a generic union returns the head (prints 7)" {
                let tast, artifact = compileSource "GenericHd" hdSrc
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "7" "Cons(7, Nil) head is 7 via a generic static method"
            }

            test "a recursive generic function self-calls via MethodSpec (prints 3)" {
                let tast, artifact = compileSource "GenericLastOr" lastOrSrc
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "3" "lastOr recurses through the list to the last element"
            }

            test "a generic recursive `foldl<'State,'T>` over a generic union folds to 15 (generic static method)" {
                let tast, artifact = compileSource "GenericFoldl" foldlSrc
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Trim())
                    "15"
                    "foldl (+) 0 [1..5] = 15 over our own generic Lst<'T>, via a generic static method"
            }

            test "the generic static method reflects as a 2-typar generic method" {
                let _, artifact = compileSource "GenericFoldlShape" foldlSrc
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
