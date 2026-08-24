module XParsec.FSharp.Codegen.Clr.Tests.UnionTests

open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

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
            // The emitted case carries field handles, not field types, so the `box` for
            // `Wrap 42` has to be synthesised upstream of codegen or the `:?> int` faults.
            test "union case with an obj field boxes a value-type construction arg" {
                runs
                    "42"
                    (lines
                        [
                            "type Boxed ="
                            "    | Wrap of obj"
                            "let v = Wrap 42"
                            "let n = match v with | Wrap o -> (o :?> int)"
                            "printfn \"%d\" n"
                        ])
            }

            test "monomorphic union + match analyses clean and surfaces TTypeKind.Union" {
                let tast = analyse unionSrc
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let unions =
                    let acc = ResizeArray<string * EqArray<TUnionCase>>()

                    for d in tast.Decls do
                        match d with
                        | TDecl.Type td ->
                            match td.Kind with
                            | TTypeKind.Union u -> acc.Add(td.Name, u.Cases)
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
                let artifact = compileSource "UnionHead" unionSrc
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "7" "Cons head extracted by the `| Cons(h, _)` arm"
            }

            test "a `match` arm selects the Nil case of an emitted union (prints 0)" {
                let src =
                    "type Lst =\n    | Nil\n    | Cons of int * Lst\nlet n =\n    match Nil with\n    | Nil -> 0\n    | Cons(h, _) -> h\nprintfn \"%d\" n"

                let artifact = compileSource "UnionNil" src
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
                        | TDecl.Let(TPat.NamedSimple(k, _, _), TExpr.Lambda _, _, _) -> k
                        | _ -> failwith "unreachable"
                    )

                Expect.isTrue fnKey.IsSome "sumList binds a lambda value"
            }

            test "recursive `sumList` folds a 3-element list (prints 6)" {
                let artifact = compileSource "UnionRecursion" recSrc
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "6" "sumList [1;2;3] = 6 via self-recursive static call"
            }

            test "nested Cons deconstructs the tail (prints the second element)" {
                let src =
                    "type Lst =\n    | Nil\n    | Cons of int * Lst\nlet second =\n    match Cons(10, Cons(20, Nil)) with\n    | Cons(_, Cons(y, _)) -> y\n    | _ -> -1\nprintfn \"%d\" second"

                let artifact = compileSource "UnionNested" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "20" "the nested `Cons(_, Cons(y, _))` bound the tail's head"
            }

            // The fold is a concrete `sum` rather than a higher-order `fold` because a
            // function parameter would pin FSharp.Core and break the BCL-only assertion.
            test "a union + recursive module fold ships as a BCL-only library DLL" {
                let src =
                    "namespace Vesper.Collections\n\ntype IntList =\n    | Empty\n    | Cons of int * IntList\n\nlet rec sum xs =\n    match xs with\n    | Empty -> 0\n    | Cons(h, t) -> h + sum t"

                let project = ProjectInfo.library "Vesper.Collections"
                let artifact = compileSourceTo project src

                expectNoFSharpCore artifact "the union + concrete fold"

                // A second load of the same bytes is a DIFFERENT assembly, so everything
                // reflected and constructed below has to come from this one `asm`.
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

                // `sum` sits directly under `namespace Vesper.Collections` with no module
                // of its own, so it emits on the Program class under its source name.
                let sumM = programClassMethodsOf asm

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

            test "union augmentation members surface on TTypeKind.Union" {
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
                        | TDecl.Type { Kind = TTypeKind.Union u } -> ValueSome u.Members
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

            test "consume union members at runtime: properties, recursion, statics" {
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

                let artifact = compileSource "UnionMembers" src

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

            test "an instance member is emitted as a real method (get_IsEmpty) on the union" {
                let artifact = compileSource "UnionMemberMeta" memberUnionSrc
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
                let artifact = compileSource "GenUnion" genUnionSrc

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Trim())
                    "6"
                    "Cons(1, Cons(2, Cons(3, Nil))) folds to 6 over our own generic Lst<'T>"
            }

            test "a generic List<'T> union compiles to a BCL-only library DLL with generic factories" {
                let src =
                    lines
                        [
                            "namespace Vesper.Collections"
                            ""
                            "type List<'T> ="
                            "    | ([]): List<'T>"
                            "    | (::): Head: 'T * Tail: List<'T> -> List<'T>"
                        ]

                // This source declares `Vesper.Collections.List`, which the mounted
                // `Vesper.List` contract also publishes, so it must be analysed under THAT
                // assembly name; under any other it is a type a reference already claims.
                let project = ProjectInfo.library "Vesper.List"

                let artifact = compileSourceTo project src

                expectNoFSharpCore artifact "the generic union (int / 'T / self fields)"

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

                // The `Head` field is the type's `!0`, so on `List<int>` it reads as `int`.
                let headField = listOfInt.GetField "Cons_0"
                Expect.isNotNull headField "List<int> has the Cons_0 (Head) field"
                Expect.equal (headField.GetValue one :?> int) 1 "Cons_0 holds the head value 1"
            }

            // Members of a generic union carry `!0` in their signatures, so each access on
            // `Lst<int>` needs a `MemberRef` on the instantiated `Lst<int>::get_Head`.
            test "generic union instance members run at runtime: chained Head/Tail, recursive Length" {
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

                let artifact = compileSource "GenMembers" src

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

            test "a generic union with members compiles to a BCL-only library DLL; members reflect + run" {
                let src = "namespace Vesper.Collections\n\n" + genMemberSrc

                let project = ProjectInfo.library "Vesper.Collections.GenMembers"
                let artifact = compileSourceTo project src

                expectNoFSharpCore artifact "the generic union + instance members"

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

            test "a generic match over a generic union returns the head (prints 7)" {
                let artifact = compileSource "GenericHd" hdSrc

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "7" "Cons(7, Nil) head is 7 via a generic static method"
            }

            test "a recursive generic function self-calls via MethodSpec (prints 3)" {
                let artifact = compileSource "GenericLastOr" lastOrSrc

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "3" "lastOr recurses through the list to the last element"
            }

            test "a generic recursive `foldl<'State,'T>` over a generic union folds to 15 (generic static method)" {
                let artifact = compileSource "GenericFoldl" foldlSrc

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Trim())
                    "15"
                    "foldl (+) 0 [1..5] = 15 over our own generic Lst<'T>, via a generic static method"
            }

            test "the generic static method reflects as a 2-typar generic method" {
                let artifact = compileSource "GenericFoldlShape" foldlSrc
                let asm = loadAssembly (Codegen.toBytes artifact)

                let foldl =
                    asm.GetTypes()
                    |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.Static))
                    |> Array.tryFind (fun m -> m.IsGenericMethodDefinition && m.GetGenericArguments().Length = 2)

                match foldl with
                | None -> failtest "no 2-typar generic static method was emitted"
                | Some m -> Expect.equal (m.GetGenericArguments().Length) 2 "foldl has two generic parameters"
            }

            // The impl matches on `this`, so the slot has to reach the union's own tag,
            // and a mis-wired vtable slot faults at the dispatch rather than compiling.
            test "a union implementing a local interface dispatches through the interface slot (prints 7 then 0)" {
                let src =
                    lines
                        [
                            "type IRank ="
                            "    abstract member Rank : unit -> int"
                            "type V ="
                            "    | Lo"
                            "    | Hi of int"
                            "    interface IRank with"
                            "        member this.Rank() = match this with | Lo -> 0 | Hi n -> n"
                            "let hi = Hi 7"
                            "let lo = Lo"
                            "printfn \"%d\" ((hi :> IRank).Rank())"
                            "printfn \"%d\" ((lo :> IRank).Rank())"
                        ]

                let artifact = compileSource "UnionIfaceRank" src

                let bytes = Codegen.toBytes artifact

                let asm = loadAssembly bytes
                let ty = asm.GetType "V"
                Expect.isNotNull ty "the assembly contains the union type V"
                let ifaceNames = ty.GetInterfaces() |> Array.map (fun i -> i.Name) |> Set.ofArray
                Expect.isTrue (ifaceNames.Contains "IRank") "V reflects as implementing the user IRank"

                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"

                let outLines =
                    output.Replace("\r", "").Split('\n') |> Array.filter (fun s -> s.Length > 0)

                Expect.equal
                    outLines
                    [| "7"; "0" |]
                    "Hi 7 ranks 7 (reads `this`) and Lo ranks 0, both via interface dispatch"
            }

            // The union also synthesises `IEquatable<V>`, so the authored impl and the
            // synthesised one must land on the same type without colliding slots.
            test "a union's user interface coexists with its synthesised IEquatable (no slot collision)" {
                let src =
                    lines
                        [
                            "type IRank ="
                            "    abstract member Rank : unit -> int"
                            "type V ="
                            "    | Lo"
                            "    | Hi of int"
                            "    interface IRank with"
                            "        member this.Rank() = match this with | Lo -> 0 | Hi n -> n"
                            "let eq = (Hi 7 = Hi 7)"
                            "let ne = (Hi 7 = Lo)"
                            "printfn \"%b\" eq"
                            "printfn \"%b\" ne"
                            "printfn \"%d\" ((Hi 5 :> IRank).Rank())"
                        ]

                let artifact = compileSource "UnionIfaceAndEq" src

                let bytes = Codegen.toBytes artifact
                let asm = loadAssembly bytes
                let ty = asm.GetType "V"
                Expect.isNotNull ty "the assembly contains the union type V"

                let ifaceNames = ty.GetInterfaces() |> Array.map (fun i -> i.Name) |> Set.ofArray
                Expect.isTrue (ifaceNames.Contains "IRank") "V reflects as implementing the user IRank"

                Expect.isTrue
                    (ifaceNames.Contains "IEquatable`1")
                    "V reflects as implementing the synthesised IEquatable<V>"

                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"

                let outLines =
                    output.Replace("\r", "").Split('\n') |> Array.filter (fun s -> s.Length > 0)

                Expect.equal
                    outLines
                    [| "true"; "false"; "5" |]
                    "structural `=` (synthesised IEquatable) and `Rank()` (user IRank) both dispatch correctly"
            }
        ]
