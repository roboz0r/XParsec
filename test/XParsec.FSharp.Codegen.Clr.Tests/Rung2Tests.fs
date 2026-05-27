module XParsec.FSharp.Codegen.Clr.Tests.Rung2Tests

open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Rung 2 of the self-hosting ladder (docs/self-host-rung2-plan.md): the backend
// expression compiler — `if`/`then`/`else`, `match`, recursion, and our-own
// union construction + deconstruction.

[<Tests>]
let tests =
    testList
        "Rung2"
        [
            test "`if true then 1 else 2` takes the then-branch (prints 1)" {
                let _, artifact =
                    compileSource "Rung2IfTrue" "printfn \"%d\" (if true then 1 else 2)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "1" "then-branch value"
            }

            test "`if false then 1 else 2` takes the else-branch (prints 2)" {
                let _, artifact =
                    compileSource "Rung2IfFalse" "printfn \"%d\" (if false then 1 else 2)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "2" "else-branch value"
            }

            test "`match` on an int literal arm hits the matching case" {
                let _, artifact =
                    compileSource "Rung2MatchConst" "printfn \"%d\" (match 1 with | 0 -> 10 | 1 -> 20 | _ -> 30)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "20" "the `| 1 ->` arm matched"
            }

            test "`match` falls through literal arms to the wildcard default" {
                let _, artifact =
                    compileSource "Rung2MatchWildcard" "printfn \"%d\" (match 7 with | 0 -> 10 | 1 -> 20 | _ -> 30)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "30" "no literal matched, the `_` default ran"
            }

            test "`match` binds a named pattern and uses it in the body" {
                let _, artifact =
                    compileSource "Rung2MatchNamed" "printfn \"%d\" (match 5 with | 0 -> 100 | n -> n + 1)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "6" "the named arm bound 5 and computed 5 + 1"
            }

            let unionSrc =
                "type Lst =\n    | Nil\n    | Cons of int * Lst\nlet head =\n    match Cons(7, Nil) with\n    | Nil -> 0\n    | Cons(h, _) -> h\nprintfn \"%d\" head"

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
                let _, artifact = compileSource "Rung2UnionHead" unionSrc
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "7" "Cons head extracted by the `| Cons(h, _)` arm"
            }

            test "a `match` arm selects the Nil case of an emitted union (prints 0)" {
                let src =
                    "type Lst =\n    | Nil\n    | Cons of int * Lst\nlet n =\n    match Nil with\n    | Nil -> 0\n    | Cons(h, _) -> h\nprintfn \"%d\" n"

                let _, artifact = compileSource "Rung2UnionNil" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "0" "the Nil arm matched on a constructed Nil"
            }

            let recSrc =
                "type Lst =\n    | Nil\n    | Cons of int * Lst\nlet rec sumList xs =\n    match xs with\n    | Nil -> 0\n    | Cons(h, t) -> h + sumList t\nprintfn \"%d\" (sumList (Cons(1, Cons(2, Cons(3, Nil)))))"

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
                let _, artifact = compileSource "Rung2Recursion" recSrc
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "6" "sumList [1;2;3] = 6 via self-recursive static call"
            }

            test "nested Cons deconstructs the tail (prints the second element)" {
                let src =
                    "type Lst =\n    | Nil\n    | Cons of int * Lst\nlet second =\n    match Cons(10, Cons(20, Nil)) with\n    | Cons(_, Cons(y, _)) -> y\n    | _ -> -1\nprintfn \"%d\" second"

                let _, artifact = compileSource "Rung2UnionNested" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "20" "the nested `Cons(_, Cons(y, _))` bound the tail's head"
            }

            let staticFnMethods (bytes: byte[]) : MethodInfo[] =
                let asm = loadAssembly bytes

                asm.GetTypes()
                |> Array.collect (fun t ->
                    t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
                )
                |> Array.filter (fun m -> m.Name.StartsWith "fn$")

            test "a top-level function is emitted as a static method, called directly (prints 42)" {
                let _, artifact =
                    compileSource "Rung2bStatic" "let twice x = x + x\nprintfn \"%d\" (twice 21)"

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "42" "twice 21 = 42 via a direct static call"

                // G8: a nil `ParamList` would throw `BadImageFormatException` on
                // `GetParameters`, so the reflection round-trip guards it.
                match staticFnMethods bytes with
                | [| m |] ->
                    Expect.isTrue m.IsStatic "emitted as a static method"
                    Expect.equal (m.GetParameters().Length) 1 "one real Param row (G8)"
                | other -> failtestf "expected one static fn, got %A" (other |> Array.map (fun m -> m.Name))
            }

            test "a recursive function recurses via a direct static call (prints 15)" {
                let src =
                    "let rec sumTo n =\n    match n with\n    | 0 -> 0\n    | _ -> n + sumTo (n - 1)\nprintfn \"%d\" (sumTo 5)"

                let _, artifact = compileSource "Rung2bRec" src
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "15" "sumTo 5 = 5+4+3+2+1+0 via a self-recursive static call"
                Expect.equal (staticFnMethods bytes).Length 1 "sumTo is the one static method (no closure)"
            }

            test "one static method calls another by a direct call (prints 13)" {
                let src =
                    "let inc x = x + 1\nlet add3 x = inc (inc (inc x))\nprintfn \"%d\" (add3 10)"

                let _, artifact = compileSource "Rung2bCross" src
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "13" "add3 10 = inc(inc(inc 10)) = 13"
                Expect.equal (staticFnMethods bytes).Length 2 "both inc and add3 are static methods"
            }

            test "a capturing function stays a closure, not a static method (prints 15)" {
                let src = "let n = 10\nlet addN x = x + n\nprintfn \"%d\" (addN 5)"
                let _, artifact = compileSource "Rung2bCapture" src
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "15" "addN 5 = 15 via a capturing closure"
                Expect.isEmpty (staticFnMethods bytes) "addN captures n, so it stays a closure (no static fn)"

                let asm = loadAssembly bytes

                let hasClosure =
                    asm.GetTypes() |> Array.exists (fun t -> t.Name.StartsWith "<closure>")

                Expect.isTrue hasClosure "a closure type was emitted for the capturing addN"
            }

            // R3 deferred: a named `module M = …` now compiles to an `M` holder type
            // carrying its functions under their *source* names (no `fn$` mangling,
            // not on the "Program" holder) — exercised here in an executable.
            let moduleStaticMethod (bytes: byte[]) (holder: string) (name: string) : MethodInfo =
                let asm = loadAssembly bytes

                match asm.GetTypes() |> Array.tryFind (fun t -> t.FullName = holder) with
                | None -> failtestf "no `%s` holder type emitted for the nested module" holder
                | Some t ->
                    match t.GetMethod(name, BindingFlags.Public ||| BindingFlags.Static) with
                    | null -> failtestf "`%s` is not a public static method on the `%s` holder" name holder
                    | m -> m

            test "a function inside a nested module compiles + runs as a static method on its holder (prints 42)" {
                let src =
                    "let start = 0\nmodule M =\n    let twice x = x + x\nprintfn \"%d\" (twice 21)"

                let _, artifact = compileSource "Rung2dNestedMod" src
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "42" "twice 21 = 42 — the nested-module function ran"

                let twice = moduleStaticMethod bytes "M" "twice"
                Expect.isTrue twice.IsStatic "twice is a static method on the M holder"
                Expect.isEmpty (staticFnMethods bytes) "carries its source name `twice`, not an anonymous `fn$`"
            }

            test "a recursive function inside a nested module recurses (prints 15)" {
                let src =
                    "module M =\n    let rec sumTo n =\n        match n with\n        | 0 -> 0\n        | _ -> n + sumTo (n - 1)\nprintfn \"%d\" (sumTo 5)"

                let _, artifact = compileSource "Rung2dNestedRec" src
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "15" "sumTo 5 = 15 via a self-recursive static call from a nested module"

                let sumTo = moduleStaticMethod bytes "M" "sumTo"
                Expect.isTrue sumTo.IsStatic "sumTo is a static method on the M holder"
                Expect.isEmpty (staticFnMethods bytes) "carries its source name `sumTo`, not an anonymous `fn$`"
            }

            test "a recursive static-method program runs as a standalone `dotnet <dll>` app (prints 15)" {
                let outDir = tmpDir "rung2b-static-app"
                let project = ProjectInfo.app "Rung2bStaticApp" outDir

                let src =
                    "let rec sumTo n =\n    match n with\n    | 0 -> 0\n    | _ -> n + sumTo (n - 1)\nprintfn \"%d\" (sumTo 5)"

                let artifact = compileSourceTo project src
                Codegen.materialiseApp project artifact

                let dllPath = System.IO.Path.Combine(outDir, "Rung2bStaticApp.dll")
                let exitCode, output = runOnDisk dllPath

                Expect.equal exitCode 0 (sprintf "dotnet exits 0 (output was: %s)" output)
                Expect.equal (output.Trim()) "15" "the recursive static method runs as a real assembly"
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

            let memberUnionSrc =
                String.concat
                    "\n"
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
                    + String.concat
                        "\n"
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

                let tast, artifact = compileSource "Rung2dMembers" src

                Expect.isEmpty
                    tast.Diagnostics
                    (sprintf "no diagnostics: %A" (tast.Diagnostics |> List.map (fun d -> d.Message)))

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                let lines =
                    output.Split('\n')
                    |> Array.map (fun s -> s.Trim())
                    |> Array.filter (fun s -> s.Length > 0)

                Expect.equal
                    lines
                    [| "false"; "10"; "3"; "true"; "7" |]
                    "xs.IsEmpty=false, xs.Head=10, xs.Length=3, Lst.Empty.IsEmpty=true, (Lst.Single 7).Head=7"
            }

            test "an instance member is emitted as a real method (get_IsEmpty) on the union (P3d.3)" {
                let _, artifact = compileSource "Rung2dMemberMeta" memberUnionSrc
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

            let genUnionSrc =
                String.concat
                    "\n"
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

            test "a generic union constructs + match-deconstructs at runtime via a recursive fold (prints 6)" {
                let tast, artifact = compileSource "Rung2dGenUnion" genUnionSrc

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

            let listLitSrc =
                String.concat
                    "\n"
                    [
                        "type List<'T> ="
                        "    | ([]): List<'T>"
                        "    | (::): Head: 'T * Tail: List<'T> -> List<'T>"
                        "and 'T list = List<'T>"
                        "let rec sum xs ="
                        "    match xs with"
                        "    | Empty -> 0"
                        "    | Cons(h, t) -> h + sum t"
                        "printfn \"%d\" (sum [1; 2; 3])"
                    ]

            test "`[1; 2; 3]` runs against the program's own declared list union (prints 6)" {
                let tast, artifact = compileSource "Rung2dListLit" listLitSrc

                Expect.isEmpty
                    tast.Diagnostics
                    (sprintf "no diagnostics: %A" (tast.Diagnostics |> List.map (fun d -> d.Message)))

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    "a `[1;2;3]` over our own list + a concrete printf references no FSharp.Core"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "6" "[1; 2; 3] built + folded over the program's own List<'T>"
            }

            test "a generic List<'T> union compiles to a BCL-only library DLL with generic factories (P3d.4)" {
                let src =
                    String.concat
                        "\n"
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

            // R2 piece 1: instance augmentation members on a *generic* union. The
            // member signatures + bodies carry the declaring-typar marker (`!0`),
            // and every member access on `Lst<int>` goes through a `MemberRef` on
            // the instantiated `TypeSpec` (`Lst<int>::get_Head`).
            let genMemberSrc =
                String.concat
                    "\n"
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

            test "generic union instance members run at runtime: chained Head/Tail, recursive Length (R2)" {
                let src =
                    genMemberSrc
                    + "\n"
                    + String.concat
                        "\n"
                        [
                            "let xs = Cons(10, Cons(20, Cons(30, Nil)))"
                            "printfn \"%b\" xs.IsEmpty"
                            "printfn \"%d\" xs.Head"
                            "printfn \"%d\" xs.Length"
                            "printfn \"%d\" xs.Tail.Head"
                        ]

                let tast, artifact = compileSource "Rung2GenMembers" src

                Expect.isEmpty
                    tast.Diagnostics
                    (sprintf "no diagnostics: %A" (tast.Diagnostics |> List.map (fun d -> d.Message)))

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                let lines =
                    output.Split('\n')
                    |> Array.map (fun s -> s.Trim())
                    |> Array.filter (fun s -> s.Length > 0)

                Expect.equal
                    lines
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
        ]
