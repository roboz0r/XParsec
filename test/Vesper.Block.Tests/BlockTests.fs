module Vesper.BlockTests.BlockTests

open System
open Expecto
open Vesper

/// The two tags exist only to be distinct. Mixing them — `rows.[0<col>]`, `rows.[0]`, or
/// passing a `BlockM<_, row>` where a `BlockM<_, col>` is expected — is a compile error, so
/// the separation is pinned by the code below type-checking, not by an assertion.
[<Measure>]
type row

[<Measure>]
type col

/// Neither structurally equatable nor comparable, so a `Block` operation that imposed
/// `'T: equality` or `'T: comparison` on its caller would not compile against it.
[<NoEquality; NoComparison>]
type Opaque = { Tag: int }

let private abc: Block<string> = Block.ofArray [| "a"; "b"; "c" |]

[<Tests>]
let uninitialised =
    testList
        "an uninitialised block"
        [
            test "is empty" {
                let d = Unchecked.defaultof<Block<int>>
                Expect.isTrue d.IsEmpty "IsEmpty"
                Expect.equal d.Length 0 "Length"
            }

            test "equals the empty block, and hashes with it" {
                let d = Unchecked.defaultof<Block<int>>
                Expect.equal d Block.empty "equal"
                Expect.equal (hash d) (hash Block.empty) "hash"
                Expect.equal (compare d Block.empty) 0 "compare"
            }

            test "enumerates no elements and converts to empty" {
                let d = Unchecked.defaultof<Block<int>>
                let mutable n = 0

                for _ in d do
                    n <- n + 1

                Expect.equal n 0 "enumerated"
                Expect.equal (Block.toArray d) [||] "toArray"
                Expect.equal (Block.toList d) [] "toList"
                Expect.equal (d.AsSpan().Length) 0 "AsSpan"
            }

            test "maps and folds as empty" {
                let d = Unchecked.defaultof<Block<int>>
                Expect.equal (Block.map string d) Block.empty "map"
                Expect.equal (Block.fold (+) 0 d) 0 "fold"
                Expect.equal (Block.tryItem 0 d) ValueNone "tryItem"
                Expect.equal (Block.tryLast d) ValueNone "tryLast"
            }
        ]

[<Tests>]
let equality =
    testList
        "equality"
        [
            test "is element-wise, not by reference" {
                let a: Block<string> = Block.ofArray [| "a"; "b" |]
                let b: Block<string> = Block.ofList [ "a"; "b" ]
                Expect.equal a b "equal contents"
                Expect.equal (hash a) (hash b) "equal hashes"
            }

            test "distinguishes contents, order and length" {
                Expect.notEqual abc (Block.ofArray [| "a"; "b"; "z" |]) "contents"
                Expect.notEqual abc (Block.ofArray [| "b"; "a"; "c" |]) "order"
                Expect.notEqual abc (Block.ofArray [| "a"; "b" |]) "length"
            }

            test "needs no equality constraint on the element" {
                let xs: Block<Opaque> = Block.ofArray [| { Tag = 1 } |]
                Expect.isFalse (Block.contains { Tag = 1 } xs) "reference equality, so a fresh value misses"
                Expect.equal (Block.distinct xs) xs "distinct"

                // Compile-only: `sort` and `=` must not impose `comparison` or `equality` either.
                let _sorts: Block<Opaque> -> Block<Opaque> = Block.sort
                Expect.equal xs xs "equal to itself"
            }
        ]

[<Tests>]
let comparison =
    testList
        "comparison"
        [
            test "is element-wise over the shorter length, then by length" {
                let ab: Block<string> = Block.ofArray [| "a"; "b" |]
                Expect.isTrue (compare ab abc < 0) "prefix orders first"
                Expect.isTrue (compare abc (Block.ofArray [| "a"; "c" |]) < 0) "element wins over length"
                Expect.equal (compare abc abc) 0 "equal"
            }

            test "sorts a block of blocks" {
                let sorted = [ abc; Block.empty; Block.ofArray [| "a" |] ] |> List.sort

                Expect.equal sorted [ Block.empty; Block.ofArray [| "a" |]; abc ] "sorted"
            }
        ]

[<Tests>]
let ownership =
    testList
        "backing array ownership"
        [
            test "ofArray copies" {
                let src = [| 1; 2; 3 |]
                let b: Block<int> = Block.ofArray src
                src.[0] <- 99
                Expect.equal (Block.toArray b) [| 1; 2; 3 |] "unaffected"
            }

            test "toArray copies" {
                let b: Block<int> = Block.ofArray [| 1; 2; 3 |]
                let out = Block.toArray b
                out.[0] <- 99
                Expect.equal b.[0] 1 "unaffected"
            }

            test "unsafeOfArray takes ownership" {
                let src = [| 1; 2; 3 |]
                let b: Block<int> = Block.unsafeOfArray src
                src.[0] <- 99
                Expect.equal b.[0] 99 "the caller's mutation is visible"
            }
        ]

[<Tests>]
let tagged =
    testList
        "a tagged block"
        [
            test "reports a tagged length and hands out tagged indexes" {
                let rows: BlockM<string, row> = Block.init 3<row> (fun i -> sprintf "r%d" (int i))
                Expect.equal rows.Length 3<row> "Length"
                Expect.equal rows.[2<row>] "r2" "Item"
                Expect.equal (Block.tryFindIndex ((=) "r1") rows) (ValueSome 1<row>) "tryFindIndex"
                Expect.equal (Block.tryItem 5<row> rows) ValueNone "tryItem out of range"
            }

            test "carries its tag through map and mapi" {
                let rows: BlockM<int, row> = Block.init 3<row> (fun i -> int i)
                let doubled: BlockM<int, row> = Block.map ((*) 2) rows
                Expect.equal doubled.[2<row>] 4 "map"

                let paired: BlockM<string, row> =
                    Block.mapi (fun i x -> sprintf "%d:%d" (int i) x) rows

                Expect.equal paired.[1<row>] "1:1" "mapi"
            }

            test "compares equal to a same-tagged block of the same contents" {
                let a: BlockM<int, col> = Block.ofArray [| 1; 2 |]
                let b: BlockM<int, col> = Block.ofList [ 1; 2 ]
                Expect.equal a b "equal"
            }

            test "an untagged block indexes by a plain int" {
                Expect.equal abc.[0] "a" "Item"
                Expect.equal (abc.Length - 1) 2 "Length is an int"
                Expect.equal (Array.init abc.Length (fun i -> abc.[i])) [| "a"; "b"; "c" |] "int round trip"
            }
        ]

[<Tests>]
let mapPreserve =
    testList
        "mapPreserve"
        [
            test "returns ValueNone when every element maps reference-equal" {
                let xs: Block<string> = Block.ofArray [| "a"; "b"; "c" |]
                Expect.equal (Block.mapPreserve id xs) ValueNone "unchanged"
            }

            test "carries the unchanged prefix when one element changes" {
                let xs: Block<string> = Block.ofArray [| "a"; "b"; "c" |]
                let mapped = Block.mapPreserve (fun s -> if s = "b" then "B" else s) xs

                match mapped with
                | ValueNone -> failtest "expected a mapped block"
                | ValueSome ys ->
                    Expect.equal (Block.toArray ys) [| "a"; "B"; "c" |] "contents"
                    Expect.isTrue (Object.ReferenceEquals(ys.[0], xs.[0])) "prefix element preserved"
                    Expect.isTrue (Object.ReferenceEquals(ys.[2], xs.[2])) "suffix element preserved"
            }

            test "is ValueNone for an empty block" {
                Expect.equal (Block.mapPreserve id (Block.empty: Block<string>)) ValueNone "empty"
            }
        ]

[<Tests>]
let moduleFunctions =
    testList
        "module functions"
        [
            test "forall2 returns false on a length mismatch rather than throwing" {
                let a: Block<int> = Block.ofArray [| 1; 2; 3 |]
                let b: Block<int> = Block.ofArray [| 1; 2 |]
                Expect.isFalse (Block.forall2 (=) a b) "mismatch"
                Expect.isTrue (Block.forall2 (=) a a) "match"
            }

            test "forall2 relates differently tagged blocks" {
                let a: BlockM<int, row> = Block.ofArray [| 1; 2 |]
                let b: BlockM<int, col> = Block.ofArray [| 1; 2 |]
                Expect.isTrue (Block.forall2 (=) a b) "equal contents"
            }

            test "append and truncate return the same block when nothing changes" {
                let a: Block<int> = Block.ofArray [| 1; 2 |]
                Expect.equal (Block.append a Block.empty) a "append empty"
                Expect.equal (Block.append Block.empty a) a "empty append"
                Expect.equal (Block.append a a) (Block.ofArray [| 1; 2; 1; 2 |]) "append"
                Expect.equal (Block.truncate 5 a) a "truncate beyond the end"
                Expect.equal (Block.truncate 1 a) (Block.ofArray [| 1 |]) "truncate"
                Expect.equal (Block.truncate -1 a) Block.empty "negative truncate"
            }

            test "concat joins the parts in order, skipping empty ones" {
                let parts: Block<Block<int>> =
                    Block.ofArray [| Block.ofArray [| 1; 2 |]; Block.empty; Block.ofArray [| 3 |]; Block.empty |]

                Expect.equal (Block.concat parts) (Block.ofArray [| 1; 2; 3 |]) "order"
                Expect.equal (Block.concat (Block.empty: Block<Block<int>>)) Block.empty "no parts"

                Expect.equal
                    (Block.concat (Block.ofArray [| (Block.empty: Block<int>); Block.empty |]))
                    Block.empty
                    "every part empty"
            }

            test "concat returns the same block when one part holds everything" {
                let only: Block<int> = Block.ofArray [| 1; 2 |]
                Expect.equal (Block.concat (Block.singleton only)) only "one part"
            }

            test "concat takes the inner blocks' index tag" {
                let parts: BlockM<BlockM<int, col>, row> =
                    Block.ofArray [| Block.ofArray [| 1 |]; Block.ofArray [| 2 |] |]

                let joined: BlockM<int, col> = Block.concat parts
                Expect.equal joined.[1<col>] 2 "indexed by the inner tag"
            }

            test "collect joins the mapped results in order" {
                let xs: Block<int> = Block.ofArray [| 1; 2; 3 |]

                Expect.equal
                    (Block.collect (fun x -> Block.ofArray (Array.replicate x x)) xs)
                    (Block.ofArray [| 1; 2; 2; 3; 3; 3 |])
                    "order"

                Expect.equal (Block.collect (fun _ -> (Block.empty: Block<int>)) xs) Block.empty "every result empty"
                Expect.equal (Block.collect Block.singleton (Block.empty: Block<int>)) Block.empty "no elements"
            }

            test "collect over one element returns that result" {
                let only: Block<int> = Block.ofArray [| 7; 8 |]
                Expect.equal (Block.collect (fun _ -> only) (Block.singleton 0)) only "one element"
            }

            test "sumBy carries the projection's measure and is zero when empty" {
                let xs: Block<int> = Block.ofArray [| 1; 2; 3 |]
                let total: int<row> = xs |> Block.sumBy (fun x -> x * 1<row>)
                Expect.equal total 6<row> "tagged sum"
                Expect.equal (Block.sumBy id (Block.empty: Block<int>)) 0 "empty"
            }

            test "distinct keeps the first occurrence" {
                let xs: Block<string> = Block.ofArray [| "b"; "a"; "b"; "c"; "a" |]
                Expect.equal (Block.toArray (Block.distinct xs)) [| "b"; "a"; "c" |] "order"
            }

            test "sort is ascending" {
                let xs: Block<int> = Block.ofArray [| 3; 1; 2 |]
                Expect.equal (Block.sort xs) (Block.ofArray [| 1; 2; 3 |]) "sorted"
                Expect.equal (Block.toArray xs) [| 3; 1; 2 |] "input untouched"
            }

            test "tryOfSeq is all-or-nothing and stops at the first ValueNone" {
                Expect.equal (Block.tryOfSeq [ ValueSome 1; ValueSome 2 ]) (ValueSome(Block.ofArray [| 1; 2 |])) "all"
                Expect.equal (Block.tryOfSeq ([]: int voption list)) (ValueSome Block.empty) "empty"

                let visited = ResizeArray<int>()

                let items =
                    seq {
                        for i in 1..4 do
                            visited.Add i
                            if i = 2 then ValueNone else ValueSome i
                    }

                Expect.equal (Block.tryOfSeq items) ValueNone "one ValueNone fails the whole"
                Expect.equal (List.ofSeq visited) [ 1; 2 ] "the elements after the ValueNone are unvisited"
            }

            test "tryMap is all-or-nothing" {
                let xs: Block<int> = Block.ofArray [| 1; 2; 3 |]

                let even n =
                    if n % 2 = 0 then ValueSome(n / 2) else ValueNone

                Expect.equal
                    (Block.tryMap (fun n -> ValueSome(n * 10)) xs)
                    (ValueSome(Block.ofArray [| 10; 20; 30 |]))
                    "all"

                Expect.equal (Block.tryMap even xs) ValueNone "one ValueNone fails the whole"
            }

            test "last and tryLast" {
                Expect.equal (Block.last abc) "c" "last"
                Expect.equal (Block.tryLast abc) (ValueSome "c") "tryLast"
                Expect.throws (fun () -> Block.last (Block.empty: Block<int>) |> ignore) "last of empty"
            }

            test "fold and foldBack visit in opposite directions" {
                Expect.equal (Block.fold (fun acc s -> acc + s) "" abc) "abc" "fold"
                Expect.equal (Block.foldBack (fun s acc -> acc + s) abc "") "cba" "foldBack"
            }

            test "iteri hands out a tagged index" {
                let rows: BlockM<string, row> = Block.ofArray [| "x"; "y" |]
                let seen = ResizeArray<int>()
                Block.iteri (fun (i: int<row>) _ -> seen.Add(int i)) rows
                Expect.equal (List.ofSeq seen) [ 0; 1 ] "indexes"
            }

            test "filter, exists, tryFind" {
                let xs: Block<int> = Block.ofArray [| 1; 2; 3; 4 |]
                Expect.equal (Block.filter (fun x -> x % 2 = 0) xs) (Block.ofArray [| 2; 4 |]) "filter"
                Expect.isTrue (Block.exists ((=) 3) xs) "exists"
                Expect.isFalse (Block.forall ((<) 1) xs) "forall"
                Expect.equal (Block.tryFind (fun x -> x > 2) xs) (ValueSome 3) "tryFind"
                Expect.equal (Block.tryFind (fun x -> x > 9) xs) ValueNone "tryFind misses"
            }

            test "ofSeq, ofList, ofResizeArray, singleton agree" {
                let expected: Block<int> = Block.ofArray [| 1; 2 |]

                Expect.equal
                    (Block.ofSeq (
                        seq {
                            1
                            2
                        }
                    ))
                    expected
                    "ofSeq"

                Expect.equal (Block.ofList [ 1; 2 ]) expected "ofList"
                Expect.equal (Block.ofResizeArray (ResizeArray [ 1; 2 ])) expected "ofResizeArray"
                Expect.equal (Block.singleton 1) (Block.ofArray [| 1 |]) "singleton"
            }
        ]

[<Tests>]
let patterns =
    testList
        "fixed-arity patterns"
        [
            test "match by arity" {
                let describe (xs: Block<int>) =
                    match xs with
                    | BlockEmpty -> "empty"
                    | BlockOne a -> sprintf "one %d" a
                    | BlockTwo(a, b) -> sprintf "two %d %d" a b
                    | BlockThree(a, b, c) -> sprintf "three %d %d %d" a b c
                    | _ -> "many"

                Expect.equal (describe Block.empty) "empty" "empty"
                Expect.equal (describe (Block.ofArray [| 1 |])) "one 1" "one"
                Expect.equal (describe (Block.ofArray [| 1; 2 |])) "two 1 2" "two"
                Expect.equal (describe (Block.ofArray [| 1; 2; 3 |])) "three 1 2 3" "three"
                Expect.equal (describe (Block.ofArray [| 1; 2; 3; 4 |])) "many" "many"
            }

            test "match a tagged block without naming its tag" {
                let rows: BlockM<int, row> = Block.ofArray [| 7 |]

                match rows with
                | BlockOne x -> Expect.equal x 7 "bound"
                | _ -> failtest "expected one element"
            }
        ]

[<Tests>]
let printing =
    test "ToString names the type and lists the elements" {
        Expect.equal (string abc) "Block [\"a\"; \"b\"; \"c\"]" "ToString"
        Expect.equal (string (Unchecked.defaultof<Block<int>>)) "Block []" "empty"
    }
