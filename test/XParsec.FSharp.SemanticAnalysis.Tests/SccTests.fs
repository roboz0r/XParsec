module XParsec.FSharp.SemanticAnalysis.Tests.SccTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis

/// Component ids by Kosaraju's two-pass algorithm, an oracle independent of the
/// implementation under test. Recursive, so it serves the small graphs generated below.
let private kosaraju (adj: int[][]) : int[] =
    let n = adj.Length
    let reverse = Array.init n (fun _ -> ResizeArray<int>())

    for u in 0 .. n - 1 do
        for v in adj.[u] do
            reverse.[v].Add u

    let seen = Array.zeroCreate<bool> n
    let finished = ResizeArray<int>()

    let rec forward u =
        seen.[u] <- true

        for v in adj.[u] do
            if not seen.[v] then
                forward v

        finished.Add u

    for u in 0 .. n - 1 do
        if not seen.[u] then
            forward u

    let componentOf = Array.create n -1

    let rec backward u c =
        componentOf.[u] <- c

        for v in reverse.[u] do
            if componentOf.[v] < 0 then
                backward v c

    let mutable c = 0

    for i in finished.Count - 1 .. -1 .. 0 do
        let u = finished.[i]

        if componentOf.[u] < 0 then
            backward u c
            c <- c + 1

    componentOf

/// Component ids as a set of member sets, in a form two labellings of the same partition
/// share: members ascending, components ordered by their members.
let private partitionOfIds (componentOf: int[]) : int list list =
    componentOf
    |> Array.mapi (fun node c -> c, node)
    |> Array.groupBy fst
    |> Array.map (fun (_, nodes) -> nodes |> Array.map snd |> Array.toList |> List.sort)
    |> Array.toList
    |> List.sort

let private partitionOf (partition: SccPartition) : int list list =
    partition.Components
    |> Seq.map (fun c -> EqArray.toList c.Members)
    |> List.ofSeq
    |> List.sort

let private render (adj: int[][]) : string =
    adj
    |> Array.mapi (fun u vs -> $"""%d{u} -> [%s{vs |> Array.map string |> String.concat "; "}]""")
    |> String.concat ", "

/// Asserts the whole output contract against `adj`: the partition Kosaraju reports, the
/// `ComponentIndex` ↔ `Components` correspondence, reverse topological order, self-edges and
/// `isRecursive`.
let private checkGraph (adj: int[][]) : SccPartition =
    let n = adj.Length
    let g = Digraph.OfSuccessors(n, fun u -> adj.[u])
    let p = Scc.compute g
    let shape = render adj

    Expect.equal (partitionOf p) (partitionOfIds (kosaraju adj)) $"partition of %s{shape}"

    for u in 0 .. n - 1 do
        let c = p.ComponentIndex.[u]
        Expect.isTrue (c >= 0 && c < p.Components.Length) $"node %d{u} has a component in %s{shape}"

        Expect.equal (p.ComponentOf u) p.Components.[c] $"ComponentOf node %d{u} in %s{shape}"

        Expect.isTrue
            (p.Components.[c].Members |> Seq.contains u)
            $"node %d{u} is a member of its own component in %s{shape}"

        Expect.equal (g.HasSelfEdge u) (Array.contains u adj.[u]) $"self-edge of node %d{u} in %s{shape}"

        Expect.equal
            (Scc.isRecursive p u)
            (Array.contains u adj.[u] || p.Components.[c].Members.Length > 1)
            $"isRecursive of node %d{u} in %s{shape}"

        for v in adj.[u] do
            let target = p.ComponentIndex.[v]

            if target <> c then
                Expect.isLessThan target c $"edge %d{u} -> %d{v} runs to an earlier component in %s{shape}"

    Expect.equal
        (p.Components |> Seq.sumBy (fun c -> c.Members.Length))
        n
        $"every node appears in exactly one component in %s{shape}"

    p

let private randomGraph (rng: Random) (n: int) (density: float) : int[][] =
    Array.init
        n
        (fun _ ->
            [|
                for v in 0 .. n - 1 do
                    if rng.NextDouble() < density then
                        v
            |]
        )

[<Tests>]
let tests =
    testList
        "Scc"
        [
            test "an empty graph has no components" {
                let p = Scc.compute (Digraph.OfSuccessors(0, fun _ -> []))
                Expect.equal p.Components.Length 0 "no components"
            }

            test "a singleton without a self-edge is Acyclic" {
                let g = Digraph.OfSuccessors(1, fun _ -> [])
                let p = Scc.compute g
                Expect.equal p.Components.[0] (Acyclic 0) "one acyclic component"
                Expect.isFalse (g.HasSelfEdge 0) "no self-edge"
                Expect.isFalse (Scc.isRecursive p 0) "not recursive"
            }

            test "a self-edge makes a singleton a Cycle" {
                let g = Digraph.OfSuccessors(1, fun _ -> [ 0 ])
                let p = Scc.compute g
                Expect.equal p.Components.[0] (Cycle(EqArray.ofList [ 0 ])) "one cyclic component"
                Expect.isTrue (g.HasSelfEdge 0) "self-edge"
                Expect.isTrue (Scc.isRecursive p 0) "recursive"
            }

            test "a two-cycle is one component with both members recursive" {
                let g = Digraph.OfSuccessors(2, fun u -> [ 1 - u ])
                let p = Scc.compute g
                Expect.equal p.Components.[0] (Cycle(EqArray.ofList [ 0; 1 ])) "one component of two members"
                Expect.isTrue (Scc.isRecursive p 0) "0 is recursive"
                Expect.isTrue (Scc.isRecursive p 1) "1 is recursive"
                Expect.isFalse (g.HasSelfEdge 0) "0 has no self-edge"
            }

            test "components come out in reverse topological order" {
                // 0 -> 1 -> 2, 1 -> 0: the singleton 2 precedes the cycle {0; 1}.
                let adj = [| [| 1 |]; [| 0; 2 |]; [||] |]
                let p = Scc.compute (Digraph.OfSuccessors(3, fun u -> adj.[u]))
                Expect.equal p.Components.[0] (Acyclic 2) "the sink component is first"
                Expect.equal p.Components.[1] (Cycle(EqArray.ofList [ 0; 1 ])) "the cycle is second"
                checkGraph adj |> ignore
            }

            test "successors are stored ascending and distinct" {
                let g = Digraph.OfSuccessors(3, fun u -> if u = 0 then [ 2; 0; 2; 1 ] else [])
                let successorsOf (node: int) = g.Successors(node).ToArray()
                Expect.equal (successorsOf 0) [| 0; 1; 2 |] "sorted, duplicates dropped"
                Expect.equal (successorsOf 1) [||] "node 1 has no successors"
            }

            test "a successor outside the node range is rejected" {
                Expect.throwsT<ArgumentException>
                    (fun () -> Digraph.OfSuccessors(2, fun _ -> [ 5 ]) |> ignore)
                    "successor 5 of a 2-node graph"

                Expect.throwsT<ArgumentException>
                    (fun () -> Digraph.OfSuccessors(1, fun _ -> [ -1 ]) |> ignore)
                    "successor -1"
            }

            test "a negative node count is rejected" {
                Expect.throwsT<ArgumentException> (fun () -> Digraph.OfSuccessors(-1, fun _ -> []) |> ignore) "-1 nodes"
            }

            test "a 200000-node chain costs heap, not call stack" {
                let n = 200_000
                let g = Digraph.OfSuccessors(n, fun u -> if u < n - 1 then [ u + 1 ] else [])
                let p = Scc.compute g
                Expect.equal p.Components.Length n "n singleton components"
                Expect.equal p.ComponentIndex.[n - 1] 0 "the sink is the first component"
                Expect.equal p.ComponentIndex.[0] (n - 1) "the source is the last component"
                Expect.isFalse (Scc.isRecursive p 0) "a chain has no cycle"
            }

            test "differential against Kosaraju over random graphs" {
                let rng = Random(20260906)
                let mutable cyclic = 0

                for _ in 1..250 do
                    for density in [ 0.05; 0.15; 0.35; 0.7 ] do
                        let p = checkGraph (randomGraph rng (rng.Next(1, 13)) density)

                        if p.Components |> Seq.exists (fun c -> c.Members.Length > 1) then
                            cyclic <- cyclic + 1

                Expect.isGreaterThan cyclic 200 "graphs of the corpus holding a multi-member component"
            }
        ]
