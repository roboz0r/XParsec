namespace XParsec.FSharp.SemanticAnalysis

open System

/// A directed graph over dense node ids `0 … NodeCount - 1`. Every successor is a node id of
/// this graph, and a node's successors are ascending and distinct.
///
/// A client interns its own nodes — bindings, lifetime variables, type shapes — into dense
/// ids and maps the results back.
[<Sealed>]
type Digraph private (xadj: int[], adj: int[]) =

    member _.NodeCount: int = xadj.Length - 1

    member _.Successors(node: int) : ReadOnlySpan<int> =
        ReadOnlySpan<int>(adj, xadj.[node], xadj.[node + 1] - xadj.[node])

    member this.HasSelfEdge(node: int) : bool =
        MemoryExtensions.BinarySearch<int, int>(this.Successors node, node) >= 0

    /// `successors` is called once per node, in ascending order, and each result is copied,
    /// sorted and deduplicated. Raises `ArgumentException` for a negative `nodeCount` or a
    /// successor outside `0 … nodeCount - 1`.
    static member OfSuccessors(nodeCount: int, successors: int -> #seq<int>) : Digraph =
        if nodeCount < 0 then
            invalidArg "nodeCount" $"node count %d{nodeCount} is negative"

        let xadj = Array.zeroCreate<int> (nodeCount + 1)
        let adj = ResizeArray<int>()
        let row = ResizeArray<int>()

        for u in 0 .. nodeCount - 1 do
            xadj.[u] <- adj.Count
            row.Clear()
            row.AddRange(successors u :> seq<int>)
            row.Sort()
            let mutable previous = -1

            for v in row do
                if v < 0 || v >= nodeCount then
                    invalidArg "successors" $"node %d{u} has successor %d{v}, outside 0 … %d{nodeCount - 1}"

                if v <> previous then
                    adj.Add v
                    previous <- v

        xadj.[nodeCount] <- adj.Count
        Digraph(xadj, adj.ToArray())

/// One strongly connected component of a `Digraph`.
type SccComponent =
    /// Every member lies on a cycle, so a lone member carries a self-edge. Members ascending.
    | Cycle of members: EqArray<int>
    /// A single member with no edge to itself.
    | Acyclic of node: int

    member this.Members: EqArray<int> =
        match this with
        | Cycle members -> members
        | Acyclic node -> EqArray.singleton node

/// A `Digraph`'s nodes partitioned into strongly connected components.
type SccPartition =
    {
        /// Reverse topological order: where a member of component `i` has an edge to a member
        /// of a different component `j`, `j < i`.
        Components: EqArray<SccComponent>
        /// Per node, the index into `Components` of the component holding it. Comparing two
        /// of these orders the components they belong to.
        ComponentIndex: EqArray<int>
    }

    /// The component holding `node`.
    member this.ComponentOf(node: int) : SccComponent =
        this.Components.[this.ComponentIndex.[node]]

    member this.NodeCount: int = this.ComponentIndex.Length

    /// The partition of `0 … nodeCount - 1` into `components`, in the given order. Raises
    /// `ArgumentException` unless every node appears in exactly one component.
    static member Create(nodeCount: int, components: EqArray<SccComponent>) : SccPartition =
        let componentIndex = Array.create nodeCount -1

        components
        |> EqArray.iteri (fun c scc ->
            for node in scc.Members do
                if node < 0 || node >= nodeCount then
                    invalidArg "components" $"node %d{node} is outside 0 … %d{nodeCount - 1}"

                if componentIndex.[node] >= 0 then
                    invalidArg "components" $"node %d{node} appears in two components"

                componentIndex.[node] <- c
        )

        for node in 0 .. nodeCount - 1 do
            if componentIndex.[node] < 0 then
                invalidArg "components" $"node %d{node} appears in no component"

        {
            Components = components
            ComponentIndex = EqArray.ofArray componentIndex
        }

    /// The partition over the nodes `keep` retains, renumbered to their positions among the
    /// retained nodes, in the same component order. Only an `Acyclic` node can be dropped:
    /// raises `ArgumentException` when `keep` rejects a `Cycle` member.
    member this.Retain(keep: int -> bool) : SccPartition =
        let renumbered = Array.create this.NodeCount -1
        let mutable next = 0

        for node in 0 .. renumbered.Length - 1 do
            if keep node then
                renumbered.[node] <- next
                next <- next + 1

        let components = ResizeArray<SccComponent>()

        for scc in this.Components do
            match scc with
            | Cycle members ->
                for m in members do
                    if renumbered.[m] < 0 then
                        invalidArg "keep" $"node %d{m} lies on a cycle and cannot be dropped"

                components.Add(Cycle(members |> EqArray.map (fun m -> renumbered.[m])))
            | Acyclic node when renumbered.[node] >= 0 -> components.Add(Acyclic renumbered.[node])
            | Acyclic _ -> ()

        SccPartition.Create(next, EqArray.ofResizeArray components)

[<RequireQualifiedAccess>]
module Scc =

    /// Partitions `g`'s nodes into strongly connected components by Tarjan's algorithm,
    /// driven by an explicit frame stack so that graph depth costs heap rather than call
    /// stack.
    let compute (g: Digraph) : SccPartition =
        let n = g.NodeCount

        let index = Array.create n -1
        let lowlink = Array.zeroCreate<int> n
        let onStack = Array.zeroCreate<bool> n
        let componentOf = Array.create n -1

        // Nodes of the components discovered so far, awaiting the root that closes them.
        let pending = ResizeArray<int>()
        // The DFS frames: the node under expansion and its cursor into that node's successors.
        let frameNode = ResizeArray<int>()
        let frameCursor = ResizeArray<int>()
        let components = ResizeArray<SccComponent>()
        let members = ResizeArray<int>()
        let mutable timer = 0

        for root in 0 .. n - 1 do
            if index.[root] = -1 then
                frameNode.Add root
                frameCursor.Add 0

                while frameNode.Count > 0 do
                    let top = frameNode.Count - 1
                    let u = frameNode.[top]

                    if index.[u] = -1 then
                        index.[u] <- timer
                        lowlink.[u] <- timer
                        timer <- timer + 1
                        pending.Add u
                        onStack.[u] <- true

                    let successors = g.Successors u
                    let mutable e = frameCursor.[top]
                    let mutable descended = false

                    while not descended && e < successors.Length do
                        let v = successors.[e]

                        if index.[v] = -1 then
                            // Resume `u` at the next edge once `v`'s subtree completes.
                            frameCursor.[top] <- e + 1
                            frameNode.Add v
                            frameCursor.Add 0
                            descended <- true
                        else
                            if onStack.[v] then
                                lowlink.[u] <- min lowlink.[u] index.[v]

                            e <- e + 1

                    if not descended then
                        if lowlink.[u] = index.[u] then
                            let componentIndex = components.Count
                            members.Clear()
                            let mutable closed = false

                            while not closed do
                                let w = pending.[pending.Count - 1]
                                pending.RemoveAt(pending.Count - 1)
                                onStack.[w] <- false
                                componentOf.[w] <- componentIndex
                                members.Add w
                                closed <- w = u

                            match members.Count with
                            | 1 when not (g.HasSelfEdge u) -> components.Add(Acyclic u)
                            | _ ->
                                members.Sort()
                                components.Add(Cycle(EqArray.ofResizeArray members))

                        frameNode.RemoveAt top
                        frameCursor.RemoveAt top

                        if frameNode.Count > 0 then
                            let parent = frameNode.[frameNode.Count - 1]
                            lowlink.[parent] <- min lowlink.[parent] lowlink.[u]

        {
            Components = EqArray.ofResizeArray components
            ComponentIndex = EqArray.ofArray componentOf
        }

    /// Whether `node` lies on a cycle.
    let isRecursive (partition: SccPartition) (node: int) : bool =
        match partition.ComponentOf node with
        | Cycle _ -> true
        | Acyclic _ -> false
