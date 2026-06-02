### 1. How Complex is the Algorithm?

From a computational standpoint, Tarjan's algorithm is as efficient as theoretically possible.

* **Time Complexity:** $\mathcal{O}(V + E)$
  * $V$ is the number of Vertices (nodes).
  * $E$ is the number of Edges (connections).
  * *Why?* The DFS visits every node exactly once. Once a node is visited, its outgoing edges are looped through exactly once. It scales linearly.
* **Space / Memory Complexity:** $\mathcal{O}(V)$
  * *Why?* In the worst-case scenario (a graph where every node points to the next in a single straight line), the algorithm will push all $V$ nodes onto the stack. The recursion depth of the DFS will also be $V$. You also need a few arrays of size $V$ to track the `index`, `lowlink`, and `onStack` states.
* **Conceptual Complexity:** Moderate.
  * The hardest part for developers to grasp is the distinction between a **back-edge** (pointing to a node currently on the stack) and a **cross-edge** (pointing to a node already processed and removed from the stack). Tarjan handles this elegantly using the `onStack` boolean check.

---

### 2. Tarjan's Algorithm in F #

F# is a functional-first language. However, purely functional graph traversals (using immutable Maps and Sets) often degrade performance to $\mathcal{O}(V \log V)$ due to dictionary lookups.

The idiomatic F# way to write this is to use **"observational purity."** This means we use highly optimized mutable arrays and variables *inside* the function, but package it in a pure, immutable function signature for the rest of the program to use.

Here is a complete, working implementation:

```fsharp
module Tarjan

/// Finds Strongly Connected Components (SCCs) in a directed graph.
/// 'n' is the number of nodes (labeled 0 to n-1).
/// 'adj' is an array of lists, where adj.[i] contains the neighbors of node i.
let findSCCs (n: int) (adj: int list array) : int list list =
    
    // -- Local Mutable State (for O(V + E) performance) --
    let index   = Array.create n -1    // -1 means "unvisited"
    let lowlink = Array.create n -1
    let onStack = Array.create n false
    let mutable stack = []             // We use an F# list as a stack
    let mutable timer = 0              // Counter for index assignment
    let mutable sccs  = []             // Accumulator for the final result

    // -- The DFS Function --
    let rec dfs u =
        // 1. Initialize the current node
        index.[u]   <- timer
        lowlink.[u] <- timer
        timer       <- timer + 1
        
        stack       <- u :: stack      // Push to stack
        onStack.[u] <- true

        // 2. Explore neighbors
        for v in adj.[u] do
            if index.[v] = -1 then
                // Case A: Neighbor is unvisited. Recurse, then update lowlink.
                dfs v
                lowlink.[u] <- min lowlink.[u] lowlink.[v]
            
            elif onStack.[v] then
                // Case B: Neighbor is on the stack (Back-edge/Cycle found!).
                lowlink.[u] <- min lowlink.[u] index.[v]
                
            // Case C: Visited but not on stack (Cross-edge). Do nothing.

        // 3. If u is a root node, pop the stack to form an SCC
        if lowlink.[u] = index.[u] then
            
            // A helper to pop nodes until we reach 'u'
            let rec popSCC currentScc =
                match stack with
                | top :: rest ->
                    stack <- rest               // Pop the stack
                    onStack.[top] <- false      // Mark as off-stack
                    
                    let nextScc = top :: currentScc
                    if top = u then nextScc     // Stop if we found the root
                    else popSCC nextScc         // Otherwise keep popping
                | [] -> currentScc

            let newScc = popSCC []
            sccs <- newScc :: sccs // Add the newly found SCC to our results

    // -- Start the algorithm --
    // We loop through all nodes in case the graph has disconnected parts
    for i = 0 to n - 1 do
        if index.[i] = -1 then
            dfs i

    // Return the purely functional list of SCCs
    sccs
```

### 3. Testing the F# Code

Let's test this with a practical example. We will build a graph with 5 nodes (0 through 4).

* Nodes `0`, `1`, and `2` form a cycle (SCC 1).
* Node `1` has an escape route pointing to `3`.
* Nodes `3` and `4` form a cycle (SCC 2).

```fsharp
// Representation of the graph:
// 0 -> 1
// 1 -> 2, 3
// 2 -> 0
// 3 -> 4
// 4 -> 3

let graphSize = 5
let adjacencyList = [|
    [ 1 ]       // Node 0 points to 1
    [ 2; 3 ]    // Node 1 points to 2 and 3
    [ 0 ]       // Node 2 points to 0
    [ 4 ]       // Node 3 points to 4
    [ 3 ]       // Node 4 points to 3
|]

let result = Tarjan.findSCCs graphSize adjacencyList

printfn "%A" result
```

**Output:**

```text
[[0; 1; 2]; [3; 4]]
```

### Why this F# implementation shines

1. **Speed:** By using `.create` to make contiguous memory blocks (`Array`), index lookups are instant.
2. **Safety:** `stack`, `timer`, and `sccs` are mutated, but because they are scoped strictly inside `findSCCs`, they cannot be modified by outside code.
3. **Pattern Matching:** The `popSCC` inner function uses F#'s powerful `match` construct to cleanly pop items off the list-based stack without needing messy `while` loops or `null` checks.
