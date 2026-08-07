namespace Vesper.UnionFind

// Semi-persistent union-find over dense `int<'M>` ids, after Conchon & Filliâtre, "A
// Persistent Union-Find Data Structure" (ML'07): the defunctorized version of Figure 3 —
// Baker persistent arrays with rerooting, the set-skip optimization, and `Invalid` nodes.

/// Contents of one persistent-int-array version (the paper's `α data`). `ResizeArray`, not
/// `array`, so the id space can grow in place.
type internal PaData =
    /// The live array — one physical buffer, shared by every version of this array.
    | Arr of ResizeArray<int>
    /// This version equals `next` everywhere except at `index`, where it holds `value`.
    /// `next` is the *newer* version (the one closer to the live array).
    | Diff of index: int * value: int * next: PaCell
    /// A version that was superseded and then backtracked past; accessing it is an error.
    | Invalid

/// A persistent-array version (the paper's `α t = α data ref`). The field is mutable so that
/// re-pointing an older version at a `Diff` leaves every existing handle to it valid.
and internal PaCell = { mutable Data: PaData }

module internal PaCell =

    let init (n: int) (f: int -> int) : PaCell =
        let a = ResizeArray<int>(max 0 n)

        for i in 0 .. n - 1 do
            a.Add(f i)

        { Data = Arr a }

    /// Point `t` at the live `Arr` by replaying the diff chain into the shared array, marking
    /// every stepped-over (newer) version `Invalid` rather than allocating reversed diffs.
    /// Explicit stack, not the paper's recursion, which overflows on long diff chains.
    let reroot (t: PaCell) : unit =
        match t.Data with
        | Arr _ -> ()
        | Invalid -> invalidOp "SemiPersistentUnionFind: reroot of an invalidated array version"
        | Diff _ ->
            let path = System.Collections.Generic.Stack<struct (PaCell * int * int)>()
            let mutable cur = t
            let mutable arr = Unchecked.defaultof<ResizeArray<int>>
            let mutable child = Unchecked.defaultof<PaCell>
            let mutable searching = true

            while searching do
                match cur.Data with
                | Diff(i, v, next) ->
                    path.Push(struct (cur, i, v))
                    cur <- next
                | Arr a ->
                    arr <- a
                    child <- cur // the cell currently owning the live array
                    searching <- false
                | Invalid -> invalidOp "SemiPersistentUnionFind: reroot reached an invalidated array version"

            // Outward to `t`: each cell in turn takes over the live array, and the newer child
            // it just superseded becomes `Invalid`.
            while path.Count > 0 do
                let struct (cell, i, v) = path.Pop()
                arr[i] <- v
                cell.Data <- Arr arr
                child.Data <- Invalid
                child <- cell

    let get (t: PaCell) (i: int) : int =
        match t.Data with
        | Arr a -> a[i]
        | Invalid -> invalidOp "SemiPersistentUnionFind: get on an invalidated array version"
        | Diff _ ->
            reroot t

            match t.Data with
            | Arr a -> a[i]
            | _ -> invalidOp "SemiPersistentUnionFind: unreachable — reroot did not yield an array"

    let set (t: PaCell) (i: int) (v: int) : PaCell =
        reroot t

        match t.Data with
        | Arr a ->
            let old = a[i]

            if old = v then
                // Set-skip: the slot already holds `v`, so share `t` instead of allocating an
                // indirection — this is what makes path compression's repeated writes cheap.
                t
            else
                a[i] <- v
                let res = { Data = Arr a }
                t.Data <- Diff(i, old, res)
                res
        | _ -> invalidOp "SemiPersistentUnionFind: unreachable — reroot did not yield an array"

    /// Append (or overwrite, when a rollback already materialized the slot) slot `k` with `v`,
    /// returning the grown newest version and turning `t` into a diff. No older version ever
    /// indexes slot `k`, so the value recorded in that diff is an arbitrary placeholder.
    let grow (t: PaCell) (k: int) (v: int) : PaCell =
        reroot t

        match t.Data with
        | Arr a ->
            if k < a.Count then a[k] <- v else a.Add v
            let res = { Data = Arr a }
            t.Data <- Diff(k, v, res)
            res
        | _ -> invalidOp "SemiPersistentUnionFind: unreachable — reroot did not yield an array"

/// Semi-persistent, NOT persistent: after `let b = a.Union(x, y)` merges two classes, reading
/// `a` again reroots the shared array back to a's state and any later use of `b` raises. Roll
/// back to an ancestor freely; never interrogate two sibling versions alternately.
[<Sealed>]
type SemiPersistentUnionFind<[<Measure>] 'M> private (rank: PaCell, parent: PaCell, count: int) =

    // `parent` is mutable so `Find` can install the path-compressed array as a hidden side
    // effect (the paper's `h.parent <- f`) without changing the set of representatives.
    let mutable parent = parent

    /// The empty partition. The first union grows the id space to cover the ids it touches.
    static member Empty: SemiPersistentUnionFind<'M> =
        SemiPersistentUnionFind(PaCell.init 0 id, PaCell.init 0 id, 0)

    /// A partition of the ids {0 .. n-1}, each its own class.
    static member Create(n: int) : SemiPersistentUnionFind<'M> =
        if n < 0 then
            invalidArg (nameof n) "SemiPersistentUnionFind: size must be non-negative"

        SemiPersistentUnionFind(PaCell.init n (fun _ -> 0), PaCell.init n id, n)

    /// The number of materialized ids (the id high-water mark). Ids at or above this are
    /// implicit singletons until a union first references them.
    member _.Count = count

    // Representative *id* of `i` in array `f`, plus the compressed array (every node on the
    // path remapped straight to the representative).
    member private this.FindAux(f: PaCell, i: int) : struct (PaCell * int) =
        let fi = PaCell.get f i

        if fi = i then
            struct (f, i)
        else
            let struct (f, r) = this.FindAux(f, fi)
            struct (PaCell.set f i r, r)

    // Representative id of `i` (assumed materialized: 0 <= i < count), compressing the path as
    // a hidden side effect installed into this version's `parent`.
    member private this.FindId(i: int) : int =
        let struct (f, cx) = this.FindAux(parent, i)
        parent <- f
        cx

    // Merge two representative ids (already materialized), yielding a new version.
    member private _.Link(cx: int, cy: int) : SemiPersistentUnionFind<'M> =
        let rx = PaCell.get rank cx
        let ry = PaCell.get rank cy

        if rx > ry then
            SemiPersistentUnionFind(rank, PaCell.set parent cy cx, count)
        elif rx < ry then
            SemiPersistentUnionFind(rank, PaCell.set parent cx cy, count)
        else
            SemiPersistentUnionFind(PaCell.set rank cx (rx + 1), PaCell.set parent cy cx, count)

    /// The representative id of the class containing `x`, compressing the path as a hidden side
    /// effect. An id at or above `Count` is its own representative (an unmaterialized singleton).
    member this.Find(x: int<'M>) : int<'M> =
        let i = int x

        if i >= count then
            x
        else
            LanguagePrimitives.Int32WithMeasure<'M>(this.FindId i)

    /// Whether `x` and `y` are currently in the same class.
    member this.Equivalent(x: int<'M>, y: int<'M>) : bool =
        let xi = int x
        let yi = int y

        if xi >= count || yi >= count then
            // At least one is an unmaterialized singleton; equal only if they are the same id.
            xi = yi
        else
            this.FindId xi = this.FindId yi

    /// The partition obtained by merging the classes of `x` and `y` (union by rank), growing
    /// the id space if either id is not yet materialized.
    member this.Union(x: int<'M>, y: int<'M>) : SemiPersistentUnionFind<'M> =
        let xi = int x
        let yi = int y
        let m = max xi yi

        if m < count then
            let cx = this.FindId xi
            let cy = this.FindId yi
            if cx = cy then this else this.Link(cx, cy)
        else
            // Materialize [count .. m] as singletons in a fresh newest version, leaving `this`
            // an ancestor, then re-enter with every id in range.
            let mutable p = parent
            let mutable r = rank
            let mutable c = count

            while c <= m do
                p <- PaCell.grow p c c // parent[c] = c: a new self-rooted singleton
                r <- PaCell.grow r c 0 // rank[c] = 0
                c <- c + 1

            SemiPersistentUnionFind(r, p, c).Union(x, y)
