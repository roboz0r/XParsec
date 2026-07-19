namespace Vesper.UnionFind

// A semi-persistent union-find data structure, after
//
//   Sylvain Conchon, Jean-Christophe Filliâtre.
//   "A Persistent Union-Find Data Structure." ML'07.
//
// This is the paper's *final* "manually defunctorized" version (the `defun.` row of
// Figure 3): Baker persistent arrays with rerooting, the set-skip optimization, and the
// `Invalid` node. Defunctorized means the abstract PersistentArray functor argument is
// inlined and monomorphized to int arrays — there is no separate polymorphic array module,
// so the disjoint-set structure collapses to two int-specialized arrays.
//
// Over `int<'M>` ids, grow-only. The union-find is keyed purely by dense integer ids; naming
// (element ↔ id) lives in a separate `DynamicStore<'T,'M>`, so the disjoint-set is decoupled
// from element bookkeeping. The domain expands on demand: each version carries its own `count`
// (its id high-water mark), the live arrays are `ResizeArray`-backed so they can append, and
// any id at or above a version's `count` is an implicit singleton that a `Union` materializes
// when it first participates. Growth is sound under semi-persistence because a fresh id is
// invisible to every older/smaller version (which never indexes past its own `count`).
//
// SEMI-PERSISTENCE — read this before using it as if it were purely functional.
// The `Invalid` node makes the arrays *semi-persistent*, not fully persistent: you may return
// to an OLDER version, but doing so invalidates every NEWER version derived from it.
// Concretely, after `let b = a.Union(x, y)`, reading `a` again reroots the shared array back
// to a's state and marks b's array `Invalid`; any subsequent access to b then raises. This is
// exactly the discipline a backtracking search wants (descend, then roll back to an ancestor
// and take a different branch) and it is why this version is as fast as the imperative one.
// It is NOT safe to hold and interrogate two sibling versions alternately.

/// One node of a persistent int array: a mutable cell (the paper's `α data ref`) whose
/// contents are either the live array, a diff against a newer version, or an invalidated
/// (backtracked-past) placeholder. The live array is a `ResizeArray` so the structure can
/// grow its id space in place.
type internal PaData =
    /// The live array — the single physical buffer; only `PaCell.init` ever allocates one.
    | Arr of ResizeArray<int>
    /// This version equals `Next` everywhere except at `Index`, where it holds `Value`.
    /// `Next` is the *newer* version (the one closer to the live array).
    | Diff of index: int * value: int * next: PaCell
    /// A version that was superseded and then backtracked past; accessing it is an error.
    | Invalid

/// A reference to a persistent-array version (the paper's `α t = α data ref`). The mutable
/// field is what lets `set`/`reroot`/`grow` re-point older versions to diffs while the newest
/// keeps the live array.
and internal PaCell = { mutable Data: PaData }

module internal PaCell =

    let init (n: int) (f: int -> int) : PaCell =
        let a = ResizeArray<int>(max 0 n)

        for i in 0 .. n - 1 do
            a.Add(f i)

        { Data = Arr a }

    /// Rerooting (Baker / §2.3.3): make `t` point directly at the live `Arr` node by walking
    /// the diff chain down to the array, then replaying the diffs into the shared array on the
    /// way back and marking each stepped-over version `Invalid` (the semi-persistent variant,
    /// §"Final Improvements": no reversed diffs are allocated).
    ///
    /// The paper's `reroot` is not tail-recursive and overflows on long diff chains; this is
    /// the equivalent explicit-stack loop.
    let reroot (t: PaCell) : unit =
        match t.Data with
        | Arr _ -> ()
        | Invalid -> invalidOp "SemiPersistentUnionFind: reroot of an invalidated array version"
        | Diff _ ->
            // Walk from `t` down the chain of newer versions to the cell holding the array.
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

            // Replay diffs from the array outward to `t`. Each cell takes over the live array;
            // its (newer) child is invalidated — that is the backtrack, discarding newer versions.
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
                // Set-skip optimization (§"Final Improvements"): the slot already holds `v`,
                // so share `t` rather than allocate a useless indirection. This is what makes
                // path compression's repeated writes to the representative near-free.
                t
            else
                a[i] <- v
                let res = { Data = Arr a }
                t.Data <- Diff(i, old, res)
                res
        | _ -> invalidOp "SemiPersistentUnionFind: unreachable — reroot did not yield an array"

    /// Append (or, after a rollback re-materialized the slot, overwrite) slot `k` with `v`,
    /// returning the grown newest version and turning the argument into a diff. The old version
    /// keeps `size = k` and never indexes slot `k`, so the recorded diff value is an arbitrary
    /// placeholder — it exists only to keep the reroot chain well-formed.
    let grow (t: PaCell) (k: int) (v: int) : PaCell =
        reroot t

        match t.Data with
        | Arr a ->
            if k < a.Count then a[k] <- v else a.Add v
            let res = { Data = Arr a }
            t.Data <- Diff(k, v, res)
            res
        | _ -> invalidOp "SemiPersistentUnionFind: unreachable — reroot did not yield an array"

/// A semi-persistent union-find over dense `int<'M>` ids. `Union` returns a new partition and
/// leaves its argument observably unchanged; `Find` returns the representative id of a class.
/// The domain grows on demand: an id at or above `Count` is an implicit singleton until a
/// union first references it. Pair with a `DynamicStore<'T,'M>` to name the ids.
///
/// See the SEMI-PERSISTENCE note at the top of this file: a value is safe to roll back to but
/// not to interrogate side-by-side with versions derived from it.
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

    /// The representative id of the class containing `x`. Performs path compression as a hidden
    /// side effect (the returned id is the only observable result). An id at or above `Count`
    /// is its own representative (an as-yet-unmaterialized singleton).
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
    /// the id space if either id is not yet materialized. Returns a new value; `this` is
    /// observably unchanged (subject to the semi-persistence contract).
    member this.Union(x: int<'M>, y: int<'M>) : SemiPersistentUnionFind<'M> =
        let xi = int x
        let yi = int y
        let m = max xi yi

        if m < count then
            let cx = this.FindId xi
            let cy = this.FindId yi
            if cx = cy then this else this.Link(cx, cy)
        else
            // Materialize the ids in [count .. m] as singletons in a fresh newest version
            // (leaving `this` as a semi-persistent ancestor), then union within it — where the
            // ids are now in range and the no-growth branch above applies.
            let mutable p = parent
            let mutable r = rank
            let mutable c = count

            while c <= m do
                p <- PaCell.grow p c c // parent[c] = c: a new self-rooted singleton
                r <- PaCell.grow r c 0 // rank[c] = 0
                c <- c + 1

            SemiPersistentUnionFind(r, p, c).Union(x, y)
