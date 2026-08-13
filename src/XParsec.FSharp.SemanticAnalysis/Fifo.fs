namespace XParsec.FSharp.SemanticAnalysis

/// An immutable FIFO queue. Enqueue and dequeue are the only operations, so a worklist
/// driven by one visits BREADTH-FIRST: what a step discovers is served after everything
/// already waiting, never ahead of it.
type Fifo<'T> =
    private
        {
            /// Dequeue order.
            front: 'T list
            /// Reverse enqueue order, flipped onto `front` only when `front` runs out, so
            /// each element moves between the two once and enqueue stays O(1).
            back: 'T list
        }

[<RequireQualifiedAccess>]
module Fifo =

    let ofList (xs: 'T list) : Fifo<'T> = { front = xs; back = [] }

    /// Enqueues `xs` left to right, so their relative order survives the round trip.
    let enqueueAll (xs: 'T list) (q: Fifo<'T>) : Fifo<'T> =
        { q with
            back = List.fold (fun acc x -> x :: acc) q.back xs
        }

    let tryDequeue (q: Fifo<'T>) : struct ('T * Fifo<'T>) voption =
        match q.front with
        | x :: rest -> ValueSome(struct (x, { q with front = rest }))
        | [] ->
            match List.rev q.back with
            | [] -> ValueNone
            | x :: rest -> ValueSome(struct (x, { front = rest; back = [] }))
