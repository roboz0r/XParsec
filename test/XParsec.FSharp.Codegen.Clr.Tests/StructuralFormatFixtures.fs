namespace Vesper

// NOT fsc-compiled: read as TEXT and compiled through this repo's own Codegen.Clr
// backend against Vesper.Core, then loaded and invoked by reflection. The `Sem*` types
// implement the Core-bound `Vesper.IStructuralFormattable`, which fsc cannot see.

// The fixture emits no braces or separators; the sink owns the `{ … }` layout.
type SemPoint =
    {
        PX: int
        PY: string
    }

    interface IStructuralFormattable with
        member this.Format(sink: IFormatSink) =
            sink.BeginRecord()
            sink.Field("X")
            sink.Child(box this.PX)
            sink.Field("Y")
            sink.Child(box this.PY)
            sink.EndRecord()

// The sink decides parenthesisation from the observed child count and shape:
// `Some (Some 3)`, but bare `Some 3` / `Some None`.
type SemOpt =
    | SemNone
    | SemSome of obj

    interface IStructuralFormattable with
        member this.Format(sink: IFormatSink) =
            match this with
            | SemNone ->
                sink.BeginCase("None")
                sink.EndCase()
            | SemSome v ->
                sink.BeginCase("Some")
                sink.Child(v)
                sink.EndCase()

// A record `Child`: the outer `Inner = ` label must land before recursing, or the
// nested record's first `Field` clobbers it.
type SemBox =
    {
        BLabel: string
        BInner: obj
    }

    interface IStructuralFormattable with
        member this.Format(sink: IFormatSink) =
            sink.BeginRecord()
            sink.Field("Label")
            sink.Child(box this.BLabel)
            sink.Field("Inner")
            sink.Child(this.BInner)
            sink.EndRecord()

// A two-payload case, to exercise the tuple arm `Pair (a, b)`.
type SemPair =
    | SemPair of obj * obj

    interface IStructuralFormattable with
        member this.Format(sink: IFormatSink) =
            match this with
            | SemPair(a, b) ->
                sink.BeginCase("Pair")
                sink.Child(a)
                sink.Child(b)
                sink.EndCase()

/// One nullary `obj`-returning function per value the `%A` tests render, each bound
/// by reflection from the F# side.
module Fixtures =

    /// `{ X = 1; Y = "a" }`
    let pointRecord () : obj = box { PX = 1; PY = "a" }

    /// `None`
    let caseNone () : obj = box SemNone

    /// `Some 3`
    let caseSome3 () : obj = box (SemSome(box 3))

    /// `Some -3`
    let caseSomeNeg3 () : obj = box (SemSome(box -3))

    /// `Some (Some 3)`
    let caseSomeSome3 () : obj = box (SemSome(box (SemSome(box 3))))

    /// `Some None`
    let caseSomeNone () : obj = box (SemSome(box SemNone))

    /// `Some <payload>` around a caller-supplied `obj`, for a payload this fixture
    /// cannot build natively. An FSharp.Core `list` renders via the engine's
    /// `IEnumerable` arm (`[1; 2]`); a Vesper cons-list gives `Cons (1, Cons (2, Empty))`.
    let someOf (v: obj) : obj = box (SemSome v)

    /// `Some { X = 1; Y = "a" }`
    let caseSomePoint () : obj = box (SemSome(box { PX = 1; PY = "a" }))

    /// `Pair (1, "a")`
    let casePair () : obj = box (SemPair(box 1, box "a"))

    /// `Pair (Some 1, Some 2)`
    let casePairSomes () : obj =
        box (SemPair(box (SemSome(box 1)), box (SemSome(box 2))))

    /// `Some (Pair (1, 2))`
    let caseSomePair () : obj =
        box (SemSome(box (SemPair(box 1, box 2))))

    /// `{ Label = "a"; Inner = { X = 1; Y = "b" } }`
    let boxRecord () : obj =
        box
            {
                BLabel = "a"
                BInner = box { PX = 1; PY = "b" }
            }
