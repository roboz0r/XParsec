namespace Vesper

// Runtime-compiled `%A` fixtures for `StructuralFormatTests.fs`. NOT fsc-compiled:
// this file is read as TEXT and compiled through this repo's own Codegen.Clr backend
// (`TestHelpers.compileFixtureFile`) against the Vesper.Core / Vesper.List contract,
// then loaded so its `obj`-returning nullary functions can be reflected + invoked.
//
// The four `Sem*` types below hand-write `IStructuralFormattable` impls that drive the
// declarative sink (`BeginRecord` / `Field` / `Child` / `BeginCase` / `EndCase`) — the
// same recursion vocabulary a synthesised `Format` body has. They own the Core-bound
// `%A` interfaces (`Vesper.IStructuralFormattable` / `Vesper.IFormatSink`), so they
// must live in a Vesper.Core-compiled assembly rather than fsc. `Fixtures` exposes one
// nullary `obj`-returning function per distinct value the `"semantic protocol"` tests
// build; each compiles to a zero-arg static method (a lone `unit` param is erased) so
// `GetMethod(name).Invoke(null, [||])` binds it.

// A record driven semantically: `Field name` marks each label, `Child` supplies
// the value. The sink owns the `{ … }` / `+2` hang policy.
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

// An option-shaped DU driven semantically: `BeginCase name; (Child payload); EndCase`.
// The sink decides nullary vs single-payload and the single-payload parenthesisation
// (`Some (Some 3)` but not `Some 3` / `Some None`) from the observed child count + the
// application-shaped mark.
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

// A record whose second field is itself a `Child` — exercises the pending-label
// invariant: the outer `Inner = ` label must be emitted before recursing, so the
// nested record's first `Field` cannot clobber it.
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

/// One nullary `obj`-returning function per distinct value the `"semantic protocol"`
/// tests render. The F# test binds each by reflection and hands it to the Vesper-compiled
/// `%A` engine, so the value's `IStructuralFormattable` impl and the engine's sink meet
/// on the single Default-ALC `Vesper.Core`.
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

    /// `Some <payload>` around a caller-supplied `obj`. Used for a payload the fixture
    /// cannot build natively — an FSharp.Core `list`, which must render via the engine's
    /// `IEnumerable` arm (`[1; 2]`); a Vesper cons-list built here instead has its OWN
    /// synthesised `IStructuralFormattable` and would render as `Cons (1, Cons (2, Empty))`.
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
