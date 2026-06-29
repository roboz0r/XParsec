namespace Vesper

// Front-end symbol contract for the `%A` structural engine
// (`structural-printer.fs`, ported from `StructuralFormat.cs`). The `.fs`
// over-exposes its layout internals (the `Doc` document tree, the `Frame` stack,
// `DocLayout`, `FrameKind`) for want of a signature; this contract encapsulates
// them and publishes only the genuine cross-package surface:
//
//   * `StructuralPrinter.Print` — the standalone "render a value to copy-pasteable
//     Vesper source" entry point. Called by `Formatter.AppendStructured` (the `%A`
//     printf hole) and directly by tests.
//   * `RuntimeFormatState` — the concrete <see cref="Vesper.IFormatSink"/> the
//     compiler-synthesised `IStructuralFormattable.Format` bodies drive. Opaque
//     (its render buffer / frame stack / counters are hidden); the contract is the
//     ctor, `Finish`, and the `IFormatSink` implementation.
//
// `IFormatSink` / `IStructuralFormattable` themselves live in
// `../Vesper.Core/structural-format.fsi` (this package depends on Vesper.Core).

/// <summary>The concrete <see cref="Vesper.IFormatSink"/>: records a value's
/// declared structure into a layout document via a frame stack, then lays that
/// document out to a string within a column + node budget. Constructed by
/// <see cref="Vesper.StructuralPrinter"/> and by the synthesised <c>Format</c>
/// bodies; the layout state (render buffer, frame stack, depth/size counters) is
/// not part of the contract.</summary>
type RuntimeFormatState =
    /// <summary>Create a layout state with a column budget of <paramref name="width"/>
    /// (0 ⇒ never break — the <c>%0A</c> flat mode) and a node budget of
    /// <paramref name="printSize"/> (F# PrintSize; nodes past it render as
    /// <c>...</c>).</summary>
    new: width: int * printSize: int -> RuntimeFormatState

    /// <summary>Lay the recorded document out to a string and release the render
    /// buffer. Fails if the layout scopes are unbalanced.</summary>
    member Finish: unit -> string

    interface Vesper.IFormatSink

/// <summary>Entry point for <c>%A</c>: renders a value as copy-pasteable Vesper
/// source (<c>5L</c>, <c>3.0</c>, <c>nan</c>, <c>[1; 2; 3]</c>). The standalone
/// "render to string" used by tests and by the printf handler's structural hole
/// (<c>Formatter.AppendStructured</c>).</summary>
type StructuralPrinter =
    /// <summary>Render <paramref name="value"/> within a column budget of
    /// <paramref name="widthBudget"/> (0 ⇒ never break, the <c>%0A</c> mode) and a
    /// node budget of <paramref name="sizeBudget"/> (F# PrintSize; nodes past it
    /// render as <c>...</c>, the <c>%.NA</c> mode).</summary>
    static member Print: value: obj * widthBudget: int * sizeBudget: int -> string

    /// <summary>Render <paramref name="value"/> with the default node budget
    /// (F#'s 10000 — plain <c>%A</c>).</summary>
    static member Print: value: obj * widthBudget: int -> string
