namespace Widgets

/// A test-fixture intrinsic/`extern` type carrying one concrete member. The `.fsi` is
/// the consumer-side contract: `widget` is an `extern` (JS-provided) type, and `Poke`
/// is a concrete accessor whose `(# … #)` body lives in the `.js.fs` companion and is
/// spliced at each call site (never a real `.Poke(` method call).
type widget = extern with

    /// `w.Poke x` splices to `x + 1` (the `.js.fs` body `(# "$0 + 1" x : int #)`).
    member inline Poke: int -> int
