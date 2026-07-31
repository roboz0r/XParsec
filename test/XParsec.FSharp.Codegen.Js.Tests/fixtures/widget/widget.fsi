namespace Widgets

/// A test-fixture intrinsic/`extern` type carrying one concrete member. The `.fsi` is
/// the consumer-side contract: `widget` is an `extern` (JS-provided) type, and `Poke`
/// is a concrete accessor whose `(# … #)` body lives in the `.js.fs` companion and is
/// spliced at each call site (never a real `.Poke(` method call).
type widget = extern with

    /// `w.Poke x` splices to `x + 1` (the `.js.fs` body `(# "$0 + 1" x : int #)`).
    member inline Poke: int -> int

/// A second intrinsic/`extern` host whose one member has a body that is NOT inline IL —
/// it is a keyed call to `widget.Poke`, foreign to `gadget.js.fs`. `inline` on the
/// declaration is the whole reason its body publishes; nothing about the body's shape is.
type gadget =
    extern with

    /// `gadget.Bump w` splices to `w.Poke 41`, which splices in turn.
    static member inline Bump: w: widget -> int
