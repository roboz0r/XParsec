namespace Widgets

/// A test-fixture intrinsic/`extern` type carrying one concrete member. The `.fsi` is
/// the consumer-side contract: `widget` is an `extern` (JS-provided) type, and `Poke`
/// is a concrete accessor whose `(# … #)` body lives in the `.js.fs` companion and is
/// spliced at each call site (never a real `.Poke(` method call).
type widget = extern with

    /// `w.Poke x` splices to `x + 1` (the `.js.fs` body `(# "$0 + 1" x : int #)`).
    member inline Poke: int -> int

    /// TWO parameters: the call site applies ONE tupled argument while the lift curries
    /// one lambda per parameter, so this is the member that pins the untupling.
    member inline Poke2: a: int * b: int -> int

/// A GENERIC `extern` host declaring an indexer, which lowers to the accessor method pair
/// `get_Item` / `set_Item`. The setter's value parameter is the declaring typar `'T`.
type 'T slot = extern with

    /// `s.[i]` reads and `s.[i] <- v` writes, both splicing from the `.js.fs` bodies.
    member inline Item: int -> 'T with get, set

/// A second intrinsic/`extern` host whose one member has a body that is NOT inline IL —
/// it is a keyed call to `widget.Poke`, foreign to `gadget.js.fs`. `inline` on the
/// declaration is the whole reason its body publishes; nothing about the body's shape is.
type gadget =
    extern with

    /// `gadget.Bump w` splices to `w.Poke 41`, which splices in turn.
    static member inline Bump: w: widget -> int

    /// The STATIC two-parameter case: a static member has no object argument to occupy curried
    /// position 0, so it untuples against a different curried offset than an instance one.
    static member inline Bump2: w: widget * n: int -> int
