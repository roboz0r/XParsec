namespace Widgets

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
