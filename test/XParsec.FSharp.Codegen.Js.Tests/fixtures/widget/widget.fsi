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

    /// The same two parameters written as two CURRIED argument groups. It keys as `[int; int]`
    /// like `Poke2`, and compiles to the same one two-parameter slot, but the published groups
    /// make the use site write `w.Poke3 3 4` — `w.Poke3(3, 4)` puts the tuple in `a`.
    member inline Poke3: a: int -> b: int -> int

/// An `extern` INTERFACE: it has no `(# … #)` repr and no body source, so its members are
/// real attached `obj.name(…)` calls rather than splices. This is the fixture surface on
/// which a CURRIED member reaches a backend's call plan.
type poker = extern interface with

    /// Two curried groups filling the one two-parameter slot: `p.Jab 3 4` is `p.Jab(3, 4)`.
    abstract member Jab: a: int -> b: int -> int

    /// A curried member whose FIRST group is ONE parameter of its own generic type, so a
    /// tuple ARGUMENT there is that single parameter, not two positions. The group's
    /// declared width is what decides; the flat key `['a; 'b]` cannot.
    abstract member Pair: a: 'a -> b: 'b -> int

/// A GENERIC `extern` host declaring an indexer, which lowers to the accessor method pair
/// `get_Item` / `set_Item`. The setter's value parameter is the declaring typar `'T`.
type 'T slot = extern with

    /// `s.[i]` reads and `s.[i] <- v` writes, both splicing from the `.js.fs` bodies.
    member inline Item: int -> 'T with get, set
