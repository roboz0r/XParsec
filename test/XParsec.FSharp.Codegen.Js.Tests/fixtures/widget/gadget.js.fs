namespace Widgets

#nowarn "42"

// The NON-IL member body. `Bump`'s body is a call to `widget.Poke` — and `widget` is
// foreign to THIS file (it arrives through `widget.fsi`, not the sibling `.js.fs`), so
// the call stays a keyed external reference rather than resolving to a local member.
// Publishing it is decided by `inline` on the declaration alone; the lifting reads no
// body shape.

type gadget =
    (# "object" #)

    with

        static member inline Bump(w: widget) : int = w.Poke 41

        // A STATIC member binds no `this`, so its curried parameters start at position 0
        // — the offset the call site's untupled arguments have to land on. Its own body
        // is a TWO-parameter instance call, so one use site untuples twice.
        static member inline Bump2(w: widget, n: int) : int = w.Poke2(n, 7)

    end
