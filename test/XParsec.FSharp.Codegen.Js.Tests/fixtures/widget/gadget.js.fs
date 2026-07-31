namespace Widgets

#nowarn "42"

// The NON-IL member body. `Bump`'s body is a call to `widget.Poke` — and `widget` is
// foreign to THIS file (it arrives through `widget.fsi`, not the sibling `.js.fs`), so
// the call stays a keyed external reference rather than resolving to a local member.
// Publishing it is decided by `inline` on the declaration alone; the lifting reads no
// body shape.
//
// ONE parameter, because a lifted body is CURRIED and a direct static-member call site
// carries its arguments as one TUPLE — an unrelated arity mismatch that bites an
// inline-IL body identically. A primitive's tupled operator escapes it by arriving as a
// trait call, whose arguments are carried individually.

type gadget =
    (# "object" #)

    with

        static member inline Bump(w: widget) : int = w.Poke 41

    end
