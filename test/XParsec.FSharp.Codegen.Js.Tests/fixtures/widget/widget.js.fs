namespace Widgets

#nowarn "42"

// The JS-target bodies for the `widget.fsi` signature file: `widget` binds the `(# "object" #)`
// intrinsic repr, and every member is `inline`, so no class method is emitted and a `w.Poke a`
// use site splices `$0 + 1` with `$0` ← `a`.

type widget =
    (# "object" #)

    with

        member inline _.Poke(x: int) : int = (# "$0 + 1" x : int #)

        member inline _.Poke2(a: int, b: int) : int = (# "$0 + $1" a b : int #)

        member inline _.Poke3 (a: int) (b: int) : int = (# "$0 + $1" a b : int #)

    end

// The indexer half of the fixture. The `set` accessor's parameters are spelled CURRIED here
// and tupled in the `.fsi`; both spell the two .NET parameters `(int, 'T)`.
type 'T slot =
    (# "object" #)

    with

        member inline this.Item
            with get (i: int) : 'T = (# "$0[$1]" this i : 'T #)
            and set (i: int) (v: 'T) : unit = (# "$0[$1] = $2" this i v : unit #)

    end
