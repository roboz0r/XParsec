namespace Widgets

#nowarn "42"

// The JS-target body source for the `widget.fsi` contract. `widget` binds the
// `(# "object" #)` intrinsic repr and carries a concrete `Poke` member whose body is a
// single inline-IL template (`$0` ← the value param `x`). The member is NOT emitted as a
// class method: it is declared `inline`, so `liftMemberBody` mints a `this`-first inline
// body served under the finalized member key and the consumer's `InlineExpansion` splices
// it at each `w.Poke a` use site.

type widget =
    (# "object" #)

    with

        member inline _.Poke(x: int) : int = (# "$0 + 1" x : int #)

        member inline _.Poke2(a: int, b: int) : int = (# "$0 + $1" a b : int #)

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
