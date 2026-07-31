namespace Widgets

#nowarn "42"

// The JS-target body source for the `widget.fsi` contract. `widget` binds the
// `(# "object" #)` intrinsic repr and carries a concrete `Poke` member whose body is a
// single inline-IL template (`$0` ← `this`… no: `$0` ← the value param `x`). The member
// is NOT emitted as a class method: this file is loaded ONLY as `inline-bodies-js`, so
// `liftMemberBody` mints a `this`-first inline body served under the finalized member
// key and the consumer's `InlineExpansion` splices it at each `w.Poke a` use site.

type widget =
    (# "object" #)

    with

        member _.Poke(x: int) : int = (# "$0 + 1" x : int #)

    end
