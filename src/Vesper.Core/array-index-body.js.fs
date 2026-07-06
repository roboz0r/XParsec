namespace global

#nowarn "42" // This construct is deprecated: it is only for use in the F# library

// The JS-target harvest source for the `array-index.js.fsi` contract. The rank-1
// array `'T[]` binds its `(# "!0[]" #)` intrinsic repr and carries the concrete
// `get_Item` accessor whose body is a single inline-IL template — a byte-copy of
// the free `GetArray` (`ops-platform.js.fs`): `(# "ldelem.any !0" type ('T) this
// index : 'T #)`, with `this` (the array receiver) standing in for `GetArray`'s
// `array` parameter.
//
// This file is loaded ONLY as `inline-bodies-js` (NOT a repr companion — its base
// name deliberately differs from the `.fsi` so the repr harvest never picks it up),
// so `harvestMemberBody` mints a `this`-first inline body served under the finalized
// member key. `InlineExpansion` splices it at each `arr.[i]` use site, emitting the
// identical `ldelem` the free `GetArray` did — the migration is pure re-plumbing.
//
// Declared in the GLOBAL namespace so the abbrev host's `SymbolKey` is bare (no
// namespace prefix). The source spelling `type 'T ``[]``` names it the bare
// backtick-escaped `` ``[]`` `` — `RuntimeNames.arrayContractName` (the single source
// of that string), agreeing ordinal-for-ordinal with the consumer contract key and
// the receiver-side lookup.

type 'T ``[]`` =
    (# "!0[]" #)

    with

        member this.get_Item(index: int) : 'T = (# "ldelem.any !0" type ('T) this index : 'T #)

    end
