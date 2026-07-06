namespace global

// JS-only augmentation of the intrinsic rank-1 array `'T[]` with a `get_Item`
// indexer accessor — the consumer-side contract half of the W9 array-index
// migration (`arr.[i]` READ). Declared in the GLOBAL namespace ON PURPOSE: the
// array's canonical identity is the BARE `TyConst("[]")` (never namespaced), so its
// member-contract key must be bare. The source spelling `type 'T ``[]``` names it
// the backtick-escaped `` ``[]`` `` — `RuntimeNames.arrayContractName`, which is the
// single source of that string and explains why the arity suffix is suppressed and
// how the contract / harvest-store / receiver-side keys all agree. A `namespace
// Vesper` decl would key it `` Vesper.``[]`` `` and silently miss the bare receiver
// lookup.
//
// This is a SEPARATE contract entry from the base `prim-types-min.fsi`
// `type 'T[] = extern` (which stays the `` Vesper.``[]`` `` `Intrinsic` shape,
// UNPERTURBED). Nothing but the array-index branch consults the bare `` ``[]`` ``
// key, so array's construction / element typing / codegen see no change.
type 'T ``[]`` =
    extern

    with

        /// `arr.[i]` read. The `.js.fs` companion (`array-index-body.js.fs`)
        /// carries the `(# "ldelem.any !0" … #)` body — a byte-copy of the free
        /// `GetArray` intrinsic — served as a member-keyed inline splice, so the
        /// JS backend emits the same computed-member read `arr[i]`.
        member get_Item: int -> 'T
