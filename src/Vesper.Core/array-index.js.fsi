namespace global

type 'T ``[]`` =
    extern

    with

        /// `arr.[i]` read. The `.js.fs` companion (`array-index-body.js.fs`)
        /// carries the `(# "ldelem.any !0" … #)` body — a byte-copy of the free
        /// `GetArray` intrinsic — served as a member-keyed inline splice, so the
        /// JS backend emits the same computed-member read `arr[i]`.
        member get_Item: int -> 'T
