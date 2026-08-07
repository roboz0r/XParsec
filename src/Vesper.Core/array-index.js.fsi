namespace global

type 'T ``[]`` =
    extern

    with

        /// `arr.[i]` read — the member-keyed form of the free `GetArray` intrinsic,
        /// emitting the same computed-member read `arr[i]`.
        member inline get_Item: int -> 'T
