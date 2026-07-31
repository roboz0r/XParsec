namespace global

#nowarn "42" // This construct is deprecated: it is only for use in the F# library

type 'T ``[]`` =
    (# "!0[]" #)

    with

        member inline this.get_Item(index: int) : 'T = (# "ldelem.any !0" type ('T) this index : 'T #)

    end
