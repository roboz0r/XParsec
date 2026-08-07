namespace global

#nowarn "42"

type 'T ``[]`` =
    (# "!0[]" #)

    with

        member inline this.get_Item(index: int) : 'T = (# "ldelem.any !0" type ('T) this index : 'T #)

    end
