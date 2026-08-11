namespace Vesper

#nowarn "42"

type 'T ``[]`` =
    (# "!0[]" #)

    with

        // The CIL mnemonics are target-neutral: JS emits `arr[i]`, `arr[i] = v`, `arr.length`.
        member inline this.Item
            with get (index: int) : 'T = (# "ldelem.any !0" type ('T) this index : 'T #)
            and set (index: int) (value: 'T) : unit = (# "stelem.any !0" type ('T) this index value : unit #)

        member inline this.Length
            with get () : int = (# "ldlen" this : int #)

    end

type 'T array = 'T[]
