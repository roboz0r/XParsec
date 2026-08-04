namespace Vesper.Collections

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =

    let zeroCreate (count: int) : 'T[] = ArrayPrelude.NewArray count

    let length (array: 'T[]) : int = array.Length

    let isEmpty (array: 'T[]) : bool = array.Length = 0

    let get (array: 'T[]) (index: int) : 'T = array.[index]

    let set (array: 'T[]) (index: int) (value: 'T) : unit = array.[index] <- value

    let create (count: int) (value: 'T) : 'T[] =
        let result: 'T[] = zeroCreate count

        for i = 0 to count - 1 do
            result.[i] <- value

        result

    let init (count: int) (initializer: int -> 'T) : 'T[] =
        let result: 'T[] = zeroCreate count

        for i = 0 to count - 1 do
            result.[i] <- initializer i

        result

    let copy (array: 'T[]) : 'T[] =
        let len = array.Length
        let result: 'T[] = zeroCreate len

        for i = 0 to len - 1 do
            result.[i] <- array.[i]

        result

    let append (array1: 'T[]) (array2: 'T[]) : 'T[] =
        let len1 = array1.Length
        let len2 = array2.Length
        let result: 'T[] = zeroCreate (len1 + len2)

        for i = 0 to len1 - 1 do
            result.[i] <- array1.[i]

        for i = 0 to len2 - 1 do
            result.[len1 + i] <- array2.[i]

        result

    let rev (array: 'T[]) : 'T[] =
        let len = array.Length
        let result: 'T[] = zeroCreate len

        for i = 0 to len - 1 do
            result.[i] <- array.[len - 1 - i]

        result

    let map (mapping: 'T -> 'U) (array: 'T[]) : 'U[] =
        let len = array.Length
        let result: 'U[] = zeroCreate len

        for i = 0 to len - 1 do
            result.[i] <- mapping array.[i]

        result

    let mapi (mapping: int -> 'T -> 'U) (array: 'T[]) : 'U[] =
        let len = array.Length
        let result: 'U[] = zeroCreate len

        for i = 0 to len - 1 do
            result.[i] <- mapping i array.[i]

        result

    let iter (action: 'T -> unit) (array: 'T[]) : unit =
        for i = 0 to array.Length - 1 do
            action array.[i]

    let iteri (action: int -> 'T -> unit) (array: 'T[]) : unit =
        for i = 0 to array.Length - 1 do
            action i array.[i]

    let fold<'T, 'State> (folder: 'State -> 'T -> 'State) (state: 'State) (array: 'T[]) : 'State =
        let mutable acc = state

        for i = 0 to array.Length - 1 do
            acc <- folder acc array.[i]

        acc

    let foldBack (folder: 'T -> 'State -> 'State) (array: 'T[]) (state: 'State) : 'State =
        let mutable acc = state
        let len = array.Length

        for i = 0 to len - 1 do
            acc <- folder array.[len - 1 - i] acc

        acc
