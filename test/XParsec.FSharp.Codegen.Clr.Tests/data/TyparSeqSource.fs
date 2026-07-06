[<Struct>]
type ArrayEnumerator =
    val Arr : int[]
    val mutable Idx : int
    new(arr: int[]) = { Arr = arr; Idx = -1 }
    member this.MoveNext() : bool =
        this.Idx <- this.Idx + 1
        this.Idx < this.Arr.Length
    member this.Current : int = this.Arr.[this.Idx]
type ISeq =
    abstract member GetEnumerator : unit -> ArrayEnumerator
[<Struct>]
type ArraySeq =
    val Arr : int[]
    new(arr: int[]) = { Arr = arr }
    interface ISeq with
        member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)
let sumSeq (s: 'S when 'S :> ISeq) : int =
    let mutable total = 0
    for y in s do
        total <- total + y
    total
printfn "%d" (sumSeq (ArraySeq([| 1; 2; 3 |])))
