[<Struct>]
type Holder(v: int) =
    interface System.IComparable with
        member this.CompareTo(o: obj) = v

let h = Holder(7)
