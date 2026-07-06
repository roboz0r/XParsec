[<Struct>]
type Holder(v: int) =
    interface System.IComparable with
        member this.CompareTo(o: obj) = v

    static member AsCmp(h: Holder) : System.IComparable = h :> System.IComparable

let h = Holder(9)
