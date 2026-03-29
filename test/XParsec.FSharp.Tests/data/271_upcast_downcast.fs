module Test

let x: obj = upcast "hello"
let y: string = downcast x
let z: seq<int> =
    upcast
        { new System.Collections.Generic.IEnumerable<int> with
            member _.GetEnumerator() = Seq.empty.GetEnumerator()
          interface System.Collections.IEnumerable with
            member _.GetEnumerator() = Seq.empty.GetEnumerator() :> _ }
