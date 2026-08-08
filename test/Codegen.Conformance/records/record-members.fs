// A record with INSTANCE members — a method (`Sum`, `AddN n`) and a property
// (`Doubled`) — reached by dot-access. Instance-member dispatch on a record object argument
// resolves on the same nominal-member path as a class or union; a field read (`v.X`)
// still lowers to a plain field access, so both forms must agree across the backends.
type Vec =
    {
        X: int
        Y: int
    }

    member this.Sum() = this.X + this.Y
    member this.AddN(n: int) = this.X + this.Y + n
    member this.Doubled = this.X * 2

let v = { X = 3; Y = 4 }
printfn "%d" (v.Sum())
printfn "%d" (v.AddN 10)
printfn "%d" v.Doubled
printfn "%d" v.X
