module Test

type Config() =
    member val Name = "default" with get, set
    member val Count: int = 0 with get, set
    member val Items: int[] = [||] with get, set

    member this.Total = this.Count + 1
