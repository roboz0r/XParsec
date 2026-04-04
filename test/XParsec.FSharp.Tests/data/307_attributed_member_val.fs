module Test

open Microsoft.Build.Framework

// member val with attribute and with get, set
// From MapSourceRoots.fs lines 177-178

type MyTask() =
    inherit System.Object()

    [<Required>]
    member val SourceRoots: int[] = [||] with get, set

    member val Deterministic = false with get, set
