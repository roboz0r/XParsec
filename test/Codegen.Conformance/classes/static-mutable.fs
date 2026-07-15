// A `static let mutable` is ONE storage location on the TYPE, shared by every instance and
// every access — a write through one instance is visible to a read through another. Two
// instances writing then both reading is what makes the sharing observable: a backend that
// gave the field per-instance storage (or dropped the store) would print 3 then 4, never 7
// twice. The CLR stores to a private static field (`stsfld`); JS assigns a property on the
// emitted class object. The read is an instance method (a closure re-reading the cell) — a
// JS nullary/unit member snapshots its value at module load, a separate limitation.
type Counter() =
    static let mutable total = 0
    member _.Add(k: int) = total <- total + k
    member _.Get() = total

let a = Counter()
let b = Counter()
a.Add 3
b.Add 4
printfn "%d" (a.Get())
printfn "%d" (b.Get())
