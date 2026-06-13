// UseSet — a standalone consumer of the `Vesper.Set` package.
//
// A real consumer program that references nothing of the package but its `.fsi`
// contract — `open Vesper.Collections` brings in `Set<'T>` + the `Set` module —
// and drives it end-to-end. It is compiled by this repo's *own* backend (not
// `dotnet`/`fsc`), loaded alongside the built `Vesper.Set.dll` and its eight
// transitive `Vesper.*` deps, and run; `UseSetSampleTests` pins the stdout below
// so the sample stays a live regression gate rather than dead documentation.
//
// HOF arguments that the module folds are written *curried* (`fun acc -> fun x`)
// per the Freeze posture the round-trip harness documents; single-argument
// predicates/projections stay uncurried.
open Vesper.Collections

// Construction — `Set.ofArray` deduplicates (the two `2`s / `1`s collapse).
let primes = Set.ofArray [| 2; 3; 5; 7 |]
let evens = Set.ofArray [| 2; 4; 6; 8; 8 |]

// Cardinality & membership.
printfn "%d" (Set.count primes) // 4
printfn "%b" (Set.contains 5 primes) // true
printfn "%b" (Set.contains 4 primes) // false

// Set algebra — function forms.
printfn "%d" (Set.count (Set.union primes evens)) // 7  ({2;3;4;5;6;7;8})
printfn "%d" (Set.count (Set.intersect primes evens)) // 1  ({2})
printfn "%d" (Set.count (Set.difference primes evens)) // 3  ({3;5;7})

// Set algebra — operator forms dispatch to the imported type's static members.
printfn "%d" (Set.count (primes + evens)) // 7
printfn "%d" (Set.count (primes - evens)) // 3

// Transforms.
let doubled = Set.map (fun x -> x * 2) primes
printfn "%d" (Set.fold (fun acc -> fun x -> acc + x) 0 doubled) // 34  (4+6+10+14)
printfn "%d" (Set.count (Set.filter (fun x -> x < 6) primes)) // 3  ({2;3;5})

// Predicates.
printfn "%b" (Set.forall (fun x -> x > 0) primes) // true
printfn "%b" (Set.exists (fun x -> x = 7) primes) // true

// Partition — a tuple-destructured let bound from the module result.
let (lo, hi) = Set.partition (fun x -> x < 5) primes
printfn "%d" (Set.count lo) // 2  ({2;3})
printfn "%d" (Set.count hi) // 2  ({5;7})

// Ordering.
printfn "%d" (Set.minElement primes) // 2
printfn "%d" (Set.maxElement primes) // 7

// Round-trip through the `'T list` / `'T array` bridges.
printfn "%d" (Set.count (Set.ofList (Set.toList primes))) // 4
printfn "%d" (Set.count (Set.ofArray (Set.toArray primes))) // 4

// Iterate in sorted order.
Set.iter (fun x -> printfn "%d" x) (Set.difference primes evens) // 3, 5, 7
