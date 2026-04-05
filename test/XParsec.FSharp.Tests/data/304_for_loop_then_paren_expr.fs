module Test

// for loop followed by parenthesized expression at same or lesser indent
// Pratt parser should NOT treat the (expr) as an argument to the for-loop
// From HashMultiMap.fs lines 137-143

open System.Collections.Generic

let makeEnumerator (items: (string * int) list) =
    let elems = List<_>()

    for kvp in items do
        elems.Add(kvp)

        for z in [ kvp ] do
            elems.Add(z)

    (elems.GetEnumerator() :> System.Collections.IEnumerator)
