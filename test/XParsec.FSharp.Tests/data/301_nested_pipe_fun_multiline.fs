module Test

// Nested ||> pipe with multiline fun body (inner fun undentation)
// From DependencyResolution.fs lines 97-107

let processEntries trie (state: int) (path: string list) =
    (state, [| 1 .. path.Length |])
    ||> Array.fold (fun acc takeParts ->
        let partialPath = List.take takeParts path
        let stateAfterFull = acc + partialPath.Length

        // The inner ||> with multiline fun whose body is at a lesser indent
        // than the outer fun's opening paren
        (stateAfterFull, [| "a"; "b" |])
        ||> Array.fold (fun innerAcc ns ->
            let queryResult = ns.Length
            innerAcc + queryResult))
