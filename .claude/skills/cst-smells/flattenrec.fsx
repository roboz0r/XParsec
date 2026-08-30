// Restructures a `let rec ... and` group into its SCC condensation, emitted in
// dependency order: a member in no cycle becomes a plain `let` (`let rec` when
// self-recursive); each multi-member cycle becomes its own `let rec ... and` chain,
// members in original relative order.
// Usage: dotnet fsi flattenrec.fsx -- <file>:<headLine> [<file>:<headLine> ...]
open System
open System.IO
open System.Text.RegularExpressions

let headRx =
    Regex(
        @"^(\s*)(let rec|and)\s+(?:\[<[^\]]*>\]\s*)?(?:private\s+|internal\s+)?(?:\(\|\s*([A-Za-z_][A-Za-z0-9_']*)|([A-Za-z_][A-Za-z0-9_']*))",
        RegexOptions.Compiled
    )

let stripComment (line: string) =
    match line.IndexOf "//" with
    | -1 -> line
    | i -> line.Substring(0, i)

let indentOf (line: string) =
    let mutable i = 0

    while i < line.Length && line.[i] = ' ' do
        i <- i + 1

    i

type Member =
    {
        Name: string
        DocStart: int // 0-based, inclusive: leading ///, //, [< lines
        HeadIdx: int
        mutable EndIdx: int
    } // 0-based, inclusive, trailing blanks trimmed

let flatten (path: string) (headLine: int) =
    let lines = File.ReadAllLines path
    let headIdx = headLine - 1
    let m0 = headRx.Match lines.[headIdx]

    if not (m0.Success && m0.Groups.[2].Value = "let rec") then
        failwithf "%s:%d is not a `let rec` head" path headLine

    let gIndent = m0.Groups.[1].Value.Length

    let nameOf (mm: Match) =
        if mm.Groups.[3].Success then
            mm.Groups.[3].Value
        else
            mm.Groups.[4].Value

    let docStartAbove (idx: int) =
        let mutable s = idx

        while s > 0
              && (let t = lines.[s - 1].TrimStart() in

                  indentOf lines.[s - 1] = gIndent
                  && (t.StartsWith "///" || t.StartsWith "//" || t.StartsWith "[<")) do
            s <- s - 1

        s

    let members = ResizeArray<Member>()

    members.Add
        {
            Name = nameOf m0
            DocStart = docStartAbove headIdx
            HeadIdx = headIdx
            EndIdx = headIdx
        }

    let mutable j = headIdx + 1
    let mutable go = true

    while go && j < lines.Length do
        let line = lines.[j]
        let trimmed = line.TrimStart()

        if trimmed = "" then
            j <- j + 1
        else
            let ind = indentOf line

            if ind > gIndent then
                members.[members.Count - 1].EndIdx <- j
                j <- j + 1
            elif
                ind = gIndent
                && (trimmed.StartsWith "///" || trimmed.StartsWith "//" || trimmed.StartsWith "[<")
            then
                j <- j + 1 // attributed to whichever member head follows, via docStartAbove
            elif ind = gIndent then
                let hm = headRx.Match line

                if hm.Success && hm.Groups.[2].Value = "and" then
                    members.Add
                        {
                            Name = nameOf hm
                            DocStart = docStartAbove j
                            HeadIdx = j
                            EndIdx = j
                        }

                    j <- j + 1
                else
                    go <- false
            else
                go <- false

    let n = members.Count
    // Edge a->b: member a's body (comment-stripped, minus its own head keyword+name) mentions b.
    let bodyOf (mem: Member) =
        [ for k in mem.HeadIdx .. mem.EndIdx -> stripComment lines.[k] ]
        |> String.concat "\n"
        |> fun b ->
            let hm = headRx.Match b
            b.Remove(hm.Index, hm.Length)

    let bodies = [| for mem in members -> bodyOf mem |]

    let refRxs =
        [| for mem in members -> Regex(@"\b" + Regex.Escape mem.Name + @"\b") |]

    let edges = Array2D.init n n (fun a b -> refRxs.[b].IsMatch bodies.[a])
    let selfRec = [| for v in 0 .. n - 1 -> edges.[v, v] |]

    // Tarjan SCC over member indices.
    let sccs =
        let index = Array.create n -1
        let low = Array.create n 0
        let onStack = Array.create n false
        let stack = System.Collections.Generic.Stack<int>()
        let mutable counter = 0
        let acc = ResizeArray<int list>()

        let rec strong v =
            index.[v] <- counter
            low.[v] <- counter
            counter <- counter + 1
            stack.Push v
            onStack.[v] <- true

            for w in 0 .. n - 1 do
                if edges.[v, w] then
                    if index.[w] = -1 then
                        strong w
                        low.[v] <- min low.[v] low.[w]
                    elif onStack.[w] then
                        low.[v] <- min low.[v] index.[w]

            if low.[v] = index.[v] then
                let comp = ResizeArray<int>()
                let mutable u = -1

                while u <> v do
                    u <- stack.Pop()
                    onStack.[u] <- false
                    comp.Add u

                acc.Add(comp |> List.ofSeq |> List.sort) // original relative order within a chain

        for v in 0 .. n - 1 do
            if index.[v] = -1 then
                strong v

        List.ofSeq acc

    let compOf = Array.create n -1

    for ci in 0 .. sccs.Length - 1 do
        for v in sccs.[ci] do
            compOf.[v] <- ci

    // Stable Kahn over the condensation: emit the component with the lowest original
    // first-member index whose callee components are all emitted.
    let nc = sccs.Length
    let emitted = Array.create nc false
    let order = ResizeArray<int>()

    let dependsOn a b = // component a references component b
        a <> b
        && sccs.[a]
           |> List.exists (fun v -> sccs.[b] |> List.exists (fun w -> edges.[v, w]))

    let firstMemberOf c = List.min sccs.[c]

    let ready c =
        not emitted.[c]
        && Seq.forall (fun d -> not (dependsOn c d) || emitted.[d]) (seq { 0 .. nc - 1 })

    while order.Count < nc do
        let c = seq { 0 .. nc - 1 } |> Seq.filter ready |> Seq.minBy firstMemberOf

        emitted.[c] <- true
        order.Add c

    let block (v: int) (keyword: string) =
        let mem = members.[v]

        let headFixed =
            Regex.Replace(lines.[mem.HeadIdx], @"^(\s*)(let rec|and)\b", "${1}" + keyword)

        [
            yield! [ for k in mem.DocStart .. mem.HeadIdx - 1 -> lines.[k] ]
            yield headFixed
            yield! [ for k in mem.HeadIdx + 1 .. mem.EndIdx -> lines.[k] ]
        ]

    let extentStart = members.[0].DocStart
    let extentEnd = members.[n - 1].EndIdx

    let rebuilt =
        [
            yield! lines.[.. extentStart - 1]
            let mutable first = true

            for c in order do
                match sccs.[c] with
                | [ v ] ->
                    if not first then
                        yield ""

                    yield! block v (if selfRec.[v] then "let rec" else "let")
                    first <- false
                | vs ->
                    for i in 0 .. vs.Length - 1 do
                        if not first then
                            yield ""

                        yield! block vs.[i] (if i = 0 then "let rec" else "and")
                        first <- false

            yield! lines.[extentEnd + 1 ..]
        ]

    File.WriteAllLines(path, rebuilt)

    let lifted =
        sccs
        |> List.filter (fun c -> c.Length = 1 && not selfRec.[c.Head])
        |> List.length

    let chains = sccs |> List.filter (fun c -> c.Length > 1)

    printfn
        "%s:%d  %d members -> %d lifted to `let`, %d chain(s) of %s, %d self-recursive `let rec`"
        path
        headLine
        n
        lifted
        chains.Length
        (chains |> List.map (fun c -> string c.Length) |> String.concat "+")
        (sccs |> List.filter (fun c -> c.Length = 1 && selfRec.[c.Head]) |> List.length)

let args = fsi.CommandLineArgs |> Array.skip 1 |> Array.filter (fun a -> a <> "--")

for a in args do
    let i = a.LastIndexOf ':'
    flatten (a.Substring(0, i)) (int (a.Substring(i + 1)))
