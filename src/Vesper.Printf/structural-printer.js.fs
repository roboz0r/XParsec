namespace Vesper

// structural-printer.js.fs — the JS-target `%A` structural formatter, authored as
// Vesper source and compiled by the JS backend (manifest `runtime-js`, library mode)
// into the committed `Vesper.Printf.mjs`. The successor to the hand-authored
// `Vesper.Printf.mjs` (printf-shared-core-plan.md Phase 3): the *same* shape-keyed,
// flat-output walker, now generated instead of hand-written.
//
// WHY NOT the CLR `structural-printer.fs`: that file is interface-dispatch (per-type
// synthesised `IStructuralFormattable.Format`) and BCL-heavy (`Span`/`ArrayPool`/
// `ITuple`/`IEnumerable`). The JS target abandoned per-type emission (the Step 5b/6
// interop invariant — read `.tag` + own keys, never `instanceof`), so the dispatch
// core cannot be shared as-is. This is the shape-keyed JS counterpart; its leaf/layout
// *forms* deliberately match the CLR `StructuralPrinter`'s flat output.
//
// FULLY SELF-CONTAINED — like `list.js.fs`, every operation is either a language
// construct or a raw `(# … #)` JS-expression intrinsic (the idiom `ArrayLoopTests`
// exercises with `newarr`). It imports nothing, so the generated `.mjs` has no
// `import`. The JS shape-keying primitives (`typeof`, `Array.isArray`, `Object.keys`,
// dynamic field / `.tag` / `.cases()` access) have no Vesper front-end symbol, so they
// are spliced verbatim. String concatenation / comparison are likewise direct `$0 + $1`
// / `$0 === $1` templates rather than the `(+)` / `(=)` operators (those would pull in
// the `Vesper.Core` runtime — `structuralEquals` — defeating self-containment).
//
// Output is always FLAT (single line): `width` is accepted but unused, so JS `%A`
// behaves like the CLR `%0A` (never-break) mode. `size` is F#'s PrintSize node budget:
// each leaf spends one unit; a cons-list caps at 100 elements or budget exhaustion,
// rendering `...` past either. The shared `Doc`/`render` width-breaking kernel is the
// tracked follow-up (printf-shared-core-plan.md Phase 3, surface B).
//
// ERASURE CORNERS (documented, unreachable through well-typed `%A`): a length-1 string
// and an F# `char` are both JS strings; an integer-valued `float` and an `int` are both
// JS numbers. F#'s static types make either confusion unreachable through a single
// `%A`; it only surfaces under `obj`-boxing (out of MVP scope).
//
// NOT Fantomas-formatted (`.fantomasignore`): FCS can't parse the `(# … #)` intrinsics.

module StructuralPrinter =

    // --- JS shape-keying + string primitives (raw expression templates) ----------

    let cat (a: string) (b: string) : string = (# "$0 + $1" a b : string #)
    let strEq (a: string) (b: string) : bool = (# "$0 === $1" a b : bool #)
    let notB (x: bool) : bool = (# "!$0" x : bool #)

    let intEq (a: int) (b: int) : bool = (# "$0 === $1" a b : bool #)
    let intLt (a: int) (b: int) : bool = (# "$0 < $1" a b : bool #)
    let intLe (a: int) (b: int) : bool = (# "$0 <= $1" a b : bool #)
    let intGe (a: int) (b: int) : bool = (# "$0 >= $1" a b : bool #)
    let intGt (a: int) (b: int) : bool = (# "$0 > $1" a b : bool #)
    let inc (a: int) : int = (# "$0 + 1" a : int #)
    let dec (a: int) : int = (# "$0 - 1" a : int #)

    let typeOf (v: obj) : string = (# "typeof $0" v : string #)
    let isArr (v: obj) : bool = (# "Array.isArray($0)" v : bool #)
    let isUndef (v: obj) : bool = (# "$0 === undefined" v : bool #)
    let isNullV (v: obj) : bool = (# "$0 === null" v : bool #)
    let toStr (v: obj) : string = (# "String($0)" v : string #)
    let lenOf (v: obj) : int = (# "$0.length" v : int #)
    let elem (v: obj) (i: int) : obj = (# "$0[$1]" v i : obj #)
    let field (v: obj) (k: string) : obj = (# "$0[$1]" v k : obj #)
    let sElem (a: string[]) (i: int) : string = (# "$0[$1]" a i : string #)
    let keysOf (v: obj) : string[] = (# "Object.keys($0)" v : string[] #)
    let tagOf (v: obj) : int = (# "$0.tag" v : int #)
    let casesOf (v: obj) : string[] = (# "$0.cases()" v : string[] #)

    // The budget is a single-cell JS array (`[size]`), mutated in place so one counter
    // is shared across the whole recursion — the analogue of the hand-authored
    // `{ n: size }` object.
    let mkBudget (size: int) : int[] = (# "[$0]" size : int[] #)
    let getB (b: int[]) : int = (# "$0[0]" b : int #)
    let setB (b: int[]) (v: int) : unit = (# "$0[0] = $1" b v : unit #)

    // --- leaf / classification helpers (none recurse into `fmtValue`) -------------

    // `"…"` with the F# escape set. `s` is a JS string; a char is a length-1 string,
    // so it is compared against string literals directly.
    let fmtString (s: obj) : string =
        let len = lenOf s
        let mutable out = "\""
        let mutable i = 0

        while intLt i len do
            let c = (# "$0[$1]" s i : string #)

            if strEq c "\\" then out <- cat out "\\\\"
            elif strEq c "\n" then out <- cat out "\\n"
            elif strEq c "\r" then out <- cat out "\\r"
            elif strEq c "\t" then out <- cat out "\\t"
            elif strEq c "\"" then out <- cat out "\\\""
            else out <- cat out c

            i <- inc i

        cat out "\""

    // A union value: a non-array object carrying a numeric `tag` and the `cases()`
    // discriminator method every emitted union base class declares.
    let isUnion (v: obj) : bool =
        if isNullV v then false
        elif notB (strEq (typeOf v) "object") then false
        elif isArr v then false
        elif notB (strEq (typeOf (field v "tag")) "number") then false
        else strEq (typeOf (field v "cases")) "function"

    // A Vesper cons-list: a union whose declaration-order cases are exactly
    // `["Empty"; "Cons"]`. Rendered `[a; b; c]`, not as the raw union.
    let isVesperList (v: obj) : bool =
        if notB (isUnion v) then
            false
        else
            let cs = casesOf v

            if notB (intEq (lenOf cs) 2) then false
            elif notB (strEq (sElem cs 0) "Empty") then false
            else strEq (sElem cs 1) "Cons"

    // A payload-bearing union in argument position parenthesises (`Some (Circle 5)`);
    // tuples / lists / records carry their own delimiters already (length 1 = just `tag`).
    let isPayloadUnion (v: obj) : bool =
        if notB (isUnion v) then false
        elif isVesperList v then false
        else intGt (lenOf (keysOf v)) 1

    // --- the structural walker ----------------------------------------------------

    let rec fmtValue (v: obj) (budget: int[]) : string =
        if isUndef v then
            "()" // unit
        elif isNullV v then
            "null"
        else
            let t = typeOf v

            if strEq t "number" then
                setB budget (dec (getB budget))
                toStr v
            elif strEq t "bigint" then
                setB budget (dec (getB budget))
                cat (toStr v) "L" // int64 / uint64
            elif strEq t "boolean" then
                setB budget (dec (getB budget))
                toStr v // String(true) === "true"
            elif strEq t "string" then
                setB budget (dec (getB budget))
                fmtString v
            elif isArr v then
                // tuple `(a, b, …)`
                let len = lenOf v
                let mutable out = "("
                let mutable i = 0

                while intLt i len do
                    let s = fmtValue (elem v i) budget
                    if intEq i 0 then out <- cat out s else out <- cat (cat out ", ") s
                    i <- inc i

                cat out ")"
            elif isVesperList v then
                // cons-list `[a; b; …]`, capped at 100 elements or budget exhaustion
                let mutable out = "["
                let mutable cur = v
                let mutable i = 0
                let mutable first = true
                let mutable go = true

                while go do
                    if notB (intEq (tagOf cur) 1) then
                        go <- false
                    elif intGe i 100 then
                        out <- (if first then cat out "..." else cat out "; ...")
                        go <- false
                    elif intLe (getB budget) 0 then
                        out <- (if first then cat out "..." else cat out "; ...")
                        go <- false
                    else
                        let h = fmtValue (field cur "Head") budget
                        if first then out <- cat out h else out <- cat (cat out "; ") h
                        first <- false
                        cur <- field cur "Tail"
                        i <- inc i

                cat out "]"
            elif isUnion v then
                // `Case` / `Case arg` / `Case (a, b, …)`
                let name = sElem (casesOf v) (tagOf v)
                let ks = keysOf v
                let klen = lenOf ks
                let mutable fcount = 0
                let mutable j = 0

                while intLt j klen do
                    if notB (strEq (sElem ks j) "tag") then fcount <- inc fcount
                    j <- inc j

                if intEq fcount 0 then
                    name
                elif intEq fcount 1 then
                    let mutable fk = ""
                    let mutable j2 = 0

                    while intLt j2 klen do
                        if notB (strEq (sElem ks j2) "tag") then fk <- sElem ks j2
                        j2 <- inc j2

                    let child = field v fk
                    let childStr = fmtValue child budget

                    if isPayloadUnion child then
                        cat (cat (cat name " (") childStr) ")"
                    else
                        cat (cat name " ") childStr
                else
                    let mutable out = cat name " ("
                    let mutable firstF = true
                    let mutable j3 = 0

                    while intLt j3 klen do
                        let k = sElem ks j3

                        if notB (strEq k "tag") then
                            let s = fmtValue (field v k) budget
                            if firstF then out <- cat out s else out <- cat (cat out ", ") s
                            firstF <- false

                        j3 <- inc j3

                    cat out ")"
            else
                // a record (or any other plain object): `{ F = v; G = w }` in own-key order
                let ks = keysOf v
                let klen = lenOf ks
                let mutable out = "{ "
                let mutable firstR = true
                let mutable jr = 0

                while intLt jr klen do
                    let k = sElem ks jr
                    let s = cat (cat k " = ") (fmtValue (field v k) budget)
                    if firstR then out <- cat out s else out <- cat (cat out "; ") s
                    firstR <- false
                    jr <- inc jr

                cat out " }"

    // Public curried entry — the surface the backend imports
    // (`structuralFormat(value)(width)(size)`). `width` is accepted but unused (flat).
    let structuralFormat (value: obj) (width: int) (size: int) : string = fmtValue value (mkBudget size)
