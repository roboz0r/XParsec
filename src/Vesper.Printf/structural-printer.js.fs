namespace Vesper

// structural-printer.js.fs — the JS-target `%A` structural formatter, authored as
// Vesper source and compiled by the JS backend (manifest `runtime-js`, library mode)
// into the committed `Vesper.Printf.mjs`. The successor to the hand-authored
// `Vesper.Printf.mjs`: the *same* shape-keyed
// walker, now generated instead of hand-written.
//
// WHY NOT the CLR `structural-printer.fs`: that file is interface-dispatch (per-type
// synthesised `IStructuralFormattable.Format`) and BCL-heavy (`Span`/`ArrayPool`/
// `ITuple`/`IEnumerable`). The JS target abandoned per-type emission (the Step 5b/6
// interop invariant — read `.tag` + own keys, never `instanceof`), so the dispatch
// core cannot be shared as-is. This is the shape-keyed JS counterpart; its leaf/layout
// *forms* deliberately match the CLR `StructuralPrinter`'s output.
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
// WIDTH-BREAKING LAYOUT. The walker
// builds a Wadler `Doc` tree (`Text`/`Line`/`Cat`/`Nest`/`Group`) — the JS analogue of
// the CLR `structural-printer.fs` `Doc` DU — and lays it out into a single-cell string
// accumulator (`renderDoc`), threading indent / broken / column exactly like CLR
// `RuntimeFormatState.RenderDoc`. A `Group` renders ALL-FLAT when its flat width fits the
// remaining budget from the current column, else ALL-BROKEN (its `Line`s become a newline
// + the active `Nest` indent). `width = 0` ⇒ never break (the `%0A` mode). The default
// `%A` width is 80 (`EmitJs.buildHole`), matching the CLR `AppendStructured` default;
// `%NA` rides the same `width` argument (so `%5A` breaks at column 5). `size` is F#'s
// PrintSize node budget: each leaf spends one unit; a cons-list caps at 100 elements or
// budget exhaustion, rendering `...` past either. The build phase (`fmtValue`) consumes
// the node budget; the render phase consumes only the width budget — the same two-phase
// split as CLR (`DispatchInner` then `RenderDoc`).
//
// ERASURE CORNERS (documented, unreachable through well-typed `%A`): a length-1 string
// and an F# `char` are both JS strings; an integer-valued `float` and an `int` are both
// JS numbers. F#'s static types make either confusion unreachable through a single
// `%A`; it only surfaces under `obj`-boxing (out of MVP scope). Cycle detection
// (the CLR visited-set) is likewise out of MVP scope — an acyclic-input runtime.
//
// NOT Fantomas-formatted (`.fantomasignore`): FCS can't parse the `(# … #)` intrinsics.

module StructuralPrinter =

    // --- JS shape-keying + string primitives (raw expression templates) ----------

    let cat (a: string) (b: string) : string = (# "$0 + $1" a b : string #)
    let strEq (a: string) (b: string) : bool = (# "$0 === $1" a b : bool #)
    let strLen (s: string) : int = (# "$0.length" s : int #)
    let notB (x: bool) : bool = (# "!$0" x : bool #)

    let intEq (a: int) (b: int) : bool = (# "$0 === $1" a b : bool #)
    let intLt (a: int) (b: int) : bool = (# "$0 < $1" a b : bool #)
    let intLe (a: int) (b: int) : bool = (# "$0 <= $1" a b : bool #)
    let intGe (a: int) (b: int) : bool = (# "$0 >= $1" a b : bool #)
    let intGt (a: int) (b: int) : bool = (# "$0 > $1" a b : bool #)
    let inc (a: int) : int = (# "$0 + 1" a : int #)
    let dec (a: int) : int = (# "$0 - 1" a : int #)
    let addI (a: int) (b: int) : int = (# "$0 + $1" a b : int #)

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

    // --- the layout document (Wadler `Doc`; the JS analogue of the CLR `Doc` DU) ---

    /// A recorded layout document. A `Group` renders all-flat or all-broken; `Nest`
    /// governs the indent broken lines hang at; `Line` is a soft break (its flat
    /// alternative when its group is flat, a newline + indent when broken).
    type Doc =
        | Text of string
        | Line of string
        | Cat of Doc[]
        | Nest of int * Doc
        | Group of Doc * bool

    // `Doc[]` children are built by spreading onto a fresh array (`emptyDocs` is never
    // mutated in place — every `append` returns a new array), so the accumulator is
    // immutable-by-construction; the mutable local that holds it is reassigned, never
    // its contents written through.
    let emptyDocs: Doc[] = (# "[]" : Doc[] #)
    let append (a: Doc[]) (d: Doc) : Doc[] = (# "[...$0, $1]" a d : Doc[] #)
    let dLen (a: Doc[]) : int = (# "$0.length" a : int #)
    let dGet (a: Doc[]) (i: int) : Doc = (# "$0[$1]" a i : Doc #)

    /// The flat (single-line) width of a `Doc` — recomputed rather than cached; the
    /// trees are small. `Group(_, parens)` adds 2 for the wrapping parentheses.
    let rec flatWidth (d: Doc) : int =
        match d with
        | Text s -> strLen s
        | Line flat -> strLen flat
        | Cat kids -> catWidth kids 0 0
        | Nest(_, inner) -> flatWidth inner
        | Group(inner, parens) -> addI (flatWidth inner) (if parens then 2 else 0)

    and catWidth (kids: Doc[]) (i: int) (acc: int) : int =
        if intGe i (dLen kids) then
            acc
        else
            catWidth kids (inc i) (addI acc (flatWidth (dGet kids i)))

    // --- the render pass: append into a single-cell string accumulator -------------
    // Mirrors CLR `RuntimeFormatState.RenderDoc` (which threads a pooled `char[]`);
    // here the buffer is a one-cell JS array `[out]` mutated in place, and each
    // `renderDoc` returns the end column.

    let mkStrCell (s: string) : string[] = (# "[$0]" s : string[] #)
    let getStr (c: string[]) : string = (# "$0[0]" c : string #)
    let emit (out: string[]) (s: string) : unit = (# "$0[0] = $0[0] + $1" out s : unit #)
    let nSpaces (n: int) : string = (# "' '.repeat($0)" n : string #)

    /// Lay `d` out into `out`, threading the current indent / broken flag / column;
    /// returns the column it ends at.
    let rec renderDoc (d: Doc) (out: string[]) (indent: int) (broken: bool) (col: int) (width: int) : int =
        match d with
        | Text s ->
            emit out s
            addI col (strLen s)
        | Line flat ->
            if broken then
                emit out "\n"
                emit out (nSpaces indent)
                indent
            else
                emit out flat
                addI col (strLen flat)
        | Nest(i, inner) -> renderDoc inner out (addI indent i) broken col width
        | Cat kids -> renderCat kids 0 out indent broken col width
        | Group(inner, parens) ->
            // The opening paren advances the column the inner content lays out from.
            let openCol = if parens then inc col else col
            // All-or-nothing: the group is flat iff its entire flat rendering fits the
            // remaining budget from the current column. `width = 0` ⇒ always flat.
            let groupBroken =
                if intEq width 0 then
                    false
                else
                    intGt (addI openCol (flatWidth inner)) width

            if parens then
                emit out "("

            let endCol = renderDoc inner out indent groupBroken openCol width

            if parens then
                emit out ")"
                inc endCol
            else
                endCol

    and renderCat (kids: Doc[]) (i: int) (out: string[]) (indent: int) (broken: bool) (col: int) (width: int) : int =
        if intGe i (dLen kids) then
            col
        else
            let col1 = renderDoc (dGet kids i) out indent broken col width
            renderCat kids (inc i) out indent broken col1 width

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

    // --- the structural walker (value → `Doc`) ------------------------------------

    let rec fmtValue (v: obj) (budget: int[]) : Doc =
        if isUndef v then
            Text "()" // unit
        elif isNullV v then
            Text "null"
        else
            let t = typeOf v

            if strEq t "number" then
                setB budget (dec (getB budget))
                Text(toStr v)
            elif strEq t "bigint" then
                setB budget (dec (getB budget))
                Text(cat (toStr v) "L") // int64 / uint64
            elif strEq t "boolean" then
                setB budget (dec (getB budget))
                Text(toStr v) // String(true) === "true"
            elif strEq t "string" then
                setB budget (dec (getB budget))
                Text(fmtString v)
            elif isArr v then
                fmtTuple v budget
            elif isVesperList v then
                fmtList v budget
            elif isUnion v then
                fmtUnion v budget
            else
                fmtRecord v budget

    // An arg-position child: a payload-bearing union parenthesises (`Some (Circle 5)`);
    // anything else renders bare. The parens ride a `Group(_, true)` so they count toward
    // the flat width and the inner can still break inside them — the CLR `BeginApplication`
    // (ArgPending ⇒ parens) semantics, made structural.
    and fmtArg (v: obj) (budget: int[]) : Doc =
        let d = fmtValue v budget
        if isPayloadUnion v then Group(d, true) else d

    // `(a, b, …)` flat; broken hangs the components under the open paren (indent 1),
    // comma + soft-line separated.
    and fmtTuple (v: obj) (budget: int[]) : Doc =
        let len = lenOf v
        let mutable inner = emptyDocs
        let mutable i = 0

        while intLt i len do
            if intGt i 0 then
                inner <- append inner (Text ",")
                inner <- append inner (Line " ")

            inner <- append inner (fmtValue (elem v i) budget)
            i <- inc i

        let mutable outer = emptyDocs
        outer <- append outer (Text "(")
        outer <- append outer (Nest(1, Cat inner))
        outer <- append outer (Text ")")
        Group(Cat outer, false)

    // `[a; b; …]` flat; broken puts the brackets on their own lines with the elements
    // nested (indent 2), `;`-separated. Capped at 100 elements or budget exhaustion,
    // rendering a single trailing `...` (the separator is emitted before the cap check,
    // so the marker reads `…; ...`).
    and fmtList (v: obj) (budget: int[]) : Doc =
        let mutable elems = emptyDocs
        elems <- append elems (Line "")
        let mutable cur = v
        let mutable i = 0
        let mutable first = true
        let mutable go = true

        while go do
            if notB (intEq (tagOf cur) 1) then
                go <- false
            else
                if notB first then
                    elems <- append elems (Text ";")
                    elems <- append elems (Line " ")

                if intGe i 100 then
                    elems <- append elems (Text "...")
                    go <- false
                elif intLe (getB budget) 0 then
                    elems <- append elems (Text "...")
                    go <- false
                else
                    elems <- append elems (fmtValue (field cur "Head") budget)
                    first <- false
                    cur <- field cur "Tail"
                    i <- inc i

        let mutable outer = emptyDocs
        outer <- append outer (Text "[")
        outer <- append outer (Nest(2, Cat elems))
        outer <- append outer (Line "")
        outer <- append outer (Text "]")
        Group(Cat outer, false)

    // `Case` / `Case arg` / `Case (a, b, …)`.
    and fmtUnion (v: obj) (budget: int[]) : Doc =
        let name = sElem (casesOf v) (tagOf v)
        let ks = keysOf v
        let klen = lenOf ks
        let mutable fcount = 0
        let mutable j = 0

        while intLt j klen do
            if notB (strEq (sElem ks j) "tag") then
                fcount <- inc fcount

            j <- inc j

        if intEq fcount 0 then
            Text name
        elif intEq fcount 1 then
            let mutable fk = ""
            let mutable j2 = 0

            while intLt j2 klen do
                if notB (strEq (sElem ks j2) "tag") then
                    fk <- sElem ks j2

                j2 <- inc j2

            let mutable outer = emptyDocs
            outer <- append outer (Text(cat name " "))
            outer <- append outer (fmtArg (field v fk) budget)
            Cat outer
        else
            // `Case (a, b, …)` — the tuple parens already disambiguate, so components are
            // normal-position children (no per-arg parens).
            let mutable inner = emptyDocs
            let mutable firstF = true
            let mutable j3 = 0

            while intLt j3 klen do
                let k = sElem ks j3

                if notB (strEq k "tag") then
                    if notB firstF then
                        inner <- append inner (Text ",")
                        inner <- append inner (Line " ")

                    inner <- append inner (fmtValue (field v k) budget)
                    firstF <- false

                j3 <- inc j3

            let mutable grp = emptyDocs
            grp <- append grp (Text "(")
            grp <- append grp (Nest(1, Cat inner))
            grp <- append grp (Text ")")
            let mutable outer = emptyDocs
            outer <- append outer (Text(cat name " "))
            outer <- append outer (Group(Cat grp, false))
            Cat outer

    // A record (or any other plain object): `{ F = v; G = w }` in own-key order; the
    // fields hang at +2 indent when the group breaks (`{ X = 1;\n  Y = 2 }`).
    and fmtRecord (v: obj) (budget: int[]) : Doc =
        let ks = keysOf v
        let klen = lenOf ks

        if intEq klen 0 then
            Text "{ }"
        else
            let k0 = sElem ks 0
            let mutable inner = emptyDocs
            inner <- append inner (fmtValue (field v k0) budget)
            let mutable jr = 1

            while intLt jr klen do
                let k = sElem ks jr
                inner <- append inner (Text ";")
                inner <- append inner (Line " ")
                inner <- append inner (Text(cat k " = "))
                inner <- append inner (fmtValue (field v k) budget)
                jr <- inc jr

            let mutable outer = emptyDocs
            outer <- append outer (Text(cat (cat "{ " k0) " = "))
            outer <- append outer (Nest(2, Cat inner))
            outer <- append outer (Text " }")
            Group(Cat outer, false)

    // Render a built `Doc` into the supplied accumulator cell, returning the string.
    // `out` is taken as a PARAMETER (not a `let` in `structuralFormat`) so it is a real
    // call binding the optimiser cannot pure-substitute — the same reason `budget` is
    // threaded as an argument. An implicit top-level `Group` lets the whole document
    // break; its end column is discarded (a wildcard bind, never `|> ignore`, which would
    // leave the `ignore` recipe a bare value codegen can't eta-expand).
    let renderRoot (d: Doc) (out: string[]) (width: int) : string =
        // The render appends into `out`; its end-column return is discarded (an effectful
        // `let _ =`, never `|> ignore`, which would leave the `ignore` recipe a bare value).
        let _ = renderDoc (Group(d, false)) out 0 false 0 width
        getStr out

    // Public entry — the surface the backend imports. A 3-param named function,
    // so it emits as a flat call `structuralFormat(value, width, size)` (Fable-style
    // flat compiled-function ABI). `width = 0` ⇒ never break (`%0A`).
    let structuralFormat (value: obj) (width: int) (size: int) : string =
        renderRoot (fmtValue value (mkBudget size)) (mkStrCell "") width

    // --- single-precision stringification (`%O` on a float32) ---------------------
    //
    // A `float32` REPRS to a JS `number` — an IEEE-754 *double* — so JS's own
    // stringification renders it at DOUBLE precision: `0.1f + 0.2f` is the float32
    // nearest 0.3 (`0x3E99999A`), whose exact double expansion is
    // `0.30000001192092896`, where .NET prints `0.3`. The arithmetic is right; only the
    // rendering is wrong. .NET's `Single.ToString()` is the SHORTEST decimal that
    // round-trips back to the same float32, so search for it: the first
    // significant-digit count whose parse survives `Math.fround` (JS's float32 rounding)
    // unchanged. 9 significant digits always round-trip a float32, so every finite value
    // finds one; `NaN` never compares equal and falls out to the plain rendering.
    //
    // This CANNOT be folded into the `fmtValue` walker above: a float32 reaching `%A` is
    // an erased JS `number`, indistinguishable at run time from a `float`. Only the
    // format hole's STATIC type knows the width, so the caller is the backend's `%O`
    // lowering (`EmitJsFormat.buildHole`, which reads `HoleSpec.Ty`), never a walker.

    let fround (x: float32) : float32 = (# "Math.fround($0)" x : float32 #)
    let toPrecision (x: float32) (digits: int) : string = (# "$0.toPrecision($1)" x digits : string #)
    let parseF32 (s: string) : float32 = (# "Number($0)" s : float32 #)
    let f32Eq (a: float32) (b: float32) : bool = (# "$0 === $1" a b : bool #)
    let f32Str (x: float32) : string = (# "String($0)" x : string #)

    /// .NET `Single.ToString()` for a float32 carried in a JS `number`: the shortest
    /// decimal that round-trips through `Math.fround`.
    let float32ToString (v: float32) : string =
        let mutable result = f32Str v
        let mutable digits = 1
        let mutable searching = true

        while searching do
            if intGt digits 9 then
                searching <- false
            else
                let candidate = parseF32 (toPrecision v digits)

                if f32Eq (fround candidate) v then
                    result <- f32Str candidate
                    searching <- false
                else
                    digits <- inc digits

        result
