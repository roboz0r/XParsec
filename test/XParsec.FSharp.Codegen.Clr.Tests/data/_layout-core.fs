type LDoc =
    | LText of string
    | LLine of string
    | LCat of LDoc list
    | LNest of int * LDoc
    | LGroup of LDoc * bool

let rec flatWidth (d: LDoc) : int =
    match d with
    | LText s -> s.Length
    | LLine flat -> flat.Length
    | LCat kids -> catWidth kids
    | LNest(_, inner) -> flatWidth inner
    | LGroup(inner, parens) -> flatWidth inner + (if parens then 2 else 0)

and catWidth (kids: LDoc list) : int =
    match kids with
    | [] -> 0
    | k :: rest -> flatWidth k + catWidth rest

let rec spaces (n: int) : string =
    if n <= 0 then "" else " " + spaces (n - 1)

type R = { Txt: string; Col: int }

let rec render (d: LDoc) (indent: int) (broken: bool) (col: int) (width: int) : R =
    match d with
    | LText s -> { Txt = s; Col = col + s.Length }
    | LLine flat ->
        if broken then
            {
                Txt = "\n" + spaces indent
                Col = indent
            }
        else
            { Txt = flat; Col = col + flat.Length }
    | LNest(i, inner) -> render inner (indent + i) broken col width
    | LCat kids -> renderCat kids indent broken col width
    | LGroup(inner, parens) ->
        let openCol = if parens then col + 1 else col
        let groupBroken = width <> 0 && openCol + flatWidth inner > width
        let r = render inner indent groupBroken openCol width

        if parens then
            {
                Txt = "(" + r.Txt + ")"
                Col = r.Col + 1
            }
        else
            r

and renderCat (kids: LDoc list) (indent: int) (broken: bool) (col: int) (width: int) : R =
    match kids with
    | [] -> { Txt = ""; Col = col }
    | k :: rest ->
        let r1 = render k indent broken col width
        let r2 = renderCat rest indent broken r1.Col width
        { Txt = r1.Txt + r2.Txt; Col = r2.Col }

let layout (d: LDoc) (width: int) : string =
    let r = render (LGroup(d, false)) 0 false 0 width
    r.Txt

let rec revOnto (xs: LDoc list) (acc: LDoc list) : LDoc list =
    match xs with
    | [] -> acc
    | h :: t -> revOnto t (h :: acc)
