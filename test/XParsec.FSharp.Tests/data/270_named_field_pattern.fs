module TestNamedFieldPatternSemicolon

type MyDU =
    | Foo of isStruct: bool * elementPats: int list
    | Bar of name: string * value: int

let f x =
    match x with
    | Foo(isStruct = false; elementPats = pats) -> pats
    | Foo(isStruct = true; elementPats = pats) -> pats
    | Bar(name = n; value = v) -> [v]
    | _ -> []
