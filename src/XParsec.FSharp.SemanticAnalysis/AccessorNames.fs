namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

/// `member x.P with get … and set …` declares its halves under `get_P` / `set_P`, which is
/// how a use site of `x.P` / `x.P <- v` looks them up.
module AccessorNames =

    let getterName (propName: string) = "get_" + propName

    let setterName (propName: string) = "set_" + propName

    /// `x.[i]` reads through `get_Item`; `x.[i] <- v` writes through `set_Item`.
    let itemGetter = getterName "Item"

    let itemSetter = setterName "Item"

    /// A signature's `with` clause halves, named. The CST keeps the two tokens in SOURCE
    /// order (`with set, get` is legal) and the parser admits no spelling but `get` / `set`,
    /// so identifying the first half determines the second.
    let halvesOf
        (nameOf: SyntaxToken -> string)
        (getSet: SyntaxToken * SyntaxToken voption)
        : {|
              Getter: SyntaxToken voption
              Setter: SyntaxToken voption
          |}
        =
        let first, second = getSet

        if nameOf first = "get" then
            {|
                Getter = ValueSome first
                Setter = second
            |}
        else
            {|
                Getter = second
                Setter = ValueSome first
            |}
