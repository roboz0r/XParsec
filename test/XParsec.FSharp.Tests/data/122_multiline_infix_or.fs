module M

let isLiteralToken (t: Token) =
    t.IsNumeric
    || t.IsOther
