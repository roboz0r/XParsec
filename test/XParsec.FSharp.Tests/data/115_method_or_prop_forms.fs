module MethodOrPropForms

type C() =
    let mutable _v = 0

    // method: (ident '.')? ident pat1 ... patn = expr
    member _.Method1(x: int) = x + 1
    member self.Method2(x: int, y: int) = x + y + 1

    // property: (ident '.')? ident = expr
    member _.Prop1 = _v

    // with set pat? pat = expr
    member _.Prop3
        with set (v: int) = _v <- v

    // with get and set
    member _.Prop4
        with get () = _v
        and set v = _v <- v

    // with set and get
    member _.Prop5
        with set v = _v <- v
        and get () = _v
