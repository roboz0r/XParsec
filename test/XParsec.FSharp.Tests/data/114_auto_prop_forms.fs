module AutoPropForms

// Form 1: member val ident = expr  (read-only, inferred type)
type D1() =
    member val P1 = 0

// Form 2: member val ident : type = expr with get  (read-only explicit)
type D2() =
    member val P2: int = 42 with get

// Form 3: member val ident = expr with get, set  (read-write)
type D3() =
    member val P3 = 0 with get, set

// Form 4: member val ident = expr with set, get
type D4() =
    member val P4 = 0 with set, get

// Form 5: static member val
type D5() =
    static member val P5 = 0 with get, set

// Form 6: default val (auto-implemented default)
type Base() =
    abstract P: string with get, set
    default val P = "base" with get, set

// Form 7: override val (auto-implemented override)
type Derived() =
    class
        inherit Base()
        override val P = "derived" with get, set
    end
