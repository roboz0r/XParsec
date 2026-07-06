[<Struct>]
type Holder<'TFunc when 'TFunc :> Fun<int, int>> =
    val F : 'TFunc
    new(f: 'TFunc) = { F = f }
let mk (f: 'TFunc when 'TFunc :> Fun<int, int>) : Holder<'TFunc> = Holder<'TFunc>(f)
let apply (f: 'TF when 'TF :> Fun<int, int>) (x: int) : int = f.Invoke x
// The STORED binding `h` is the target: its `Holder` field
// must lay out `'TFunc` as the `<closure>$` value-struct. The
// closure is then dispatched by passing `h.F` through a typar
// combinator (`apply`) — the constrained-dispatch path —
// which reads `h.F`'s (rewritten) value-struct type for the
// `!TF` MethodSpec, so the read of the stored struct is correct.
let h = mk (fun x -> x + 1)
printfn "%d" (apply h.F 41)
