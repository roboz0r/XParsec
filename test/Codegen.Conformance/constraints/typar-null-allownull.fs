// `when 'a : null` at a class that DECLARES `null` inhabits it. `[<AllowNullLiteral>]` is the
// only way a bare nominal satisfies the constraint.
[<AllowNullLiteral>]
type Node(v: int) =
    member this.V = v

let onlyNull<'a when 'a: null> (x: 'a) = x

let f (n: Node) = onlyNull n
ignore f
