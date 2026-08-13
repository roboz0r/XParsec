// The same class refuses `when 'a : not null`: having declared that `null` inhabits it, it
// cannot also promise the absence of `null`.
[<AllowNullLiteral>]
type Node(v: int) =
    member this.V = v

let notNull<'a when 'a: not null> (x: 'a) = x

let f (n: Node) = notNull n
ignore f
