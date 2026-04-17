// Named-field destructure pattern followed by `as alias` in a let binding.
// `let Foo(field = field) as name = expr`
// Affects: CheckDeclarations.fs
type NB = NormalizedBinding of valSynData: int
let data = NormalizedBinding 1

let run () =
    let NormalizedBinding(valSynData = valSynData) as bind = data
    valSynData, bind
