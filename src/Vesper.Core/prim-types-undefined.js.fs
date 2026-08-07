namespace Vesper

#nowarn "42"

type undefined = (# "undefined" #)

// `[<Global>]` suppresses the definition — a lowered `const undefined = undefined` could
// not initialise — so every reference is the bare `undefined` the template carries. The
// inner `: undefined` types the node; without it an operand-less template infers `unit`.
[<AutoOpen>]
module Undefined =

    [<Global>]
    let undefined: undefined = (# "undefined": undefined #)
