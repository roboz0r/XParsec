namespace Vesper

#nowarn "42"

type undefined = (# "undefined" #)

// `[<Global>]` emits no definition, so every reference is the bare `undefined`. Keep the
// inner `: undefined`: a template with no result annotation infers `unit`.
[<AutoOpen>]
module Undefined =

    [<Global>]
    let undefined: undefined = (# "undefined": undefined #)
