# XParsec.FSharp.Codegen.Js

## Terminology

"Arrow" names the ECMAScript concept only: an arrow function, TypeScript `=>`. An F# or Vesper
function type, lambda or closure is a function, a lambda or a closure, never an "arrow type" or
an "arrow signature" for `'a -> 'b`. The two targets represent functions differently, since
`Vesper.Fun` lowers to a real JS arrow function while staying a nominal interface on CLR, so one
word for both hides the lowering boundary in the comments meant to explain it. Naming the `->`
token `arrow` in the parser is fine, because that is the glyph.

## String-valued enums are intentional

`type Mode = | On = "on" | Off = "off"` (`TEnumLiteral.String`, `TEnumVariant.String`) widens F#,
which restricts enums to integral and char underlying types. It is a designed feature supporting
a natural TypeScript idiom.
