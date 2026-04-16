// Multi-line bitwise `|||` chain whose last operand is a parenthesized
// `match` expression whose pattern bars are at the same column as the
// opening paren.
// Affects: ilwrite.fs
type Conv = FastCall | StdCall | ThisCall | CDecl | Default | VarArg

let callconvToByte ntypars (hasthis: byte) (bcc: Conv) =
    hasthis |||
    (if ntypars > 0 then 0x10uy else 0x00uy) |||
    (match bcc with
    | FastCall -> 0x04uy
    | StdCall -> 0x02uy
    | ThisCall -> 0x03uy
    | CDecl -> 0x01uy
    | Default -> 0x00uy
    | VarArg -> 0x05uy)
