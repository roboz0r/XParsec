// Method call with a type-annotated argument expression `meth(name: type)`.
// The inner `:` is a type ascription on a value, not a named-argument syntax.
// FxResolver.fs uses this in `if not (p.WaitForExit(timeout: int))`.

open System.Diagnostics

let wait (p: Process) (timeout: int) =
    if not (p.WaitForExit(timeout: int)) then
        raise (System.TimeoutException())
    else
        p.WaitForExit()
