namespace XParsec.FSharp.Codegen.Clr

// The `cil { }` computation expression — thin sugar over `Cil.zero` /
// `Cil.combine` for composing typed `Op`s.

type CilBuilder() =
    member inline _.Zero() : Op<'x, 'x> = Cil.zero
    member inline _.Yield(op: Op<'i, 'o>) : Op<'i, 'o> = op
    member inline _.Delay(f: unit -> Op<'i, 'o>) : unit -> Op<'i, 'o> = f
    member inline _.Run(f: unit -> Op<'i, 'o>) : Op<'i, 'o> = f ()
    member inline _.Combine(a: Op<'i, 'm>, b: unit -> Op<'m, 'o>) : Op<'i, 'o> = Cil.combine a b
    member inline _.Bind(m: OpV<'i, 'm, 'a>, f: 'a -> Op<'m, 'o>) : Op<'i, 'o> = Cil.bind m f

[<AutoOpen>]
module CilBuilderExpr =
    let cil = CilBuilder()
