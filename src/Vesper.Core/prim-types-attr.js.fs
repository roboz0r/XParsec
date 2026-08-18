namespace Vesper

#nowarn "42"

[<AbstractClass>]
type Attribute = (# class "!Vesper.Attribute" #)

and AbstractClassAttribute() =
    inherit Attribute()
