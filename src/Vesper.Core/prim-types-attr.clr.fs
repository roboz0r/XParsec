namespace Vesper

#nowarn "42"

[<AbstractClass>]
type Attribute = (# class "System.Attribute" #)

and AbstractClassAttribute() =
    inherit Attribute()
