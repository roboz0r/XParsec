namespace Vesper

#nowarn "42"

// Impl side: per-target binding for the attribute base. Inheriting `Attribute`
// is the Vesper way to declare an attribute; the target supplies the underlying
// mechanism.

[<AbstractClass>]
type Attribute = (# "System.Attribute" #)
