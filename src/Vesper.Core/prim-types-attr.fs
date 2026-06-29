namespace Vesper

#nowarn "42"

// Impl side: per-target binding for the attribute base. Inheriting `Attribute`
// is the Vesper way to declare an attribute; the target supplies the underlying
// mechanism.

// `class`-tagged: a HERITABLE external reference base (derived types may
// `inherit Attribute`), not an opaque value repr. See compiler-attributes.fs.
[<AbstractClass>]
type Attribute = (# class "System.Attribute" #)
