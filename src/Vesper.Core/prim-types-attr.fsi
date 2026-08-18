namespace Vesper

/// <summary>The base type for attributes: an attribute is any type that inherits
/// this.</summary>
///
/// <category>Basic Types</category>
[<AbstractClass>]
type Attribute = extern class with

    /// <summary>Creates the base an attribute inherits.</summary>
    new: unit -> Attribute

/// <summary>Adding this attribute to class definition makes it abstract, which means it need not
/// implement all its methods. Instances of abstract classes may not be constructed directly.</summary>
and AbstractClassAttribute =
    inherit Attribute

    /// <summary>Creates an instance of the attribute</summary>
    new: unit -> AbstractClassAttribute
