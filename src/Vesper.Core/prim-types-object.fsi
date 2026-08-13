namespace Vesper

/// <summary>An abbreviation for the CLI type <see cref="T:System.Object"/>.</summary>
///
/// <category>Basic Types</category>
type obj = extern class with
    /// <summary>Equatable but NOT comparable: every value has reference identity, none
    /// has an ordering.</summary>
    interface equatable<obj>

    /// <summary>Creates an object instance.</summary>
    new: unit -> obj

/// <summary>An abbreviation for the CLI type <see cref="T:System.Object"/> or null.
/// With the 'nullable reference types' feature, this is an alias to 'obj | null'.</summary>
///
/// <category>Basic Types</category>
type objnull = obj | null
