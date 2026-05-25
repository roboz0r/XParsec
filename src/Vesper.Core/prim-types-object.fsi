namespace Vesper

/// <summary>An abbreviation for the CLI type <see cref="T:System.Object"/>.</summary>
///
/// <category>Basic Types</category>
type obj = extern

/// <summary>An abbreviation for the CLI type <see cref="T:System.Object"/> or null.
/// With the 'nullable reference types' feature, this is an alias to 'obj | null'.</summary>
///
/// <category>Basic Types</category>
type objnull = obj | null
