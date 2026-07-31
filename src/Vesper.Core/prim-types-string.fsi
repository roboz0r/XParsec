namespace Vesper

/// <summary>An abbreviation for the CLI type <see cref="T:System.Char"/>.</summary>
///
/// <category>Basic Types</category>
type char = extern

/// <summary>An abbreviation for the CLI type <see cref="T:System.String"/>.</summary>
///
/// <category>Basic Types</category>
///
/// <remarks>Declares NO <c>(+)</c> yet, unlike every numeric width — concatenation is
/// still the one surviving clause on the operator itself. The reason it could not move
/// (a BCL CALL body, where the numeric widths' are mnemonics, and the
/// <c>{ platform -&gt; canon }</c> map that presents a <c>System.String</c> PARAMETER as a
/// Vesper <c>string</c> being absent inside Core's own compile) is FIXED: a compilation
/// now names its own package, whose reverse axis seeds the metadata leaf. Moving the
/// declaration here is outstanding work, not a blocked case.</remarks>
type string = extern
