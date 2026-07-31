namespace Vesper

/// <summary>An abbreviation for the CLI type <see cref="T:System.Char"/>.</summary>
///
/// <category>Basic Types</category>
type char = extern

/// <summary>An abbreviation for the CLI type <see cref="T:System.String"/>.</summary>
///
/// <category>Basic Types</category>
///
/// <remarks>Declares NO <c>(+)</c>, unlike every numeric width — concatenation is still
/// the one surviving clause on the operator itself. What blocks it is specific to a BCL
/// CALL body (the numeric widths' are mnemonics): the <c>{ platform -&gt; canon }</c> map
/// that presents a <c>System.String</c> PARAMETER as a Vesper <c>string</c> is folded
/// from the REFERENCED packages' contracts, and Vesper.Core references none — so inside
/// its own compile the two are unrelated types and <c>String.Concat(x, y)</c> matches no
/// overload. Nor can the argument be spelled as the BCL face to meet it: annotation
/// resolution canonicalises <c>System.String</c> BACK to this type, so the asymmetry is
/// one-sided and has no source-level answer.</remarks>
type string = extern
