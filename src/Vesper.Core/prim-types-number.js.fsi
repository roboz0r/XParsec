namespace Vesper

// JS-only transparent abbreviation for the TS `number` token. Appended to the
// contract for the JS target ONLY (`files-js`): `number` is a TS concept with no
// CLR analog. A JS `number` IS `float` semantics, so this is a transparent alias of
// `float` (NOT an `extern` intrinsic, NOT a new numeric type) — it pins the COVARIANT
// identity: a JS `number` read as a value is a `float`. The front end widens `number`
// to the int/float/float32 family only at CONTRAVARIANT argument positions; that
// widening lives in the unifier, this file only pins the covariant identity.
//
// `float` is defined in the base `prim-types-float.fsi`, so this abbreviation resolves
// its RHS from the already-built base contract.

/// <summary>A transparent abbreviation for <c>float</c> — the covariant identity of
/// the TS <c>number</c> token (a JS <c>number</c> read as a value is a <c>float</c>).
/// The front end widens <c>number</c> to the int/float/float32 family at contravariant
/// argument positions; here it pins only the covariant read.</summary>
///
/// <category>Basic Types</category>
type number = float
