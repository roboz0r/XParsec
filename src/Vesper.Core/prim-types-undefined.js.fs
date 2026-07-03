namespace Vesper

#nowarn "42"

// JS-only intrinsic binding for the absence-sentinel type `undefined`. It exists
// ONLY on the JS target (no CLR analog), so it ships as a `files-js` contract with no
// base `.fs`/`.fsi` — this single companion is BOTH the primitive *marker* (publishing
// the `extern` as an `Intrinsic`, not an opaque `Class`) AND the `platform` face for
// JS. It projects to the JS `undefined` runtime tag.
//
// `undefined` is a DISTINCT type identity from `unit` (whose own JS repr is also
// `undefined`). The `canon` (identity) key is the `.fsi` name itself, so the unifier
// keeps them separate — `canonName` resolves `undefined` forward through this published
// intrinsic (which precedes the reverse map that otherwise collapses the shared
// `"undefined"` platform repr onto `unit`). The repr coincidence unit→`undefined` at the
// VALUE level stays a backend fact and never collapses the type identities.
//
// (`null`, the sibling sentinel, is not declared — `null` is a reserved keyword and is
// already distinct from `unit` without a registered intrinsic; see the `.fsi`.)

type undefined = (# "undefined" #)
