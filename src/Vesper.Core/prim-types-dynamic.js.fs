namespace Vesper

#nowarn "42"

// JS-only intrinsic binding for the opaque `dynamic` type. It exists ONLY on the JS
// target (no CLR analog), so it ships as a `files-js` contract with no base `.fs`/`.fsi`
// — this single companion is BOTH the primitive *marker* (publishing the `extern` as an
// `Intrinsic`, not an opaque `Class`) AND the `platform` face for JS. It projects to the
// JS `any` runtime tag.
//
// The `canon` (identity) key is the `.fsi` name `dynamic`; the JS platform tag is `"any"`
// (the TS type it lands from). `dynamic` gets no special unifier behaviour — it is a
// plain nominal intrinsic like `undefined`, so the front end threads it as
// `TyConst "dynamic"` everywhere with no new DU case.

type dynamic = (# "any" #)
