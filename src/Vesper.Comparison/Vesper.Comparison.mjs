// Vesper.Comparison — JS runtime structural comparator,
// the ordering analogue of Vesper.Core.mjs's `structuralEquals`. A committed
// platform-support asset declared by this package's manifest `runtime-js` key; the
// backend materialises it beside the output and the `comparison.js.fs` aggregate
// `< > <= >=` bases import the curried `structuralCompare` through the ordinary
// external-call path (`JsImports.addRef`). `cmp` stays private and uncurried.
//
// `cmp` mirrors F#'s structural `compare`, keyed on value SHAPE not class identity
// (so a `List_Cons` instance and a runtime { tag, Head, Tail } cell compare alike):
// primitives by `<` / `>` (F# `char` is a length-1 string; bigint among bigints),
// tuples (arrays) lexicographically then by length, unions by case `tag` first, then
// records / unions field-by-field in declaration (own-key) order. Returns a three-way
// sign (-1 / 0 / 1) the bases test against 0. It must agree with Vesper.Core.mjs's
// `eq` — equal values compare 0 — a cross-package contract the Step6 tests pin down.
//
// ERASURE CORNER (as in Vesper.Core.mjs): two nullary cases ({ tag: 0 }) of different
// types compare 0; F#'s types make that unreachable through `<` (statically
// same-typed). Route B (the --compiling-fslib bootstrap) eventually replaces this
// hand-authored file with a backend-compiled module — same shape, same import.

function cmpSign(d) { return d < 0 ? -1 : d > 0 ? 1 : 0; }

function cmp(a, b) {
  if (a === b) return 0;
  if (a === null || a === undefined) return (b === null || b === undefined) ? 0 : -1;
  if (b === null || b === undefined) return 1;
  const t = typeof a;
  // number / bigint / string / boolean all order by the JS relational operators
  // (false < true; F# char is a length-1 string ordered by code unit).
  if (t !== "object") return a < b ? -1 : a > b ? 1 : 0;
  if (Array.isArray(a)) {
    const n = a.length < b.length ? a.length : b.length;
    for (let i = 0; i < n; i++) { const c = cmp(a[i], b[i]); if (c !== 0) return c; }
    return cmpSign(a.length - b.length);
  }
  // A union discriminates on its case index before any field — compared here
  // explicitly so correctness does not ride on `tag` being the first own-key.
  if ("tag" in a) { const c = cmp(a.tag, b.tag); if (c !== 0) return c; }
  for (const k of Object.keys(a)) {
    if (k === "tag") continue;
    const c = cmp(a[k], b[k]);
    if (c !== 0) return c;
  }
  return 0;
}

// Public entry — the surface the backend imports. The flat (Fable-style) compiled
// form: a saturated structural `compare` / `<` collapses to `structuralCompare(a, b)`.
export const structuralCompare = (a, b) => cmp(a, b);
