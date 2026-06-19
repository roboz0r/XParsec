// Vesper.Comparison — JS runtime structural comparator,
// the ordering analogue of Vesper.Core.mjs's `structuralEquals`. A committed
// platform-support asset declared by this package's manifest `runtime-js` key; the
// backend materialises it beside the output and the `comparison.js.fs` aggregate
// `< > <= >=` bases import the FLAT (Fable-style) two-arg `structuralCompare` through
// the ordinary external-call path (`JsImports.addRef`) — a saturated compare collapses
// to `structuralCompare(a, b)`. `cmp` stays private.
//
// `cmp` mirrors F#'s structural `compare`, keyed on value SHAPE not class identity
// (so a `List_Cons` instance and a runtime { tag, Head, Tail } cell compare alike):
// primitives by `<` / `>` (F# `char` is a length-1 string; bigint among bigints),
// tuples (arrays) lexicographically then by length, unions by case `tag` first, then
// records / unions field-by-field in declaration (own-key) order. Returns a three-way
// sign (-1 / 0 / 1) the bases test against 0. It must agree with Vesper.Core.mjs's
// `eq` — equal values compare 0 — a cross-package contract the Step6 tests pin down.
//
// TYPE BRAND (as in Vesper.Core.mjs): emitted instances carry a non-enumerable
// `$type`; when both operands are branded, a mismatch orders by it first, so two
// different unions' nullary cases ({ tag: 0 }) no longer compare 0 — agreeing with
// `eq`. Brand-less plain cells stay structural. Route B (the --compiling-fslib
// bootstrap) eventually replaces this hand-authored file with a backend-compiled
// module — same shape, same import.

function cmpSign(d) { return d < 0 ? -1 : d > 0 ? 1 : 0; }

// DISPATCHER vs STRUCTURAL CORE split, mirroring Vesper.Core.mjs's `eq`. `cmp`
// dispatches to a per-instance `CompareTo` when present — the override slot for a
// future `[<CustomComparison>]` type, which emits its own `CompareTo` (no shared base
// class). `cmpStructural` is the non-dispatching shape walk and the DEFAULT path:
// ordinary unions/records carry no `CompareTo`, so structural ordering applies. A custom
// override must compare FIELD values (re-entering `cmp`), never call `cmp` on its own
// `this`. A plain `{ tag, … }` cell has no `CompareTo` and stays structural.
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
  if (typeof a.CompareTo === "function" && typeof b.CompareTo === "function") return cmpSign(a.CompareTo(b));
  return cmpStructural(a, b);
}

function cmpStructural(a, b) {
  // A type-branded instance discriminates by type before anything else (mirrors
  // `eq`'s `$type` check, so equal values still compare 0). Checked only when BOTH
  // carry the brand, so a plain cell stays structural. See Vesper.Core.mjs.
  if (a.$type !== undefined && b.$type !== undefined) {
    const c = cmp(a.$type, b.$type);
    if (c !== 0) return c;
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
