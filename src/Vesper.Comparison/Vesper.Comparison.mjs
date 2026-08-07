// Vesper.Comparison — the JS structural comparator. Hand-authored and committed, named by
// the manifest's `[targets.js] runtime` key; `a < b` emits `structuralCompare(a, b) < 0`.
// Values that `structuralEquals` (Vesper.Core.mjs) calls equal must compare 0.

function cmpSign(d) { return d < 0 ? -1 : d > 0 ? 1 : 0; }

// `cmp` calls `a[Symbol.for("vesper.comparison")](b)` when both operands carry that slot —
// what a `[<CustomComparison>]` type emits — and `cmpSign` narrows its answer to -1/0/1.
// That method must compare FIELD values, re-entering `cmp`; `cmp` on its own `this` loops.
const COMPARISON = Symbol.for("vesper.comparison");
function cmp(a, b) {
  if (a === b) return 0;
  if (a === null || a === undefined) return (b === null || b === undefined) ? 0 : -1;
  if (b === null || b === undefined) return 1;
  const t = typeof a;
  // `false < true`; an F# `char` is a length-1 string, ordered by code unit.
  if (t !== "object") return a < b ? -1 : a > b ? 1 : 0;
  if (Array.isArray(a)) {
    const n = a.length < b.length ? a.length : b.length;
    for (let i = 0; i < n; i++) { const c = cmp(a[i], b[i]); if (c !== 0) return c; }
    return cmpSign(a.length - b.length);
  }
  if (typeof a[COMPARISON] === "function" && typeof b[COMPARISON] === "function") return cmpSign(a[COMPARISON](b));
  return cmpStructural(a, b);
}

function cmpStructural(a, b) {
  // A union's `$type` (a prototype getter, the qualified name) orders first, so `None` and
  // `Empty` — both `{ tag: 0 }` — do not compare 0. Records and plain cells have none.
  if (a.$type !== undefined && b.$type !== undefined) {
    const c = cmp(a.$type, b.$type);
    if (c !== 0) return c;
  }
  // `tag` orders before any field — compared here, not left to own-key order (hence the skip).
  if ("tag" in a) { const c = cmp(a.tag, b.tag); if (c !== 0) return c; }
  for (const k of Object.keys(a)) {
    if (k === "tag") continue;
    const c = cmp(a[k], b[k]);
    if (c !== 0) return c;
  }
  return 0;
}

export const structuralCompare = (a, b) => cmp(a, b);
