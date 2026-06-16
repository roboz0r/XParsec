// Vesper.Core — JS runtime structural core (codegen-js-steps.md Step 6).
//
// Committed platform-support asset — the JS analogue of Vesper.Printf's committed
// CLR DLL. Declared by this package's manifest `runtime-js` key, read by the JS
// backend through `ReferencedProject.runtimeModules`, materialised beside the
// output and imported as `./Vesper.Core.mjs`. The `ops-platform.js.fs` equality /
// hash inline bodies reference `equals` / `structuralHash` verbatim from their
// `$N` templates, so these exports keep those exact (unaliased) names.
//
// Generic `equals` / `structuralHash` keyed on value SHAPE, never on class
// identity. Both walkers handle the full emitted value surface: primitives
// (number / bigint / string / boolean — F# `char` is a length-1 string), tuples
// (arrays), and records / unions (plain objects; a union carries a numeric `tag`
// own-key). Because dispatch is structural, a value built inline (a `List_Cons`
// class instance) and one built by a structural runtime module (a plain
// { tag, Head, Tail } cell) compare and hash identically — the same interop the
// Step 5b list runtime relies on.
//
// ERASURE CORNER (documented, not a bug for the supported set): type erasure makes
// every nullary case { tag: 0 } — None, Empty, a nullary Dot — structurally
// identical, so `equals` reports them equal. F#'s type system makes that
// comparison unreachable through `=` (it is statically same-typed); it only
// surfaces if such values are boxed into a shared `obj` collection. Closing it
// needs per-type methods carrying a type brand — the post-MVP slot that also
// serves custom equality and monomorphic-dispatch performance.
//
// Route B (the --compiling-fslib bootstrap) eventually replaces this hand-authored
// file with a backend-compiled module — same shape, same imports.

export function equals(a, b) {
  if (a === b) return true;
  if (a === null || a === undefined || b === null || b === undefined) return false;
  if (typeof a !== "object") return false;
  if (Array.isArray(a)) {
    if (!Array.isArray(b) || a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) { if (!equals(a[i], b[i])) return false; }
    return true;
  }
  if (Array.isArray(b)) return false;
  const ka = Object.keys(a);
  const kb = Object.keys(b);
  if (ka.length !== kb.length) return false;
  for (const k of ka) {
    if (!Object.prototype.hasOwnProperty.call(b, k)) return false;
    if (!equals(a[k], b[k])) return false;
  }
  return true;
}

function combineHash(h, x) { return (Math.imul(h, 31) + x) | 0; }

function stringHash(s) {
  let h = 0;
  for (let i = 0; i < s.length; i++) { h = (Math.imul(h, 31) + s.charCodeAt(i)) | 0; }
  return h;
}

export function structuralHash(x) {
  if (x === null || x === undefined) return 0;
  const t = typeof x;
  if (t === "number") return x | 0;
  if (t === "bigint") return Number(BigInt.asIntN(32, x));
  if (t === "boolean") return x ? 1 : 0;
  if (t === "string") return stringHash(x);
  if (Array.isArray(x)) {
    let h = 0;
    for (let i = 0; i < x.length; i++) { h = combineHash(h, structuralHash(x[i])); }
    return h;
  }
  let h = 0;
  for (const k of Object.keys(x)) { h = combineHash(h, structuralHash(x[k])); }
  return h;
}
