// Vesper.Core — JS runtime structural core.
//
// Committed platform-support asset — the JS analogue of Vesper.Printf's committed
// CLR DLL. Declared by this package's manifest `runtime-js` key, read by the JS
// backend through `ReferencedProject.runtimeModules`, materialised beside the
// output and imported as `./Vesper.Core.mjs`. The `ops-platform.js.fs` `=` / `<>` /
// `hash` base arms delegate to the non-inline `Vesper.Core` values
// `structuralEquals` / `structuralHash`, which the backend imports from here through
// the ordinary external-call path (`JsImports.addRef`) — so these two exports are
// the public surface, aliased like any other Vesper module function. They are FLAT
// (Fable-style) two-arg functions — a saturated `=` / `hash` call collapses its spine
// to `structuralEquals(a, b)` / `structuralHash(x)`; the recursive walkers
// (`eq` / `hashOf`) stay private and never cross the module boundary.
//
// Generic `eq` / `hashOf` keyed on value SHAPE, never on class
// identity. Both walkers handle the full emitted value surface: primitives
// (number / bigint / string / boolean — F# `char` is a length-1 string), tuples
// (arrays), and records / unions (plain objects; a union carries a numeric `tag`
// own-key). Because dispatch is structural, a value built inline (a `List_Cons`
// class instance) and one built by a structural runtime module (a plain
// { tag, Head, Tail } cell) compare and hash identically — the same interop the
// Step 5b list runtime relies on.
//
// TYPE BRAND: emitted union instances carry a non-enumerable `$type` (the type's
// qualified name) on their prototype, so `eq` / `structuralCompare` distinguish two
// different unions' nullary cases (both `{ tag: 0 }` — None vs Empty) instead of
// reporting them equal. The brand is checked only when BOTH operands carry it, so a
// hand-built plain `{ tag, … }` cell still interoperates structurally with an emitted
// instance (the Step 5b/6 invariant). Records and plain classes are NOT branded (only
// unions are). Residual erasure corners, both unreachable through `=` (which is
// statically same-typed): two BRAND-LESS plain cells of different nullary cases still
// compare equal; and two structurally identical record values of different types
// compare equal. A future per-type `Equals`/`CompareTo` on the prototype would also
// serve custom equality and monomorphic dispatch.
//
// Route B (the --compiling-fslib bootstrap) eventually replaces this hand-authored
// file with a backend-compiled module — same shape, same imports.

// DISPATCHER vs STRUCTURAL CORE split. `eq` dispatches to a per-instance `Equals` when
// present — the override slot for a future `[<CustomEquality>]` type, which emits its
// own `Equals` method that the backend picks up by presence (no shared base class).
// `eqStructural` is the non-dispatching shape walk and the DEFAULT path: ordinary
// unions/records carry no `Equals`, so structural equality (`$type` + own-keys) applies.
// The no-loop invariant for a custom override: its `Equals` must compare FIELD values
// (re-entering `eq`), never call `eq` on its own `this`. A plain `{ tag, … }` cell has
// no `Equals` and falls straight to `eqStructural` — the Step 5b/6 cell-vs-instance
// interop is preserved.
function eq(a, b) {
  if (a === b) return true;
  if (a === null || a === undefined || b === null || b === undefined) return false;
  if (typeof a !== "object") return false;
  if (Array.isArray(a)) {
    if (!Array.isArray(b) || a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) { if (!eq(a[i], b[i])) return false; }
    return true;
  }
  if (typeof a.Equals === "function" && typeof b.Equals === "function") return a.Equals(b);
  return eqStructural(a, b);
}

function eqStructural(a, b) {
  if (Array.isArray(b)) return false;
  // Emitted union instances carry a non-enumerable `$type` brand (the type's qualified
  // name) on their prototype (records and plain classes are unbranded). When BOTH
  // operands are branded, a mismatch is unequal — this closes the erasure corner where
  // two different unions' nullary cases (both `{ tag: 0 }`) compared equal. The check is
  // tolerant: a plain `{ tag, … }` cell has no `$type`, so a cell-vs-instance comparison
  // skips it and stays structural
  // (the Step 5b/6 interop invariant). `$type` is non-enumerable, so the own-key walk
  // below never sees it; `hashOf` deliberately ignores it so equal values still hash
  // equal (a branded instance and an equal plain cell).
  if (a.$type !== undefined && b.$type !== undefined && a.$type !== b.$type) return false;
  const ka = Object.keys(a);
  const kb = Object.keys(b);
  if (ka.length !== kb.length) return false;
  for (const k of ka) {
    if (!Object.prototype.hasOwnProperty.call(b, k)) return false;
    if (!eq(a[k], b[k])) return false;
  }
  return true;
}

function combineHash(h, x) { return (Math.imul(h, 31) + x) | 0; }

function stringHash(s) {
  let h = 0;
  for (let i = 0; i < s.length; i++) { h = (Math.imul(h, 31) + s.charCodeAt(i)) | 0; }
  return h;
}

// DISPATCHER (`hashOf`) vs STRUCTURAL CORE (`hashStructural`), same shape as `eq`:
// `hashOf` defers to a per-instance `GetHashCode` (the override slot for a future
// custom-equality type), else the default `hashStructural` walk. Hashing IGNORES
// `$type` (it is non-enumerable, never an own-key), so an equal branded instance and
// plain cell still hash equal — the brand discriminates equality, not the hash.
function hashOf(x) {
  if (x === null || x === undefined) return 0;
  const t = typeof x;
  if (t === "number") return x | 0;
  if (t === "bigint") return Number(BigInt.asIntN(32, x));
  if (t === "boolean") return x ? 1 : 0;
  if (t === "string") return stringHash(x);
  if (Array.isArray(x)) {
    let h = 0;
    for (let i = 0; i < x.length; i++) { h = combineHash(h, hashOf(x[i])); }
    return h;
  }
  if (typeof x.GetHashCode === "function") return x.GetHashCode();
  return hashStructural(x);
}

function hashStructural(x) {
  let h = 0;
  for (const k of Object.keys(x)) { h = combineHash(h, hashOf(x[k])); }
  return h;
}

// Public entries — the surface the backend imports. The flat (Fable-style) compiled
// form: a saturated `=` call collapses to `structuralEquals(a, b)`; `hash` to a single
// `structuralHash(x)`.
export const structuralEquals = (a, b) => eq(a, b);
export const structuralHash = (x) => hashOf(x);
