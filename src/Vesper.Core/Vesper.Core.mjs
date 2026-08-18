// Vesper.Core — the JS structural runtime. Hand-authored and committed, named by the
// manifest's `[targets.js] runtime` key; the backend copies it beside its output and calls
// in flat: `a = b` emits `structuralEquals(a, b)`, `hash x` emits `structuralHash(x)`.

// `eq` calls `a[Symbol.for("vesper.equality")](b)` when both operands carry that slot —
// what a `[<CustomEquality>]` type emits. Such a method must compare FIELD values (which
// re-enter `eq`); calling `eq` on its own `this` loops.
const EQUALITY = Symbol.for("vesper.equality");
function eq(a, b) {
  if (a === b) return true;
  if (a === null || a === undefined || b === null || b === undefined) return false;
  if (typeof a !== "object") return false;
  if (Array.isArray(a)) {
    if (!Array.isArray(b) || a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) { if (!eq(a[i], b[i])) return false; }
    return true;
  }
  if (typeof a[EQUALITY] === "function" && typeof b[EQUALITY] === "function") return a[EQUALITY](b);
  return eqStructural(a, b);
}

function eqStructural(a, b) {
  if (Array.isArray(b)) return false;
  // A union's `$type` is a prototype getter (the qualified name) — never an own-key below;
  // records and plain classes have none. Compared only when BOTH carry it, so `None` and
  // `Empty` (both `{ tag: 0 }`) differ while a hand-built `{ tag: 0 }` cell still matches.
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

// `hashOf` calls `x[Symbol.for("vesper.hash")]()` when present — what `override
// GetHashCode` emits. `$type` is a prototype getter, so it is never walked: a branded
// instance and an equal hand-built `{ tag, … }` cell hash equal.
const HASH = Symbol.for("vesper.hash");
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
  if (typeof x[HASH] === "function") return x[HASH]();
  return hashStructural(x);
}

function hashStructural(x) {
  let h = 0;
  for (const k of Object.keys(x)) { h = combineHash(h, hashOf(x[k])); }
  return h;
}

// `src.GetEnumerator()` lowers to `enumeratorOf(src)`. JS's `next()` returns
// `{ value, done }` — one call for advance AND read — where the cursor splits them into
// `MoveNext()` / `Current()`, so the result parks in `cur` between the two.
class NativeEnumerator {
  constructor(iter) {
    this.iter = iter;
    this.cur = undefined;
  }
  MoveNext() {
    const r = this.iter.next();
    if (r.done) {
      this.cur = undefined;
      return false;
    }
    this.cur = r.value;
    return true;
  }
  Current() {
    return this.cur;
  }
  // A generator implements `return()` by running its `finally` blocks; a plain array
  // iterator has no `return`, hence the guard.
  [Symbol.dispose]() {
    if (typeof this.iter.return === "function") this.iter.return();
  }
}

export const structuralEquals = (a, b) => eq(a, b);
export const structuralHash = (x) => hashOf(x);
// Every `seq` source is wrapped, Vesper or native: `Symbol.iterator` is the only surface
// either has — a `seq<'T>` impl emits `*[Symbol.iterator]()`, a native array has nothing
// else — so there is no `GetEnumerator` on either to dispatch against.
export const enumeratorOf = (src) => new NativeEnumerator(src[Symbol.iterator]());
