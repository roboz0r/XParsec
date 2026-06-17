// Vesper.Printf — JS runtime for `%A` structural formatting (codegen-js-steps.md Step 6).
//
// Committed platform-support asset — the JS analogue of the committed Vesper.Printf
// CLR DLL (which compiles `structural-printer.fs`). Declared by this package's
// manifest `runtime-js` key, read by the JS backend through
// `ReferencedProject.runtimeModules`, materialised beside the output and imported as
// `./Vesper.Printf.mjs`. The backend's `EmitJs.buildHole` lowers every `%A`
// (`Structured`) format hole to a curried call of the `structuralFormat` export
// (`structuralFormat(value)(width)(size)`), imported + `$`-aliased like any other
// Vesper module function.
//
// Why a hand-authored JS runtime rather than the CLR `structural-printer.fs`: that
// file is interface-dispatch (it resolves records / unions via the per-type
// synthesised `IStructuralFormattable.Format`) and BCL-heavy (`Span` / `ArrayPool` /
// `ITuple` / `IEnumerable`). The JS target abandoned per-type emission (the Step 5b/6
// shape-keyed interop invariant — `.tag` + own-keys, never `instanceof`), so the
// dispatch core cannot be shared as-is. This file is the shape-keyed JS counterpart;
// the shared-core refactor (a `structural-printer.js.fs` compiled here) is the
// tracked successor (see printf-shared-core-plan.md). The leaf/layout *forms* this
// file emits deliberately match the CLR `StructuralPrinter`'s flat output.
//
// Output is always FLAT (single line). The CLR engine's group-based line breaking at
// the `width` budget is the deferred refinement; `width` is accepted but unused, so
// JS `%A` behaves like the CLR `%0A` (never-break) mode. The `size` budget is F#'s
// PrintSize node count: each leaf spends one unit and a collection caps at 100
// elements; past the budget further values render `...` (the `%.NA` mode).
//
// ERASURE CORNERS (documented, unreachable through well-typed `%A`): a length-1
// string and an F# `char` are both JS strings, so a char renders `"a"` not `'a'`; an
// integer-valued `float` (3.0) and an `int` (3) are both JS numbers, so a whole float
// renders `3` not `3.0`. F#'s static types make either confusion unreachable through a
// single `%A`; it only surfaces under `obj`-boxing (out of MVP scope).

function fmtString(s) {
  let out = "\"";
  for (let i = 0; i < s.length; i++) {
    const c = s[i];
    if (c === "\\") out += "\\\\";
    else if (c === "\n") out += "\\n";
    else if (c === "\r") out += "\\r";
    else if (c === "\t") out += "\\t";
    else if (c === "\"") out += "\\\"";
    else out += c;
  }
  return out + "\"";
}

// A union value: a non-array object carrying a numeric `tag` own-key and the
// `cases()` discriminator method every emitted union base class declares.
function isUnion(v) {
  return v !== null && typeof v === "object" && !Array.isArray(v)
    && typeof v.tag === "number" && typeof v.cases === "function";
}

// A Vesper cons-list: a union whose declaration-order cases are exactly
// `["Empty", "Cons"]`. Rendered `[a; b; c]` (F#'s list form), not as the raw union.
function isVesperList(v) {
  if (!isUnion(v)) return false;
  const cs = v.cases();
  return cs.length === 2 && cs[0] === "Empty" && cs[1] === "Cons";
}

// Render a union payload in argument position. A payload-bearing union parenthesizes
// (`Some (Circle 5)`); tuples / lists / records carry their own delimiters already.
function fmtArg(v, budget) {
  if (isUnion(v) && !isVesperList(v) && Object.keys(v).length > 1) {
    return "(" + fmtValue(v, budget) + ")";
  }
  return fmtValue(v, budget);
}

function fmtValue(v, budget) {
  if (v === undefined) return "()";          // unit
  if (v === null) return "null";
  const t = typeof v;
  if (t === "number") { budget.n--; return String(v); }
  if (t === "bigint") { budget.n--; return String(v) + "L"; }   // int64 / uint64
  if (t === "boolean") { budget.n--; return v ? "true" : "false"; }
  if (t === "string") { budget.n--; return fmtString(v); }
  if (Array.isArray(v)) {
    return "(" + v.map((x) => fmtValue(x, budget)).join(", ") + ")";   // tuple
  }
  if (isVesperList(v)) {
    const parts = [];
    let cur = v;
    let i = 0;
    while (cur.tag === 1) {
      if (i >= 100 || budget.n <= 0) { parts.push("..."); break; }
      parts.push(fmtValue(cur.Head, budget));
      cur = cur.Tail; i++;
    }
    return "[" + parts.join("; ") + "]";
  }
  if (isUnion(v)) {
    const name = v.cases()[v.tag];
    const fields = Object.keys(v).filter((k) => k !== "tag");
    if (fields.length === 0) return name;
    if (fields.length === 1) return name + " " + fmtArg(v[fields[0]], budget);
    return name + " (" + fields.map((k) => fmtValue(v[k], budget)).join(", ") + ")";
  }
  // A record (or any other plain object): `{ F = v; G = w }` in own-key order.
  return "{ " + Object.keys(v).map((k) => k + " = " + fmtValue(v[k], budget)).join("; ") + " }";
}

// Public, curried entry — the surface the backend imports (`structuralFormat(v)(width)(size)`).
export const structuralFormat = (value) => (width) => (size) => fmtValue(value, { n: size });
