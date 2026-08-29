# Vesper follow-ups plan

Items found while running the comment-hygiene sweep over every `src/Vesper.*`
directory (2026-08-07). The sweep itself is finished and changed no code — this
file is the record of everything it surfaced that *is* a code change, deferred
so the sweep could stay mechanically verifiable.

**Ephemeral.** Delete this file once the items land. Anything here that turns out
to be durable belongs in the code — a type that makes it correct by construction,
failing that a sited comment or a test.

Sibling docs in this directory use `-followups-plan.md`; this one is spelled
`-follow-ups-` as requested. Rename if the inconsistency grates.

## Verification legend

| mark | meaning |
| --- | --- |
| **[V]** | traced to the cited lines directly, in this session |
| **[R]** | reported by a sweep agent with cited evidence; spot-check before acting |

Reading every comment as an unverified claim turned up ~25 false or fabricated
ones. Those were *corrected in place* and are not listed here. What follows is
only the residue that needs a decision about code.

---

## A. Correctness

### A1. `%A` output diverges between targets **[V]**

```
structural-printer.js.fs:192   Text(cat (toStr v) "L")   // int64 / uint64
structural-printer.clr.fs:131  | :? int64  -> "L"
structural-printer.clr.fs:132  | :? uint64 -> "UL"
```

A `uint64` prints `5L` on JS and `5UL` on CLR — the JS comment acknowledges the
arm covers both. Separately the JS side has no `fixFloat` equivalent, so `3.0`
prints `3` on JS (`Text(toStr v)` at `:189`) and `3.0` on CLR. `%A` output is meant
to be copy-pasteable F#; on JS it currently is not.

### A2. `BigInteger.op_Implicit` has no visible resolution path on CLR **[V]**

```
ops-platform.clr.fs:102   let inline bigint (value: int32) : bigint =
                              System.Numerics.BigInteger.op_Implicit(value)
MetadataSymbols.fs:393    |> Array.filter (fun m -> not m.IsSpecialName)
```

`op_Implicit` is a `SpecialName` method, and the external-member walk filters
those out. `prim-types-string.clr.fs` cites this same mechanism as its reason for
calling `String.Concat` rather than `op_Addition`. Grepping `src/` finds no
special case for `op_Implicit`/`op_Explicit` outside plan docs.

Latent unless `bigint` is exercised — which is probably why it has not surfaced.
Either widen the walk for conversion operators or give `bigint` a different body.

### A3. `Vesper.Comparison.mjs` `cmp` disagrees with `Vesper.Core.mjs` `eq` **[V]**

Two guards `eqStructural` has and `cmpStructural` does not:

- `Vesper.Comparison.mjs:18` — the `Array.isArray(a)` arm never tests `b`. Against
  a record, `b.length` is `undefined`, so the loop is skipped and the arm returns
  `cmpSign(a.length - undefined)` = `cmpSign(NaN)` = `0`: an array compares equal
  to a record. `eq` guards this — `eqStructural` opens with
  `if (Array.isArray(b)) return false`.
- `Vesper.Comparison.mjs:36` — `cmpStructural` walks `Object.keys(a)` with no
  `ka.length !== kb.length` check, which `eqStructural` does have. Extra own-keys
  on `b` are invisible, so `cmp` returns 0 where `eq` returns false.

Both are unreachable through a statically same-typed `<`, so this is a latent
asymmetry rather than a live bug. But the two runtimes have a stated contract
that values `structuralEquals` calls equal must compare 0, and it is not held by
construction. A shared guard, or one walker parameterised by its combiner, would
close it.

Same shape, lower stakes: `Vesper.Core.mjs:28-29` `eqStructural` calls
`Object.keys(b)` without confirming `b` is an object, so `eq({0:"a"}, "a")` is
`true`. **[R]**

---

## B. Contract / implementation gaps

### B1. `AppendForcedSignZeroPaddedFloat` is missing from `formatter.fsi` **[V]**

Defined at `formatter.clr.fs:179`, emitted by the CLR backend (it appears in
`XParsec.FSharp.Codegen.Clr`'s `FormatHandles`), absent from the contract. Every
other `Append*`/`Guard*` member is listed, so this reads as an omission rather
than a deliberate hide — and an `.fsi` that omits a public member makes it
inaccessible.

### B2. `list.fsi` declares six members `list.fs` does not implement **[R]**

`Empty`, `Item`, `GetSlice`, `GetReverseIndex`, `Cons`, `List.Create`.

### B3. `Vesper.List` algorithmic issues **[R]**

- `rev` is O(n²) — `append (rev t) (h :: [])` rather than an accumulator loop.
- `length`, `map`, `filter`, `append`, `rev` and `member this.Length` are all
  non-tail-recursive; a long list stack-overflows on both targets. FSharp.Core
  uses mutation for each.
- `ListEnumerator<'T>` is `[<Struct>]` but boxed at its only construction site
  (`list.fs:28`), and `[<Struct>]` is erased on JS. The attribute buys nothing.

### B4. Smaller ones **[R]**

- `Diagnostics.fs:102` **[V]** — `relFile`'s guard is
  `if rel = "" || rel.Contains ":" || rel.StartsWith "/"`, so a legitimate
  relative path containing a colon collapses to its basename. The comment above
  it claims only the different-drive case.
- `Program.fs` — `--globals`/`--lib-globals`/`--ambient-modules` dispatch via
  `when args.Length >= 4 && args.[0] = "…"` guards while `--package` uses a real
  array pattern. A mistyped invocation silently indexes `args.[3..]` as `.d.ts`
  paths.
- `TsInterop.isObjectTypeFlag` takes `tsExports` explicitly but every call site
  passes the same module-level `ts`.
- `StructuralFormatRecipe.fs`'s `BeginApplication`/`EndApplication` may be dead:
  `IFormatSink` has no such members and the CLR printer parenthesises via
  `LastAppShaped`. Check for a live consumer.
- `TypeMap.fs` — two deleted comments asserted the code throws on an unknown
  printed form. It does not; it degrades to an opaque `Structural` with a
  `StructuralObjectStubbed` warning. Decide which was intended.

### B5. Two contracts name types outside their dependency closure, and the members are dropped **[V]**

The `.fsi` front end drops a MEMBER whose signature names a type the compilation
cannot resolve, reporting `ConformanceVerdict.SignatureNotPublished` (`V245`);
each drop below is therefore a warning today and a hole in the published surface.
(Relocated from the deleted fsi-front-end-plan, 2026-08-28.)

- **`Vesper.List`'s `GetSlice` names `int option`** (`list.fsi:64`) while
  `manifest.*.toml` depends on `Vesper.Core` alone. Either the dependency is
  missing or the declaration does not belong in that contract. (The
  `ResizeArray` half of this item is FIXED: it moved to `list-bcl.clr.fsi`, a
  CLR-only signature file, because the RHS is a BCL type the js contract cannot
  name.)
- **`Vesper.Printf`'s `Formatter` constructors name `TextWriter` /
  `StringBuilder`** (`formatter.fsi:13,17`), the same BCL-only case as
  `ResizeArray` above — a `formatter-bcl.clr.fsi` split is the precedented fix.

---

## C. Type-refactor candidates

Each of these funded a comment block that the 3-line ceiling cut. The name of
that comment is the acceptance test: **if the refactor lands and the sentence
still needs writing, the refactor was the wrong shape.**

| # | change | comment it should delete |
| --- | --- | --- |
| C1 | `Schema.Member`: replace `Kind` + `Type: TypeRef option` + `Signatures: list` with a DU payload | "a property is `Some`/`[]`, a method is `None`/non-empty" — narrated in both `mapMember` and `ctorMemberOf` |
| C2 | `carryStructural`'s `faithful: bool` → a two-case result (`Faithful of fields * index \| Opaque`) | "fields non-empty IFF faithful" (was the 9-line block in `TypeMap.fs`) |
| C3 | `Signature.TypeParamBounds` → length-carrying representation | the arity doc **and** the runtime check at `Codec.fs:491` |
| C4 | `Export.Class` heritage: separate `Extends`/`Implements`, or a tagged entry | the 18-line `ExportMap.fs` header explaining that the provider re-disambiguates a flattened `TypeRef list` by name |
| C5 | `MapCtx.DeclaringEnv`/`MethodEnv` → distinct wrapper types | "a swap typechecks" (both are `Ts.Symbol list`) |
| C6 | `Extractor`: an `ExtractMode` DU feeding one driver, replacing five `run*` wrappers | the 14- and 11-line headers, plus three copies each of the `Version = None` / span-relativization / refs facts |
| C7 | `RuntimeFormatState`: fold `SemFrames` into `Frames`' `Collect` case | the hand-pop in `EndCaseP`/`FormatTuple` and `PopWrap`'s otherwise-unreachable `Collect` arm |
| C8 | `Dispatch` returns app-shapedness with the `Doc` instead of the mutable `LastAppShaped` | "every composite writer must reset it last" — the longest surviving doc in `structural-printer.clr.fs` |
| C9 | `SemiPersistentUnionFind`: a scoped/branching API so a superseded version is unusable | the type doc's invalidation contract (currently a runtime `invalidOp`) |
| C10 | `rerootToArray : PaCell -> ResizeArray<int>` | three identical `invalidOp "unreachable — reroot did not yield an array"` arms |
| C11 | `$type` brand → a per-type prototype slot | the brand contract, currently prose in `Vesper.Core.mjs`, `Vesper.Comparison.mjs` **and** `JsPrint.fs` |
| C12 | An `optionalField` helper in `Codec.fs` | three copies of the omit-when-empty rule |
| C13 | A signature marker for spliced/intrinsic-repr members | "spliced at the use site, never emitted", stated in `prim-types-int.fsi`, `prim-types-min.fsi` and `ops-platform.fsi` |

Lower priority: `rank`/`parent`/`count` travel as a positional triple through
`SemiPersistentUnionFind`'s constructor (the repo prefers records over wide
tuples); `Export.Interface`/`Export.Class` are 5- and 6-field DU tuples.

---

## D. Open question: `set.fsi` provenance **[R]**

`set.fsi:2` claims the file is a verbatim copy of `FSharp.Core/set.fsi` with
"namespace + `Microsoft.FSharp.*` opens the only diffs". But `Set.partitionWith`
(`set.fsi:767`, implemented in `set.clr.fs`) could not be accounted for as an
FSharp.Core member, and its example id `set-partitionWith-1` breaks the numeric-
suffix-free convention every neighbour uses (`set-add`, `set-remove`, …).

Either it is a Vesper addition — in which case line 2 is false and should say so
— or it comes from a newer FSharp.Core. Settling it needs the upstream file,
which is not in the repo. Worth resolving because the whole reason `set.fsi` was
left untouched by the sweep is that it is diffable against upstream.

---

## E. Deliberately not doing

- **Rewriting ported `.fsi` docs to hit a comment ratio.** `set.fsi`,
  `comparison.fsi`, `array.fsi`, `ops-std.fsi`, `prim-types-nd-array.fsi` and
  others are FSharp.Core transliteration. Their 0.1:1 ratios and 24-line blocks
  are correct, and cutting them destroys the re-sync property.
- **`src/Vesper.Ts.Extractor/vendor/TypeScript.fs`** — vendored Glutinum code
  (1632 comment lines), out of scope for any house convention.
- **The five generated `.mjs` files** (`Array`, `List`, `Option`, `Printf`,
  `Seq`). Only `Vesper.Core.mjs` and `Vesper.Comparison.mjs` are hand-authored.
- **The `#nowarn "42"` trailing comments.** Removed from the three files that had
  them, matching the 27 that did not. If the explanation is wanted, it should be
  on all 30.
- **`Unchecked.defaultof<'T>` yielding `null` on JS and `0` on CLR.** Intended, not
  a divergence to fix: `defaultof` is the platform-defined default of an *extern*
  type, so the target owns the default, and JS has no per-type zero. The `.fsi` now
  says so ("the target's own default"); the old doc described only the CLR body,
  and that wording was the sole defect. All three uses in shared code
  (`seq.fs:59`, `SemiPersistentUnionFind.fs:42-43`) are definite-assignment
  placeholders that never read it as a numeric zero.
