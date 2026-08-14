# Cons-list removal: review follow-ups

## **`Vesper.Core` cannot IMPLEMENT its own capability types.**

`seq`, `enumerator` and `disposable` are `extern interface`s. Inside the declaring package
`interface seq<'T> with` fails with `Type 'seq' is not an interface` — Core can implement
nominal interfaces (`Fun`) but no extern one.

NAMING one in an emitted signature now works: the intrinsic-repr arm of the signature encoder
instantiates a generic capability off its `(# … #)` repr, which is why `IFormatSink.Sequence`
takes `seq<obj>`. Only the implement side is still open.

This is why the shared `%A` boxing adapter (`BoxedItems`) lives in `Vesper.List` rather than
beside `IFormatSink` in Core: it has to implement `seq<obj>`, and every other collection
package already depends on `Vesper.List`.

## Generated `.mjs` diffs churn on every edit

Minted JS names are `_s<n>` / `_m<n>` off the file-wide TAST pool slot, so inserting one
declaration renumbers every name after it: adding `BoxedItems` to `list.fs` rewrote ~100
lines of `Vesper.List.mjs` that did not change. The committed runtime assets are therefore
not reviewable by diff.

Fixing it means numbering minted names densely PER TOP-LEVEL DECLARATION at emit time
(dense-per-module does not help — an insertion still shifts everything after it), which needs
a renumbering table threaded through the ~34 name-allocation sites in the JS backend and a
shadowing argument for each emission form. Deferred as cosmetic: the failure mode of getting
it wrong is silently-wrong JS.

## Overload buckets appended at the end — `NominalEmit.fs:72-77`

```fsharp
emittedMembers.[mem.Name] <- prior @ [ em ]
```

Read the bucket, append one at the end, write it back, once per member. Buckets are overload
counts, so this is O(k²) with k ~2 — cost is irrelevant, it is listed because it is the
clearest instance of the anti-use and the fix is `em :: prior` plus a `List.rev` at the read if
declaration order matters there (CHECK — it may, for overload resolution).
