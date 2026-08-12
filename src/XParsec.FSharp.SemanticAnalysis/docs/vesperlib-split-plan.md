# Split `VesperLib.fs`

`VesperLib.fs` is 1595 lines. It crossed 1000 long before the curried-`.fsi`-member work and
has been growing under it: that commit added a third member-signature concern to a file whose
other 1500 lines are contract *extraction*. The follow-up quality pass (A–F) took ~20 lines back
out; the file is still the largest in the project by a wide margin.

Nothing here is a bug. This is a decomposition task, and the only reason it was deferred is that
it touches no behaviour and would have buried the A–F review changes in a 1500-line move diff.

## Why the file is hard to work in

One `module VesperLib` holds four passes that share nothing but `ExtractCtx`:

| lines | concern |
|------:|---------|
| 21–235 | freeze helpers: `freezeBodyType`, `freezeInterfaces`, `setterSignature`, `freezeMemberSig`, `resolveConstraints`, `finalizeVal` |
| 237–449 | `finalizeDeferred` — the one pass that drives them |
| 450–1122 | declaration extraction: vals, type registration, abbrev / record / union / enum bodies, members, bodied class-likes |
| 1124–1300 | `extractTypeSig` |
| 1303–1595 | module / namespace walk and the `extractSymbols` / `extractIntrinsicReprs` entry points |

Every helper is `let private`, so the compiler enforces nothing about which pass may call
which — and in fact they barely do. Verified by grep: the whole 21–235 block is reached only
from `finalizeDeferred`, with a single exception (`typeNameHasStructAttr`, also used at 1016 by
`extractBodiedClassLike`). The freeze half and the extraction half are already decoupled; only
the file boundary says otherwise.

## The split

The `VesperLib/` folder already holds this shape — `Manifest.fs`, `TyparCapture.fs`,
`TypeTranslate.fs`, each a `module VesperLib<Concern>` in namespace
`XParsec.FSharp.SemanticAnalysis`, `open`ed by name from `VesperLib.fs`. Three more files in the
same style, inserted between `TypeTranslate.fs` and `VesperLib.fs` in the `.fsproj`:

1. **`VesperLib/Finalize.fs`** — `module VesperLibFinalize`, ~430 lines.
   Lines 21–449 minus `typeNameHasStructAttr`. Self-contained: the freeze helpers become
   `private` to this module and only `finalizeDeferred` is exposed.

2. **`VesperLib/ExtractTypes.fs`** — `module VesperLibExtractTypes`, ~660 lines.
   Lines 527–1122 plus `typeNameHasStructAttr`: type-declaration registration and the per-kind
   body extractors.

3. **`VesperLib.fs`** keeps ~490 lines — val extraction (450–525), `extractTypeSig`, the
   module/namespace walk, and the two entry points. This is the file's actual subject and the
   name then fits it.

Optionally a fourth cut at `extractTypeSig` if (3) is still felt to be doing two things; that
one is a judgement call better made once the first three land.

### Constraints to respect

- **F# top-down order.** The split must follow existing call order. It does: 21–449 calls only
  into `TyparCapture` / `TypeTranslate`, and 527–1122 calls only into those plus
  `typeNameHasStructAttr`. No cycle, so no forward reference to design around.
- **`let private` → module-scoped.** Helpers crossing a new file boundary have to widen. Keep
  the widening minimal: expose `finalizeDeferred` and the extraction entry points, leave
  everything else `private` in its new module. If a helper turns out to need widening that the
  call graph does not justify, that is a signal the cut is in the wrong place.
- **Pure move.** No renames, no signature changes, no comment rewrites in the same commit. A
  refactor that relocates a doc block is the moment it is least likely to be checked; keeping
  the diff a verifiable move is what makes that safe. Comment corrections, if any are wanted,
  go in a separate pass afterwards.

### Verification

`git diff --stat` should show ~1100 lines moved and near-zero net change. Then a full
`-Action Build` plus the `XParsec.FSharp.SemanticAnalysis.Tests` (1347), `Codegen.Js.Tests`
(649) and `Codegen.Clr.Tests` (1465) suites, all of which are green at the time of writing.

## Related

`ExternalSymbols.fs` is 976 lines and rising — it gained the `ExternalSignature` module in the
A–F pass. It is not over the line yet, but it holds the `ExternalSignature` / `ExternalMember` /
`ExternalTypeShape` / `IExternalSymbol*` families together, and the signature half now has a
clean seam. Worth watching rather than acting on.
