# Exception type identity on JS

**Status (2026-08-16): the lowering has LANDED; what is left is the catch that would observe
it.** Delete when `TryWith` lands and the corpus row is green (`feedback_plan_docs_ephemeral`).

## What landed

- **The roster is BCL-shaped.** `exceptions.js.fsi` / `.fs` declare `SystemException` under
  `exn` and every other root under it, with `ArgumentNullException : ArgumentException`.
- **It emits real classes.** `Vesper.Core/exceptions.mjs` is `class SystemException extends
  Error` and so on down, each constructor chaining `super(message)`. Committed and pinned
  regenerable by `ExceptionTests`, like `Vesper.Option.mjs`.
- **A construction site names the class.** `raise (ArgumentException "bad")` imports
  `ArgumentException` from `Vesper.Core`'s barrel and constructs it, where it used to flatten to
  `new Error("bad")`. A caught `ArgumentNullException` reports its own `constructor.name` and is
  still `instanceof Error`, which the suite runs under Node.
- **Erasure narrowed to what has no class behind it.** A base whose chain reaches a repr naming
  a runtime class (`exn` → `Error`) is `extends`-ed; one reaching a SENTINEL
  (`"!Vesper.Attribute"`) is still dropped, because there is nothing to extend and nothing
  constructs it. An ordinary class hierarchy, which bottoms out in neither, still fails loudly:
  its members lower to free `Type__member` functions that no prototype chain would dispatch.

## What it does NOT yet do

- **`try … with` is unemittable on BOTH backends.** `TExprG.TryWith` parses, infers and freezes,
  but neither `Codegen.Clr/EmitExpr.fs` nor `Codegen.Js/EmitJs.fs` has an emit arm. So no F#
  program can yet ask the question the prototype chain now answers, and the only catching test
  in the JS suite reaches for a raw JS `try/catch` template to get at it. That is the next piece
  of work, and it is not small.
- **`instanceof` is realm-fragile** (iframes, workers, `vm` contexts). The prototype chain is
  what `extends` buys; how a catch INTERROGATES it is the catch lowering's choice, and this
  codebase already has the better answer — `Vesper.Core.mjs` brands union prototypes with a
  non-enumerable `$type` and keys the equality registry by `Symbol.for("vesper.equality")`.
- **A host-originated `TypeError` has no Vesper identity**, so `:? NullReferenceException` stays
  a CLR-only proposition unless the catch lowering also translates host errors — which is
  probably not worth doing.
- **`checkedDivisor` still throws a bare `exn`** carrying the BCL's message ("Attempted to
  divide by zero.") because `DivideByZeroException` is not in the roster. It is hand-authored
  in `Vesper.Core.mjs`, which now sits beside a generated `exceptions.mjs` it could import from,
  so the blocker is only that the type is undeclared. The corpus's `arith-div-by-zero` row will
  say so once catches are nominal.
- **The `exn` ROOT still drops every constructor argument past the first** (`EmitJs.fs`, the
  `ExnRepr` arm), because `Error` has no slot for more. A declared class takes them all, so this
  now bites only `exn` itself. Relatedly the roster declares `ArgumentNullException(message)`
  where the BCL takes `paramName`.
