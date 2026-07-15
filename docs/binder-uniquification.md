# Binder uniquification — a shared pass still owed

The class-preamble work already **rejects** any duplicate field name across the four
field-minting families (ctor params, `val` fields, static lets, instance lets) — a conservative
rejection of valid F# (F# uniquifies by source position: `v@4` beside `v`, `x@10` beside `x`),
sound because on CLR a `Field` row is identified by `(Parent, Name, Signature)` and static-ness
lives in the *flags*, so two same-named fields are a duplicate row, not two fields. That part is
in the code.

**What is still owed is the general case.** Ordinary local `let` shadowing miscompiles on JS
today:

```fsharp
let x = 1
let x = x + 1   // in statement position
```

emits `const x = …; const x = …;` into one block — a hard JS `SyntaxError`. It survives inside a
function body only by accident: a pure `let` is substituted away, and an impure one lowers to its
own IIFE arrow (a fresh JS scope per binder). The break is confined to `JsStatement.Const` in a
flat statement list. CLR is safe — `EmitBindings` keys a local slot by `NodeKey` and IL locals are
unnamed.

So the pass is owed generally, not just for preamble binders. F# uniquifies by source position and
`NodeKey.Offset` already gives exactly that — a general uniquify-or-reject keyed on
`NodeKey.Offset` closes both the preamble family and the flat-statement local-`let` case with one
mechanism.
