# One front end for `.fsi`

**Status (2026-08-15): new plan.** Spun out of the `sig-only` discussion — this is the
enabler the other three plans depend on. Delete when it lands
(`feedback_plan_docs_ephemeral`).

## The gap: name resolution is implemented twice

A `.fs` goes through the real passes. A `.fsi` goes through `VesperLib.ExtractCtx`, which
resolves by **name string plus ambient prefixes** rather than by scope and `TypeKey`:

```fsharp
// AssemblyFiles.fs:231-233
let ctx = VesperLib.ExtractCtx.empty scope.Target
ctx.AmbientShapes <- (fun name -> scope.Visible.TryLookupType name |> ExternalSymbols.typeShapeOf)
ctx.DependencyAmbientPrefixes <- scope.Visible.AmbientOpenPrefixes
```

`ReferencedProject.fs:390-394` sets up the same thing for the other caller. An over-wide
lookup keyed on a written name IS a string key (`feedback_overwide_types_are_string_keys`);
the sink to narrow is `ExtractCtx`, not each call site.

`AttributeDecode.fs:6-7` states the consequence without naming the cause:

> Class-shaping attributes match on the long-ident's LAST SEGMENT, not on a resolved
> `TypeKey`: the `.fsi` extractor caller has no resolver.

So `AttributeDecode`'s name lists and `TypeTranslate.fs:166,171`'s `AutoOpen` /
`RequireQualifiedAccess` matching are not independent warts. They are what the mini-resolver
can express. Deleting the lists and deleting `ExtractCtx`'s lookup are one job.

## Expected dataflow (user, 2026-08-15)

Five files in compile order — `a.fsi`, `a.fs`, `b.fs`, `c.fsi`, `c.fs`:

- `a` and `c` pair; `b` does not.
- `a.fsi` and `a.fs` are both checked against the target provider.
- `a.fs` is turned into signatures + bodies.
- `a.fs`'s signatures are checked against `a.fsi`.
- The provider stacks with **`a.fsi`'s signatures and `a.fs`'s bodies**.
- `b.fs` is checked against the stacked provider, turned into signatures + bodies; no `b.fsi`,
  so no conformance check.
- The provider stacks with `b.fs`'s signatures + bodies.
- `c.fsi` and `c.fs` are checked against the stacked provider; `c.fs` → signatures + bodies;
  `c.fs`'s signatures checked against `c.fsi`.
- Codegen.

The publication half of this is already what the code does — `AssemblyFiles.fs:383-392`
("the `.fs`-derived signatures exist only to be checked against the published ones and
discarded; a `.fsi`'s are what survive") and `:399`
(`View = withInlineBodies bodies signatures`). What changes is that the `.fsi` half is
RESOLVED against the stack rather than extracted beside it.

## Two callers, one of them only apparently hard

- **`signatureView`** (`AssemblyFiles.fs:226`) already receives `composed`, the full provider
  stack. It has everything the real passes need and uses the mini-resolver anyway. Convert
  first; it is the one with no bootstrap question.
- **`buildProviderWith`** (`ReferencedProject.fs:384`) builds the provider *from* `files`, so
  only its dependencies' shapes are ambient. This looks circular and is not: it already
  iterates `manifest.Files` in declared order "so a later contract's RHS (`Vesper.disposable`)
  is already in the registry" (`:407-408`) — the same incremental accumulation
  `analyseAssemblyWith` performs over `.fs` units with its nearest-first `visible` stack
  (`AssemblyFiles.fs:412-417`). It is a fold over the provider stack, not a new mechanism.

## Constraints the merged path must keep

- **Pre-scan order.** Both callers extract intrinsic reprs from the `.fs` BEFORE the `.fsi`,
  so `type t = extern` picks `IntrinsicPlatform.Repr` over `Unsupported`
  (`AssemblyFiles.fs:235-237`, `ReferencedProject.fs:396-405`).
- **Declared order within a package**, per the fold above.

## Scope: resolution and constant folding, not inference

A `.fsi` spells out its types, so unification and inference are skippable. It is not
expression-free, though — attribute arguments need evaluating:

```fsharp
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false)>]
```

That is constant folding over enum literals plus named-property arguments. It is the one
expression form a contract carries, and it is exactly what
[attribute-representation-plan](attribute-representation-plan.md) needs in order for an
attribute to survive as written.

## What it unlocks

- `AttributeDecode`'s five name lists and `TypeTranslate.fs:166,171` are deleted rather than
  worked around.
- **Conformance can compare resolved signatures instead of CSTs.** `Conformance.fs:9-11` is
  explicit that today it checks "Presence, not signatures", over the parsed CSTs. A resolved
  `.fsi` is what would catch a contract whose declared types disagree with the compiler's —
  see [printf-contract-plan](printf-contract-plan.md) for a live instance.
