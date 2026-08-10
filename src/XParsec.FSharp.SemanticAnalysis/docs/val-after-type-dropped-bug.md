# A `val` written after a `type` in the same module body is silently dropped

Open defect. Not a plan: this file stays until the bug is fixed, then goes with it.

## Symptom

In a signature file, a `val` that follows a `type` declaration inside the SAME module body
never reaches the extracted symbol table. It lands in neither `ctx.Symbols` nor
`ctx.Skipped`, so nothing reports it and a use site fails later with an unresolved name.

Reproduced against:

```fsharp
module M =
    type T = { X: int }
    val f: T -> int
```

`M.f` is absent. Moving the `val` above the `type` publishes it.

## Scope

- It predates the per-declaration finalize rework — that change altered WHEN a declaration is
  frozen, not WHICH module elements are walked.
- No Vesper contract happens to be written that way, which is why no suite catches it. Every
  `.fsi` in `src/Vesper.*` puts its `val`s in a module whose body has no `type`, or ahead of
  one.

## Where to look

The extractor's module-body walk (`extractModuleSigElement` and the element loop that feeds
it), or the signature parser's module-body production. The two candidates are that the parser
does not produce the `Val` element at all once a `Type` element precedes it, or that the
extractor's walk stops consuming elements after a `Type`. Dumping the parsed CST for the
snippet above distinguishes them in one step, and that is the first thing to do.

## Fix must include

A test in `SignatureExtractorTests` pinning both orders — `val` before `type` and `val`
after `type` — so the asymmetry cannot come back.
