# JS backend — class emission is still partial

The JS backend emits user classes, primary-ctor preambles (including `static let`/`static do`,
which emit as module-load init), and instance/static members. Two gaps remain.

## Loud (rejected, not silently wrong)

`inherit`, and a secondary ctor alongside a primary, `failwith` rather than emit a class with the
feature missing:

- **`inherit`** is the substantive one — JS emits no `extends` and no `super(…)`, so base-ctor
  side effects would vanish and inherited members read `undefined`. It is on neither backend's
  conformance `run` list precisely because a `run = ["clr","js"]` program for it could not pass
  today; its rejection is pinned in the JS suite. Implementing it means real `extends`/`super(…)`
  emission and a cross-backend conformance case (the CLR/JS ctors were implemented independently
  and *did* diverge here once — a conformance case is the only structural defence against a
  recurrence).

## Silently wrong (reported, worth fixing before it bites)

A `val`-form **secondary ctor whose field initialisers are not the positional identity**
(`new(a) = { x = a + 1 }`, or a body with `let`s). The positional field ctor covers the identity
case — which is why `Vesper.List`'s `ListEnumerator` works, and why a blanket rejection of
secondary ctors was wrong — but not this one. Distinguishing them needs each `TCtorFieldInit`
checked against the ctor's params.
