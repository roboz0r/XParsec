# NameResolution walk — collapse the kind-batched loops (perf)

Outstanding, **performance only** — not needed for any scoping behaviour.

File-order scoping now carries visibility on the **query**, not on walk state: every local
claim and value binding stores a `VisibleFrom: int`, and every by-name registry face takes a
use-site position, so

> a claim is visible at use `U`  ⟺  `claim.VisibleFrom ≤ U.Offset`.

Because the answer no longer depends on *when* a node is visited, only *where it is*,
`NameResolution.walkElems`' four kind-batched loops (registration → all class bodies → all
nominal bodies → all module elems) no longer encode any ordering fact. Collapse them to one
ordered pass. Justify the change as perf; it must not alter any resolution outcome.

## Hazard — leave `Unification.walkElems`' third loop alone

That loop runs in declaration order for a **typing** dependency, not a scoping one: a class
member calling an *earlier* module function needs its real generalised scheme, and a *later*
module function over the class needs the member's already-typed body. Batching either way
breaks one direction. The scoping information that ordering once also carried is gone (moved to
`VisibleFrom`), but the typing dependency remains — so this collapse must not touch it.
