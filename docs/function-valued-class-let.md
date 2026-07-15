# A function-valued class `let` should be a private method

We lower a function-valued class `let` as an instance **field holding a closure over `this`**. F#
instead emits an `Assembly`-visible instance **method** — and eta-expands even
`let f = fun () -> …` into one (probed by reflection).

The divergence is in **cost, not correctness**: our field reads sibling internal fields fine,
`let rec` works, and first-class use (`List.map bump`) works. It costs one closure allocation per
instance per function-`let`.

The method lowering needs a "private instance method on a user class" concept, which exists in
**neither** backend today — every user member is `Public`, and compiler-generated code is closure
classes, not private methods — plus eta-expansion with correct currying at every first-class use
site, in both backends. Hence deferred, deliberately.
