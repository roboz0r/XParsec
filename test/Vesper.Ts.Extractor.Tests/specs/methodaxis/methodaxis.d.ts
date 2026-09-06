// Phase-3.5 method-axis fixture: a member's OWN generic type parameter (`<U>`). The
// schema's method axis (`MethodTypar`) represents it FAITHFULLY — `x: U` maps to
// `MethodTypar 0`, the method `TypeParams` COUNT (1) is emitted, and the golden carries
// ZERO diagnostics (no erasure). The provider rehydrates it as `FTTypar(Method, 0)`,
// which it freshens per call site. (Before Phase 3.5 the single-axis `Typar` could not
// carry a method reference, so `U` was erased to `obj` + a `method-scope-typar-erased`
// Warning; that degrade is gone now that the method axis exists.)

export interface Mapper {
  apply<U>(x: U): void;
}
