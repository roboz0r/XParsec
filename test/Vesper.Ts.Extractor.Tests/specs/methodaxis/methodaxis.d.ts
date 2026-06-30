// Phase-2 degradation fixture: a METHOD-AXIS generic. A member's OWN generic type
// parameter (`<U>`) cannot be represented by the schema's single-axis `Typar`, so its
// REFERENCE is erased to `obj` and a `method-axis-typar-erased` Warning is recorded —
// the extraction degrades instead of throwing. Exactly ONE reference (the parameter)
// so the golden carries exactly one diagnostic; the method `TypeParams` COUNT (1) is
// still emitted, and the degraded member rehydrates through the provider (obj maps).

export interface Mapper {
  apply<U>(x: U): void;
}
