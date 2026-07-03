// Dynamic fixture: exercises `any` → Dynamic and an OBJECT-ONLY intersection
// `Named & Aged`, which the checker merges into a faithful `Structural` carrying its
// merged fields (name, age) — no diagnostic. The operands are named interfaces so TS
// keeps it a genuine intersection rather than simplifying it away.

export interface Named {
  name: string;
}

export interface Aged {
  age: number;
}

export interface Box {
  payload: any;
  person: Named & Aged;
}

export function identity(value: any): any;
