// Tier-3 item 11 (generics) fixture. Exercises DECLARING-axis generics end-to-end:
//   - a generic INTERFACE `Box<T>` with a member typed `T` and a method returning `T`
//     (both declaring-axis references → Typar 0);
//   - a generic CLASS `Container<T>` whose member is a generic INSTANTIATION `Array<T>`
//     (→ Named("Array", [Typar 0])) and whose method takes a `T`;
//   - a generic TYPE ALIAS `Pair<A, B> = A | B` (two declaring typars → Typar 0/1);
//   - a CONCRETE instantiation `Box<number>` as a member type (→ Named("Box", [float]));
//   - a generic FREE FUNCTION `identity<T>(x: T): T` (the function's own typar → Typar 0).
//
// METHOD-AXIS generics are DEFERRED: a generic method on a generic type (`map<U>(…): U`)
// whose own `U` is referenced cannot be represented by the schema's single-axis `Typar`
// without a contract bump, so it is intentionally absent here (it would throw in mapType).
// The additive method `TypeParams` COUNT is still emitted; `Box.get` carries it as 0.

export interface Box<T> {
  value: T;
  get(): T;
}

export class Container<T> {
  items: Array<T>;
  add(item: T): void;
}

export type Pair<A, B> = A | B;

export interface Registry {
  box: Box<number>;
}

export function identity<T>(x: T): T;
