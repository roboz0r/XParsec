// Vesper.List — JS runtime cons-list module (codegen-js-steps.md Step 5b).
//
// Committed platform-support asset (see Vesper.Core/Vesper.Core.mjs for the
// pattern): declared by this package's manifest `runtime-js` key, read by the JS
// backend through `ReferencedProject.runtimeModules`, materialised beside the
// output and imported as `./Vesper.List.mjs`. Operates structurally on cons-list
// values — { tag: 0 } = Empty, { tag: 1, Head, Tail } = Cons — the exact
// own-property shape the backend emits inline for the Vesper.Collections.List
// union (List_Empty / List_Cons instances). The JS match compiler tests `.tag`
// and reads `.Head`/`.Tail` (never `instanceof`), so a list the consumer built
// with the inline classes and one these functions build (plain cons cells) are
// fully interchangeable. Curried (`map(f)(xs)`) to match the backend's
// unary-arrow calling convention. Route B (the --compiling-fslib bootstrap)
// eventually replaces this hand-authored file with a backend-compiled module.
const empty = { tag: 0 };
const cons = (h, t) => ({ tag: 1, Head: h, Tail: t });

export const length = (xs) => {
  let n = 0;
  while (xs.tag === 1) { n = n + 1; xs = xs.Tail; }
  return n;
};

export const isEmpty = (xs) => xs.tag === 0;

export const head = (xs) => {
  if (xs.tag === 0) throw new Error("The input list was empty.");
  return xs.Head;
};

export const tail = (xs) => {
  if (xs.tag === 0) throw new Error("The input list was empty.");
  return xs.Tail;
};

export const rev = (xs) => {
  let acc = empty;
  while (xs.tag === 1) { acc = cons(xs.Head, acc); xs = xs.Tail; }
  return acc;
};

export const map = (f) => (xs) => {
  let acc = empty;
  while (xs.tag === 1) { acc = cons(f(xs.Head), acc); xs = xs.Tail; }
  return rev(acc);
};
