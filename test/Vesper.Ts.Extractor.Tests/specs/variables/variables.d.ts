// Variable fixture: `export const`/`export let` singletons (item 4). A `.d.ts`
// `export const` is implicitly ambient (`declare const`); `isConst` is read off
// the binding's combined node flags, so `const` → true and `let` → false.

export const apiVersion: string;
export let requestCount: number;
