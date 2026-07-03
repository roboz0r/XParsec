// Step 2 fixture: a GLOBAL-scope (script) `.d.ts` — top-level `declare`/`interface`,
// NO `export` (an `export`/`import` would make the file a MODULE and route it through
// the module entry `moduleSymbolOf`, which stays fatal for a script). Exercises the
// ambient-global entry mode `extractGlobals`, which enumerates the checker's GLOBAL
// scope by SYMBOL:
//   • a FUSED class-like global `Thing` — the type-side `interface Thing<T>` (instance
//     members) + the value-side `declare var Thing` (merged into the same symbol) +
//     the separate `ThingConstructor` (ctors + statics) fuse into ONE `Export.Class`
//     (arity 1; instance `value`/`get`; static `from`; a `.ctor`). `ThingConstructor`
//     is CONSUMED by the fusion and must NOT also appear as a standalone interface;
//   • a global interface `Widget` MERGED across two fixture files (`a` here, `b` in
//     `globals.extra.d.ts`) — proves symbol-based enumeration UNIONS the cross-file
//     declarations into one interface carrying BOTH members;
//   • a free global function `greet`;
//   • a global type alias `Id`.

interface Thing<T> {
  value: T;
  get(): T;
}

interface ThingConstructor {
  new <T>(v: T): Thing<T>;
  from<T>(v: T): Thing<T>;
}

declare var Thing: ThingConstructor;

interface Widget {
  a(): void;
}

declare function greet(name: string): string;

type Id = string;
