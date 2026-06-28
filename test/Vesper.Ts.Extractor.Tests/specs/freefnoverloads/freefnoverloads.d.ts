// Tier-2 item 9b fixture: free-function overloads. F# has no free-function
// overloading, so a function with N>1 call signatures cannot ride the name-keyed
// `funcs` map (last would win). The provider groups overloaded free functions of a
// module as static members of ONE synthetic erased type named after the module
// (`freefnoverloads` -> `Freefnoverloads`); the call erases at JS emit to the bare
// export. `format` has two overloads with DISTINCT argSigs (string vs number/float).
// `shout` is non-overloaded, proving it stays a bare free function (`TryLookup`).

export function format(x: string): string;
export function format(x: number): string;

export function shout(message: string): string;
