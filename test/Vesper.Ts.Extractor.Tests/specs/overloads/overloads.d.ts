// Tier-2 item 9 (member overloads) fixture: a method with N call signatures must
// expand into N `ExternalMember`s, each carrying its own `MemberKey` argSig. The
// interface method `format` has two overloads with DISTINCT argSigs (string vs
// float); the class method `add` has three (arity 1, then two arity-2 distinguished
// by param types) — exercising both the InterfaceMethod and Method kinds and the
// count- and type-based argSig distinctions. All argSigs are distinct, so the
// duplicate-argSig guard's negative case holds.

export interface Formatter {
  format(x: string): string;
  format(x: number): string;
}

export class Calc {
  add(x: number): number;
  add(x: number, y: number): number;
  add(x: string, y: string): string;
}
