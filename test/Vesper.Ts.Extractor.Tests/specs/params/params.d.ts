// Tier-2 items 7–8 fixture: parameter optionality and rest detection.
//   required  ⇒ optional:false rest:false   (template, separator)
//   optional  ⇒ optional:true              (prefix? — the `?` form)
//   rest      ⇒ rest:true                   (...args, ...parts)
//
// NOTE: ambient `.d.ts` forbids parameter initializers (TS1039 "Initializers are
// not allowed in ambient contexts"), so the `x: T = default` optional form is NOT
// expressible here — only the `?` form is. The extractor still classifies an
// initializer as optional (`initializer.IsSome`); that arm is unit-covered by the
// detection logic, not by a `.d.ts` golden, because the grammar can't express it.

export interface Formatter {
  format(template: string, prefix?: string, ...args: string[]): string;
}

export function join(separator: string, ...parts: string[]): string;
