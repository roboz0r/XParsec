// Union fixture: exercises TyOr + the distinct null/undefined classifier
// (resolved fork — null/undefined are their own members, never folded to unit).
// Requires strictNullChecks, else TS collapses `T | null` to `T`.

export interface Config {
  id: string | number;
  nickname: string | null;
  middleName: string | undefined;
  note: string | null | undefined;
}

export function pick(value: string | number): string | undefined;
