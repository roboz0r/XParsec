// Dynamic fixture: exercises `any` → Dynamic (item 12) and intersection erase
// `A & B` → obj (item 15). The intersection operands are named interfaces so TS
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
