// Literal fixture (R4a step 2): string/number literal TYPES are now FAITHFUL
// `TypeRef.Literal` arms (`"GET"|"POST"` → a union of literals, `0|1|2` likewise),
// carried structurally for the front end's directional admission. A BOOLEAN literal
// has no literal arm (design §"string first; skip bool") so `true` still erases to
// `bool`; a non-integer numeric literal (no int64 wire form) erases to `float`.

export interface Request {
  method: "GET" | "POST" | "PUT";
  retries: 0 | 1 | 2;
  verbose: true;
}

export function send(method: "GET" | "POST"): string;
