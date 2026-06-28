// Literal fixture: v1 policy erases literal unions to their base type
// (string-literal union → string, number-literal union → float, boolean
// literal → bool). Nominal-enum lowering is deferred (plan mapping table).

export interface Request {
  method: "GET" | "POST" | "PUT";
  retries: 0 | 1 | 2;
  verbose: true;
}

export function send(method: "GET" | "POST"): string;
