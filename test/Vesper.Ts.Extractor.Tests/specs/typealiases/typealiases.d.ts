// Type-alias fixture (item 6): an alias to a PRIMITIVE and an alias to a UNION.
// The producer emits the RESOLVED target via `mapType`, so `Id` resolves to
// `string` and `Numeric` to a `string | float` union (TS `number` → `float`).

export type Id = string;
export type Numeric = string | number;
