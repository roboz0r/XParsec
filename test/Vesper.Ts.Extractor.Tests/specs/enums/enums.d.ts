// Enum fixture (item 5): a NUMERIC enum and a STRING enum. Member values come from
// `checker.getConstantValue` and the schema stores them as `string option` (numbers
// stringified). NB: the numeric members carry EXPLICIT initializers — in a
// non-const ambient (`.d.ts`) enum, `getConstantValue` returns `undefined` for
// IMPLICITLY auto-numbered members, so those would extract as a null value.

export enum Direction {
  Up = 1,
  Down = 2,
  Left = 3,
  Right = 4,
}

export enum HttpMethod {
  Get = "GET",
  Post = "POST",
}
