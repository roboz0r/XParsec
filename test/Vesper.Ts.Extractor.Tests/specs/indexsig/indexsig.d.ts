// Index-signature + optional-graduation fixture.
//
// Exercises two extraction graduations that were previously stubbed:
//   1. TS index signatures `{ [k: K]: V }` — captured on the `index` facet of a named
//      interface (`Dict`), of a class, and of an anonymous `Structural` (`lookup`).
//   2. Optionality (`foo?: T`) — a NAMED member rides `Member.Optional`; an anonymous
//      `Structural` field (which has no optional channel) carries `T | undefined`.

// A named interface bearing a string index signature whose value type is
// `string | undefined` (the `NodeJS.Dict`/`ProcessEnv` shape). No named members — the
// whole content is the index signature, carried on `Export.Interface`'s `index` slot.
export interface Dict {
    [key: string]: string | undefined;
}

// An interface mixing an OPTIONAL named member (`foo?`) with a required one; `foo`
// carries `Member.Optional = true`, read off the property symbol's optional flag.
export interface Config {
    foo?: number;
    bar: string;
}

export interface Point {
    x: number;
    y: number;
}

// A variable typed by an ANONYMOUS object with an optional field: the structural field
// `timeout` has no optional channel, so it carries `number | undefined`.
export const settings: { name: string; timeout?: number };

// A variable typed by an anonymous index-signature object → a `Structural` carrying the
// index facet `(string, number)` with no named fields.
export const lookup: { [k: string]: number };

// `Partial<Point>` — tsc pre-evaluates it to a resolved object whose property symbols
// carry the optional flag while each property type stays `T[P]`, so every field carries
// `T | undefined`.
export const partialPoint: Partial<Point>;
