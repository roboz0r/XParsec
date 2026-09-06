# Codegen.Js follow-ups — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent — they rot. Constructs and file names only.

Readability and modern-output improvements to the emitted JavaScript. None changes what a
program prints; each is pinned by the JS byte-identity goldens, which it regenerates. The
examples are from `goldens/module-tuple-poly.js`.

---

## R1. Array destructuring for a tuple-pattern binding

`EmitJs` lowers a module-scope `let (f, g) = …` (and `compileMatchPattern` a nested one)
through a temp and positional reads:

```js
const _let8 = [(x) => x, (y) => y];
const f = _let8[0];
const g = _let8[1];
```

An ES2015 array pattern says the same in one statement:

```js
const [f, g] = [(x) => x, (y) => y];
```

Needs a `JsStatement` binding form whose target is a pattern rather than a name (`Const`,
`Let` and `Export` all take `name: string`), and `JsPrint` for it. A wildcard leaf is an
elision (`const [, g] = …`), a nested tuple a nested pattern. The temp stays for a pattern
that binds through a refutable test, which an array pattern cannot express.

## R2. Template literals for a multi-segment format

`EmitJsFormat.buildFormatArg` seeds a `""` and chains `+` so the first operator already
concatenates strings:

```js
console.log((("") + (a) + (" ") + (b) + (" ") + ((c ? "true" : "false")) + (" ") + (d)));
```

A template literal carries the same segments with no seed and no parenthesised operands:

```js
console.log(`${a} ${b} ${c} ${d}`);
```

Needs a `JsExpr` template form (quasis and expressions) and its printing, with backtick,
`${` and backslash escaped in the literal segments. Each hole keeps the projection
`buildHole` gives it today; only the concatenation changes. A `bool` hole can drop its
`? "true" : "false"` conditional, because `${c}` interpolates a boolean to the same text,
but a `bigint` hole keeps its own rendering: `${5n}` prints `5`, which is what `%d` wants,
where `console.log(5n)` prints `5n`.
