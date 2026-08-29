# Integer-width printf holes on JS

**Status (2026-08-23): a finding, unfixed.** Split out of the CLR one-width-handler work,
which landed 2026-08-23. That change deleted `PrintfHoleForm.rendersAnyWidth` / `rendersWidth`, the front-end gate that
refused a hole whose settled type its handler could not render. The gate was written from the
CLR handler table, so it never covered JS, and its removal changes nothing here: the defect
below was already reachable.

## The defect

`EmitJsFormat.fs:135-165` lowers the four non-decimal integer forms through JS's **32-bit**
unsigned coercion:

- `%x` / `%X` / `%B` / `%o` → `(v >>> 0).toString(base)`
- `%u` → `(v >>> 0).toString()`

Every integer specifier types its argument over `RuntimeNames.integerFormatKeys` — the ten
integral kinds from `sbyte` to `unativeint` — so `v` is only sometimes a 32-bit quantity.

**Narrower than 32 bits: wrong digits.** F# reinterprets the bits at the value's OWN width, which
the CLR oracle confirms: `%x` of `-1y` is `ff` and of `-1s` is `ffff`; `%u` of `-1y` is `255` and
of `-1s` is `65535`; `%o` of `-1y` is `377`. JS has no narrowing on these types (no masking
appears in `JsEmitHelpers.fs`), so `-1y` reaches the emitter as the number `-1` and
`(-1 >>> 0).toString(16)` gives `ffffffff` for all three widths.

**64-bit: a runtime throw.** `int64` / `uint64` are JS `bigint` (`EmitJsContext.fs:443-467`,
`JsAst.fs:36`). `>>>` on a bigint raises `TypeError: Cannot mix BigInt and other types`, so
`sprintf "%x" -1L` compiles and then throws where the CLR prints `ffffffffffffffff`.

`nativeint` / `unativeint` need a decision of their own, since JS has no pointer width.

## Shape of the fix

The CLR fix was to give the handler a lossless normal form: the value's own-width bits,
zero-extended to 64, which prints identical digits in unsigned decimal and in any radix. The same
normal form works here, but the emitter has to compose it in JS rather than in one `conv`
opcode, and the two representations diverge:

- **`bigint` source** — `BigInt.asUintN(64, v).toString(base)` is the whole conversion.
- **`number` source** — the own-width unsigned reinterpretation is `v >>> 0` at 32 bits,
  `v & 0xFF` at 8, `v & 0xFFFF` at 16. The mask is a function of the hole's static type, which
  the emitter holds as `hole.Ty`, exactly as the CLR emitter picks its `conv` pair.

So this wants one `hole.Ty` → coercion table beside the existing radix table, not a per-specifier
change. `EmitJsFormat` currently reads no hole type at all on this path.

**The mask applies at every narrow width, signed and unsigned alike.** The CLR emitter first
truncated only `sbyte` and `int16`, on the premise that an unsigned narrow value arrives already
in range. It does not. `prim-types-int.clr.fs:7` claims "the bitwise mnemonics stay in range and
need none", and `~~~` / `<<<` are indeed declared with no normalising `conv`, so `%u` of
`~~~200uy` printed `4294967095` where F# prints `55`. The CLR emitter now truncates at all four
narrow widths, which is free when the value is already in range; the JS table must cover `byte`
and `uint16` for the same reason. Whether `prim-types-int.clr.fs` should normalise `~~~` and
`<<<` at the source instead is open, and is a Vesper.Core question rather than a printf one.

Whether the two branches should converge on `BigInt` for every width is a separate call:
uniform, but it makes `%x` of an `int` allocate where a `>>>` would not.

## Also unresolved

`Vesper.Printf.mjs` is hand-written and is where a shared helper would live if the inline
expression grows past a coercion. Check it before adding a fourth spelling of "unsigned at
width" to the emitter.

## Confirming it

`test/XParsec.FSharp.Codegen.Js.Tests` runs emitted JS under Node. The check is the CLR suite's
own bar: for each of the ten widths, `sprintf "%x" / "%u" / "%o" / "%B"` of `-1` at that width
must match what `dotnet fsi` prints for the same expression. No JS test covers a non-`int`
integer hole today.
