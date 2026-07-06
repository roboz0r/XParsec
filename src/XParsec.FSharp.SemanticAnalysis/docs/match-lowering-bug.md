# Self-host bug: char OR-patterns mis-lower (fall through to the wildcard)

**Status:** open, worked-around at the one site that hit it. Not yet isolated to a minimal repro.

## Symptom

When the compiler compiles itself (the self-host path — `Pipeline.analyseForSelfHost` +
`Codegen.compile`, as the test harness does for `Vesper.Printf.dll`), a `match` on a **char**
with an **OR-pattern** arm is mis-lowered: the OR-pattern arm never matches, and control falls
through to the `_` (wildcard) arm at runtime.

## Where it was hit

`src/Vesper.Printf/formatter.fs`, `Formatter.ZeroPadAfterSign`. The sign-detection was first written:

```fsharp
let signOffset =
    if charsWritten > 0 then
        match this.Chars.[startingPos] with
        | '-'
        | '+'
        | ' ' -> 1
        | _ -> 0
    else
        0
```

Self-host-compiled, this returned `0` even when `this.Chars.[startingPos]` was `'+'` (or `'-'`
or `' '`) — i.e. the OR-pattern arm was skipped and the `_ -> 0` arm ran. Observable failure:
`printfn "%+08.2f" 3.14159` printed `000+3.14` instead of `+0003.14` (the zeros were inserted
*before* the forced sign because `signOffset` came back `0`).

The **regular F# compiler** builds the same source correctly (the project also builds under
`dotnet build`); only the **self-host** compilation of `formatter.fs` mis-behaves. So this is a
codegen/pattern-match-lowering bug in *this* compiler, not an F# issue.

## Workaround in place

Rewritten as boolean equality (`||`), which the self-host compiler lowers correctly:

```fsharp
let lead = if charsWritten > 0 then this.Chars.[startingPos] else '0'
let signOffset = if lead = '-' || lead = '+' || lead = ' ' then 1 else 0
```

There is a sited comment at that call site warning not to "simplify" it back to an OR-pattern.

## How it was confirmed

With the on-disk self-host package cache cleared (`rm -rf tmp/pkg-Vesper.Printf`) both times:
the OR-pattern form failed the `%+08.2f` / `% 08.2f` run-parity tests in
`test/XParsec.FSharp.Codegen.Clr.Tests/PrintfHappyPathTests.fs`; the boolean form passed. So it
is the pattern lowering, not a stale artifact.

## Open questions for whoever picks this up (not yet isolated)

1. **Scope of the bug.** Is it OR-patterns in general (any operand type), or char-literal
   patterns specifically, or only OR-patterns that sit alongside a `_` wildcard arm? Build a
   minimal self-host repro varying each axis:
   - `match c with | 'a' | 'b' -> 1 | _ -> 0` (char OR + wildcard) — the observed case.
   - `match n with | 1 | 2 -> 1 | _ -> 0` (int OR + wildcard).
   - `match c with | 'a' -> 1 | 'b' -> 1 | _ -> 0` (separate arms, no OR) — does this work?
   - OR-pattern with no `_` arm (exhaustive).
2. **Which stage.** Is the OR-pattern collapsed wrongly in the front end (Desugar / decision-tree
   construction) or mis-emitted in codegen? A front-end dump of the lowered decision tree for the
   repro will localise it. Suspect the decision-tree / switch lowering for combined (`|`) patterns.
3. **Regression test.** Once isolated, add a self-host test (compile a tiny module with a char
   OR-pattern via the self-host path, run it, assert the OR arm is taken) so a fix is pinned and
   the workaround comment in `formatter.fs` can eventually be removed.

## Suggested entry points

- Pattern desugaring / decision-tree: `src/XParsec.FSharp.SemanticAnalysis/Freeze/Patterns.fs`,
  `Passes/Desugar.fs`.
- Match codegen (CLR): `src/XParsec.FSharp.Codegen.Clr/` (the `switch`/branch emit for matches).
- Reproduce through the same harness the failure surfaced in: the self-host package build in
  `test/XParsec.FSharp.Codegen.Clr.Tests/TestHelpers.fs` (`buildPackage` / `compileStructuralEngine`).
