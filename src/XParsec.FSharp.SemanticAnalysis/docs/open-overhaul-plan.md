# One ranked scope stack for `open`

*UNSTARTED. Written 2026-08-31 against the code as it stands after the `ImplicitOpen` rename.
Two separable defects are described. Defect A is small, independently verifiable, and does not
depend on the rest; defect B is the overhaul. Steps are ordered 1 → 2 → 3 → 4, and step 1 is
worth landing on its own whatever happens to the rest.*

*Scope is `SemanticAnalysis` alone. `IExternalSymbolResolver.ImplicitOpens`
(`ExternalSymbols.fs:380`) is implemented in `Codegen.Clr` and `Codegen.Js`, but both return the
empty list or forward, so neither backend changes.*

## The defect

The same fact — "this container is in scope at this position" — is carried three ways, and the
three are ranked on axes that cannot be compared. Precedence is therefore decided by **which
channel a name arrives through**, not by where it sits in the source.

| channel | representation | what orders it |
|---|---|---|
| `OpenScope.Prefixes` (`OpenScope.fs:29`) | `string list`, path AS WRITTEN | list position; re-resolved at every query |
| `OpenScope.Locals` → `UseSite.Opens` (`OpenScope.fs:32`, `SymbolKeys.fs:138`) | `LocalOpen { Path; Scope; ScopeDepth; Offset }` | list position; `Offset` carried but unused for shadowing |
| `IExternalSymbolResolver.ImplicitOpens` (`ExternalSymbols.fs:380`) | resolved `ModuleContainer` | nothing — always probed last |
| local declarations | `LocalScope` / `TypeRegistry` | `VisibleFrom <= useSite.Offset` |

One channel already does the right thing, for one kind of symbol. `TypeRegistry.pathReaches`
(`:245`) enumerates every route to a container as `ReachRoute.Ancestor | Opened of offset | Root`
(`:223`), and `claimRank` (`:291`) takes the **maximum `BindingRank { Depth; Offset }`**
(`SymbolKeys.fs:133`) over them. That is the model this plan generalises. It applies today to
this file's own **types** and to nothing else.

### Defect A — a relative `open` is dropped on the bare-name route

`ScopeContents.openedContainers` (`ExternalSymbols.fs:245`) resolves each prefix with
`scope.TryContainer p`, which is keyed by FULL path (compare `childPath`, `Containers.fs:56`).
`OpenScope.Prefixes` stores the path as written. So an `open` written relative to an enclosing
one contributes no container to a bare name.

`Containers.firstSegmentContainers` (`:69`) composes relative prefixes (`p + "." + segment`), so
the *qualified*-path route does not have this hole. Only bare names do.

**Confirmed by running it.** Un-pending the `STEP 1` list of
`LongIdentResolutionTests.fs` and printing the diagnostics gives:

```
["Unresolved identifier: f"; "Unresolved identifier: f"]
```

Both `x` and `y` fail, and with an unresolved-name error rather than the type mismatch that an
ordering defect would produce — the test never reaches the shadowing question it was written to
pin. A control probe with the same two files and the `open` written absolute
(`open Test.Lib.A` instead of `open Test.Lib` + `open A`) **passes**, which isolates relative
spelling as the variable.

### Defect B — precedence by channel, not by position

`LongIdent.valueInEnv` (`:375`) is a fixed ladder: `openedLocalValue` (`:364`), then
`externalValueInScope`. `caseInEnv` (`:384`) has the same shape. An enclosing scope's own
bindings are bound by the walk before either runs. So no `open` can shadow a declaration written
above it, at any position — which is what the `STEP 3` lists of `LongIdentResolutionTests.fs` pin.

`InferResolve.admitsBareExternalRecord` (`:188`) is the third encoding of "is this container
implicitly open", after `openedContainers` and `firstSegmentContainers`. It is correct, and
stricter than the string comparison it replaced, but it exists only because `OpenScope` no longer
carries the implicit set.

## Root cause

`CstModuleTree` is the only place that knows both the enclosing containment and the source
offset of an `open`, and it **already computes both** — `DeclContainment.sourcePath`,
`List.length containment.Modules` and `openToken.StartIndex` at `CstModuleTree.fs:218` — then
stores a dotted string and discards the container.

That is the discarded-intermediate failure: `TryContainer`, `tryContainerUnder`, `atPath` and
`OpenScope.candidates` are four re-derivations of the container from that string, and defect A is
one of them disagreeing with the others.

## Target shape

Resolve each `open` to its container once, at the walk, stamped with the rank it already has.

```fsharp
/// One entry of the scope in force at a use site. `Rank` orders it against a local
/// declaration on one axis, so a later entry shadows an earlier one.
type ScopeEntry = { Container: ModuleContainer; Rank: BindingRank }
```

Three sources, one ranked list:

| source | rank |
|---|---|
| written `open` | `{ Depth = enclosing module count; Offset = `open` keyword }` |
| local declaration | the rank `claimRank` already computes |
| `ImplicitOpen` | the floor — which is what "probed strictly BEHIND every explicit `open`" means today, expressed as a rank rather than as append position |

Resolution becomes "walk in rank order, first hit wins" for values, cases and types alike.

### What this deletes rather than moves

- `OpenScope.Prefixes` (`:29`), and `candidates` / `tryQualify`'s string composition (`:47`, `:65`)
- the `prefixes` parameter of `openedContainers` — `PassContext.fs` and `Intrinsics.fs` already pass `[]`
- the `atPath` / `subContainer` asymmetry in `firstSegmentContainers` (`Containers.fs:69`), where
  prefixes go through `LocalScope.tryContainer` and implicit opens through
  `TypeRegistry.tryContainerUnder` — two different local lookups for one question, one loop apart
- the `valueInEnv` and `caseInEnv` ladders (`LongIdent.fs:375`, `:384`)
- the `ctx.ImplicitOpens |> List.exists` guard in `admitsBareExternalRecord` (`InferResolve.fs:188`)
- `LocalOpen.ScopeDepth` + `Offset`, which collapse into the `BindingRank` they already are

## Staged plan

**Step 1 — close defect A.** Make `openedContainers` compose a relative prefix against the
containers already found, as `firstSegmentContainers` does, or resolve prefixes to containers at
the walk. Un-pend the `STEP 1` list of `LongIdentResolutionTests.fs`, whose two cases isolate the
relative spelling against the absolute-open control already passing beside them.

Independent of steps 2-4 and worth landing alone.

**Step 2 — carry the container.** Add `Container: ModuleContainer voption` to `LocalOpen`, filled
at `CstModuleTree.fs:218`. Additively, beside `Path`, per the repo's swap-behind-an-alias rule.
Re-point `TypeRegistry.openedContainer` (`:199`) to read it. No behaviour change; this is the
step that makes the four re-derivations one.

**Step 3 — one ranked stack.** Introduce `ScopeEntry`, generalise `claimRank` from types to
values and cases, and replace the `valueInEnv` / `caseInEnv` ladders. The three `STEP 3` lists of
`LongIdentResolutionTests.fs` come un-pended here — 17 cases across values, union cases, record
types, depth, and `[<AutoOpen>]` ranking. **This changes behaviour** and is bounded by the
semantics below.

The `[<AutoOpen>]` list is separable and larger than it looks: those cases need the same-assembly
`[<AutoOpen>]` module contributed at all before its rank means anything (semantics §4).

**Step 4 — delete the string channel.** Remove `OpenScope.Prefixes`, `candidates`, `tryQualify`
and the `prefixes` parameter. Separate change, per the delete-the-old-one-separately rule.

## Semantics, probed 2026-08-31

Each rule below was run against `dotnet fsi`, with a negative control — the same source with the
annotation flipped — that produced FS0001. All four are pinned in
`LongIdentResolutionTests.fs`, under the "`open` precedence" list, and each `ptest`
names the step that un-pends it.

1. **`Depth` before `Offset` — confirmed.** The two axes only disagree under `rec`, where a
   declaration is visible from the top of its scope; nesting otherwise puts the outer entry
   ahead of the inner scope on both axes at once. The discriminating probe:

   ```fsharp
   module rec Outer =
       module Inner =
           open Test.Lib.C          // C.g : string, depth 2
           let z : string = g ()    // resolves C.g

       let g () : int = 10          // depth 1, hoisted over the whole scope
   ```

   `claimRank` generalises unchanged.

2. **An `open` and a `let` at the same depth rank by offset alone — confirmed.**
   `let g` … `open C` … `g ()` resolves `C.g`. The rule extends to union cases, to record types
   under field-driven inference, and to a later `open` over an earlier LOCALLY declared type's
   cases.

   A case test must bind without an annotation and give the two cases distinguishable payloads:
   F#'s type-directed disambiguation reaches a SHADOWED case through an expected type, so
   `let c : U1.E = Zip` compiles either way and pins nothing.

3. **`open` inside a `rec` scope — settled, no decision required.** F# refuses an `open` that is
   not first in a `rec` module:

   ```
   FS3200: In a recursive declaration group, 'open' declarations must come first in each module
   ```

   This compiler already emits that message verbatim, so step 3 never meets the shape. The
   comment at `OpenScopeTests.fs:51` reads FS3200 as a hoisting rule and hoists a late `open`
   over the whole body; that path is unreachable for accepted source, making it a comment to fix
   rather than behaviour.

4. **Implicit opens are a floor — FALSIFIED for `ImplicitOpen.AutoOpen`.** An `[<AutoOpen>]`
   module ranks AT the `open` that brought its enclosing scope into view, so it shadows whatever
   is written above that `open`:

   ```fsharp
   open Test.Auto.D        // D.h : int
   let a : int = h ()
   open Test.Auto          // activates [<AutoOpen>] Auto, h : string
   let b : string = h ()   // Auto.h shadows the EARLIER explicit open
   ```

   It shadows an earlier local `let h` the same way. The floor holds only for a module in scope
   with no `open` written for it — `ImplicitOpen.AssemblyAutoOpen` and
   `ImplicitOpen.CurrentFileScope`. So `IExternalSymbolResolver.ImplicitOpens`' contract,
   "probed strictly BEHIND every explicit `open`" (`ExternalSymbols.fs:380`), is wrong for one of
   its three cases, and `ScopeEntry` must carry the activating `open`'s rank for `AutoOpen`.

   **Found while probing this:** a same-assembly `[<AutoOpen>]` module inside a namespace
   contributes nothing at all. Every one of the four AutoOpen cases fails first with
   `Unresolved identifier: h`, whether reached through `open Test.Auto` or through the consuming
   file's own `namespace Test.Auto` header. Contributing it is the first half of that work and
   ranking it the second.

## Scope and risk

This is the name-resolution core. Touched: `OpenScope`, `CstModuleTree`, `TypeRegistry`,
`Containers`, `ExternalSymbols` (`ScopeContents`), `Passes/NameResolution/LongIdent`,
`Passes/Unification/InferResolve`, `Intrinsics`, `PassContext`.

The regression surface is every name in every suite, so `XParsec.FSharp.SemanticAnalysis.Tests`,
`XParsec.FSharp.Codegen.Clr.Tests`, `XParsec.FSharp.Codegen.Js.Tests` and `Vesper.Tests` all
gate each step. Per the repo's reading rule, a tightening change that turns a green test red is a
finding first: the old test may have pinned resolution-by-channel.

## Related, out of scope

Found while tracing this, pre-existing, and not part of the plan:

`AssemblyAnalysis.conformSignature` homes a resolved signature with
`ExternalSymbolProviders.stack (ValueSome …) [] [ r.Published ]`. `stack`'s ambient argument
**shadows** rather than merges (`ExternalSymbolProviders.fs:281`), and `r.Published` does carry
the surface's implicit opens (`PublishedSurface.fs:444`). So a `.fsi`'s `[<AutoOpen>]` module
appears to be in scope for consumers of the assembly but not for later files of the same
assembly. Read from the code; not reproduced against a failing case.
