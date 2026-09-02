# Same-name type resolution

*Written 2026-09-01, against the code as it stands after the abbreviation-representation work
(`dealias` in expression and pattern position; `KindRegistry` reduced to a bare
`Dictionary<TypeKey, 'Info>` with the short-name index deleted; `tryKeyOfArity` and
`tryKeyOfArglessName` in the `tryPickWinner` shape this document changes). Not started. Every F#
verdict below was probed with `dotnet fsi` on 2026-09-01, and every case is pinned in
`test/XParsec.FSharp.SemanticAnalysis.Tests/SameNameResolutionTests.fs`.*

## The rule

A type name is claimed at an ARITY. `(name, arity)` is the unit of both declaration and
lookup, and the consequences run in three directions.

- **One module may hold `T` and `T<'a>` at once.** Arity overloading inside a single module is
  legal, and `DuplicateTypeNameTests` is right to collide only within an arity.
- **Shadowing is per-arity and blind to kind.** Within one arity bucket the max-rank claim wins
  and its kind is read afterwards, so a nearer class puts a same-arity record of that name out
  of reach entirely: `t.X` on the shadowed record is FS0039, not a fallthrough to the record.
  Across arities there is no shadowing at all, so a bare `T` reaches an outer arity-0 record
  past a nearer `T<'a>`, of any kind, and past a later `open` that supplies one.
- **What a bare name means depends on POSITION, and the two positions disagree.**
  - In **type** position a bare name is arity 0, exactly, and the type argument is never
    inferred from context: `T` where only `T<'a>` is in scope is FS0033, "expects 1 type
    argument(s) but is given 0". This holds at every type position probed — a parameter or
    return annotation, a type ARGUMENT (`C list`), an `inherit` clause, `typeof<>`, and
    `interface … with` — for a record, union, class, abbreviation and a BCL generic alike.
    Several arities and none of them 0 is still FS0033, reported against the best-ranked
    candidate rather than as an ambiguity. `C<_>` is how the arity is left to inference.
    (Step 5 diverges here on purpose: this compiler will name the NEAREST arity instead.)
  - In **expression and pattern** position a bare name is normal F# and resolves: the order is
    an arity-0 claim, else the candidates' agreed arity, else refuse. `T(1)`, `C.Make 5` and a
    union-case pattern all work with no type arguments written. Where the instantiation cannot
    be inferred from the arguments or the return type the access still succeeds, under the
    FS1125 WARNING. The arity-0 claim wins outright and is not reconsidered when the value
    arguments then fail against its constructor (FS0501). Candidates that disagree on arity
    refuse with FS1124, "Multiple types exist called 'T', taking different numbers of generic
    parameters."

  The split is syntactic, not a matter of degree: an omitted type argument is an error in type
  position and the norm in expression position.

`TypeRegistry.tryKeyOfArglessName` (`TypeRegistry.fs:351`) already states the expression-position
order and implements it.

## Where the compiler stands

*Updated after step 2.* Nineteen of the twenty-two pinned cases are green: arity overloading
within a module, every cross-arity reach, both kind-shadowing cases, every bare-name
expression-position case, and, since step 2, every type-position case. Type resolution runs
through `NameResolutionLongIdent.resolveType` (`LongIdent.fs:611`) over `TypeClaims`, which is
kind-agnostic and ranked. GAP 3, 6 and 7 remain, pended in `SameNameResolutionTests.fs`.

### GAP 1, 2, 4 and 5 — a bare generic name in TYPE position is accepted. Closed by step 2.

*Was:* `resolveType` fell back from the written arity to ANY arity:

```fsharp
let local =
    match TypeRegistry.tryWrittenTypeClaim ctx.Types useSite written arity with
    | ValueSome claim -> ValueSome claim
    | ValueNone -> TypeRegistry.tryWrittenTypeClaimAnyArity ctx.Types useSite written
```

A written `T` carries `arity = 0`, misses, and the fallback hands back the `T<'a>` claim. The
annotation is accepted and nothing is reported. The external leg beside it passes
`WrittenArity.Exact arity` and has no such fallback, so the gap is local claims only.

`resolveType` serves every type position, so the same leniency covers a bare generic name at a
type argument and in an `inherit` clause; both are pinned. `InheritParent.fs` passes
`WrittenArity.Exact targs.Length` on its external leg for the same reason the annotation's does,
and is lenient on the local one for the same reason too.

The fallback is not gratuitous: it is what lets `resolveType` report a name that exists at the
wrong arity as a TYPE rather than as an unresolved segment, which the verdict stamp downstream
depends on. Removing it wholesale would turn FS0033 into an unresolved-name diagnostic, which
is a worse answer. The fix has to distinguish the two outcomes.

*Now:* `resolveType` returns `TypeNameResolution`, whose `LocalAtOtherArity` arm is the claim
found at another arity. `classifyTypeRef` reports that arm as FS0033 and stamps
`TypeRefVerdict.LocalTypeAtOtherArity claim`, so the reference stays a type and the claim
travels with the verdict.

### GAP 3 — an arity ambiguity in expression position reaches the freeze

`T(1)` with `A.T<'a>` and `B.T<'a,'b>` both opened resolves to nothing:
`tryKeyOfArglessName`'s agreed-arity scan finds two arities, returns `ValueNone`, and no
diagnostic is emitted at the use site. The unresolved type survives to the freeze, where it
trips the internal backstop:

> internal compiler error: the frozen TAST holds 1 unresolved TyVar(s)

An internal backstop is not a verdict a user can act on. F# names the fault at the use site
(FS1124) and says how to fix it.

### GAP 6 and 7 — a WRITTEN arity in expression position is unreported

Step 2 made NameResolution the sole owner of the local FS0033, reporting it from
`classifyTypeRef`, which runs over TYPE references. An `Expr.TypeApp` is not one. `Scope.fs`'s
`TypeApp` arm resolves the applied name itself and, on `LocalAtOtherArity`, stamps the claim
without reporting, leaving the arity to Unification.

Unification reaches it in `inferTypeApp` (`InferTypeOps.fs:36`), which reports only against a
`TypeAppTarget.Scheme` or `Nominal` — a generalised binding, or a class / record / union RESULT.
Two applied forms type onto neither:

| written, with `E` an enum and `U<'a>` a union | F# | this compiler |
| --- | --- | --- |
| `E<int>.A` | FS0033, "does not expect any type arguments, but here is given 1" | no user diagnostic |
| `U<int, string>.Case 1` | FS0033, "expects 1 type argument(s) but is given 2" | no user diagnostic |

An enum-case and a union-case qualifier each discard the written type arguments before
`inferTypeApp` has a target, so nothing is reported and the unpinned TyVars reach the freeze:

> internal compiler error: the frozen TAST holds 2 unresolved TyVar(s)

The ctor (`C<int, string>(1)`) and static-member (`C<int, string>.M`) forms of the same mistake
report correctly, and are pinned green beside the two gaps. So the fault is the qualifier paths,
not the `TypeApp` arm as a whole.

## A correction to `tryKeyOfArity`'s doc

`TypeRegistry.fs:329` claims:

> A same-named type of another kind shadows it into a MISS.

The mechanism it describes does not exist. `tryPickWinnerRanked` (`TypeRegistry.fs:199`) applies
`pick` BEFORE ranking, so a claim `keyInKind` rejects is skipped rather than allowed to win, and
a lower-ranked same-kind claim can still be returned. The sentence nonetheless describes correct
F# semantics, which is why it was written; the shadowing simply happens somewhere else, in
`resolveType` over `TypeClaims`, and the kind-specific lookups are never asked a question whose
answer would expose the difference. Their callers (`InferCtor.fs:285`, `InferIdentExpr.fs:73`
and `:177`, `Elaborate/Resolve.fs:34`, `InheritParent.fs:79`) each already hold a kind.

Two ways to close it, in preference order:

1. **Make the signature carry it.** Rank first, then read the kind: `tryPickWinnerRanked` takes
   an `admit` predicate and returns the winning `TypeIdentity`, and the caller projects it into
   its own registry, missing when the winner is of another kind. That makes the doc true by
   construction and removes the possibility of the two paths disagreeing later.
2. **State what the code does.** Replace the sentence with "Claims of another kind are skipped,
   not ranked: cross-kind precedence is the caller's, through the name table", matching
   `tryAbbrev`'s doc at `TypeRegistry.fs:544`.

(1) costs a signature change across five call sites and is the shape the rest of this plan
wants anyway. Until one of them lands, the sentence is false and should not be cited.

## Staged plan

**Step 1 — pin the semantics. Done.** `SameNameResolutionTests.fs`, fifteen cases in four lists,
each quoting its F# verdict under its FS code. Ten green; the five gaps pended, each failing for
the reason given above when un-pended.

**Step 2 — separate "wrong arity" from "not a type". Done.** `resolveType` returns
`TypeNameResolution` (`Type` / `LocalAtOtherArity` / `Unresolved`), and `TypeRefVerdict` carries
the claim: `LocalType claim` at the written arity, `LocalTypeAtOtherArity claim` otherwise.
`classifyTypeRef` reports the latter as FS0033, once, because the first visit of a site settles
its verdict and a later stamping walk reads it back. GAP 1, 2, 4 and 5 closed.

NameResolution is the sole owner of the local FS0033. `Translate.translateTypeRef` reads the
stamped verdict at every written reference instead of re-deriving the claim from the registry,
so it reports no arity of its own; a claim at another arity recovers by fitting the written
args to the claim's arity with fresh TyVars, which is the one recovery for a bare `Box`, a
`T<int>` against `T<'a, 'b>`, and a qualified `A.T<int>` alike. The `float<m>` carrier is the
one by-name resolution left, because no walk stamps a measured carrier. One fixture reddened,
exactly as predicted — `UnificationGenericsTests`' "generic type written bare", which asserted
no diagnostic for `let b : Box = …`. It now asserts FS0033 and keeps its back-fill assertion,
since recovery still types `b` as `Box<int>`.

**Step 3 — report the arity ambiguity at the use site.** `tryKeyOfArglessName`'s scan already
computes the fact (`oneArity = false`); it discards it and returns `ValueNone`, so the caller
cannot tell "no such name" from "several arities". Return the disagreement, and add a
diagnostic kind for FS1124. Closes GAP 3, and removes one route to the unresolved-`TyVar`
backstop.

**Step 4 — rank before reading the kind.** Option (1) above: `tryPickWinnerRanked` ranks over
an `admit` predicate and hands back the winning claim; `tryKeyOfArity` and
`tryKeyOfArglessName` project it into `reg` and miss when the winner is of another kind. No
behaviour change is expected, since no current call site can observe the difference. The test
to write first is the one that would: a kind-specific lookup asked for a name whose max-rank
claim is of another kind.

**Step 5 — report a written arity no claim holds, and name the nearest arity.** Closes GAP 6
and 7. Two parts; the second is a deliberate divergence from F#.

*Report it.* The enum-case and union-case qualifier paths must carry the written type arguments
as far as the claim's arity so the two can be compared. `Scope.fs`'s `TypeApp` arm holds both
numbers already, so reporting there is the small fix, but it double-reports the ctor and
static-member forms, which step 2 pinned as reported ONCE (`expectOneUserError`). The report
belongs where the qualifier resolves its claim, so that reported-once holds by construction
instead of by a case analysis over target kinds.

*Name the nearest arity.* FS0033 names whichever candidate `tryWrittenTypeClaimAnyArity`
(`TypeRegistry.fs:424`) ranks first, and rank is scope distance, not arity distance. Probed
2026-09-01: with `A.T<'a,'b,'c>` and `B.T<'a>` both opened and `T<int,int,int,int>` written, F#
reports against `B.T<_>` — "expects 1 type argument(s) but is given 4" — the nearer open, three
arities out, while `A.T` is one arity out and is the likelier intent. Suggest the claim of
NEAREST arity instead, ties broken by rank.

`writtenTypeClaims` (`TypeRegistry.fs:244`) already returns every reachable claim rank-ordered,
so this is a stable sort by `abs(claim.TyparArity - written)` over a list that exists, not a new
lookup. `tryWrittenTypeClaimAnyArity` is the single caller whose choice changes, and it feeds
`TypeNameResolution.LocalAtOtherArity`, so type position gains the better suggestion at the same
time.

The divergence is confined to WHICH candidate an already-emitted FS0033 names; whether the
diagnostic fires at all is unchanged. Any fixture asserting a specific "expects N" against a
multi-arity name is therefore a finding to re-read rather than a regression.

## Semantics confirmed

Answered by the 2026-09-01 probes, and pinned:

1. **Arity overloading within one module** is legal, and a bare name means arity 0 there.
2. **Cross-arity reach outward** holds through nesting and through `open`, across kinds, and an
   arity-0 claim outranks a later `open`'s generic one.
3. **Kind shadowing within an arity** is total: the shadowed type is unreachable, not merely
   outranked, and it applies by rank, so a later `open` shadows exactly as a nearer declaration
   does.
4. **Type position never infers a type argument, and expression position always may.** The
   split is by syntactic position, uniform within each: nine type positions all give FS0033,
   and constructors, static accesses and union-case patterns all resolve bare. Type position
   was the one place F# was stricter than this compiler; step 2 closed it.
5. **Expression position prefers arity 0 and does not backtrack.** `T(1)` against a
   `unit`-constructor `T` is a constructor-arity error, never a silent fallthrough to `T<'a>`.
6. **An uninferable instantiation is a WARNING, not an error** (FS1125), and only in expression
   position. Any diagnostic added for it must not be an error.

## Scope and risk

Step 2 landed in `PassContext.TypeRefVerdict`, `LongIdent.resolveType`, `TypeRefStamp`,
`ResolvedItem`, `Scope`'s `TypeApp` arm and `Translate`, and needed no new diagnostic kind: the
existing `Kind.TypeArgArity` already carries FS0033. Step 3 touches
`TypeRegistry.tryKeyOfArglessName`, its four `*Bare` callers and the diagnostic kinds. Step 4
touches `TypeRegistry` only. Step 5 touches the enum-case and union-case qualifier paths, plus
`tryWrittenTypeClaimAnyArity`'s choice of candidate. All four are front-end, publish nothing new,
and leave the frozen blob and its codec alone.

Step 5 carries the wider blast radius of the four, because the nearest-arity change reaches every
existing FS0033 against a multi-arity name, in type position as well as expression position.
`Kind.TypeArgArity` already carries the arity it quotes, so no new diagnostic kind is needed.

The risk was concentrated in step 2, and it proved narrower still: one fixture across the four
suites. Only TYPE positions redden — a fixture writing `T(1)`, `C.Make 5` or a union-case
pattern goes through `tryKeyOfArglessName` and is untouched, and those are where a bare generic
name is idiomatic F#.
