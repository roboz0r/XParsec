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
- **In TYPE position, shadowing is per-arity and blind to kind.** Within one arity bucket the
  max-rank claim wins and its kind is read afterwards, so a nearer class puts a same-arity
  record of that name out of reach entirely: `t.X` on the shadowed record is FS0039, not a
  fallthrough to the record. Across arities there is no shadowing at all, so a bare `T` reaches
  an outer arity-0 record past a nearer `T<'a>`, of any kind, and past a later `open` that
  supplies one.
- **In EXPRESSION position the candidate set is the CLASSES, and a claim of another kind is
  invisible to it.** A bare name there denotes a constructor, so a nearer record neither
  shadows a same-arity class (`T()` takes the outer class while `t: T` in the same scope takes
  the nearer record) nor settles the arity with an arity-0 claim (`T(1)` takes the sole class
  `T<'a>` past a record `T`). Probed 2026-09-03.
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

*Updated after step 4a.* Twenty-seven of the twenty-nine pinned cases are green: arity
overloading within a module, every cross-arity reach, every kind-shadowing case in both
positions, every bare-name expression-position case including the arity ambiguity, and every
type-position case. Type
resolution runs through `NameResolutionLongIdent.resolveType` (`LongIdent.fs:611`) over
`TypeClaims`, which is kind-agnostic and ranked. GAP 6 and 7 remain, pended in
`SameNameResolutionTests.fs`.

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

### GAP 3 — an arity ambiguity in expression position reaches the freeze. Closed by step 3.

*Was:* `T(1)` with `A.T<'a>` and `B.T<'a,'b>` both opened resolved to nothing:
`tryKeyOfArglessName`'s agreed-arity scan found two arities, returned `ValueNone`, and no
diagnostic was emitted at the use site. The unresolved type survived to the freeze, where it
tripped the internal backstop:

> internal compiler error: the frozen TAST holds 1 unresolved TyVar(s)

An internal backstop is not a verdict a user can act on. F# names the fault at the use site
(FS1124) and says how to fix it.

*Now:* `TypeRegistry.arglessExprClaim` returns `ArglessClaim`, and `typeInEnv` stamps
`ResolvedItem.AmbiguousTypeArity` for the `Disagreement` arm, which `Scope.fs`'s `reportExpr`
reports once as `Kind.AmbiguousTypeArity` (FS1124). `tryKeyOfArglessName` recovers to the claim
of nearest arity, so the binding still types and the backstop stays quiet.

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
a lower-ranked same-kind claim can still be returned. The sentence was written believing it
describes correct F# semantics, and the kind-specific lookups were believed never to be asked a
question whose answer would expose the difference. Their callers (`InferCtor.fs:285`,
`InferIdentExpr.fs:73` and `:177`, `Elaborate/Resolve.fs:34`, `InheritParent.fs:173`) each
already hold a kind.

Two ways to close it, in preference order:

1. **Make the signature carry it.** Rank first, then read the kind: `tryPickWinnerRanked` takes
   an `admit` predicate and returns the winning `TypeIdentity`, and the caller projects it into
   its own registry, missing when the winner is of another kind. That makes the doc true by
   construction and removes the possibility of the two paths disagreeing later.
2. **State what the code does.** Replace the sentence with "Claims of another kind are skipped,
   not ranked: cross-kind precedence is the caller's, through the name table", matching
   `tryAbbrev`'s doc at `TypeRegistry.fs:544`.

*Settled by step 4's probes: (2).* The sentence is false in F# as well as in the code. The
kind-specific lookups serve expression position, where the candidate set is the CLASSES, so a
record outranking a class must NOT shadow it: `T()` under a nearer record takes the outer class
(probed 2026-09-03, and pinned). Under (1) that lookup would miss, so (1) would encode the
wrong rule. Kind shadowing is TYPE position's, and type position resolves through `resolveType`
over `TypeClaims` already.

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

**Step 3 — report the arity ambiguity at the use site. Done.** One scan, `rankedClaims`, is
the primitive under every by-name lookup in `TypeRegistry`. `arglessClaimOf` reads a ranked
claim list into `ArglessClaim` (`Takes` / `Disagreement` / `NoClaim`), each arm carrying the
winning claim so no consumer looks it up again; `tryKeyOfArglessName` and the new
`arglessExprClaim` both read it. `Kind.AmbiguousTypeArity` carries the name and the ascending
arities and files under FS1124. GAP 3 closed, and one route to the unresolved-`TyVar` backstop
removed.

Three things the 2026-09-01 probes settled while the step was written, each now pinned:

- The candidate set is by POSITION, not by kind. A bare name in expression position denotes a
  constructor, so `A.T<'a>` (class) beside `B.T<'a,'b>` (record) is no ambiguity — F# accepts
  `T(1)` there and takes the class — while two records at different arities are FS1124.
  (Written as "a non-generic claim of ANY kind still settles the name first". Step 4 probed
  that and it is false: an arity-0 record leaves `T(1)` to the class `T<'a>`, and leaves two
  disagreeing classes at FS1124. The claim of another kind is outside the candidate set, so it
  supplies no arity.)
- F# reports FS1124 alone, so the recovery must ground the expression. `tryKeyOfArglessName`
  takes the claim of NEAREST arity, which is step 5's rule read at the written arity 0.
  `TyUnknown` will not serve: a binder unified with it links nothing, and the free `TyVar`
  reaches the backstop anyway.
- `typeInEnv` picked the max-rank claim at ANY arity, which disagreed with the arity
  `tryKeyOfArglessName` settles on in Unification, and reported a bare name landing on a record
  as an unresolved identifier. It now reads the claim at the settled arity, so the two stages
  agree.

The residue is `T.M 1`, which F# reports as FS1124 too. A qualified name goes through
`typeFirst`, not `typeInEnv`, so it is still silent.

`inherit T(y)` under the same disagreement is NOT a residue. It is type position, and F# reports
FS0033 against the max-rank claim (`B.T<_,_>`) there, never FS1124. `tryKeyOfArglessName`'s
silent recovery is therefore the right verdict for every `*Bare` caller reached from
`InheritParent`, and the 2026-09-02 probe pinned it in `SameNameResolutionTests.fs`.

**Step 4 — rank before reading the kind. Done, as option (2), and it is a behaviour change.**
The test written first was the one that would observe the difference — a kind-specific lookup
asked for a name whose max-rank claim is of another kind — and F# answers it against option
(1): `T()` beside a nearer same-arity record takes the outer CLASS, and `T(1)` beside an
arity-0 record takes the class `T<'a>`. Both were red, reporting "Unresolved identifier: T".

So the kind filter inside the ranked scan is the RIGHT rule for the kind-specific lookups,
because in expression position the kind IS the candidate set, and `tryKeyOfArity`'s doc now
says so. What was wrong is who else read the kind:

- `TypeRegistry.arglessExprClaim` settled the name over every claim and narrowed to the classes
  only on a `Disagreement`. It now takes the argless rule over the classes first and falls back
  to every claim, which is where a name no class claims reports its arity disagreement. That
  fallback is what keeps two records at different arities reporting FS1124.
- `InheritParent.resolveInheritArgName` cascaded record → union → class, so a nearer class lost
  an `inherit Base<T>(t)` argument to an outer record of the same arity, giving "Type mismatch:
  N+T vs T" where F# is clean. It was first repaired in place, by reading the max-rank claim at
  the written arity through `TypeClaims` and dispatching on the kind after.

**Step 4a — read the stamped verdict instead of re-deriving it.** That in-place repair was the
third time an `inherit` argument's own copy of `resolveType` was corrected to agree with the
original. `classifyGroupTypeNames` stamps the whole `inherit` clause at step 2 of
`registerGroup`, and `fillGroupBaseTypes` runs at step 7, so the verdict is already cached when
the argument resolves. The first cut of this step read the verdict through a fourth copy,
`resolveInheritArgRef` beside a hand-pruned `translateInheritArg`. The review collapsed both:
an `inherit` argument is now `UnificationTranslate.translateType`, run under the class's
typar scope through `TypeRegistration.underTyparScope` at the one call site in
`MemberRegistration.registerInheritedSlot`, exactly as a ctor parameter annotation or a `val`
field already is. `resolveInheritParent` lost its `Map` typar-scope parameter with it, and
`resolveClaimedType` and `fitArgs` stay private to `Translate.fs`. Five divergences closed,
the first four pinned in `UnificationInheritanceTests.fs` where a test distinguishes them:

- A wrong-arity argument built a nominal whose key declared an arity its args did not carry,
  reporting "Type mismatch: T\`1 vs T\`1" beside the correct FS0033. The args are now fitted.
- A qualified argument (`inherit Base<A.T>(t)`) fell to a fresh TyVar; F# resolves it.
- The external leg matched at `WrittenArity.Any` where every other position matches at the
  exact arity.
- An enum, an intrinsic binding and an abbreviation resolve through the claim rather than
  landing opaque.
- The copy sent an array, a `null` member, an anonymous union, a `when`-constrained type and
  every measured carrier (`inherit Base<float<kg>>`) to a fresh TyVar, and an unresolved name
  to an opaque `TyConst` that unified as a real nominal. `translateType` models the shapes and
  yields `TyUnknown` for the name, whose FS0039 the classifying walk already reported at the
  same site.

**Step 4b — the PARENT reads the stamped verdict too.** `resolveInheritParent`
is the last copy of `resolveType` in the tree: its own `nameAndArgs` walk, the
`li.Idents.Length <> 1` rejection, `TypeRegistry.tryClass` by name, `tryIntrinsicKeyOf` for a
heritable local, then `tryPickExternalWritten` through `providerBaseOf`. The verdict for the
parent's site is stamped by the same step-2 walk that stamps its arguments, so the parent
dispatches on it instead:

- `LocalType` claim: `TypeDeclKind.Class` admits through `tryClassByKey` (an interface is
  `BaseVerdict.Interface`); `Abbreviation` follows `tryAliasedKey` to the class it aliases, as
  `keyInKind` does today; `IntrinsicBinding` is the heritable-local arm; every other kind is
  `BaseVerdict.NotAClass`.
- `LocalTypeAtOtherArity`: FS0033 is already reported; recover as `NotAClass` or the fitted
  class, whichever step 5 settles for the argument position.
- `ExternalType key`: `ctx.Provider.TryLookupType key` through `providerBaseOf`, replacing the
  by-name `tryPickExternalWritten` scan.
- `UnknownType`: `BaseVerdict.UnknownName`.

This deletes `nameAndArgs`, `tryCtorBearingCanon`'s by-name scan and `resolveThroughProvider`,
and lifts the qualified-parent `NotYetSupported` for free, since the verdict already resolves
`inherit A.Base<int>(v)`. The test to write first is the qualified parent, red today, plus a
parent of each kind (record, union, enum, abbreviation to a class, interface, external class,
`exn`, `Attribute`) so each `BaseVerdict` arm is pinned before the copy goes.

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
3. **Kind shadowing within an arity** is total IN TYPE POSITION: the shadowed type is
   unreachable, not merely outranked, and it applies by rank, so a later `open` shadows exactly
   as a nearer declaration does. Expression position shadows within the CLASSES only (7).
4. **Type position never infers a type argument, and expression position always may.** The
   split is by syntactic position, uniform within each: nine type positions all give FS0033,
   and constructors, static accesses and union-case patterns all resolve bare. Type position
   was the one place F# was stricter than this compiler; step 2 closed it.
5. **Expression position prefers arity 0 and does not backtrack.** `T(1)` against a
   `unit`-constructor `T` is a constructor-arity error, never a silent fallthrough to `T<'a>`.
6. **An uninferable instantiation is a WARNING, not an error** (FS1125), and only in expression
   position. Any diagnostic added for it must not be an error.
7. **The expression-position candidate set is the CLASSES, and FS1124 is over them.** A lone
   class claim settles the arity past a same-named record of any other arity, and past a record
   at arity 0, which supplies nothing. Two classes at different arities are the ambiguity.
   Where no class claims the name at all, every claim is the set, which is how two records at
   different arities report FS1124. Corrected 2026-09-03; the first reading had an arity-0
   claim of any kind settling the name.

## Scope and risk

Step 2 landed in `PassContext.TypeRefVerdict`, `LongIdent.resolveType`, `TypeRefStamp`,
`ResolvedItem`, `Scope`'s `TypeApp` arm and `Translate`, and needed no new diagnostic kind: the
existing `Kind.TypeArgArity` already carries FS0033. Step 3 landed in `TypeRegistry`,
`ResolvedItem`, `LongIdent.typeInEnv`, `Scope`'s expression report and the diagnostic kinds
plus their codec; the four `*Bare` callers needed no change, because the recovery inside
`tryKeyOfArglessName` keeps them on `voption`. Step 4 landed in `TypeRegistry.arglessExprClaim`
and `InheritParent`, needed no signature change, and reddened nothing across the
SemanticAnalysis and two Codegen suites; step 4a landed in `InheritParent` and
`MemberRegistration.registerInheritedSlot`, deleting code only, and reddened nothing across the
same three suites. Step 4b touches `InheritParent` alone. Step 5
touches the enum-case and union-case qualifier paths, plus `tryWrittenTypeClaimAnyArity`'s
choice of candidate. All four are front-end, publish nothing new, and leave the frozen blob and
its codec alone.

Step 5 carries the wider blast radius of the four, because the nearest-arity change reaches every
existing FS0033 against a multi-arity name, in type position as well as expression position.
`Kind.TypeArgArity` already carries the arity it quotes, so no new diagnostic kind is needed.

The risk was concentrated in step 2, and it proved narrower still: one fixture across the four
suites. Only TYPE positions redden — a fixture writing `T(1)`, `C.Make 5` or a union-case
pattern goes through `tryKeyOfArglessName` and is untouched, and those are where a bare generic
name is idiomatic F#.
