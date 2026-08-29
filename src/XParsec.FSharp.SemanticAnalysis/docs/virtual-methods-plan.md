# Virtual methods: resolving an `override` to the slot it actually targets

Scope: everything the compiler needs to decide "which virtual slot does this `override`
conform to?" — the inheritance walk, the data saying which members ARE slots, selection
among same-named slots, and generic slots.

Already landed (2026-08-09), and the starting point for everything below:
`Unification.checkOverrideConformance` + `tryBaseSlotType` pin an `override` to the nearest
same-named instance member up the LOCAL `inherit` chain, falling back to the three hardcoded
`System.Object` signatures. That closed the spurious-error defect recorded in
`semantic-analysis-followups-plan.md`; the two limits that entry names are tranches 1 and 3
here.

Claims about the source were read at the cited lines. F# behaviour was taken from `dotnet fsi`
on throwaway scripts, not from memory, and the errors below are quoted verbatim.

## The target behaviour (F# oracle)

| # | Source | F# says |
|---|--------|---------|
| A | base `abstract Store: int -> unit`; derived `override this.Store x = …` | `x` is `int` — the slot types the unannotated parameter |
| B | `override this.Nope(x: int)` with no such base member | `error FS0855: No abstract or interface member was found that corresponds to this override` |
| C | base declares `M: int -> unit` AND `M: string -> unit`; derived `override this.M x = …` | `error FS3213: The member 'M<'a0> : 'a0 -> unit' matches multiple overloads of the same method. Please restrict it to one of the following: M: string -> unit, M: int -> unit.` |
| D | base member is a plain non-virtual `member _.M(x: int)`; derived `override this.M(x: int)` | `error FS0855` — same as B: a non-virtual member is not a slot |
| E | the C hierarchy, but derived writes `override this.M(x: string)` | compiles — an ANNOTATED parameter selects the slot |
| F | base `abstract M<'C> : 'C -> 'C`; derived `override this.M x = x` | compiles — the override need not respell the typar |
| G | base `B<'T>` with `abstract M: 'T -> unit`; derived `inherit B<int>()`, `override this.M x = …` | `x` is `int` — the derived's type args substitute into the slot |

C is the load-bearing one: F# generalises the unannotated override to `'a0 -> unit` FIRST and
then reports ambiguity against the candidate set. Selection is by arity plus whatever the
source annotated (E), not by inferring the parameter from the body.

G already holds — `tryClassChainMember` instantiates against the parent's args — but is
unpinned by any test.

## What the code has today

**The pin.** `Passes/Unification.fs` — `tryBaseSlotType` reads `info.BaseType`, resolves it to
a `TyClass`, and hands off to `tryClassChainMember`. That walk (`EngineCore.fs:204-237`) is
LOCAL-ONLY: `TypeRegistry.tryClassByKey` misses on an external key and the walk ends. Lookup is
`Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic)` — first same-named match wins.

**The local tier records no virtuality.** `MemberRegistration.fs:304-309` matches
`MemberKeyword.Override | Default` → `true`, `Member | Abstract` → `false`, and stores that one
bool as `TypeMemberInfo.IsOverride` (`TypeInfos.fs:81`). So after registration a base's
`abstract M` and its plain `member this.M` are indistinguishable — case D cannot be told from a
real slot, and case B cannot be diagnosed.

**The external tier records none either.** `ExternalMember` (`ExternalSymbols.fs:234-260`) has
`IsStatic`, `Storage`, `Signature`, `MethodTyparArity`, `IsOptional` — no virtual/abstract flag.
`ExternalClassFlags.IsAbstract` (`:310`) is TYPE-level. `MethodAttributes` is never read
anywhere in `src/`; the CLR provider is reflection-based (`MetadataSymbols.fs:333-347`), so
`m.IsVirtual` / `m.IsFinal` / `m.GetBaseDefinition()` are one property away.

**The two-tier parent step already exists.** `EngineCore.subtypeParentOf` (`:427-451`) spans
local `info.BaseType`, external `shape.FrozenBaseType` and intrinsic `surface.BaseType`. It is
`private`. `ExternalSymbols.openSignature` turns an `ExternalMember` into an instantiated
`SemType` — `checkInterfaceConformance` (`Unification.fs:546`) already does exactly the
"external member → expected type" step this needs.

**`obj`'s slots are in no provider.** `IntrinsicClassSurface.Members` is the contract `.ctor`s
BY CONSTRUCTION: the doc says so (`ExternalSymbols.fs:394-396`) and the republish filters
`m.Name = ".ctor"` (`VesperLib.fs:383`). `prim-types-object.fsi` declares only
`new: unit -> obj`. So the hardcoded triple in the pass cannot be replaced by a provider lookup
without tranche 5.

**Cross-unit.** A project-local base published to another project travels
`TastDecl.IsOverride` (`TastDecl.fs:180`) → `FrozenCodecDecls.fs:305`/`:354` →
`FrozenSignature.memberOf` (`:110-120`) → `ExternalMember`. Any virtuality flag that must
survive a project boundary has to travel all four.

## Tranche 1 — the walk crosses into external bases

Generalise `tryBaseSlotType` into a chain walk that does not stop at the project boundary:
local `ClassTypeInfo` → external `ExternalTypeShape.Class` (find by name in `shape.Members`,
`openSignature` at the instantiated args) → intrinsic surface → continue through the tier's
own base.

- `EngineCore.subtypeParentOf` is the parent step; un-privatise it or lift the pair
  (parent, members-of) into one two-tier accessor beside `tryClassChainMemberDecl`.
- Keep the Object triple as the terminal fallback until tranche 5.

**Acceptance:** `type D() = inherit exn(); override this.ToString() = "x"` conforms against the
provider's slot rather than the hardcoded triple; a BCL base's non-Object `Equals` overload
stops being reported as a mismatch against `obj -> bool`.

**Deletes:** the "an external base's slots are not read here" clause on `tryBaseSlotType`, and
the matching paragraph in `semantic-analysis-followups-plan.md`.

**Effort:** ~0.5 day. **Risk:** low-moderate — it changes where `Equals`/`ToString` slots come
from for every class in the corpus that overrides them; the Clr + Js suites are the check.

## Tranche 2 — record which members are slots

The data both tiers are missing. Independent of tranche 1, but tranche 3 is much cheaper after
it (virtuality prunes the candidate set before selection runs).

**Local.** Widen `TypeMemberInfo.IsOverride: bool` to a three-case flag — the keyword is
already in hand at `MemberRegistration.fs:304-309`, it is only being collapsed. Consumers to
update: `Unification.fs:241` (skip generalisation for an override), the pin, `:945`
(`GetHashCode` presence probe), `Elaborate/ClassMembers.fs:184`/`:206`,
`Elaborate/Members.fs:126`/`:149`, `TastConvert.fs:216`.

**External.** Add the flag to `ExternalMember` with a `false` default in `OfKey` — additive, so
the four other producers and all eight test helpers compile untouched. Fill it at:
`MetadataSymbols.fs:339` (`m.IsVirtual && not m.IsFinal`), `FrozenSignature.fs:110` (from the
TAST member), `TsManifestMembers.fs:47` (an interface member is a slot), `VesperLib.fs:895`
(the contract `abstract member` form).

**Cross-unit.** If a base in ANOTHER project must be slot-accurate, the flag must be carried on
`TastDecl` + `FrozenCodecDecls` and needs a `Cache.CodeVersion` bump (`Cache.fs:61`). Scoping
that out is defensible for now — a cross-project base then falls back to name-only, i.e.
today's behaviour — but it should be a stated decision, not an omission.

**Unlocks:** the FS0855 analogue (cases B and D), which the compiler cannot express today.

**Effort:** ~1 day, +0.25 for the codec/version bump. **Risk:** low; wide but mechanical.

## Tranche 3 — select among same-named slots

The chicken-and-egg tranche. At check time the override's type is a partly-inferred `TyVar`
chain: arity is readable, annotated parameters are readable, unannotated ones are what we are
trying to learn. F# resolves by arity + annotations and propagates the rest (E), then reports
ambiguity if more than one survives (C).

1. Read the override's arity off the `TyFun` chain; filter candidates by arity.
2. Filter further by each parameter position the source annotated (an unresolved domain var
   matches anything).
3. Exactly one survivor → unify. None → the FS0855 analogue. More than one → the FS3213
   analogue, listing the candidates as F# does.

`UnificationInferOverload.pickBestOverload` / `memberParamTypes` are the right machinery but
assume every argument type is known; this needs a "some positions unknown" mode. Do NOT reuse
it by feeding fresh metavars — that admits everything.

**Effort:** 1-2 days, most of it test design (curried vs tupled arity, partial annotation,
ambiguity message shape). Per the systematic-isolation-test rule this wants its own test file
and a matrix, not examples bolted onto `UnificationInheritanceTests`.

**Risk:** moderate. It is the first place the compiler DIAGNOSES an override, so it can reject
code that compiles today.

## Tranche 4 — generic slots

`tryClassChainMember` routes through `instantiateMemberCall`, which mints FRESH vars for the
base's method typars — a call-site instantiation, not a slot match. Case F needs the override's
own declared typars aligned POSITIONALLY with the slot's. Interacts with
`generaliseMemberTypars` (`Unification.fs:243`), which already skips overrides, and with
`TypeMemberInfo.DeclaredTyparCount`.

**Effort:** ~0.5 day, but entangled with tranche 3 — do them together.

## Tranche 5 (optional) — source the Object triple from the `obj` contract

Deletes the five hardcoded `TyConst`s and the name-keyed `match` in the pass, which is the
last place the front end spells a slot signature rather than reading one.

1. Declare the three slots on `type obj = extern class with` in `prim-types-object.fsi`
   (`Equals: objnull -> bool`, `GetHashCode: unit -> int`, `ToString: unit -> string`). The
   parser already accepts `abstract member` in an `extern` body — that is how `disposable` /
   `equatable` publish their surfaces (`capabilities.fsi:7-8`, `:14-15`;
   `SignatureParsing.fs:382-415` shares the path with `extern class`).
2. Widen the `.ctor`-only republish filter at `VesperLib.fs:383` and the ctors-only contract on
   `IntrinsicClassSurface.Members` (`ExternalSymbols.fs:394-396`). `MemberRegistration.fs:764`
   probes only for the EXISTENCE of a `.ctor`, so it is unaffected.
3. `objnull` in the contract is consistent with the existing reference-null erasure at this
   seam (`stripReferenceNull` on both sides).

**Effort:** ~0.5-1 day. **Risk:** moderate — it moves a fact the whole front end depends on
into the core library, and both backends read that surface.

## Test plan

One file, `VirtualSlotTests.fs`, mirroring the oracle table: A-G as rows, plus the
external-base rows tranche 1 adds and the diagnostic rows tranche 3 adds. G is worth pinning
first — it holds today and nothing guards it.

Full-suite gates: `XParsec.FSharp.SemanticAnalysis.Tests`, then `Codegen.Clr.Tests` and
`Codegen.Js.Tests` (both exercise `override` emission end to end), then `Vesper.Tests`.

## Effort summary

| Tranche | Work | Effort |
|---|---|---|
| 1 | walk crosses into external bases | 0.5 day |
| 2 | virtuality on both tiers (+codec) | 1-1.25 days |
| 3 | slot selection + FS0855/FS3213 analogues | 1-2 days |
| 4 | generic slots | 0.5 day (with 3) |
| 5 | Object triple from the `obj` contract | 0.5-1 day |

Recommended: 1 + 2 as one body of work (~1.5-2 days) — it is the correctness that matters and
it turns a prose invariant into data the compiler holds. 3 + 4 next, together, with their own
test matrix. 5 last, or never, since it is a cleanup with real reach.

## Open questions

**O1. Does a local `abstract` member emit correctly today?** `LayoutNodes.fs:126-131` picks
method attributes from `isIfaceImpl` / `IsStatic` / `IsOverride`, so a member declared
`abstract` in a class falls to `instanceMethodAttrs` — a plain non-virtual instance method —
unless something filters bodiless members earlier. If nothing does, tranche 2's flag fixes an
emission defect as well as an inference one. Check before assuming either way.

**O2. Is a cross-project base in scope?** Decides whether tranche 2 takes the codec change and
the `CodeVersion` bump, or stops at the compilation unit.

**O3. Which name is the diagnostic reported at?** F# reports at the override's identifier
(`vm_b.fsx(7,19)`, the member name). `mInfo.DeclSite.Tok` is that token, so the analogue is
free — but the FS3213 analogue also has to RENDER the candidate list, and there is no existing
member-signature renderer for that shape.
