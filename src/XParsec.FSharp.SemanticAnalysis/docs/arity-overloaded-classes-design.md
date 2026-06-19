# Design: arity-overloaded project-local classes / interfaces

**Deliberate design doc — written BEFORE any implementation, the way `Fun2` was
designed (`rung3-handoff.md` §2).** *Ephemeral plan doc* per
[[feedback_plan_docs_ephemeral]] — scoped to exactly one body of work (giving
`PassContextTypes.Class` an `(name, arity)` key so `Fun<,>` and `Fun<,,>` coexist
as the same overloaded name, then migrating `Fun2` → `Fun<,,>`). Delete this file
(and every comment that references it) once the work below lands; the CODE +
isolation tests are the durable record, and `[[project_arity_overloaded_type_names]]`
is the authoritative memory for the union machinery this mirrors.

This doc is READ-ONLY analysis + a staged plan. It names real `file:line`
touch-points. Where a touch-point could not be found it is flagged an **open
question**, not invented.

---

## 0. The constraint being lifted

`rung3-handoff.md` §2 (`:43-48`) locks the *current* reason `Fun2` is a distinct
nominal type rather than `Fun<,,>`:

> Vesper's project-local class/interface registry (`Types.Class`) is keyed by
> **bare name** with no arity dimension (unlike unions, which are `(name, arity)`-keyed),
> so declaring two interfaces both named `Fun` collides.

`rung3-handoff.md` §4 (`:104-108`) states the deferral and the size estimate:

> give `Types.Class` an `(name, arity)` key + update the ~47
> name-resolution/freeze/codegen lookups, mirroring the union machinery.

The union machinery already does exactly this for `Choice\`2`…`Choice\`7`. This
doc's whole job is to (a) port that machinery to classes, and (b) reconcile the
"~47" estimate against the real lookup-site inventory — which turns out to be a
**material overcount** once codegen is examined (§2).

---

## 1. The key change

### 1.1 Current state — bare-name class registry

The registry dictionary is `PassContextTypes.Class : Dictionary<string, ClassTypeInfo>`
(`SideTables.fs:565`), keyed by the **bare short name**. The accessor wrappers are
all bare-name (`SideTables.fs:664-677`):

```fsharp
// SideTables.fs:664
let registerClass (types) (name: string) (info: ClassTypeInfo) = types.Class.[name] <- info
// :666
let containsClass (types) (name: string) = types.Class.ContainsKey name
// :668
let tryClass (types) (name: string) : ClassTypeInfo voption = …TryGetValue name…
// :676  — projects the key to its BARE simpleName, dropping arity:
let tryClassByKey (types) (key: SymbolKey) = tryClass types (SymbolKeyOps.simpleName key)
```

`SymbolKeyOps.simpleName` (`SymbolKeyOps.fs:100-107`) explicitly **strips the
`` `N `` arity suffix** (`bareName n`, `:107`). So even a caller holding a fully
arity-qualified `SymbolKey.TypeKey(_, _, "Fun\`3")` collapses onto `"Fun"` the
moment it hits `tryClassByKey`. *That* projection — not the dictionary type alone —
is the collision mechanism.

Crucially, the class's **identity key is already arity-qualified**. Registration
mints it via `key = stampLocalTypeKey ctx declKey declNs name typeParams.Length`
(`MemberRegistration.fs:516`), and `stampLocalTypeKey` routes through
`SymbolKeyOps.arityName` (`SymbolKeyOps.fs:40-44`, the single `` Name`n `` rule).
So `ClassTypeInfo.Key` for `Fun<,,>` is already `Fun\`3`. The bare-name *dictionary
key* and the bare-name *`tryClassByKey` projection* are the only two places that
throw the arity away.

### 1.2 The union key — the precise blueprint

Unions are keyed by `keyFor name arity = SymbolKeyOps.arityName name arity`
(`SideTables.fs:636`). The full reader/writer surface (`SideTables.fs:692-733`):

```fsharp
// :692  register under the arity-key, maintaining a single-arity bare alias,
//        withdrawing it on the second arity (UnionBareArity bookkeeping :596)
let registerUnion (types) (name) (arity: int) (info) =
    types.Union.[keyFor name arity] <- info
    if arity > 0 then …bare-alias maintenance via types.UnionBareArity…
// :710  duplicate test — arity-key only
let containsUnion (types) (name) (arity) = types.Union.ContainsKey(keyFor name arity)
// :715  resolve by (name, arity) — exact arity-key, NO bare fallback
let tryUnion (types) (name) (arity) = …TryGetValue (keyFor name arity)…
// :727  resolve by SymbolKey — uses the key's name component AS-IS (already arity-suffixed),
//        i.e. it does NOT call simpleName, so arity survives:
let tryUnionByKey (types) (key) =
    match key with SymbolKey.TypeKey(name = name) -> …TryGetValue name… | _ -> ValueNone
```

The single most important contrast: `tryUnionByKey` (`:727-733`) reads
`TypeKey(name = name)` **verbatim** (the name already carries `` `N ``), whereas
`tryClassByKey` (`:676-677`) funnels through `simpleName` and **drops** it. Closing
the class gap is, at its core, making `tryClassByKey` behave like `tryUnionByKey`.

### 1.3 Proposed class key

Mirror unions exactly. "Arity" = **generic type-parameter count**
(`typeParams.Length` at registration, `MemberRegistration.fs:498,516`; the same
quantity unions call `typeArity`, `TypeRegistration.fs:308`).

1. Re-key the dictionary on `keyFor name arity` (the `` Name`n `` string), not bare
   `name`. The dictionary type `Dictionary<string, ClassTypeInfo>` is **unchanged**;
   only the key STRING changes (exactly as unions did — `types.Union` is also
   `Dictionary<string, _>`).
2. Add arity-taking accessors mirroring the union four:
   - `registerClass (types) (name) (arity) (info)` — write `keyFor name arity`,
     maintain a single-arity bare alias via a new `ClassBareArity` dict (mirror
     `UnionBareArity`, `SideTables.fs:596`), withdraw on the second arity.
   - `containsClass (types) (name) (arity)` — `keyFor name arity` only.
   - `tryClass (types) (name) (arity)` — exact arity-key, no bare fallback.
   - `tryClassByKey` — rewrite to read `TypeKey(name = name)` **verbatim** (copy
     `tryUnionByKey`, `:727-733`), so the arity-suffixed name survives.
3. Add `ClassBareArity : Dictionary<string, int>` to `PassContextTypes`
   (`SideTables.fs:557-605`) + `PassContextTypes.empty` (`:608-620`), mirroring
   `UnionBareArity` (`:587-596`, `:618`).

The bare alias (so a *single*-arity class read by bare name keeps resolving) is
what lets the migration land additively: every existing single-arity class
(`Fun<,>`, all user classes) keeps its bare-name read working until/unless a second
arity of the SAME name registers. Only `Fun` (once `Fun2`→`Fun<,,>` lands) ever
has two arities, so only `Fun`'s bare alias is ever withdrawn.

---

## 2. Inventory of lookup / update sites, by layer

The estimate is "~47" (`rung3-handoff.md` §4). The **reconciled count of sites that
actually need touching is ~22 front-end call sites + 6 registry-API definitions +
the registry type/`empty` — codegen needs ZERO new arity plumbing**. The "~47" was
an over-estimate that almost certainly counted (a) codegen `UserTypes`/`Layout`
lookups that are *already* arity-keyed, and (b) the noise of unrelated
`TTypeKind.Class` / `ExternalTypeShape.Class` / `TypeDefn.Class` DU-case matches
(dozens of those exist and are NOT registry lookups). See §2.5.

> Method: `grep` for the genuine registry surface only —
> `tryClass `, `tryClassByKey`, `containsClass `, `registerClass `, and the raw
> `ctx.Types.Class.{TryGetValue,ContainsKey,[…]}` dictionary reads. DU-case matches
> on `*.Class` were excluded by hand.

### 2.1 Registry definitions (the change itself) — `SideTables.fs`

| site | line | change |
|---|---|---|
| `PassContextTypes.Class` field | `:565` | key string becomes `` name`arity `` (type unchanged) |
| `PassContextTypes` add `ClassBareArity` | `:557-605` | new field, mirror `UnionBareArity` `:596` |
| `PassContextTypes.empty` | `:608-620` | init `ClassBareArity` (`:618` mirror) |
| `registerClass` | `:664` | take `arity`; write `keyFor`; maintain bare alias |
| `containsClass` | `:666` | take `arity`; `keyFor` |
| `tryClass` | `:668-671` | take `arity`; `keyFor` |
| `tryClassByKey` | `:676-677` | read `TypeKey(name=…)` verbatim (copy `tryUnionByKey` `:727`) |

### 2.2 Name-resolution layer

| site | line | what it does / change |
|---|---|---|
| duplicate-type guard | `MemberRegistration.fs:484-487` | `containsClass ctx.Types name` → add arity; also `ctx.Types.Union.ContainsKey name` here is a bare read but is the duplicate guard (keep bare-alias semantics or switch to `containsUnion` w/ arity) |
| `registerClass` call | `MemberRegistration.fs:583` | thread `typeParams.Length` (already in scope `:498,516`) |
| member-type resolution | `MemberRegistration.fs:703` | `tryClass ctx.Types name` → arity (arity derivable from the use-site type args) **OQ** |
| class lookup | `MemberRegistration.fs:758` | `ctx.Types.Class.TryGetValue name` — bare; arity in hand? **OQ** |
| qualified-static head | `MemberRegistration.fs:790` | `…TryGetValue (ctx.NameOf nameLi.Idents.[0])` — only a *written name* string, NO arity → **bare-alias path (load-bearing, §6)** |
| parent-class lookup | `MemberRegistration.fs:833` | `…TryGetValue parentName` — bare; arity from base-type args? **OQ** |
| duplicate guard | `MemberRegistration.fs:487` | (same as `:484` row) |
| name-is-a-type test | `NameResolution.fs:323` | `…TryGetValue name` — recognition only; bare alias suffices |
| qualified-static recognition | `Scope.fs:356-357` | `ContainsKey typeName` + `.[typeName].Members` — **written name only, NO arity → bare-alias path (load-bearing, §6)** |
| name-shadowing test | `Scope.fs:142` | `ContainsKey name` — recognition; bare alias suffices |
| module-vs-type test | `Elaborate.fs:1396` | `ContainsKey moduleName` — recognition; bare alias suffices |
| class lookup | `Elaborate.fs:436,1039` | `…TryGetValue name` — bare; arity? **OQ** |

### 2.3 Freeze layer — `Freeze/Resolve.fs`

| site | line | change |
|---|---|---|
| is-local-class test | `:104,111` | `ContainsKey n` — recognition; bare alias suffices |
| class lookup | `:145` | `…TryGetValue typeName` — bare; arity? **OQ** |
| `tryClassByKey` | `:277,673,718` | **fixed for free** once `tryClassByKey` reads the key verbatim (§2.1) — these callers already hold an arity-qualified key |

### 2.4 Inference / unification layer (`Passes/Unification*`)

All of these hold either a `SymbolKey` (→ `tryClassByKey`, fixed for free) or a
use-site with type-args in hand (arity derivable). The `tryClassByKey` sites need
**no edit** beyond §2.1's verbatim-read fix:

`Unification.fs:704,852` (`tryClassByKey`); `Engine.fs:1252,1533` (`tryClassByKey`);
`InferCtor.fs:33` (`tryClassByKey`); `InferControlFlow.fs:378,391,441,474`
(`tryClassByKey`); `InferRecordAccess.fs:294` (union, unrelated); `Infer.fs:159`,
`InferRecordAccess.fs:210,251`, `InferControlFlow.fs:505,546` (`tryClass …name` —
arity-taking, **OQ** whether arity is in scope).

Bare `ctx.Types.Class.TryGetValue` reads needing an arity threaded (or a bare-alias
fallback): `Unification.fs:914,1078,1209`; `Engine.fs:337,515,541,771`;
`InferCtor.fs:274`; `InferIdentExpr.fs:73,215`; `Translate.fs:187,389`;
`InferResolve.fs:54`. Two of these are `TyClass(clsKey,_)` guards
(`Engine.fs:466`, `InferRecordAccess.fs:506`) that call
`tryClass … (simpleName clsKey)` — they should switch to `tryClassByKey clsKey`
(verbatim) so arity survives.

### 2.5 Codegen / contract-publishing / compiled-name layer — **NO arity plumbing needed**

This is where the "~47" over-counts. Codegen does **not** key off the front-end
`PassContextTypes.Class` dictionary at all; it keys off `SymbolKey` and the frozen
`TTypeDecl.Key`, both already arity-qualified:

- `ClrProvider.RegisterUserType` / `UserTypeHandle` key `env.UserTypes` by
  `SymbolKey` (`ClrProvider.fs:51,132,208`). The key's name is already `` `n ``.
- `Layout.nominalSlot.MetaName = SymbolKeyOps.arityName td.Name td.TypeParams.Length`
  (`Layout.fs:443`) — **already arity-suffixed** for every nominal type, classes
  included. The closure slot is the same (`Layout.fs:842`).
- External class refs (`ClrEnv.fs:450,470`) already `arityName` the simple name.
- Contract extraction (`VesperLib.fs:795`, `TypeTranslate.fs:235`) already
  `arityName`s.

So the **compiled-name scheme is already correct** (§3). Codegen will emit
`Fun\`3` the instant the front-end mints a `Fun<,,>` whose key is `Fun\`3` — which
it already does (`stampLocalTypeKey`). The only codegen risk is a *cross-check*
that assumes "bare class name ⇒ unique type" (none found in the registry-lookup
grep; flagged as an open question to re-grep before M5).

### 2.6 Reconciliation vs "~47"

Genuine touch-points: **7 registry defs + ~22 front-end call sites ≈ 29**, of which
the ~12 `tryClassByKey`/`TyClass(clsKey)` sites are **fixed in bulk** by the single
verbatim-read change in §2.1 (they need no per-site edit). Net hand-edited call
sites: **~10-15**. The "~47" is a material overcount — most plausibly it swept in
codegen `*.Class` DU matches and already-arity-keyed `UserTypes`/`Layout` sites.
**State this in the handoff when the work lands.**

---

## 3. Compiled-name scheme

No new scheme is needed — the `` Name`n `` (CLR backtick-arity) convention is
*already* the law for every nominal type via `SymbolKeyOps.arityName`
(`SymbolKeyOps.fs:40-44`), consumed identically by the union, record, class, and
closure emission paths (`Layout.fs:443,842`; `ClrEnv.fs:450,470`).

- `Fun<,>` → compiled `Fun\`2`; `Fun<,,>` → compiled `Fun\`3`. Distinct CLR
  metadata names, exactly as `Choice\`2`/`Choice\`3`.
- JS backend: the same arity-suffixed `SymbolKey` name flows through; JS class
  emission (`EmitJs.fs:1397,1463`) reads `TTypeKindG.Class` off the frozen decl
  whose `Key`/`Name` is already arity-qualified, so no JS-specific change.
- **Backward-compat with already-emitted `Fun`/`Fun2` references:** today
  `Vesper.Core` emits an interface literally named **`Fun2`** (arity-2 in NAME,
  arity-3 in generics — `core-types.fs:13` `Fun2<'A,'B,'C>`). After migration it
  becomes `Fun<,,>` → metadata `Fun\`3`. **This is a binary-breaking rename of the
  Vesper.Core contract** (`Fun2` → `Fun\`3`). Because Vesper.Core is rebuilt from
  source in the same tree (no external consumers pin it), the break is contained:
  every consumer (`struct-seq.{fsi,fs}`, `StructSeqTests.fs`) recompiles against
  the new name. The committed `Vesper.Core.dll` reference must be regenerated
  (`REGEN_VESPER_CORE_REF=1`, `rung3-handoff.md` §5) as part of the rename step.

---

## 4. Migration of `Fun2` → `Fun<,,>`

The current shapes (`core-types.fs`):

```fsharp
type Curried<'A,'B,'C>(f: Fun2<'A,'B,'C>, a: 'A) =        // :13
    interface Fun<'B,'C> with member _.Invoke(b) = f.Invoke(a, b)
type Flattened<'A,'B,'C>(f: Fun<'A, Fun<'B,'C>>) =        // :17
    interface Fun2<'A,'B,'C> with member _.Invoke(a,b) = …  // :18-19 (let-split workaround :25-26 lives in the .fsi/older form)
let curryFun (f: Fun2<'A,'B,'C>) (a: 'A) : Fun<'B,'C> = … // :24
let flatten  (f: Fun<'A, Fun<'B,'C>>) : Fun2<'A,'B,'C> = …// :26
```

`Fun` itself is declared in `core-types.fsi` (the contract; `Fun2` likewise). The
rename touches: the `Fun2` interface decl in `core-types.fsi`; every `Fun2` mention
in `core-types.fs` (`:13,17,18,24,26`); and the `Fun2` constraints in
`struct-seq.fsi` (`:99,109` `fold`'s `'TFunc :> Fun2<'State,'T,'State>`) and
`struct-seq.fs`; and the `Fun2`-typed fixtures (`StructSeqTests.fs` `SumAcc`).

### Safe ordering — the `(name,arity)` key lands FIRST, rename SECOND

YES — the two are cleanly separable and MUST be staged so the tree stays green:

**Step A (key only).** Land §1+§2 with `Fun2` *still a distinct name*. After Step A,
`Fun` is single-arity (arity 2) and keeps its bare alias; `Fun2` is single-arity
(generic arity 3) under name `"Fun2"`. Nothing collides, nothing renames. The
registry is now arity-capable but exercised only by a NEW isolation test that
declares two same-named interfaces of different arity (M1 below). The whole existing
corpus is unaffected because no existing class name is multi-arity.

**Step B (rename).** Rename `Fun2<'A,'B,'C>` → `Fun<'A,'B,'C>` in `core-types.{fsi,fs}`,
`struct-seq.{fsi,fs}`, and tests. Now `Fun` has TWO arities (2 and 3); the bare
alias for `Fun` is withdrawn on the second registration (`ClassBareArity` →`-1`,
mirror `SideTables.fs:702-706`). Every read of `Fun` must now be arity-qualified —
which is why Step A must already have threaded arity through the load-bearing reads
(§6). Regenerate `Vesper.Core.dll` ref. Update `.parsed` goldens for the changed
`.fsi`/`.fs` (`rung3-handoff.md` §5).

If any load-bearing bare-`Fun` read is still un-arity'd after Step A, Step B will
mis-resolve `Fun` (alias withdrawn → bare read misses). So Step A's acceptance gate
is: *every* bare-name class read that could ever see `Fun` is either arity-threaded
or provably recognition-only (bare alias OK). M2 below pins this.

---

## 5. Interaction with the §3 lambda-lowering design + §3.1 gaps

`rung4-lambda-lowering-design.md` is the sibling epic (source lambdas riding the
struct-`Seq` pipeline). Shared touch-points:

1. **`Fun2` is referenced by name throughout rung4's design** (`§2.1`, `§3`,
   `§5.1.1`, e.g. `rung4-…:236-244` "Saturated 2-arg → flat `Fun2<'A,'B,'C>`").
   After the §4 rename those become `Fun<,,>`. **Coordination:** the rung4 pass
   synthesises `App` nodes around `curryFun`/`flatten` and retypes params to
   `Fun`/`Fun2` constrained typars — it resolves those types **by name through the
   provider stack** ([[project_contract_demotion]]). If rung4 lands its
   name-resolution of `Fun2` BEFORE this epic's rename, the rename must update
   rung4's resolved-symbol lookup too. **Recommendation: land this epic's Step A
   (key) independently of rung4; sequence Step B (rename) and rung4's `Fun2`
   references in one coordinated change, or land rung4 first against `Fun2` and
   rename last.** They must not both edit `core-types.fs`'s `Fun2`/`Curried`/
   `Flattened` block concurrently.

2. **§3.1 gap 1 (chained-receiver freeze mistype)** — `core-types.fs` `Flattened.Invoke`
   carries the `let`-split workaround (`rung4-…:213-214,256-267`). The §4 rename
   edits that exact method body. **Do not drop the `let`-split during the rename**
   unless gap 1 is independently fixed; the rename is a pure name change, not a
   body change.

3. **§3.1 gap 2 (fieldless `[<Struct>]` parse recovery)** is orthogonal — it bites
   rung4's synthesised closures and rung4's isolation tests, NOT this epic's
   registry change. This epic's isolation tests declare *interfaces* (no fields), so
   they are unaffected; but M1's "two same-named interfaces" test should be written
   as plain `type Fun<'A,'B> = abstract …` interface decls, which don't trip the
   struct-recovery path.

**Net:** the ONLY hard conflict is the `core-types.fs` `Fun2` block. Both epics
want to touch it. Sequence them; do not parallelize Step B with rung4.

---

## 6. Risks / open questions

1. **Does the union machinery share reusable code, or is it union-specific?**
   *Mostly reusable by copy, not by call.* The key-derivation (`keyFor` =
   `arityName`, `SideTables.fs:636`) and the bare-alias bookkeeping
   (`UnionBareArity`, `:692-706`) are written against `types.Union` /
   `types.UnionBareArity` directly — they are not generic over the table. The
   honest port is to **duplicate the four accessors + the `*BareArity` field** for
   classes (≈30 lines, mirroring `:692-733`), NOT to abstract a shared helper now
   (the `TypeRegistry` comment at `:638-644` already anticipates this: classes
   "aren't arity-overloaded today … the wrappers exist so identity creation funnels
   through one place — the single seam an arity key would be threaded through if
   these ever overload"). A shared generic helper is a *follow-up* cleanup once both
   record-bare and class-arity paths exist, not a prerequisite.

2. **Load-bearing bare-name class reads that have NO arity in hand.** The two
   genuinely hard sites are the **qualified-static-access** recognizers that see
   only a *written type name* string: `Scope.fs:356-357`
   (`ContainsKey typeName && staticIn …Class.[typeName].Members`) and
   `MemberRegistration.fs:790`. A user writes `Fun.SomeStaticThing` with no arity —
   so these MUST rely on the bare alias. **Risk:** once `Fun` is multi-arity (Step B)
   the bare alias is withdrawn and these reads miss. **Mitigation:** `Fun`/`Fun<,,>`
   are *interfaces with no static members*, so the static-access recognizers will
   simply not match `Fun` (no static member named X) — the miss is harmless for
   `Fun` specifically. But the general hazard remains for any future multi-arity
   class WITH static members; document that qualified-static access cannot
   disambiguate arity from a bare written name (it would need the F# rule of
   resolving by the type-argument count at the use site, which these recognizers
   don't have). **Open question:** confirm `Scope.fs:356` never needs to resolve a
   multi-arity class with statics; if it could, this needs a written-arity-aware
   path (out of scope for the `Fun` migration).

3. **Name-collision hazard: a class and an interface of the same name, different
   arity** (e.g. a class `Foo<'A>` and interface `Foo<'A,'B>`). The duplicate-type
   guard (`MemberRegistration.fs:484-487`) currently rejects ANY same-bare-name
   second definition across record/union/abbrev/class. After arity-keying, it must
   reject only same-`(name,arity)` (mirror `containsUnion`, `TypeRegistration.fs:311`).
   **Risk:** if the guard is loosened to arity-aware for classes but the
   cross-kind checks (`containsRecord name`, `Union.ContainsKey name`) stay bare, a
   class `Foo<'A,'B>` and a union `Foo\`2` could now both register and later
   mis-resolve. The union duplicate guard already only checks `containsRecord`
   bare + `containsUnion` arity (`:312-311`); classes should match that shape and
   the interaction must be tested (M4).

Secondary open questions (flagged inline above as **OQ**): whether each bare
`ctx.Types.Class.TryGetValue name` use site at `MemberRegistration.fs:703,758,833`,
`Elaborate.fs:436,1039`, `Freeze/Resolve.fs:145`, and the Unification bare reads
(§2.4) has the use-site arity in scope, or must fall back to the bare alias. Each
needs a per-site read when implementing; none is expected to be load-bearing for
the `Fun` migration (only `Fun` is multi-arity, and it's an interface used by name
with explicit type args at every site).

---

## 7. Staged implementation plan (isolation-test-driven)

Per [[feedback_systematic_tests_over_whackamole]]: each milestone is the smallest
isolation test that forces the capability, then diagnose → fix → iterate. Ordered
by dependency and risk. The FIRST milestone is a **red** test demonstrating the
current collision.

> Build/test ONLY via `./claude_tools.cmd` (`rung3-handoff.md` §5) — **but another
> agent is editing the tree concurrently; do not build while that holds.**

### M0 — RED: demonstrate the current collision
**Smallest test:** a single source file declaring two interfaces of the same name,
different generic arity:
```fsharp
type Fun<'A,'B> = abstract Invoke: 'A -> 'B
type Fun<'A,'B,'C> = abstract Invoke: 'A * 'B -> 'C
```
Assert (today) the SECOND registration raises `"Duplicate type definition: Fun"`
(`MemberRegistration.fs:489-495`) — proving the bare-name collision. Pure probe, no
fix. **Proves:** the exact wall the epic removes. Risk: none (characterization).

### M1 — GREEN: arity-keyed registry (Step A, key only)
**Depends on:** M0. **Smallest test:** M0's two-interface file now registers BOTH
(no duplicate diagnostic), and a use site of each (`x: Fun<int,int>` vs
`y: Fun<int,int,int>`) resolves to the correct distinct `ClassTypeInfo` with the
right member arity. **Proves:** §1 (re-key `Class` dict + `ClassBareArity` +
arity accessors, `SideTables.fs:565,664-677`) AND `tryClassByKey` verbatim-read
(`:676` → copy `:727`). Risk: medium (the bulk `tryClassByKey` consumers must keep
resolving single-arity classes via the surviving bare alias).

### M2 — Bare-alias survival across the existing corpus (Step A acceptance gate)
**Depends on:** M1. **Smallest test:** the existing class/interface golden + unit
suites (`ClassTests.fs`, `NameResolutionTests.fs`, `FreezeTests.fs`) all stay green
— every single-arity class still resolves by bare name. **Proves:** no load-bearing
bare read regressed; the bare alias (§1.3) covers recognition-only sites
(`Scope.fs:142`, `NameResolution.fs:323`, `Elaborate.fs:1396`, `Resolve.fs:104,111`).
Risk: medium (this is where an un-threaded bare read would surface).

### M3 — Two same-name classes of different arity, WITH members
**Depends on:** M1. **Smallest test:** two CLASSES (not interfaces) `Box<'A>` and
`Box<'A,'B>`, each with a distinct instance method; construct + call each; assert
both emit (`Box\`1`, `Box\`2` metadata names via `Layout.fs:443`) and dispatch to
the right member. **Proves:** §2.5 (codegen needs no arity plumbing — the
arity-suffixed `SymbolKey`/`MetaName` already disambiguate) end-to-end. Risk:
low-medium (validates the codegen "free" claim).

### M4 — Cross-kind name-collision guard
**Depends on:** M1. **Smallest test:** a class `Foo<'A,'B>` alongside a union
`Foo<'A>` (different kinds, names overlap); assert the duplicate guard
(`MemberRegistration.fs:484-487`) behaves per the union model — reject same
`(name,arity,kind)` but not a benign cross-arity coexistence — and that neither
mis-resolves the other. **Proves:** §6 risk 3 resolved. Risk: medium (the
cross-kind guard interaction).

### M5 — Re-grep codegen for bare-name class assumptions
**Depends on:** M3. **Smallest test:** a targeted grep audit (no new fixture) for
any codegen path that assumes a bare class name is unique (none found in the
registry grep, but the closure/JS paths weren't exhaustively audited). Convert any
finding into a fixture. **Proves:** §2.5 "no codegen change" claim is exhaustive,
not just sampled. Risk: low.

### M6 — Migrate `Fun2` → `Fun<,,>` (Step B)
**Depends on:** M1-M5 green. **Smallest test:** rename in `core-types.{fsi,fs}` +
`struct-seq.{fsi,fs}` + fixtures; the rung-3 "wall iv" zero-alloc proof
(`StructSeqTests.fs:978`) still passes with `Fun<,,>` in place of `Fun2`, asserting
the `constrained.` dispatch (`0xFE 0x16`) and NO `box` (`0x8C`). Regenerate
`Vesper.Core.dll` ref (`REGEN_VESPER_CORE_REF=1`) + `.parsed` goldens. **Proves:**
the rename keeps the struct-seq pipeline green and `Fun` now legitimately overloads
arities 2 and 3 (bare alias for `Fun` withdrawn, both resolved by arity). Risk:
medium (binary-breaking contract rename; coordinate with rung4 per §5).

### M7 — `buildPackage`-gated Vesper.Core/Vesper.Seq after the rename
**Depends on:** M6. **Smallest test:** the strict package path
([[reference_buildpackage_gates_on_diagnostics]]) builds `Vesper.Core` and
`Vesper.Seq` with the renamed `Fun<,,>` and a client consuming `StructSeq.fold`'s
`'TFunc :> Fun<'State,'T,'State>` constraint. **Proves:** the rename survives the
strict gate, not just the lenient inline path. Risk: low-medium (package path
surfaces front-end gaps the inline path hides).

---

## 8. Cross-references

- `rung3-handoff.md` — §2 (`:43-48`) the constraint lifted; §4 (`:104-108`) the
  deferral + "~47".
- `rung4-lambda-lowering-design.md` — the sibling epic; shared `Fun2` touch-point
  (§5 here).
- `SideTables.fs:622-767` `module TypeRegistry` — the union (name,arity) machinery
  this mirrors; `:638-644` the comment anticipating the class arity seam.
- `SymbolKeyOps.fs:40-44` `arityName` — the single `` Name`n `` rule (CLR
  backtick-arity) already shared by registry key, `SymbolKey`, and metadata name.
- Memories: `[[project_arity_overloaded_type_names]]` (the union model),
  `[[project_seq_struct_pipeline_ladder]]`, `[[project_semtype_nominal_key_asm_none]]`,
  `[[reference_buildpackage_gates_on_diagnostics]]`,
  `[[feedback_systematic_tests_over_whackamole]]`,
  `[[feedback_plan_docs_ephemeral]]`.
