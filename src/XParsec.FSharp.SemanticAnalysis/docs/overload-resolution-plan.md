# Follow-up: universally-total local member-key mint

Overload resolution itself has **landed** (front-end resolution, subtyping,
betterness ranking, user-declared overload sets, the inference→Freeze key
handshake, and the duplicate / ambiguous / no-applicable diagnostics). This
document is the single remaining follow-up it deferred, written to be picked up
cold.

Line numbers below are as of the landing commit (`bf3b5003`) and will drift —
treat them as starting points, confirm against the disk.

## What this is

A user (project-local) member's TAST key is minted by `LocalSymbolKey.ofMember`
(`TypeInfos.fs:49`):

```fsharp
let ofMember (declKey: TypeKey) (name: string) (arity: int) (kind: MemberKind) : SymbolKey =
    SymbolKeyOps.memberKey declKey name (EqArray.ofList (List.replicate arity (FTUnknown ""))) 0 kind
```

The `ArgSig` is `arity` copies of the placeholder `FTUnknown ""` ("only the
length is ever read") and `MethodTyparArity` is hardcoded `0`. So the key is
**not a total overload identity**: two same-name, same-*value*-arity overloads —
`Show(int)` / `Show(string)`, or `M<'a>()` / `M<'a,'b>()` — mint the *identical*
key.

The overload work already closed the observable hole for **overloaded instance
method calls**: inference resolves the call by argument type, freezes the chosen
member's parameter signature into a total `MemberKey`, records it on the call
node (`Resolution.LocalMemberCall`, `PassContext.fs:221`), and `mkMethodCall`
(`Elaborate/Resolve.fs:605`) reads it back verbatim — total key, by
construction, for that path only.

Everywhere else the placeholder still stands. This follow-up retires it so a
lossy local member key is **unrepresentable by construction** — the deep form of
the project's "inference determines an unambiguous key; every consumer is a
straight lookup" rule.

## Scope — which mints are actually lossy

Not all `ofMember` call sites need touching. The discriminator is
`MemberKind` × value-arity:

- **Property mints (arity 0) are already total.** A property's `ArgSig` is empty
  and a property name is unique on a type (properties do not overload by
  argument), so `{decl, name, [], 0, Property}` cannot collide. **Leave them.**
  These are `ElaborateExpr.fs:189/210/328`, `Elaborate/Idents.fs:145`,
  `Elaborate/Resolve.fs:752/760/776`, `Elaborate/Access.fs:176/183`.
- **Method mints are the lossy surface** — the `ArgSig` placeholder erases
  parameter types and the `0` erases method-typar arity:
  - `Elaborate/Resolve.fs:611` — `mkMethodCall`, instance. **Total only for
    overloaded names today** (via the `LocalMemberCall` handshake); the
    non-overloaded fallback is still placeholder.
  - `Elaborate/Resolve.fs:631` — `mkInterfaceMethodCall`. Placeholder.
  - `Elaborate/Resolve.fs:644` — `mkStaticMethodCall`. Placeholder.
  - `Inline.fs:261` — inline-expansion method key. Placeholder.

There is **no correctness bug** left: a non-overloaded name's placeholder key is
genuinely unique (nothing else shares the name at that arity/kind), and the
overloaded instance-call path is already total. This is a by-construction
*hardening*, not a fix — which is why it was deferred.

## Why (and when) to do it

Do it when a **second consumer** needs local member keys to be uniformly total —
most likely the JS overload-name mangling work
([js-overload-mangling-plan](js-overload-mangling-plan.md)), which keys emitted
names by signature and so needs a total `ArgSig` at *every* method-key mint, not
only the ones a front-end call happened to resolve as overloaded. Until then the
placeholder is harmless and the enhancement's blast radius (below) outweighs its
benefit.

## Mechanism

The freezing machinery already exists, built for the overloaded-call path in
`Passes/Unification/InferOverload.fs`:

- `freezeUserMemberArgSig (declTypars) (m: TypeMemberInfo) : EqArray<FrozenType>`
  — freezes a member's value parameters into the declaring type's open typars
  (`FTTypar(Declaring, i)`) and its own method typars (`FTTypar(Method, j)`),
  the same structural form `ExternalSymbols.argSigOfParameters` mints for
  external members.
- `frozenUserMemberKey (declKey) (declTypars) (m) : SymbolKey` — the total
  `MemberKey` from that argSig + the member's real `MethodTyparArity`
  (`m.EffectiveMethodTypars.Length`).

Two shapes for applying it universally; **the first is preferred** because it
keeps the one declaring-typar cut in a single place and matches the rule the
codebase already follows:

**(1) Inference stamps, Elaborate reads (preferred).** Extend the
`LocalMemberCall` handshake from "only overloaded instance calls" to **every**
resolved local method reference — instance, interface, static, and inline. At
each resolution point in Unification, stamp `frozenUserMemberKey` on the node's
side table; every method mint site then reads it with no placeholder fallback,
and `ofMember`'s method use disappears. The cost is finding the resolution point
for each method-reference *kind* (the instance path has the probe
`tryInferLocalInstanceMethodCall`; static / interface / inline resolve
elsewhere and need an equivalent stamp). This is the literal "inference
determines the key" mechanism.

**(2) Mint from the resolved member at the site.** Each of the four method mint
sites already holds (or can look up by `declKey` + name) the resolved
`TypeMemberInfo`; call `frozenUserMemberKey` there directly. Distributes the
typar-cut to four Elaborate sites rather than centralizing it in inference —
simpler to wire, but four copies of the cut and it does not embody the
"inference decides" rule.

Either way, once every method mint is total, delete `ofMember`'s method path
(keep the arity-0 property mint, or split the helper) so a placeholder method
key is unrepresentable.

## Constraints

- **Do not touch `Engine.unify` / the hot path.** This is all Elaborate-side
  mint + a side-table stamp at existing resolution points.
- **Properties stay as they are** — already total; widening them is churn with no
  benefit and risks the property-get key paths.
- **Behaviour-preserving for non-overloaded names.** Their total key must select
  the *same* member the placeholder key did; the enhancement only sharpens the
  key, it must not change which member any existing lookup resolves.
- Reuse `freezeUserMemberArgSig` / `frozenUserMemberKey` — do not re-roll the
  declaring/method-typar freeze.

## Test strategy

- A non-overloaded local method call's minted key is now a total `MemberKey`
  (real `ArgSig`, real `MethodTyparArity`) rather than the placeholder — assert
  the `ArgSig` heads match the declared parameter types.
- An interface-method and a static-method call likewise carry total keys.
- Generic-arity overloading is now distinguishable: `M<'a>()` / `M<'a,'b>()` mint
  keys differing in `MethodTyparArity` (the placeholder collapsed both to `0`).
- Every existing test stays green — this changes key *content*, not member
  selection, so any test asserting on a local member key's shape updates from
  placeholder to total; any asserting on *which* member is called is unaffected.
- The consuming feature (JS mangling) gets the end-to-end "two overloads emit
  distinct callable names" test.

## Cross-references

- `TypeInfos.fs:49` — `LocalSymbolKey.ofMember`, the placeholder mint to retire.
- `Passes/Unification/InferOverload.fs` — `freezeUserMemberArgSig`,
  `frozenUserMemberKey`, `memberSignatureKey`: the freeze machinery to reuse.
- `PassContext.fs:221` — `LocalMemberCall` side table; the handshake to widen
  under mechanism (1).
- `Elaborate/Resolve.fs:605` — `mkMethodCall`, the readback template.
- `SemanticInfo.fs:634` — `MemberKey`, already a total identity type; only the
  local mint is lossy.
- [js-overload-mangling-plan](js-overload-mangling-plan.md) — the likely second
  consumer that motivates doing this.
