# JS overload-name mangling

## Problem

JavaScript has no method overloading: a JS object exposes one property per name.
The CLR backend gets overload resolution for free from metadata, so `Child(obj)`
and `Child(int)` are simply distinct `MemberRef`s. On JS they collide.

Today `JsExternalMembers.mangledName` is **nominal-only**:

```fsharp
// JsExternalMembers.fs
let mangledName typeName isStatic isProperty memberName =
    if isStatic then typeName + "_" + memberName
    elif isProperty then typeName + "__get_" + memberName
    else typeName + "__" + memberName
```

There is no parameter-type or arity component anywhere, so any two overloads of a
name mangle to the same JS identifier and one silently shadows the other. This
blocks every overloaded interface member — the immediate motivating case being the
typed-`Child` overloads on `IFormatSink` (`Child(int)`, `Child(string)`, … that let
`%A` skip boxing primitive children instead of routing everything through
`Child(obj)`).

The mangling must be a pure deterministic function of the member's *signature*,
because `IFormatSink` spans three independently-compiled worlds that never
coordinate: **defined** in `Vesper.Core`, **implemented** in `Vesper.Printf`, and
**called** from the synthesised `Format` body in every downstream program. All
three must compute the same JS name in isolation.

## Compilation model (settles the ABI questions)

The world is rebuilt: a Vesper compile emits the Core/Printf runtime JS together
with the program. There is no stable-ABI obligation for a precompiled runtime `.js`
to survive a recompile of its callers — a version skew is exactly the F# story of a
`MissingMethodException` at runtime, which we accept. This is what makes
**only-when-overloaded** mangling safe (see Decision 6).

## Identity source: `SymbolKey`

`MemberKey` or `BindingKey` carries the total identity of any callable member through to codegen.

## Decisions

1. **Fold the return type into the discriminant, always.** Overload resolution has
   already happened in the front end, so the mangler only needs to *uniquely name a
   resolved member*, never to resolve. Including the return type therefore cannot
   split a legal overload pair (F# forbids two members differing only in return,
   outside conversion operators) — it only adds entropy — and it makes
   `op_Explicit`/`op_Implicit` fall out with no special case.
2. **Sigil = `$`, reserved by construction.** `$` is JS-idiomatic for generated
   code. A hard "unspeakable in F#" guarantee is unachievable (F# ``quoted``
   identifiers admit nearly everything a JS identifier does; the F#-illegal chars
   are not JS-identifier-legal either), so instead we *reserve* `$` by escaping it
   out of every user-derived name component.

   **Escaping scheme (also covers JS-illegal chars — one mechanism).** In every
   user identifier, any char that is `$` or JS-identifier-illegal → `$` + its
   **uppercase two-hex-digit code point**: `$` → `$24`, `-` → `$2D`, space → `$20`.
   (`$uXXXX` is reserved for the rare non-ASCII-illegal char; deferred, since most
   Unicode is already ID-legal.) Applied **always**, to every component (type and
   member names) — not conditional on overloading.

   This is unambiguous against both discriminant forms by the char right after `$`:
   hash = `$0…`; readable = `$` + lowercase letter; escape = `$` + hex digit, and for
   all printable ASCII (0x20–0x7E) the first hex nibble is **2–7** — never `0`, never
   a lowercase letter. Three disjoint prefixes (`$0` / `$<lower>` / `$2`–`$7`), so an
   escaped `$` can never masquerade as a discriminant separator.

   Escaping *always* (not only for overloaded members) is deliberate: the collision
   party is the `$`-containing member, which may itself be non-overloaded (a raw
   `` ``foo$0ABC23`` `` colliding with an overloaded sibling `foo`'s hash) — a
   cross-member clash the per-overload-set assertion does not see. Unconditional
   escaping makes it impossible by construction, in one trivial string pass.
3. **Hash = truncated sha256, 30 bits → 6 base32 chars** (single-case, RFC 4648
   `A-Z2-7` or base32hex `0-9A-V`; base32 packs 5 bits/char). sha256 chosen for
   bit-exact cross-runtime reproducibility (CLR / Node / browser) — it runs at
   compile time over a tiny input, so performance is irrelevant.

   30 bits is astronomically safe because the collision domain is **per
   `(Type, member-name)` overload set**, not global: the full name is
   `Type__member$0<hash>`, so distinct overloaded names never share a hash space, and
   `n` = overloads of *one* name (realistically ≤ dozens). Birthday bound
   `n²/2³¹`: a dozen overloads ≈ 1-in-15-million, 50 ≈ 1-in-860k, and most sets are
   n = 2–4 (≈ 1e-8). Width is a *margin* knob, not a correctness one — the emit-time
   collision assertion (below) turns any residual collision, even a pathological
   machine-generated set of hundreds, into a loud deterministic build error, never a
   silent miscompile. If that assertion ever fires, widening the injected callback to
   48 bits (10 chars) is a one-line change. *Fallback:* FNV-1a 64-bit (zero-dependency
   integer arithmetic) if a self-host target lacks a sync sha256 (browser
   `SubtleCrypto` is async-only). The correctness safeguard below makes the exact
   algorithm and width low-stakes.

   **Why base32, not base64.** JS identifiers admit only `[A-Za-z0-9]` plus `$` and
   `_`; base64 needs two non-alphanumeric chars (`+`/`/`, or `-`/`_`) and `-` is
   illegal, leaving only `_` clean (`$` is the reserved sigil) — one spare for two
   slots. base32 is fully alphanumeric, no mapping. base64 saves ~2 chars on a 64-bit
   hash (11 vs 13) — irrelevant here. base32's single case is also **case-insensitive
   safe**: the scheme may be reused for targets/sinks with case-folding identifiers
   (VB.NET, Pascal, Ada, unquoted SQL, PowerShell, case-insensitive filesystems if a
   name becomes a symbol), where base64's mixed case would collide.

   **The alphabet is the injected `hash` callback's concern**, not baked in — the
   callback returns the rendered payload (digest + alphabet + width), and `Codegen.
   Common` owns only the marker prefix + disjointness invariant. So base32 is the
   *default*; a case-sensitive target wanting shorter names can inject base64 without
   touching Common.
4. **Well-known readable tokens for the primitive intrinsics (locked).** The
   readable-token set is the niladic scalar `extern` intrinsics declared across
   `Vesper.Core`'s `prim-types-*.fsi` — the "target primitive capability set." Each
   freezes to `FTConst(name, [])`, and **the token is that intrinsic's *simple*
   name** (identity map — no new vocabulary is invented, so the ABI commitment is
   just to already-frozen language surface, and injectivity is automatic because the
   simple names are distinct). The locked leaf set:

   ```
   bool  char  string  obj  decimal
   int   sbyte  byte  int16  uint16  uint32  int64  uint64
   float  float32
   nativeint  unativeint
   ```

   The set is **closed** — the qualified-intrinsic-identities milestone (landed; successor:
   `contract-sourced-intrinsic-identity-plan.md`) gave the intrinsic *identities* a namespace
   (`FTConst("string")` → the qualified `Vesper.string` identity) but did **not** expand the
   set. So this list is stable across that milestone; only the *identity representation* the
   hash reads changed, not the membership. The readable token stays the **simple** name
   (`Vesper.int` → `int`), so qualification never bloats the JS token; the namespace,
   once carried, joins the *hashed* serialization (Decision below), exactly as a
   nominal's `SymbolKey` triple already does. Membership is tested on the simple name
   regardless of how identity is represented.

   `unit` is **excluded**: it is special-cased and *erased* — a lone `unit` param and
   a `unit` return both vanish before mangling — so it never surfaces as a token.
   Mangling therefore runs on the **erased** signature; an erased/void return is
   inherently well-known (a fixed absence marker) and never forces the hash path,
   which keeps Decision 2 (fold in the return) intact. Aliases are **not** separate
   entries — they resolve to their canonical intrinsic before mangling (`int32`→`int`,
   `int8`→`sbyte`, `uint8`→`byte`, `uint`→`uint32`, `double`→`float`,
   `single`→`float32`, `objnull`→`obj`). Pointer/interop intrinsics (`nativeptr<'T>`,
   `voidptr`, `ilsigptr<'T>`) and generic/function intrinsics (`'T[]`, `Fun<…>`) are
   **excluded** (never JS overload params, or handled structurally).

   **Path selection — readable only for single-parameter overloads.** A member takes
   the readable path iff it has **exactly one value parameter**, that parameter's type
   is in the locked set, **and** it is not a conversion operator
   (`op_Explicit`/`op_Implicit`). The readable name is then the single token
   `<Type>__<member>$<paramtoken>`. Everything else — arity 0, arity ≥ 2, a
   non-well-known param, or a conversion operator — hashes.

   The arity-1 restriction is deliberate: it makes the readable name a *single* token
   naming the sole parameter, which deletes any multi-parameter grammar (separators,
   a params-vs-return marker, whole-signature composition). It loses nothing on the
   motivating case — every `IFormatSink.Child` overload is arity-1 over a well-known
   primitive, so `Child$int` / `Child$string` / `Child$obj` all stay readable.

   Why arity-1 needs no set-level uniqueness scan, and why conversion operators are
   carved out: F# forbids two same-name methods with identical parameter types unless
   they differ only by return type, which only `op_Explicit`/`op_Implicit` may do. So
   within any legal type, two arity-1 same-name overloads share a parameter type
   **only** if they are conversion operators. Excluding those, a single param token is
   *guaranteed* unique across the set — no scan needed. Conversion operators (same
   param, differ by return) would collide under a param-only name, so they hash.
   Return type never enters the readable name (it stays in the hash per Decision 2);
   the emit-time collision assertion (below) remains the backstop.

   Arity-0 is not a readable case at all: two zero-parameter overloads can differ only
   by return type or generic arity, so there is no parameter to name and nothing the
   well-known *param* set can key on — they hash. (F# has no sensible return-only
   overload outside conversion operators, which are themselves arity-1.)

   Two invariants keep it safe:
   - **Disjoint by construction.** Hash tokens carry a marker the readable path
     never emits: readable = `$` + simple name (always begins with a letter); hash =
     `$0` + base32. No identifier token can begin with a digit, so `$0…` is disjoint
     from every readable `$<name>` — and unlike a letter marker this holds even under
     case-folding (a target-portability hedge, see Decision 4). A readable token can
     never equal a hash token regardless of the primitive names.
   - **Graceful degradation.** The readable path keys on the intrinsic's simple name;
     if some primitive turns out to freeze *nominally* rather than as `FTConst`, it
     simply falls to the hash path — uglier, never wrong — and the emit-time
     collision assertion (below) still guarantees uniqueness.
5. **Mangle only overload-participating members.** "Is this name overloaded" is a
   deterministic function of the frozen declaring type's member set, visible to
   every consumer, so non-overloaded members keep their clean `Type__member` name.
   The only hazard — adding an overload later *renames* the previously-bare member —
   is a cross-version concern, ruled out by the rebuilt-world model above.

## Scheme

### Serialization (the canonical form that is hashed)

A deterministic pre-order walk of a `FrozenType` into bytes/string. **Invariant:**
`serialize(t₁) = serialize(t₂)  ⟺  t₁ = t₂` under `FrozenType` structural equality.

- `FTConst(name, args)` → the intrinsic's full identity + serialized `args` in
  order. The identity carries a namespace (the qualified-intrinsic-identities
  milestone landed: `Vesper.string`), so the serializer reads *whatever the
  qualified identity is* (so `string` and a hypothetical `Other.string` never
  collide) — mirroring how nominals serialize their `SymbolKey` triple. Hashed
  bytes churn when the identity representation changes is harmless under the
  rebuilt-world model (names churn uniformly). Note this is the *serialization*
  identity — the readable **token** (if taken) stays the simple name regardless.
- `FTFun`, `FTTuple` → order-significant, serialize children in order.
- `FTRecord/Union/Class/Enum(key, args)` → the `SymbolKey` triple `(asm, ns,
  arity-name)` + serialized `args` in order.
- `FTTypar(axis, index)` → `axis` tag + `index`.
- `FTLiteral`, `FTConditional`, `FTKeyOf`, `FTIndexedAccess` → structural, all
  children threaded.
- **`FTOr` is the trap.** It is set-semantic (`A | B ≡ B | A`) but stored
  insertion-ordered. Serializing in storage order would give two equal signature
  types two different names → two JS properties for one overload → silent
  wrong-dispatch. So `FTOr` members must be **canonically ordered** (sort by their
  own serialization) before emission. Everywhere else order is significant and left
  as-is.

The member discriminant hashes `serialize(params in order) ++ serialize(return)`,
over the **erased** signature (`unit` params/return already dropped — Decision 5).

### Name construction

```
non-overloaded                         →  <Type>__<member>            (unchanged)
overloaded, arity-1, well-known param,
  not a conversion operator            →  <Type>__<member>$<paramtoken>   (readable)
overloaded, anything else              →  <Type>__<member>$0<hash>        (hashed)
```

The readable form is a **single** parameter token — no separators, no return token,
no whole-signature composition (Decision 5). The hash form covers arity 0, arity ≥ 2,
non-well-known params, and conversion operators, and hashes the full erased signature
(params ++ return). The two forms are disjoint by the leading `$0` digit marker.

### Collision safeguard (makes hash width low-stakes)

When emitting a type's members, all their frozen signatures are in hand at once.
**Assert the mangled names within a `(Type, member-name)` overload set are unique;
hard-error at emit on any collision.** This converts an astronomically-rare
truncated-hash collision from a silent runtime miscompile into a deterministic build
failure, and decouples correctness from the hash algorithm and truncation width.

## Home & signature

The serializer + discriminant builder live in **`XParsec.FSharp.Codegen.Common`**
(which already references `SemanticAnalysis`, so `FrozenType`/`SymbolKey` are in
scope). SemanticAnalysis has no reason to mangle, but a second backend needing
overload names inherits this for free — that is the whole point of the placement.

The **hash algorithm is a function parameter**, not baked in:

```
// in Codegen.Common
mangleMember :
    hash: (byte[] -> string)          // serialized-sig bytes → 6 base32 chars; swappable
    -> overloadSet: FrozenSig list    // the (Type, name) overload group, for readable/hash + collision check
    -> member: FrozenSig
    -> string
```

Keeping `hash` a parameter means a self-host target that lacks a sync sha256 injects
FNV-1a without touching Codegen.Common, and tests can inject an identity/deterministic
stub. The boundary and contract:

- **Callback owns** digest + truncation + base32 render; **Common owns** the
  serialization → bytes, prepending the `$0` marker, and the emit-time uniqueness
  assertion. The marker staying on Common's side means a misbehaving custom callback
  can never break the readable/hash disjointness invariant.
- **Callback contract:** pure and deterministic (same bytes → same string on *every*
  host — this is the cross-assembly-stability guarantee), output `[A-Za-z0-9]` only
  (no `$`/`_`, so it can't interfere with the sigil or base-name escaping). Common
  cannot enforce purity; the assertion is the backstop.
- **Default impl (shipped by Common):** `sha256 → base32(RFC 4648) → first 6 chars`
  (= first 30 bits; base32-encode then take 6 chars — no bit-masking/endianness to
  get wrong). Implemented against `System.Security.Cryptography`, so it assumes a
  CLR (sync-sha256) host — true today; a self-host-to-JS build overrides at the
  injection point below.
- **Injection point:** a field on the JS emit context/env already threaded to
  `JsExternalMembers.mangledName`, set once at driver entry — not a new parameter
  through every call site.

## Implementation touch points

- `FrozenType` canonical serializer + discriminant builder in `Codegen.Common`,
  with `hash` injected (above). Backend-agnostic; CLR doesn't consume it today.
- The locked well-known leaf set + readable/hash path selection (the arity-1 rule),
  all in `Codegen.Common`. (`FTTypar` renders only in the hashed serialization —
  above — never in a readable token, since a type-variable param is not well-known
  and so always hashes.)
- `JsExternalMembers.mangledName` — extend to take the member's `FrozenType`
  signature and its overload-set context, call the shared builder, and append the
  discriminant when the name is overloaded. Base-name `$`-escaping lives here.
- Emit-time collision assertion over each type's overload sets (the correctness
  backstop; makes the hash width low-stakes).
- First consumer: the `IFormatSink.Child` typed overloads — add the overloads to
  the interface, the CLR emit side (`sinkChild` picks the typed handle by field
  `FrozenType`, else boxes to `Child(obj)`), and the runtime sink impls in
  `structural-printer.clr.fs`.
