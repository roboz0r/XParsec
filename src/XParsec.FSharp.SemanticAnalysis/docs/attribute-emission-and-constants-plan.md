# CLR attribute emission and compile-time constant evaluation — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent — they rot. Constructs and file names only.

---

## 1. Why these are one document

An attribute argument is a constant expression, and putting one in metadata means
evaluating it. `[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct)>]`
cannot be encoded without folding `|||` first. So emission is the consumer that makes
evaluation necessary, and evaluation is the prerequisite that makes emission general.
Either alone buys nothing: folding with no consumer is dead machinery, and emission
restricted to zero-argument attributes is the special case that already exists.

They are NOT one piece of work. Constant evaluation has consumers of its own (§3) and
should land first, on its own merits.

---

## 2. Where it stands

**Attribute emission exists for exactly one attribute.** `Metadata.AddCustomAttribute`
wraps the SRM row, and the `Assembler` drives it once — for `[<IsByRefLike>]`, naming a
constructor the provider resolves and a FIXED blob (prolog `0x0001`, zero named args).
So the metadata plumbing is present and proven; what is absent is anything that turns a
SOURCE attribute into a row, and any encoding of arguments.

**Constant evaluation does not exist.** A compile-time constant is REPRESENTABLE — the
TAST carries one, with integral width as a witness — but nothing folds an expression down
to one. There is no fold pass, no constant-expression evaluator, no arm that turns
`1 <<< 3` into a constant.

**`[<Literal>]` is parsed and not implemented.** The signature grammar carries a
literal's `= expr` tail and the conformance pass reads the `ValLiteral` shape; nothing
downstream elaborates it.

**Attribute ARGUMENTS are read off raw tokens today.** The one argument that changes
compilation — `[<CompilationRepresentation(ModuleSuffix)>]` — is recognised by matching
token text, taking the lexed file and the raw attribute nodes. That is why neither feature
has been needed: no argument has ever had to be a VALUE.

---

## 3. Constant evaluation: three consumers, one feature

Worth stating because the shape of the answer depends on it. This is a general
constant-folding step over the expression tree, not an attribute-shaped helper:

| consumer | what it needs | status |
|---|---|---|
| attribute arguments | enum flag combination (`A ||| B`), the first real one | blocked on this |
| `[<Literal>]` constants | integer/string arithmetic (`1 <<< 3`) | parsed, unimplemented |
| optional / default parameter values | already store a compile-time constant | works for literals only |

The third is the tell: the codebase already has a notion of "a constant that must be known
at compile time", reached today only by writing a literal. Folding widens the door the
other two walk through.

---

## 4. What emission needs beyond the row that exists

- **A driver** — which source attributes become rows, on which entities (type, member,
  parameter, assembly). Today: one hardcoded call site.
- **Constructor selection** — an attribute class has constructors, and the row names one.
  Picking it from the written argument list is overload resolution on the attribute type.
- **Blob encoding** — fixed args by type, named args (property vs field), enums by their
  underlying type, strings, arrays. The existing blob is a constant.
- **Nothing on JS.** Attributes erase there; that is decided and recorded in
  `Vesper.Core`'s manifest. This work is CLR-only by construction, which is why it can be
  sequenced without reference to the JS target at all.

### The distinction that has to be made first

Several attributes in the vocabulary are **metadata FLAGS, not rows**: `[<Sealed>]`,
`[<AbstractClass>]` and `[<Struct>]` set `TypeAttributes` bits (and choose a base type),
they do not become `CustomAttribute` entries. Others — `[<CompiledName>]`,
`[<CompilationRepresentation>]`, `[<AutoOpen>]`, `[<RequireQualifiedAccess>]` — are
FRONT-END directives that change names and scoping and need no runtime residue at all
unless BCL interop demands it.

So "emit the attributes" is three different answers, and the split has to be made before
any of it is built:

1. flag-mapped — already the backend's job, mostly already done;
2. front-end directive — no row unless a consumer needs to read it back;
3. genuinely reflective — a row, an encoded blob, and the whole of §4.

Only the third needs this plan. Sizing it starts with sorting the vocabulary into these
three buckets, and the answer may be that group 3 is currently EMPTY — in which case this
work is speculative until something wants to read an attribute at run time.

---

## 5. Sequencing

1. **The attribute-vocabulary tranche lands first** (its own plan): declares the ~11
   undeclared attribute types, routes the load-bearing decoders through resolution, and
   flips an unresolved attribute type to an error. Argument expressions stay syntactic.
2. **Constant evaluation**, on its own merits, with `[<Literal>]` as the proving consumer
   — it is the one with no other blockers.
3. **Attribute-argument resolution and typing**, including enum flag combination. Only
   meaningful once 2 exists.
4. **Emission**, for whatever falls in group 3 above.

Nothing in the JS port waits on any of it.

---

## 6. Open questions

- **What is the goal — interop or round-trip?** If a C# consumer of `Vesper.Core.dll`
  should see the attributes, group 3 is non-empty and the target set is whatever the BCL
  spellings are. If the goal is only that Vesper reads back its own markers, nothing needs
  a row today, because the front end reads them from source.
- **Pass-through for unrecognised attributes?** Once the vocabulary is complete and an
  unresolved attribute is an error, every attribute is known — so the question becomes
  whether a user-DEFINED attribute (not a compiler marker) is emitted. It should be, and
  that is the case that makes group 3 non-empty for real.
- **Where does folding live?** A pass over the TAST, or an evaluator the consumers call?
  The three consumers in §3 are reached at different stages, which argues for a shared
  evaluator over a pass that rewrites the tree.
