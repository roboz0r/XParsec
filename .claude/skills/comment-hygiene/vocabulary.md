# Retired vocabulary

The H18 word list: the senses each word was covering and the term that replaced it. Load this
when writing a name or a comment, and when triaging an H18 grep hit.

**The replacement column is the worked project's, not a prescription.** What transfers is the
method — split the senses, then reuse the word YOUR codebase already has — and the "kept for"
verdicts, which mark the term-of-art meaning each word is reserved for in any codebase. The
pure metaphors — `holder`, `spine`, `drain`, `face`, `flow` — were simple substitutions;
their replacements, the two diagnostics, and the three-subsystems test are in `taxonomy.md`'s
H18 section.

**Where structural vocabulary is allowed:** in a lexer or parser, grammar-production names
win (`headBindingPattern`, `parseHead`, `PatHeadOperatorParser` stay); past the parser the
name states the semantic role.

## `head`

Kept for cons (`head :: tail`, `List.head`, `Pat.Cons(head, …)`) and for the top of a stack
where being the top element is the point. `header` and `ahead`/`lookAhead` never matched.

| sense | term |
| --- | --- |
| applied expression of an `App` chain (`f a b` → `f`) | `fn` for params and locals, `Function` for types (`AppliedFunction`, `ExternalFunction`, `RebuiltFn`) |
| first segment of a `LongIdent` (`A.B.C` → `A`) | `anchorIdent` / `anchorKey` / `anchorTy` |
| outermost type constructor of a nominal | `tyCtor` (`tyCtorKey`, `sameTyCtor`) |
| LHS of a binding | `pattern` (the CST field is `Binding.pattern`), or `variable` |
| constructor at the head of an application | `ctorFun` (expression) / `ctorPat` (destructuring) |
| innermost enclosing scope | `current` / `innermost`, with `enclosing` for the rest |
| shallow zonk | `zonkShallow` |

Avoid `callee` unless caller and callee are both in scope and the relationship is the point;
do not reach for `binder` for the LHS of a binding (collides with the monadic sense).
`headKey` exposed the mode: a `NodeKey` for `li.Idents.[0]` in six files and a `TypeKey` for
a type constructor in another.

## `receiver`

Retired outright, no surviving sense: Smalltalk heritage doing seven jobs, two of them
misnomers (a static access receives nothing; a trait's support type is not an expression).

| sense | term |
| --- | --- |
| instance operand of a member access (`r.X`, `r.M(a)`) | `objArg` / `objArgTy`, which is F#-compiler-native |
| qualifying type of a static access | `qualifier`; a "search these type keys" list is a `surface` |
| dotted lead-in during name resolution | `segments` / `anchorIdent` / `prefix` |
| the SRTP trait's candidate types | `supportTys` (a search set; a single candidate in it is a `supportTy`) |
| JS lowering where the instance is an explicit first parameter (`<Type>__<member>`) | "type-prefixed" |
| the IL instance operand | "this pointer", ECMA-335's own wording |
| the type constructed by `new T(args)` | `ctorTy` |

For the instance operand: `objTy` reads as "the SemType for `obj`", `target` collides with
the backend target, `instance` with generic instantiation.

## `binder`

Kept for the monadic `'a -> M<'b>` (parser combinators, the `Option`/`Result` ports). For
`let x = 1`: the LHS construct is a **pattern**, a name it introduces is a **bound variable**
(`BoundVar*`), and `Binding` is the whole construct. "Spells"/"spelling" are fine for source
characters as input; once resolved it is a name, an identifier or a type, so a method
recording one is `SetBoundVarName`.

## `leaf`

Kept for a tree node with no children and for a profiler's leaf or self frame.

| sense | term |
| --- | --- |
| last source in a composed provider stack | see `tail` below |
| record of lookup functions | `…Channels` (`NamedChannels`, `KeyedChannels`, `KeyIndexedChannels`) |
| callback fired at every node | `tryReplace` |
| what a pattern introduces | bound variable |
| a type argument at a position | `argument` / `slot` |
| innermost pattern under wrappers | `namingPat` |
| typar dictionary key | `TyparKey` |
| a scalar `%A` prints atomically | `atom` |

Separating the senses surfaced two falsehoods — a callback named `leaf` that fires at
interior nodes, and a name asserting a stack position its value does not have. A comment
forced to negate its own noun is the tell.

## `tail`

Kept for a cons cell (`head :: tail`, `List.Tail`) and for tail calls and tail position.

| sense | term |
| --- | --- |
| layer-2 provider stack | `PlatformMetadataFactory` / `platformMetadata` / `dotnetMetadata` |
| last segment of a long ident | `last segment`, `memberName`, `ClassAnchorMethod` |
| end of a ctor, a scope or a member list | `END`, `LAST` |
| the rest of a fused operator token | `the rest`, `nextChar` / `nextTok` |
| a function type's result | `Codomain` |
| a `ValueTuple\`8` nesting | `Rest`, .NET's own name |
| inferred typars after the declared ones | `the implicit ones` |
| a dispatcher's final arm | `fallback` |
| the JS trampoline | `TrampolineParams`, `buildTrampolineBody` |

Do not name the provider stack's last layer `bcl…`: it reads any referenced .NET assembly,
not only the base class library.

## `arrow`

Reserved for the ECMAScript/TypeScript concept: an arrow function, TS `=>`. An F# function
type, lambda or closure gets `function`, `lambda` or `closure`, never "arrow type" or "arrow
signature" for `'a -> 'b` — one word for both hides the lowering boundary. Naming the `->`
token `arrow` in the parser is fine: that is the glyph, not a concept.

## `contract`

Names one thing: the set of types and function signatures a signature file publishes **after
analysis**. A file at any stage is a *[parsed|analysed] (signature|implementation) file*,
never "a contract", "a contract file" or "a `.fsi` contract". Legitimate and untouched: the
provider-layer `Contract` types and builders, "the contract stack/provider/surface", "a
contract extracted from a real `.fsi`", "the implementation does not satisfy the contract".

Retired: "contract `.fsi` files in compile order" → *signature files in compile order*; "a
contract with no companion implementation" → *a signature file with none*; "a `.fs` owes no
contract" → *owes no signature file*; "per-contract" → *per signature file*. In ``an `.fsi`
contract's rebuilt pattern`` the pattern does come from the analysed contract, so drop the
``.fsi`` rather than renaming.

## `package`

Names the distributable artifact: a compiled assembly with the supporting parts a consumer
links against — a JS package of modules, runtime assets and an `index.mjs` barrel, or a NuGet
package on CLR (npm's `package.json` sense).

| sense | term |
| --- | --- |
| a directory with a manifest and the sources it lists | *project* (`ReferencedProject`, `ProjectInfo`) |
| the manifest's file list read and parsed | `ParsedManifest` |
| that read resolved to symbol providers | `AnalysedManifest` |
| the unit being compiled | *assembly* (`CompilingAssembly`, `AssemblySources`) |
| a pre-built dependency read as metadata | *reference assembly* (layer 2) |
| the emitted, consumable artifact | *package* (`JsPackage`) |

Four words, four things, and the worked project's own provider layer already drew the line —
"a referenced project beats a referenced assembly".

## `harvest`

Retired as a wrong metaphor — code does not grow and is not consumed. One word covered two
operations: **extract / extraction** reads declared facts out of a parsed artifact into a
table (`IntrinsicBindings.ofImplementationInto`, `extractCompanion`); **lift / lifting** turns a type
member into a standalone this-first curried inline body (`liftMemberBody`) — it mints new
lambda nodes, so it is not extraction (`collect` was unavailable: `collectInlineBodies` is
its caller). Derived nouns: "harvest store" → "inline-body store", "harvest-only decl" →
"lift-only decl", "member harvest" → "surfaced member set".

## `name` (the verb)

Kept for literally assigning a name (`+` → `op_Addition`); every noun use is untouched. The
verb is a term of art for assigning a name, so "an id names the row" sends a reader looking
for a naming step that does not exist. `writing.md`'s construction 5 retires the verb in doc
comments; this is the H18 side of the same rule.

| sense | term |
| --- | --- |
| a key or id identifies its row, node or type | `identifies` / `denotes` |
| an ident resolves to a symbol | `resolves to` |
| a manifest lists a path | `lists` / `references` |
| a token points to a source position | `points to` / `carries` |
| codegen hardcodes an identity the providers omit | `hardcodes` / `spells` |
| an emitted wrapper binds a value | `binds` |
| a diagnostic refers to a thing by a phrase | `refers to it as` / `calls` |

The negated form (`names no type`) is H17, and the H17 entry sends its positive form here.

## `answer` (the verb)

Retired outright — a lookup, key or claim answers nothing. Repo-wide residue exists; cut it
wherever a sweep touches the file.

| sense | term |
| --- | --- |
| a fact obtained from sources consulted in order | `X first, then Y`, or `read from X, then Y` |
| a lookup miss maps to a default | `an unresolved key yields false` |
| a forward reference is satisfied | `resolves` |
| a predicate parameter is backed by a source | `is read from` |

## `ride`

Retired outright — a value does not ride anything. `is carried on`, `is stored on`, `arrives
as`; where the point is that the information is an attribute or a list entry, say that
directly (`` `[<Struct>]` is an attribute``, `impls are carried on the shared extension
list`).

## `speak`

Retired outright — a layer, scheme or table speaks nothing. `consumes` / `shares` /
`exchanges` for a data shape; `uses forward slashes` for a format convention.

## `realise`

Retired for constructing a type from its parts: `rebuild` (a destructured nominal back to
its applied type), `instantiate` (a scheme at a use site). The noun senses never occurred.

## `blame` and its idioms

`takes the blame`, `blamed the source`, `stayed silent` — retired outright; diagnostics are
reported, not blamed.

| sense | term |
| --- | --- |
| the token a diagnostic attaches to | `the token the diagnostic is reported at` |
| a producer that already reported | `already diagnosed at its source` |
| a producer that did not report | `no diagnostic reported at its source` |

## `repr`

Retired as an abbreviation. Its main job — the string a type-position `(# "…" #)` binding
records (`"System.Int32"` on CLR, `"number"` on JS, `"!0[]"` for the array constructor) — is
now the `PlatformTypeId` struct wrapper, so the axis is typed rather than a naming
discipline. The full word "representation"/"represents" stays for prose that genuinely means
representation; the F# attribute `CompilationRepresentation` is untouchable.

| sense | term |
| --- | --- |
| the `(# "…" #)` type-position string | `PlatformTypeId` (`typeId`, `tryPlatformTypeId`) |
| a canon paired with what its target binds | *binding* (`IntrinsicBinding`, `IntrinsicBindings`, `IntrinsicBindingInfo`) |
| a canon → type-id map | `IntrinsicTypeMap` (unchanged), `ofBindings` |
| "X reprs to Y" (verb) | *binds to* |
| the declared extern form (opaque/heritable/capability) | `ExternForm` (was `DeclaredRepr`) |
| a lookup by exact metadata-rendered name | `tryMetaType` / `tryMetaTypeAt`, after `typeMetaName` |

Still open, deliberately: `ValRepr` / `ClosureRepr` / `RegionRepr` / `EmittedEnumRepr` and
their prose ("value repr", "IL repr") are the *value-representation* family — a different
concept that a rename here must not conflate. Retire them on their own terms or not at all.

## Naming a new operation

Grep for what the sibling operation is already called before coining anything. Where one word
is doing two jobs, split it rather than picking a third. When a rename cannot find an
existing word, the concept is not modelled — a type candidate.
