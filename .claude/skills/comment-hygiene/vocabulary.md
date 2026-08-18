# Retired vocabulary

The H18 word list with the senses each word was covering and the term that replaced it. Every
replacement is a word this codebase already used for that concept; none is a coinage. Load this
when writing a name or a comment, and when triaging an H18 grep hit.

The general test needs no list: **a term appearing across three unrelated subsystems is either
genuinely universal or overloaded, and there are very few genuinely universal terms.**

Two diagnostics identify the mode before any renaming. The word is a metaphor rather than a term of
art (`holder`, `spine`, `drain`, `face`, `harvest`), so a picture accepts any concept that fits
it and accretes. Or the word *is* a term of art, for something else (`head`, `binder`,
`receiver`, `arrow`, `flow`, `leaf`, `tail`), which is worse, because a reader who knows the
term is actively misled rather than merely uninformed.

## Where the structural vocabulary is allowed

In the lexer and parser, structural-descriptive terms are fine and grammar-production names win:
`headBindingPattern`, `parseHead`, `PatHeadOperatorParser` mirror `pars.fsy` and stay. In
SemanticAnalysis and the backends the lexical terminology disappears, and the name states the
semantic role instead.

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

Avoid `callee` unless caller and callee are both in scope and the relationship is the point. Do
not reach for `binder` for the LHS of a binding; it collides with the monadic sense below.

`headKey` was the defect that exposed the mode: a `NodeKey` for `li.Idents.[0]` in six files and a
`TypeKey` for a type constructor in another.

## `receiver`

Retired outright, with no surviving sense. It is Smalltalk message-passing heritage rather than
compiler vocabulary, and it was doing seven jobs, two of them outright misnomers: a static
access receives nothing, and a trait's support type is a type rather than an expression.

| sense | term |
| --- | --- |
| instance operand of a member access (`r.X`, `r.M(a)`) | `objArg` / `objArgTy`, which is F#-compiler-native |
| qualifying type of a static access | `qualifier`; a "search these type keys" list is a `surface` |
| dotted lead-in during name resolution | `segments` / `anchorIdent` / `prefix` |
| the SRTP trait's type | `supportTy` |
| JS lowering where the instance is an explicit first parameter (`<Type>__<member>`) | "type-prefixed" |
| the IL instance operand | "this pointer", ECMA-335's own wording |
| the type constructed by `new T(args)` | `ctorTy` |

For the instance operand, `objTy` reads as "the SemType for `obj`", `target` collides with the
backend target, and `instance` collides with generic instantiation.

In `System.Console.Out`, `System` is the `anchorIdent` before resolution and a namespace after;
`System.Console` is the `prefix` before and a type after. Those are CST syntax, not a TAST
expression.

## `binder`

Kept for the monadic `'a -> M<'b>` (`XParsec/Combinators.fs`, the `Vesper.Option` and
`Vesper.Result` ports). The word does not apply to `let x = 1`: the LHS construct is a **pattern**, a
name it introduces is a **bound variable** (`BoundVar*`), and `Binding` is the whole `let x = 1`
construct. `Binding` and `binding` were being used for each other's referent on adjacent lines.

"Spells" and "spelling" are fine while referring to source characters as input: a token's glyph,
a CLR metadata name. Once those characters are resolved it is a name, an identifier or a type,
so a method recording one is `SetBoundVarName`.

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

Separating the senses surfaced two falsehoods: a callback named `leaf` that fires at interior
nodes, and a name asserting a stack position its value does not have, since tests composed it
first. A comment forced to negate its own noun is the tell.

## `tail`

Kept for a cons cell (`head :: tail`, `List.Tail`), where no more appropriate word refers to the
actual content of the list, and for tail calls and tail position, which is the term of art used
for its real meaning.

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

Do not name the provider stack's last layer `bcl…`: it reads any referenced .NET assembly rather
than only the base class library. "Tail" was flatly wrong for it too, because it appends one
layer last rather than covering everything after the first.

## `arrow`

Reserved for the ECMAScript and TypeScript concept: an arrow function, TS `=>`. An F# or Vesper
function type, lambda or closure gets `function`, `lambda` or `closure` as the context calls
for, never "arrow type" or "arrow signature" for `'a -> 'b`. Naming the `->` token `arrow` in
the parser is fine, because that is the glyph rather than a concept.

`Vesper.Fun` lowers to a real JS arrow function while staying a nominal interface on CLR, so one
word for both makes the lowering boundary invisible in exactly the comments meant to explain it.

## `contract`

Names one thing: the set of types and function signatures a signature file publishes **after
analysis**. A file at any stage is a *[parsed|analysed] (signature|implementation) file*, never
"a contract", "a contract file" or "a `.fsi` contract".

Legitimate and untouched: `PackageProviders.Contract`, `buildContract*`, `composeContract`,
`compilationContract`, "the contract stack/provider/surface", "a contract
extracted from a real `.fsi`", "the implementation does not answer the contract".

Retired: "contract `.fsi` files in compile order" becomes *signature files in compile order*; "a
contract with no companion implementation" becomes *a signature file with none*; "a `.fs` owes
no contract" becomes *owes no signature file*; "per-contract" becomes *per signature file*. In
``an `.fsi` contract's rebuilt pattern`` the pattern does come from the analysed contract, so
drop the ``.fsi`` rather than renaming.

`PackageSource` types are `SignatureEntry` and `ImplementationEntry` with a `Companion` field,
and `ParsedPackage` fields are `Signatures` and `Implementations`.

## `harvest`

Retired as a wrong metaphor, since code does not grow on its own and is not consumed. One word
had been covering two operations, and the split is the point:

- **extract / extraction** reads declared facts out of a parsed artifact into a table
  (`extractIntrinsicReprsInto`, `extractCompanion`). Already established as `Vesper.Ts.Extractor`,
  `extractTypeSig`, `ExtractCtx`.
- **lift / lifting** turns a type member into a standalone this-first curried inline body
  (`SymbolProviders.liftMemberBody`). It mints new lambda nodes, so it is not extraction.
  `collect` was unavailable, because `collectInlineBodies` is its caller.

Derived nouns: "harvest store" became "inline-body store", "harvest-only decl" became "lift-only
decl", and "member harvest" became "surfaced member set".

## `name` (the verb)

Kept for literally assigning a name: a backend names an unnamed construct after its slot, F#
names operators specially (`+` → `op_Addition`). Every noun use is untouched. CLAUDE.md's
construction 5 retires the verb in doc comments; this is the H18 side of the same rule. The
verb is a term of art for assigning a name, so "an id names the row" sends a reader looking
for a naming step that does not exist. ~108 verb hits over 67 files, unswept.

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

## Naming a new operation

Grep for what the sibling operation is already called before coining anything. Where one word is
doing two jobs, split it rather than picking a third. When a rename cannot find an existing
word, the concept is not modelled, which is a type candidate.
