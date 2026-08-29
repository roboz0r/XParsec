# Codegen.Clr.Tests follow-ups — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent unless they pin a specific declaration — they rot.

Raised by the comment-hygiene sweep of `test/XParsec.FSharp.Codegen.Clr.Tests`
(78 files excluding `data/`, baseline 16952 code / 5245 comment, 3.2:1, 786 blocks of 3+
lines, longest 35). `data/` is off-limits to the sweep: those files are F# source fed to the
compiler under test, so editing a comment there changes a test input.

Reading every comment against the code it claims to describe turns up work the sweep itself
cannot do, because it changes code:

- **Part A — defects and duplication.** Wrong assertions, tests that pass on the wrong thing,
  helpers open-coded until the comment explaining them is load-bearing.
- **Part B — prose that should be a type.** A comment that was genuinely load-bearing and
  long, where the durable fix makes the sentence unnecessary. Each names the comment it
  would delete — that naming is the acceptance test.

**The sweep is complete.** All 78 files (excluding `data/` and the empty `Program.fs`) have
been swept, in four batches, each of three agents, with every batch audited against the code it
claims to describe. Code 16952 lines unchanged throughout; comments 5245 → 2326, so 3.2:1 →
7.3:1. No comment block anywhere exceeds three lines, against a starting maximum of 35.

H19 (the em-dash causal hedge) postdates batches 1 and 2, so those 18 files took a separate
single-rule pass afterwards: 174 em-dash comment lines → 16, all 16 kept as the sanctioned
`` `literal` — gloss `` shape with the literal on the left. Project-wide 29 remain, the balance
being batch 3 and 4 files that had the rule from the start.

That pass is the strongest argument for the rule. Naming the connective is a verification step,
and forcing it falsified five separate claims that had survived a full sweep *and* a review —
including a `Verbatim` `HoleKind` that does not exist and a `withCore` sentence asserting the
opposite of its own guard. See A57.

---

# Part A — code defects and duplication

## A1. A test asserts a property it does not check: the two-site lambda allocation count

`StructSeqTests.fs`, test `the same non-capturing lambda at two sites allocates once`. The
fixture `data/NonCapturingLambdaTwoSites.fs` applies the same lambda text twice:

```fsharp
let a = apply (fun x -> x + 1) 41
let b = apply (fun x -> x + 1) 9
```

The test collects every `<closure>$*` `.cctor` and asserts each `newobj`s exactly once. It
never asserts **how many** closure types were emitted, and never checks that either
construction site `ldsfld`s rather than `newobj`ing. One closure type per syntactic lambda —
two allocations — passes.

The two deleted comments contradicted each other on exactly this point: the header claimed
*"Proven by counting closure types (one)"* (no such count exists) while an inline comment
three lines below said *"Two distinct source lambdas, so two closure types."*

Fix: decide the intended count, assert `List.length closureCctors`, and assert `ldsfld` at
the use sites. The sibling `NonCapturingLambdaCachedSingleton` test already does the
`ldsfld` / no-`newobj` pair on `Main` — copy that shape.

## A2. `peInterfaceImplCount` is assembly-wide; two assertions read it as per-type

The helper sums `GetInterfaceImplementations().Count` over **every** `TypeDefinition` in the
PE. Both call sites, in `ClassTests.fs`, describe the number as belonging to one type:

- `"C carries two InterfaceImpl rows (IEnumerable<int> + IEnumerable)"`
- `"C<'T> carries two InterfaceImpl rows (IEnumerable<'T> + IEnumerable)"`

Both hold only because those two programs emit exactly one interface-implementing type.
Adding another silently changes what the tests assert.

Fix: take a declaring type (`peInterfaceImplCount bytes "C"`), or rename it to say it is
assembly-wide.

## A3. Assertion messages in `ClassTests.fs` name members that do not exist

Test `` `Box<'T>.Member` lowers to Static{Method,Property} (no Elaborate TODO TypeApp) ``.
The source under test declares `Tag`, `Origin`, `ReadTag`, `ReadOrigin`. The two final
assertions read:

```fsharp
Expect.isTrue (staticCalls > 0) "`Box<'T>.Make x` (in Remake) lowered to a TExpr.StaticMethodCall"
Expect.isTrue (staticGets > 0) "`Box<'T>.Tag` (in MakeTagged) lowered to a TExpr.StaticPropertyGet"
```

`Make`, `Remake` and `MakeTagged` appear nowhere in the file. On failure the message sends a
reader hunting for members that do not exist. Fix: name `ReadOrigin` / `ReadTag`.

## A4. A plan-doc milestone label survives inside two assertion messages

```fsharp
Expect.isEmpty closureCctors "no value-struct closure was given a Step-B caching .cctor"
```

Twice in `StructSeqTests.fs`. `"Step-B"` is a milestone label from a plan doc and belongs in
neither code nor a failure message. It is a string literal, hence a code change and out of
scope for a comment sweep. Suggested: drop the two words.

## A5. `parseFile` — the `Result.` qualification is unexplained, and its stated reason was false

The deleted comment claimed `open …SemanticAnalysis` brings `Severity.Error` into scope and
would shadow `Result`'s `Error`. `Severity` is `[<RequireQualifiedAccess; Struct>]`
(`Diagnostics.fs`), and its own comment says RQA is there precisely so that cannot happen.
`TestHelpers.fs` uses bare `Ok`/`Error` at three sites and `Result.`-qualified at five.

Fix: pick one spelling for the file. If some site genuinely needs the qualifier, the
shadowing source should be named at that site.

## A6. The `Vesper.Seq` `Enumerable.Range` workaround may no longer be needed

The deleted rationale claimed the Vesper cons-list declares `IEnumerable<'T>` in its `.fsi`
but does not implement it in `list.fs`, so a list value is not a runtime seq.
`src/Vesper.List/list.fs` has `interface seq<'T> with` (plus an `enumerator<'T>` and a
`Vesper.disposable` impl below it). The claim is stale.

Probe: feed a cons-list value to a `Vesper.Seq` driver. If it works, drop the
`System.Linq.Enumerable.Range` source.

## A7. `peClosureBaseTypeNames` yields `"<none>"` as a silent sentinel

The deleted parenthetical claimed the non-`TypeReference` base handle was *"surfaced rather
than dropped so an unexpected shape fails loudly"*. It is yielded as the string `"<none>"`
into the result list; nothing raises and no test is obliged to look at it. Either `failwithf`
on that arm or accept the sentinel — as written the guard buys nothing.

## A8. Do unions carry interface impls end to end?

A deleted `StructTests.fs` header justified using classes (`MyList` / `MyOpt`) as drivers in
`data/format-sink-frame-stack.fs` on the claim that union interface impls are "unsupported
front-to-back". The front end contradicts it: `FrozenCodecDecls.fs` carries
`TTypeKindG.Union(cases, members, interfaces)`, and `ExternalSymbols.fs` mentions a union's
declared `interface <ty>` impls.

Establish which is true. If unions do carry them, the class drivers in that data file are
unnecessary indirection and the fixture simplifies.

**Settled by batch 4: unions do carry them.** `src/Vesper.List/list.fs` declares
`List<'T>` as a union and implements `interface seq<'T> with` on it directly, and the Set and
List module suites drive that impl and pass. A `SetModuleTests.fs` comment asserting the
opposite (and crediting a `ListSeq` wrapper class that exists nowhere in the tree) has been
deleted. The `data/format-sink-frame-stack.fs` class drivers are therefore unnecessary
indirection, and A6's `Enumerable.Range` workaround rests on the same dead claim.

## A9. Four copies of the "find a method-def by qualified type name" walk

`peMethodIlWhere`, `peMethodsIlWhere`, `peMethodIl` and `peMethodReturnElementType` each
re-declare a local `typeMatches` / `qualifiedOf` rebuilding `Namespace.Name` from `td.Name` +
`td.Namespace`, and three then repeat the identical `RelativeVirtualAddress = 0 → [||]` /
`GetMethodBody → GetILReader → ReadBytes` body.

Candidate: one `peFindMethods : byte[] -> string -> (string -> bool) -> MethodDefinitionHandle seq`
plus one `ilBytesOf`, with the four public helpers as projections. Also deletes the
`qualifiedOf`-vs-`typeMatches` naming split.

Corroborating evidence that these were carved out of one another: `peMethodIlWhere` carried
**two** stacked `///` blocks, the first describing `peMethodIl`, which sat ~100 lines lower
with no doc at all. The sweep relocated it.

## A10. `compileStructuralEngine` and `compileFixtureFile` are the same function

Both force `vesperCoreDll` / `vesperListDll`, take `deps = [Core; List; Comparison]`,
`List.choose OutputPath` over `buildPackage`, `buildContract (List.map srcManifest deps)`,
`tmpDir`, `ProjectInfo.library` with `OutputPath` + `References`, `parseFile`,
`analyseForSelfHost`, a `Diagnostic.errors` check with a near-identical `failwithf`,
`Codegen.compile`, `Codegen.materialise`, then a dedicated non-collectible ALC. They differ
only in source string vs source file, and returning a `Func` delegate vs an `Assembly`.

Candidate: one `compileSelfHostAssembly`, with `compileStructuralEngine` as a
`CreateDelegate` over its result.

## A11. Three copies of "get the `Vesper.Printf` output path or fail"

```fsharp
match ((buildPackage "Vesper.Printf").Value |> snd).OutputPath with
| Some p -> p
| None -> failwith "buildPackage Vesper.Printf produced no OutputPath"
```

Verbatim in `vesperPrintfDll`, `vesperPrintfPath` and `packageAlcPrintf`.

## A12. Three copies of the exit-0 + equality assertion

`runs`, `runsSelfHost` and `runsPackages` have byte-identical bodies after the compile step:
`output.Replace("\r", "").Trim()`, the `exitCode <> 0` `failwithf`, the `actual <> expected`
`failwithf`. `runsPrintf` is a fourth near-copy with a deliberately different trim rule —
leading spaces must survive.

## A13. The IL probes are open-coded ~30 times in `StructSeqTests.fs`

- `constrained.` prefix: `il |> Array.windowed 2 |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)` — 14 sites
- box: `Array.contains 0x8Cuy il` — 12 sites
- closure `.cctor` query: `peMethodNames bytes |> List.filter (fun (ty, m) -> ty.StartsWith "<closure>$" && m = ".cctor")` — 4 sites

and bare opcode bytes `0x73` / `0x7E` / `0x80` / `0x28` appear unnamed throughout.

Named helpers in `TestHelpers.fs` (`ilHasConstrainedPrefix`, `ilHasBox`, `peClosureCctors`,
and an opcode module) would carry those facts in the names.

Would delete: the per-step hex glosses the sweep cut by hand and that grow back whenever a
test is added — `// (4) apply4's body dispatches via constrained. (0xFE 0x16) with NO box.`,
`// The single newobj of the closure lives in the closure type's .cctor, followed by a stsfld
(0x80) into the singleton field.` With named helpers the assertion line reads as the
sentence and the parentheticals have nothing left to explain.

## A14. Two siting fixes the sweep could not make

- `StructSeqTests.fs`: `generic struct seq implements IEnumerable<'T> escape hatch and
  enumerates` sits between the fully-generic map pipeline and the `fold` test. The fold
  test's doc block had drifted onto the escape-hatch test; the sweep moved the prose back.
  Moving the escape-hatch test out of the middle of the seq/fold progression removes the trap
  that caused the drift.
- The `L`-prefix rationale for `data/_layout-core.fs` — `LDoc`/`LText`/`LCat`/`LGroup` are
  named that way to avoid binding the external `Vesper.Doc` / `Vesper.DocGroup` in scope in
  the default test stack — currently lives in `StructTests.fs`, one file away from the names
  it explains. It belongs in the data file, which the sweep must not touch.

## A15. A dead `name` parameter, discarded at 180 call sites

`PrintfHappyPathTests.fs`:

```fsharp
let private runPrints (name: string) (src: string) (expected: string) =
    let exitCode, output = withPrintfAlc (fun alc -> runDriverInAlc alc src)
```

`name` is never referenced in `runPrints` or `runParity`, and `runDriverInAlc` takes no name.
180 call sites each pass a distinct literal (`"PHpHi"`, `"E1Sprintf"`, …) that is discarded.

The sibling `entryPointThrew` does `compileSource "PHpStarThrow" src`, so a per-run assembly
name was the intent. Threading it through is the better fix — it makes the ALC-per-run
assemblies distinguishable. Dropping the parameter means deleting 180 literals.

## A16. `runPrints` exists three ways across the two Printf files

`PrintfPartialTests.runPrints` equals `PrintfHappyPathTests.runParity` minus the dead `name`;
`PrintfHappyPathTests.runPrints` differs only in `Trim()` vs `TrimEnd('\r', '\n')`. One
`TestHelpers` helper parameterised on the trim rule replaces all three.

## A17. `disjointFrom` computes the intersection, not the disjoint part

`InlineFreezeThawTests.fs`:

```fsharp
let disjointFrom (xs: TyVarId list) (ys: TyVarId list) =
    ys |> List.filter (fun y -> xs |> List.exists (fun x -> x = y))
```

It returns the elements of `ys` that **are** in `xs`. Both uses are
`Expect.isEmpty (disjointFrom …)`, so the assertions are correct while reading as the opposite
of what they check. Rename to `overlapWith`, or invert the body.

## A18. `printProgram`'s parameter never varies

`FrozenCacheIncrementalTests.fs` — every call passes `"hello"`. Its deleted doc claimed the
literal varies to model a source edit; the edit test uses inline `"let x = 1"` / `"let x = 2"`
instead, deliberately, because a string edit does not perturb the structural digest. Either
drop the parameter or make it `let private helloProgram`.

## A19. A hand-rolled dedupe sits beside `List.distinct` in the same file

`InlineFreezeThawTests.distinctCells` is an O(n²) accumulate-if-not-seen loop over
`TyVarId list`. `TyVarId` compares structurally — the loop's own predicate is `seen = tv` —
and the same file uses `List.distinct` for the same job in `distinctLeafCount` and in the
`pLeaves` / `cLeaves` bindings.

## A20. The self-`AssemblyRef` guard is copy-pasted at five sites

`CrossFileTests.fs` repeats verbatim:

```fsharp
let refs = peAssemblyRefs bytes
Expect.isFalse
    (refs |> List.contains asmName)
    (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)
```

Each copy previously carried its own two- or three-line comment re-explaining the invariant;
the sweep deleted the comments, and `TestHelpers.expectNoSelfAssemblyRef asmName bytes` would
delete the code.

Would delete: *"a wrong resolution would emit a self-`AssemblyRef` and fault the loader"*,
stated once at the helper instead of five times.

## A21. `CountingStore` is upcast at the binding and downcast at every read

`FrozenCacheIncrementalTests.fs` binds `let store = CountingStore() :> ICacheStore`, then reads
`(store :?> CountingStore).Stores` — five downcasts across four tests. Keep the concrete type
in the binding and upcast at the `compileCached` call.

## A22. `CapturedMutableTests.discover` hand-rolls the production discovery pipeline

~45 lines re-implementing what `Layout.buildFile` and `ModuleClassPlan.create` do: pool the
freeze output, index `FunVerdicts` node-keyed, build the three bound-variable-keyed maps, mint
a `ModuleKey`, then `collectModuleValues` → `staticEligible` → `bridgeStaticFnEscapes` →
`collectStaticFns` → typar map → `discoverClosures`.

Four deleted comment blocks existed only to assert the harness still matched the real path
(*"so this harness cannot drift from the real path"*, *"Mirror `ModuleClassPlan.create`"*).
Nothing enforces any of it.

Fix: expose the discovery prefix as one entry point both `Layout` and the test call.
Acceptance test: after it lands, none of those four sentences needs writing.

## A23. Two test programs are duplicated

- The `mkCounter` counter source and its `1/2/3` assertions are built in both
  `CapturedMutableTests.fs` and `ClosureTests.fs`, one via `compileSource` + `runEntryPoint`,
  the other via `runsLines`, with no observable difference.
- `let mkConst x = let f = fun () -> x in f` is inlined in six `CapturedMutableTests.fs` tests
  plus once in `ClosureTests.fs`. A shared binding or a `data/*.fs` file makes it editable in
  one place.

## A24. More plan-doc milestone labels in assertion strings and test names — DONE

## A25. Two known gaps recorded only as prose

Deleted comments claimed these are uncovered. Neither was re-verified; if they still hold, the
honest form is a pending test named for the gap, not a comment:

- `FunctionTests.fs` — a 3+-argument generic function (`a - b - c`), where params stay typars
  and codegen cannot infer the static-method instantiation for arg 3; and partial application
  of a user multi-arg function (`let inc = add 1`), reported as "cannot encode SemType: TyVar".
- `CrossFileTests.fs` — cross-file union-case **construction** fails upstream with
  `Unresolved identifier` on the local `CtorIndex` while the codegen behind it is in place.
  It mirrors the record-construction test that does exist.

## A26. Is a top-level `let inline` exportable across files?

`InlineFreezeThawTests.fs` documented a module-held `let inline` as the ONLY shape with a
declaring container chain, hence the only exportable identity, and a top-level one as
spliceable within its own file only. `CrossFileTests.fs`'s test *"two files run: file 2 calls
and EXPANDS file 1's top-level bindings"* does exactly that, and passes. The doc was deleted.

Establish whether the publish boundary still refuses any shape, and state the surviving rule
once, somewhere enforceable.

## A27. `asSymbolScheme` passes a hand-written string naming its own call site

`InlineFreezeThawTests.asSymbolScheme` calls
`FrozenTypeBridge.localTyparInTemplate "InlineFreezeThawTests.asSymbolScheme"` — a key that
drifts silently on rename. Already recorded upstream in
`semantic-analysis-followups-plan.md`; noted here as a second consumer of whatever replaces it.

## A28. Every `ChoiceTests.fs` arity row is a positive test

`ChoiceFrontEnd` type-checks one value per arity. A deleted comment on the arity-7 test
described a negative assertion instead: that annotating a `Choice2Of2` constructor as
`Choice`3` is a type error. Nothing in the file asserts that a wrong-arity annotation is
rejected, so the comment was describing a test that does not exist.

Fix: add the negative case the comment claimed, or accept that arity mismatch is unpinned.

## A29. `mapi` and `iteri` are tested on a source where index equals element

`ArrayModuleTests.fs`, `mapi feeds the index to the function` and `iteri pairs each element
with its index`. Both build `Array.init 3 (fun i -> i)` and both lambdas discard the element
(`fun i -> fun x -> i`). Swapping index and element, or passing garbage as the element, keeps
both green.

Fix: build with `Array.init 3 (fun i -> i + 10)` and consume both arguments.

## A30. `Result.defaultWith` is tested with a recovery that discards the error

`ResultTests.fs` — the recovery is `fun e -> -1`. Threading the error value into the recovery
is the whole difference from `defaultValue`, and nothing checks it.

## A31. The four reflection suites re-declare one unboxing preamble

`asBool` / `asInt` are declared identically (`fun (o: obj) -> o :?> _`) in `OptionTests`,
`ResultTests` and `ListModuleTests`, and the surrounding scaffolding is near-identical in all
four module suites: a `Lazy<Assembly>` from `buildPackage`, a `Lazy<Type>` via
`GetType(…).MakeGenericType(…)`, `GetMethod(name).Invoke(null, args)` case-factory wrappers,
and an `instanceGet`. One helper over a `Lazy<Type>` (case factory / instance get / unbox)
removes roughly 40 lines.

## A32. Four empty tests exist only as cross-references

`test "higher-order combinators covered by OptionModuleCallRuntime (Gap 2 Layer D)" { () }`
and its Result / List / Array twins assert nothing. They also carry the milestone labels of
A24. The surviving comment beside each says the same thing.

## A33. The curried-folder workaround is ceremony resting on a false claim

All the module suites explain that the folder is written `fun s -> fun x -> …` because
`translatePat` does not lower `fun s x -> …`. **The claim is false.** `translateFun`
(`ElaborateExpr.fs`) takes the parameter patterns as an array and curries them into nested
`TExpr.Lambda`, calling `translatePat` on each parameter separately, so a multi-arg lambda
never forms the multi-arg `Pat.Named` the claim depends on. Empirically it compiles and runs:
`ClosureTests.fs` executes `(fun a b -> a * b)` and `(fun x z -> f z x)`, and
`OperatorRoutingTests.fs` runs `fun x y -> x + y`.

`translatePat` does have a catch-all `failwithf` that a lowercase-anchored `Pat.Named` with
argument patterns reaches, which is presumably how the sentence was rationalised. That is a
fact about patterns, not about lambda arity, and nothing routes a lambda into it.

Fix: rewrite the folders in the Seq / Set / List / Array / Option / Result suites as
`fun s x -> …` and delete the sentence everywhere. If any one of them then fails, that failure
is the real constraint and wants a named test.

## A34. `assertRangePartition` uses `""` as its unclaimed-row sentinel

`MetadataStructure.fs`. `owner = Array.create (total + 1) ""`, then `owner.[row] <> ""` is the
overlap check and `owner.[row] = ""` the gap check. The values are rendered type names, and
`nameOf` can return an empty one, so a single malformed `TypeDef` defeats both checks at once
in the helper whose whole job is catching malformed metadata. Wants `string voption` or a
two-case DU.

## A35. `assertTypeMembersMetadata` resolves expectations by rendered name

Same file: `actual |> List.tryFind (fun t -> t.Name = e.Type)`. `nameOf` falls back to the
bare name when a nested-flagged type has no `NestedClass` row, dropping the `Outer+` prefix,
so two rows can render alike and the second is then never checked.

## A36. `DiagnosticTests.fs` uses the program under test as the test name

`-> test src { failsWith fragment src }`. Test-run output lists multi-line programs with
literal `\n` escapes instead of naming what is rejected.

## A37. The `Vesper.int` intrinsic-shape assertion is written three times

The ~12-line `ExternalTypeShape.Intrinsic { Id = { Canon = …; Platform = IntrinsicPlatform.Repr … } }`
match, plus the same two `Expect.equal`s on `RuntimeNames.intKey` and `"System.Int32"`, appears
in `SymbolProviderWiringTests.fs`, `OpsPlatformClrTests.fs` and, varied, `MetadataSymbolsTests.fs`.

## A38. Two files assert the same `Vesper.Seq` fact

`PackageBuildTriage.fs` `Vesper.Seq builds BCL-only` and `PackageBuildTests.fs`
`buildPackage Vesper.Seq builds a BCL-only DLL…` both assert `FSharpCoreDependencies` is empty
for that package.

## A39. The on-disk run sequence is open-coded four times

compile → `materialiseApp` → `runOnDisk` → assert stdout, in `RunnableAppTests.fs` (×2),
`FSharpCoreDepsTests.fs` and `ClrDriverTests.fs`, differing only in source and expected output.
`TestHelpers` has the in-process equivalent (`runsLines`) but no on-disk one.

## A40. Does an unconstrained list literal default to `FSharpList`?

`FSharpCoreDepsTests.fs` pins both halves: `let nums = [1; 2; 3]` with `printfn "%A"` pins
`FSharpList`1` and its `Cons` / `get_Empty`, while the same literal consumed by an `inline sum`
using `List.fold` pins nothing, because the constraint builds a Vesper `List` instead. The two
tests agree, and together they say the *default* list representation is FSharp.Core's.

That cuts against the BCL-only goal the rest of the file pins. Establish whether it is
intended; if it is, the pair wants a comment saying so at the type, not two tests a reader must
reconcile.

## A41. `ControlFlowTests.fs` covers neither `while` nor `for..to`, and its excuse was false

The deleted header claimed both hit the `buildExpr` catch-all with no IL emission yet.
`EmitExpr.fs:66-67` routes `ExprShape.ForTo -> EmitLoops.buildForTo` and
`ExprShape.While -> EmitLoops.buildWhile`; the catch-all covers `TryWith` and `Range` only.
`for i in 1..n` is exercised, but from `StructTests.fs` (`runsDataLines "for-in-range-counted"`).

The header's "when they land, add their rows here" was waiting on something that has already
landed. Add the two rows.

## A42. Is an inline DU match in argument position still broken?

`MatchTests.fs` writes every DU row let-bound. The deleted comment said an inline DU match in
argument position trips a bug in `Elaborate.translateApp`, which exists (`Elaborate/Apply.fs`)
and does carry a `"Unification bug or free TypeVar"` failure. The claim was not verified before
deletion. Either it reproduces, and wants a named pending test, or it does not and the rows
inline.

## A43. `traitBase`'s `StaticOptimization` arm is unreachable

`ArithmeticOperatorTests.fs`. All seven arithmetic bodies in `ops-platform.clr.fs` are bare
trait calls with no `when ^T : …` clause, unlike `(=)` / `(<>)` in the same file. The unwrap
arm is a tripwire nothing states. Delete it, or assert it is never taken.

Also: `ForInTests.fs` builds the `constrained.`-prefix window scan twice, once for presence and
once for absence. Another consumer for the `TestHelpers` helper of A13.

## A44. Two holes in the `ComparisonModuleTests.fs` primitive table

The per-primitive table runs 4 operators × 2 truth values. `byte`, `bool` and `int64` have all
eight rows; `float32` has seven, missing `1.5f > 2.5f` → `false`. The `char` block has six and
delegates its `<` rows to `ComparisonTests.fs`, which carries only `'a' < 'b'` → `true`, so
`char <` in the false direction is asserted in neither file.

## A45. The equality suites re-declare one reflection toolkit five ways

The `BindingFlags` triple `Public ||| Instance ||| DeclaredOnly` is declared five times
(`StructuralEqualityTests.fs` twice, plus `CustomEqualityComparisonDispatchTests.fs`,
`EqualityAttributeTests.fs`, `StructuralComparisonTests.fs`). The same accessors carry three
names apiece: `equalsMethod` / `equalsObjMethod` / `equalsObj`, `typedEqualsMethod` /
`typedEquals`, `hashMethod` / `getHash`. `let errors (tast: TastFile) = tast.Diagnostics |>
Diagnostic.errors` appears verbatim in three of the seven files.

## A46. `StructuralComparisonTests.fs` rebuilds two fixtures verbatim

`String.concat "\n" [ "[<StructuralComparison>]"; "type Holder = { N: int }"; "let h = { N = 0 }" ]`
three times, and a `type Pair = { X: int; Y: int }` opt-in fixture four times.

Those fixture strings also spell `Holder`, a retired term. They are string literals, so the
sweep could not touch them; `Container` is the codebase's replacement, though any neutral name
serves a fixture.

## A47. The data-type suites re-declare four more shared helpers

- `let private lines xs = String.concat "\n" xs` in both `ListTests.fs` and `UnionTests.fs`,
  with `RecordTests.fs`, `EnumTests.fs` and `LocalModuleTests.fs` inlining the same
  `String.concat "\n" [...]`. One `TestHelpers` binding removes five sites.
- `declaredInstance` again, twice inside `RecordTests.fs` alone (`monoTests`, `genericTests`).
  Same helper as A45, which counts five more.
- `TupleTests.fs` declares `invokeIntFn` byte-identically in `destructureTests` and
  `lambdaParamTests`, differing only in the assembly-name literal.
- `let prelude = "open Vesper.Collections\n"` three times across `SeqModuleTests.fs` (twice)
  and `SetModuleTests.fs`.

## A48. `RecordTests.fs` and `UnionTests.fs` hold the same test written twice

The `IRank` interface fixture, and the
`ty.GetInterfaces() |> Array.map (fun i -> i.Name) |> Set.ofArray` + `Contains "IRank"` pair,
appear four times across the two files. Their interface-impl and coexists-with-`IEquatable`
tests are structurally identical.

## A49. `RecordTests.fs` compiles the same program eight times

`type Point = { X: int; Y: int }` with `let p = { X = 0; Y = 0 }` is recompiled under four
assembly names (`RecMeta`, `RecNoDep`, `RecEqMeta`, `RecEqValue`), and `type Box<'T> = { Value: 'T }`
under four more. Eight compile-and-load cycles where two shared artifacts would serve.

## A50. An assertion message names the inverse of the function under test

`ConformanceRoundTripByteIdentityTests.fs`: `"toPools (ofPools frozen) emits a structurally
identical assembly"`. The helper runs `TastPools.rePool frozen (TastUnpool.ofPools frozen)`,
and `TestHelpers.fs` says `rePool` correctly one screen away.

`toPools` is not a dangling name — it exists, and goes the other way
(`TastFile -> FrozenPools`). A reader who looks it up finds a real function that does the
opposite of what the test does, so the message misleads more than a typo would.

## A51. A guard's failure path performs the access it guards

`StaticOptimizationTests.fs`, first test:

```fsharp
if clauses.Length > 0 && clauses.[0].Constraints.Length = 1 then …
else failtestf "unexpected first clause: %A" clauses.[0]
```

On an empty `clauses` the `else` indexes `clauses.[0]` and throws `IndexOutOfRangeException`
instead of failing the test. The guard is also dead: `Expect.equal clauses.Length 3` two lines
above already aborts on an empty list.

## A52. `ctx` is bound and discarded at two `ExternalMemberTests.fs` sites

Two tests write `let ctx, tast = analyseWithCtx provider …` and read only `tast`; neither calls
`Unification.zonk`. Both want `analyseWith`. The other three `analyseWithCtx` sites do use
`ctx.Store`.

## A53. `ConformanceTyparsTests.fs` applies two strictnesses to one check

The `list.fs` test asserts `Expect.isEmpty tast.Residue.Diagnostics` — every severity. The
`formatter.clr.fs` test asserts emptiness of `tast.Residue.Diagnostics |> Diagnostic.errors`
— errors only. One of the two is wrong about what "analyses cleanly" means.

## A54. Four `ExternalMemberTests.fs` tests are one test with a different source string

The `EqualityComparer` and short-name-under-`open` pairs run identical zonk checks and
identical `SymbolKey.Member` assertions, differing only in the source and in `EqList [ … ]`
versus `EqArray.tryFind` extraction; the two "key equals the provider's resolved key" tests
differ only in the source. Roughly 50 lines. A helper parameterised by source collapses all
four.

## A55. Does a bare constructor application resolve by written arity?

`ArityOverloadedClassTests.fs` holds both sides. Its deleted header called a bare `Box(…)`
across two arities an unresolved hazard, and the `Box` test writes explicit
`new Box<…>(…)` accordingly — while the last test in the same file writes a bare `Foo(1, 2)`
against a class `Foo<'A,'B>` and a union `Foo<'A>` and expects it to type-check.

Either bare application does resolve by written arity, and the `new` spelling is unnecessary,
or the last test passes for some other reason. The surviving comment is phrased neutrally
pending a verdict.

## A56. 45 em-dashes remain in assertion messages and test names — DONE

## A57. Five claims the H19 pass falsified

Recorded because each survived a full comment sweep and a review before a punctuation pass
caught it — the hedge was load-bearing in the sense that it hid the joint where the claim
failed.

- `TestHelpers.withCore` — "calls resolve — never into the package that DEFINES the type".
  Resolution goes exactly there; the guard is `project.AssemblyName = asmName` → skip. Verified
  against the `ensure` body.
- `PrintfHappyPathTests` `%.2M` — "a literal precision is inert — the plain `Verbatim` decimal
  hole". `HoleKind` has no `Verbatim` case (`Formatted`, `BoolText`, `Unsigned`, `Octal`,
  `UnsignedZeroPad`, …); `Verbatim` exists only as `StringKind.VerbatimString` in the parser.
  The test two lines below asserts `HoleKind.Formatted`.
- `PrintfHappyPathTests` `% A` — "cannot be the oracle — XParsec admits it and the expected
  value is pinned literally". The tests below call `runParity … (sprintf "%A" 42)`; F#'s `%A`
  is the oracle.
- `StructSeqTests` — "the slot is minted on `IBox`1<!T>` — a generic parameter". Apposition
  asserts the slot is a generic parameter; the generic parameter is its argument.
- `StructTests` — "application-shaped — `Some (Some 1)`, but a bare `Some 1`". The clause after
  "but" had no predicate at all.

A sixth shape appeared eight times: a dash written immediately before the connective the author
already had (`— for`, `— and`, `— but`, `— hence`, `— otherwise`). Each was fixed by deleting
the mark alone, which is the tell that the mark was reflex rather than meaning.

## A58. A retired negation survives in a test name — DONE

---

# Part B — prose that should be a type

## B1. ALC placement policy wants to be a type, not six comment blocks

Six blocks in `TestHelpers.fs` (`PackageLoadContext`, the `buildPackage` Printf special case,
`vesperPrintfDll`, the ALC-separable header, `PrintfLoadContext`, `packageAlcPrintf`, plus
the `vesperStructuralPrintMethod` prologue) exist to restate one invariant: *every
participant in a run must resolve `Vesper.Core` to the same loaded assembly*.

The file has **five** distinct load contexts — Default, `packageAlc`, a throwaway per
contract-only package, `PrintfLoadContext`, and a per-engine/per-fixture non-collectible one
— and the rule for which one a given DLL belongs in lives only in prose, with
`manifest.Name = "Vesper.Printf"` as an inline string test inside `buildPackage`.

Candidate: a small module owning the placement decision — a
`Placement = Default | PackageRegistry | Throwaway | Dedicated of name` returned by one
function keyed on package name.

Would delete: *"registering a second copy here would bind a driver's `Vesper.Formatter` to
the wrong one"*, and the five sibling blocks restating it.

## B2. The `name$<offset>` emitted-name scheme is re-implemented as a string prefix test

`ClassTests.fs` opens with two private helpers that exist only to re-implement the emitted
metadata-name scheme:

```fsharp
let private topLevelNameMatches (source: string) (emitted: string) : bool =
    emitted = source || emitted.StartsWith(source + "$")
```

The scheme (`x` → `x$<offset>`, so a shadowed `let x` stays a distinct row) is spelled once
in production and again as a `StartsWith` here. A named type for an emitted top-level name,
constructible from a source name and comparable, removes both.

Would delete: the surviving 2-line doc on `topLevelNameMatches`, and the need for any comment
explaining why the match is a prefix match.

## ~~B3. `triple` puns the `%A` format and alignment slots~~

Done 2026-08-23, by deleting the projection the tuple came from rather than by reshaping the
test helper. `ClrHoleFormat.toDotNetFormat` returned `HoleKind * string option * Alignment`
whose third slot held a total WIDTH for the zero-pad kinds and a field alignment otherwise;
it now returns `ClrHoleFormat.HoleCall`, a DU whose payload is the operands each
`Vesper.Formatter` member takes. `triple` and `kindOf` are gone with it: a non-`%A` hole is
asserted as one `callOf hole` value, and `%A` reads `widthBudgetOf` / `sizeBudgetOf` off its
`PercentA` form, so neither budget passes through a format slot.

## B4. `Closure.Repr` and `IsValueStruct` are two fields whose relationship is prose

Found in `src/` while verifying a test comment. `EmitTypes.fs` documents `Repr` as "the
front-end verdict that a readonly-struct shape is ADMISSIBLE. Necessary but not sufficient:
`IsValueStruct` is the codegen gate." Candidate: one field with an explicit
admissible/taken shape.

This is what let `CapturedMutableTests.fs` carry the false claim that `Repr` "is inert
(emission ignores it)" — `EmitClosures.fs` computes and stores it. A type that made the
gating explicit would have made the comment unwritable.

## B5. Leaf walks return duplicates and every caller dedupes

`InlineFreezeThawTests.localLeavesIn`, `typarLeavesIn` and `semRootsOf` each return a flat list
with duplicates, and every caller pipes it through `List.distinct` / `distinctCells`. Their
docs claimed "first-occurrence pre-order", which was false; the sweep deleted rather than
corrected them.

Candidate: fold the dedupe into the walk, or a `DistinctLeaves` wrapper — either makes the
false doc unwritable and collapses A19 with it.

## B6. `withCore` is a producer protocol enforced only by prose

The same sentence — `printfn` binds `Vesper.Printf` and its deps, so their on-disk paths must
be resolvable references for the bundle to copy them — is cloned at three call sites in the
metadata/driver files. A caller who forgets `withCore` gets a `FileNotFoundException` at run
time, not a compile error.

Candidate: have `ProjectInfo.app` (or `materialiseApp`) derive the reference set from the
artifact's own dependencies. All three copies then delete.

## B7. `thrownBy` requires a source shape it cannot state

`ExceptionTests.fs`. It calls `Array.exactlyOne` on the emitted methods and invokes with
`[| box 0 |]`, so every caller must supply exactly one top-level function taking exactly one
ignored `int`. A caller who adds a second `let` gets an `exactlyOne` failure naming nothing.
Wants a parameter or a builder that carries the shape.

## B8. `raise`'s signature is respelled in prose per test

`ExceptionTests.fs` carried two spellings, `System.Exception -> 'T` and
`'e -> 'a when 'e :> exn`, one test apart. Neither matched `ops-platform.fsi`, which has
`val inline raise: exn: 'TException -> 'T when 'TException :> exn`. The sweep corrected both,
but a signature copied by hand at each site will drift again.

## B9. The synthesised capability co-slots are a list in prose

`RecordTests.fs` spells the set out: non-generic `IEnumerable.GetEnumerator`,
`IEnumerator.Current`, `Reset`. The backend presumably holds the same set as a value. Exposing
it lets the test assert against it, which deletes the comment and makes the set checkable
rather than transcribed.

## B10. `counterSrc` hides its newline in an invisible trailing `""`

`CapabilityMemberAccessTests.fs`. The last element of its `String.concat "\n"` list is an
empty string, which is what supplies the separating newline every
`counterSrc + String.concat "\n" […]` site depends on. Delete the blank and every use site
breaks, with nothing at the definition saying so. A `withCounter : string list -> string`
makes the contract explicit instead of positional.

---

# Verified during the sweep — no action

- `Vesper.Fun`5<a,b,c,d,r>` is genuinely the widest declared arity: `prim-types-min.fsi`
  declares `Fun<'A,'B>` through `Fun<'A,'B,'C,'D,'E>`. The arity-4 test's "widest flat form"
  claim stands.
- `rewriteClosureLeaves` is genuinely absent from `src/`. The comment saying so was true, but
  it is history, so it was deleted rather than kept.
- `vesperCoreDll` compiles Core from the **19** impl files the manifest lists, not the two
  named in the deleted doc. Nothing to fix in code beyond not re-adding a manifest-driven
  enumeration to prose.
- `IlIr` does reject an unbalanced body: `Throw` sets reachability to `ValueNone` and the
  function returns `Result<_, string>`.
- `//#include` is real in 8 data files; the `preamble-*.fs` conformance split cited in a
  `ClassTests.fs` header exists (6 cases).
- The `%A` / `%O` / `fprintf` exclusion in `PrintfPartialTests.fs` holds against the marker
  guard in `InferApp.fs`.
- `EmitStructuralFormat.fs` **does** synthesise the semantic sink calls
  (`BeginRecord; (Field; Child)×n; EndRecord`, `BeginCase; Child×k; EndCase`); a comment
  saying the emitter "does not yet call them" was false and is gone.
- `bridgeStaticFnEscapes` is the real escaping-export mechanism (`EmitClosures.fs`), not the
  `forceExportedStaticFns` a `CrossAssemblyEscapeTests.fs` header credited — that identifier
  is nowhere in the tree.
- `TestHelpers.withCore` adds to `ProjectInfo.References`; it does not touch a load context,
  contrary to a deleted `ClosureTests.fs` claim.
- The `OptionTests.fs` `OptionCtorFrontEnd` header claimed reading a constructed value back is
  gated, so the smoke test exits 0 without inspecting. The next list, `OptionCtorRuntime`,
  asserts `(Some 5).IsSome`, `None.IsNone` and `(Some 5).Value`. Deleted.
- `SeqModuleTests.fs`'s header claimed `truncate` delegates to
  `System.Linq.Enumerable.Take<TSource>`. It returns a hand-written `TruncateSeq<'T>`, and
  `src/Vesper.Seq/seq.fs` uses no LINQ at all. The same file's `toArray` comment claimed
  `ResizeArray` accumulation plus `ToArray()`; the implementation is a doubling `'T[]` buffer
  and one exactly-sized copy, which its own sited comment states correctly.
- `SetModuleTests.fs`'s `toList`/`ofList` comment named a `ListSeq` wrapper class that exists
  nowhere in the tree, justified by "interface impls on union types are not yet supported".
  See A8: `List<'T>` is a union and implements `interface seq<'T>` directly.
- `RecordTests.fs`'s capability comment cited `RecordMember.Member`. `RecordMember` has
  exactly two cases, `Ctor` and `Field`.
- `ComparisonModuleTests.fs`'s header claimed `src/Vesper.Comparison/manifest.toml` has
  `[targets.clr] impl = []`. It lists `comparison.clr.fs`. The manifest's own comment holds the
  true version: every declaration is `inline`, so it compiles to an empty DLL.
- Both file:line citations in `CustomEqualityComparisonDispatchTests.fs` pointed at the wrong
  lines. `(=)` lowers at `ops-platform.clr.fs:40-41` (`:114` is `let inline failwith`), and the
  `<` base clause is `comparison.clr.fs:9` (`:40` is a `>=` byte clause). Both corrected.
  A third instance of the file:line-citation problem already noted at the end of this section.
- `StructuralEqualityTests.fs`'s `genericTests` header named `IEquatable<List<!0>>`. No `List`
  type exists in that file or its fixtures, which declare `Box<'T>` and `Lst<'T>`.
- `MetadataStructure.fs`'s `assertRangePartition` comment named an identifier `claim` that the
  function has never had; the array is `owner`. The failure message says "claimed by", which is
  the likely source of the drift.
- `PackageBuildTests.fs`'s header claimed the file exercises the two proven packages only
  (Core + List). It also has a `Vesper.Seq` test.
- `OperatorRoutingTests.fs`'s header credited `ClrSymbolProviders.inlineBodies` with collecting
  the `(=)` body. No such member: `ClrSymbolProviders` exposes `contractInlineBodies` and its
  two variants, which is what the tests in that file actually call, and the only `inlineBodies`
  in the tree is an unrelated internal in `Codegen.Common/SymbolProviders.fs`.
- Two dangling citations, both deleted: a `ListModuleTests.fs` header pointing at
  `list-min.fs` (`src/Vesper.List/` holds `list.fs` and `list.fsi`, and no `list-min.fs`
  exists anywhere in the tree), and `OptionTests.fs` + `ResultTests.fs` both citing
  `vesper-result-handoff.md`, which is in no directory of this repo.

Three comments cited FSharp.Core sources that are not in this repo — `printf.fs:1085`,
`printf.fs:632` vs `:649-657`, `sformat.fs countNodes`. The facts were kept and restated; the
citations cannot be checked by any reader of this repo. If provenance matters for these, the
durable form is an `fsi`-verified assertion or a named test, not a line number in a file
nobody has.

---

# Coverage

None. Batch 4 finished the project.

Per-batch record, all with zero non-comment lines changed and Fantomas clean:

| batch | scope | comments |
|---|---|---|
| 1 | `TestHelpers`, `StructSeq`, `Class`, `Struct` | — |
| 2 | printf & structural format; captured mutables, closures, functions & bindings; freeze/thaw, cross-file & self-host | — |
| 3 | collections; operators & control flow; metadata, IL & driver | 1411 → 677 |
| 4 | external members & conformance; data types & collections; equality, comparison & hash | 1076 → 408 |
| H19 | the 18 batch-1/2 files, single rule | 174 → 16 em-dash lines |

The suite is green at 1452 after batches 3, 4 and the H19 pass.

What a sweep structurally cannot reach is string literals: test names and assertion messages.
A24, A56 and A58 covered that surface and are done; the suite is green at 1452 throughout,
Fantomas clean, with no non-string line changed. The 31 em-dashes left in the project are all
the sanctioned `` `literal` — gloss `` form.

The one string-literal item still open is the `Holder` fixture names of A46. Those are source
text fed to the compiler under test rather than prose, so they belong with that item's dedupe
rather than with a rename pass.
