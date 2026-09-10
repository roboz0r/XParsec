# Class preamble lowering in the TAST plan

Move the lowering decision for a class's primary-ctor parameters and instance `let`s out of the
CLR backend and into semantic analysis. A ctor param or a value `let` becomes a field or a
`.ctor` local; a `let` bound to a lambda becomes a method of the class instead, which is what
fsc emits. A `.ctor`-local reference in the preamble is an ordinary bound variable.

Tranche A (steps 1–4) is the field/local decision. Tranche B (steps 5–7) is the method
lowering, and depends on tranche A for the `BoundVar` key on `TClassLetG`.

## Root cause

`Elaborate/ClassMembers.fs` (`instanceFieldRewrite`) holds each ctor param's
`DeclSite.BoundVar` and each preamble `let`'s `DeclKey`, discards them, and rewrites every
reference, in member bodies AND in the preamble's own frame, to
`FieldGet(Var this, name)` / `FieldSet(Var this, name, …)`. `TClassLetG` carries no key at all.

Commit `1084b0d5` then has the CLR backend reconstruct what was discarded.
`LayoutNodes.ctorLocalNames` walks the whole class by string name to decide which names are
referenced only from the `.ctor` frame through `this`, and threads the result through
`ClassDecl.CtorParams` / `ClassDecl.InstancePreamble` storage tags into six consumers. The
`.ctor` emitter then needs a name-keyed `EmitEnv.CtorLocals` dictionary, a `CtorLocal`
location DU (`Slot | Arg`), `tryCtorLocal`, `(|CtorLocalAccess|_|)`, `ILInstr.Ldarga`, and
three `Slot | Arg` opcode-pair branches in `EmitMember` (`pushObjArgAsThis`,
`buildFieldGet`, `buildAssignment`) to turn `this.x` back into a local access.

That is the CLAUDE.md rule "a stage that discards an intermediate and makes consumers
re-derive it is wrong". The JS backend will need the same decision (`EmitJs.fs`
`emitInstancePreamble` stores every `let` as `this.<name>`), and a second copy of the walk is
the point at which the wrong layer becomes entrenched.

The same discard blocks tranche B outright. A method lowering has to rewrite a reference's
APPLICATION SPINE, and after `instanceFieldRewrite` there is no key at the head of the spine to
match on.

Name keying is also fragile. A preamble may shadow (`let a = …` then `let a = a + 1`); every
name-keyed table in the current design collapses both bindings onto one entry, and
`FieldKey.ClassLetField(td.Key, name)` already collides for the field-backed case.

## Target shape

Semantic analysis decides the lowering once, with keys, and the TAST states it.

```fsharp
/// Where a primary-ctor parameter or a value `let` lives after the primary constructor runs.
[<RequireQualifiedAccess>]
type ClassValueStorage =
    /// A backing field on the instance, stored by the primary constructor.
    | Field
    /// A local of the primary constructor. Every reference is inside the constructor's own
    /// frame and is a `Var` of the binding's key.
    | CtorLocal

/// How a class preamble `let` is emitted.
[<RequireQualifiedAccess>]
type ClassLetLowering =
    | Value of ClassValueStorage
    /// A method of the class: instance for a `let`, static for a `static let`. `Init` is a
    /// lambda chain, the method's parameters are that chain's source groups, and its own
    /// generic parameters are the binding's generalised typars.
    | Method

type TCtorParamG<'ty, 'id> =
    { BoundVar: BoundVarKeyG<'id>; Name: string; Type: 'ty; Storage: ClassValueStorage }

type TClassLetG<'ty, 'id, 'body> =
    { BoundVar: BoundVarKeyG<'id>; Name: string; Type: 'ty; IsMutable: bool
      Init: 'body; Lowering: ClassLetLowering }
```

A ctor param's value arrives as an argument, so it is never a `Method`; the two types say so
rather than a comment saying so.

`TClassG.CtorParams` becomes `Block<TCtorParamG<'ty, 'id>>` (it currently borrows
`TRecordFieldG`). `TBaseCtorCallG.CtorParams` already carries the same keys for base args and
becomes redundant with `TClassG.CtorParams`; delete it in the same change.

`TPreambleEntryG` gains the `'id` parameter through `TClassLetG`. A `static let` value is always
`Value Field` (a `.cctor` has no local of interest); it keeps the same record so the static and
instance preambles stay one type.

### Reference lowering

In `classDeclaringType.lowerBody` (member bodies, interface impls) a `Value` reference rewrites
as it does today: `FieldGet` / `FieldSet` on `this`.

In the instance preamble (`TypeDecls.fs:517`) the rewrite applies only to `Value Field` keys. A
reference to a `Value CtorLocal` key stays `TExpr.Var k`, and a `let mutable` local's
`c <- c + 1` stays `TExpr.Assignment(Var k, …)`, which the existing local-mutable machinery
in both backends already handles.

A reference to a `Method` key stays `TExpr.Var k` EVERYWHERE, member bodies included. That is a
`Var` with no local slot behind it, which is new: the backend resolves it against the enclosing
class's preamble, and the object argument is the enclosing body's own `this` (a member's
`ThisKey`, or `TClassG.ThisKey` inside the preamble). Tranche B's step 6 states what each
backend does with it.

### The storage decision

The method decision runs FIRST, because a method's reads are forced to `Field` (see below).
Then a ctor param or a value `let` is `Field` when referenced by any of:

- a member body or interface-impl body;
- the body of a `Method`-lowered `let`;
- a lambda anywhere in the instance preamble (see assumption 2);
- a secondary constructor's explicit field-init block (`TSecondaryCtorBodyG.ExplicitFieldInit`);
- a `FieldGet` / `FieldSet` through any object argument other than `this` (a preamble reading
  `other.x` on another instance of the class).

Otherwise it is `CtorLocal`. The walk runs over `TExpr` before the rewrite, collects referenced
`NodeKey`s, and needs no `inFrame` / `viaThis` tracking because a member-body reference is by
construction outside the frame. It lives beside `instanceFieldRewrite` in
`Elaborate/ClassMembers.fs`.

## Tranche B: preamble functions as methods

`let get () = x` is a method, not storage. The CLR backend today allocates a `<closure>$0` in
the `.ctor` and holds it in a `Fun<ValueTuple, T>` field
(`goldens/preamble-generic.clr.cs`), and the JS backend allocates an arrow per instance
(`goldens/preamble-generic.js`). The `preamble-let-rec` golden pays for it twice over: `fact`
recurses through `capture0.fact.Invoke(k - 1)`, a field load plus an interface call per step
where a method is a direct `call instance`.

This is orthogonal to `function-representation-plan.md`, which makes closures cheaper. A
preamble function needs no closure at all.

### What fsc does

Verified by declaring each shape in a `dotnet fsi` script and reflecting over the compiled type
with `GetFields` / `GetMethods` under `BindingFlags.NonPublic ||| DeclaredOnly`, which shows the
emitted names and visibility.

| preamble binding | emitted as |
|---|---|
| `let get () = x`, `let f x = …`, `let f = fun x -> …` | instance method, `assembly` visibility, source name |
| `let rec even x = … and odd x = …` | two methods, direct mutual calls |
| `let id2 x = x` | generic method `id2<'a>`, generalised whether or not anything calls it |
| `static let f x = x + n` | static method, `assembly`; `n` stays a static field |
| `let f : int -> int = inc`, `let add = (+) n` | field of function type |
| `let mutable f = fun x -> x` | mutable field of function type |
| a method-lowered `let` that also escapes (`member this.Get() = f`) | the method, plus a closure at the escape site |

The discriminator is whether the initialiser is a lambda, which `TClassLetG.Init` states
directly: `let f = fun x -> …` is a method and `let f = (+) n` is a field, at the same
function type. Three consequences:

- **A method's reads are fields.** `let k = n * 2 + 1` beside `let f () = k` makes `k` a field,
  and a `let mutable c` read by a preamble function is a field for the same reason. Hence the
  ordering in the storage decision above.
- **Names collide.** `let f` shadowing `let f` emits `f@4` and `f`. `let X` beside
  `member this.X` is FS0905, a hard error, so no mangling is needed for that case — a
  diagnostic is.
- **An unreferenced function still emits.** `let unused x = x + n` is a method and `n` is a
  field, so the emission is driven by the binding, not by a use.

### Typars

A `Method`-lowered `let` carries its own generic parameters, alongside the class's. Verified
across four shapes:

| preamble binding | emitted as |
|---|---|
| `let f = fun x -> x`, never called | `f<'a>('a) : 'a` |
| `let f = fun x -> x`, called at `int` only | `f<'a>('a) : 'a`, the member calling it at `int` |
| `let f x = x`, called at `int` and at `string` | one `f<'a>('a) : 'a`, two instantiations |
| `let wrap x = (x, v)` on `type P<'T>(v: 'T)` | `wrap<'b>('b) : Tuple<'b,'T>` — the method's own typar beside the class's |

So generalisation is the ordinary one, and `MethodTypars` is the `LocalFunction` scheme's
quantified set remapped to `TyparScope.Member` of the emitted method
(`typar-scope.md`: `Type > Member > LocalFunction*`). A class typar in the body stays
`TyparScope.Type` and is untouched by the remap.

`mutable` blocks generalisation, which is what separates the two lambda-valued forms: an
unconstrained `let mutable f = fun x -> x` defaults its typar to `obj` and stores an
`FSharpFunc<obj, obj>` in a settable field, with no value-restriction error reported.

### Shape: the TAST states the verdict, each backend emits its own row

`ClassLetLowering.Method` is the whole of the shared representation. The TAST gains no node for
a preamble call and no synthesised member; each backend lowers `Var k` against the class it is
already emitting, as each already lowers a top-level `let f x = …` to a method of its own
choosing.

The alternative considered was a synthesised private `TTypeMemberG` appended to
`TClassG.Members`, with references lowered to `TExpr.MethodCall(Var this, …)` in Elaborate. It
would reuse both backends' member paths wholesale, and the surface cost is smaller than it
first appears: `FrozenSignature.fs:125` already drops an `Accessibility.Private` member from the
published surface, so conformance and a `.fsi` would never see one. It was rejected because it
puts a binding that no source declares into the member list that `MemberKey` overload
resolution, the frozen codec and every member consumer read, to save work in two backends that
already own a function-to-method lowering.

### CLR emission

`MethodKey.PreambleFn`, keyed by the class, which of its two preambles, and the index within
that preamble — indexed rather than named, because names shadow. The emitted name is the source
name, suffixed with the index where an earlier preamble method in the same class shares it.

The row is an ordinary method of the class, so the declaring type's typars are already in scope
and the encoder path is the member one, NOT `GenericStaticFnSignature`. Parameters come from
`CompiledFns.compileValue` / `FlatParams` (`Codegen.Common/CompiledFns.fs:125`), which peels a
lambda chain to flat parameters and is backend-neutral. A generalised binding adds
`MethodTypars` on the row and makes each call site a `MethodSpec`, which is the same handling a
declared `member this.Map<'C>` already gets.

A non-saturated reference eta-expands to a closure over a saturated call, exactly as
`EmitBridges.bridgeReferences` (`EmitBridges.fs:14`) does for `MethodKey.StaticFn`. It is not
the same function, because the eta body needs the object argument spliced in and that argument
is the enclosing body's own `this`: the pass runs per body, with that body's `this` key, so the
closure captures `this` by the ordinary capture path. Read `buildEta` before writing it and keep
the two shaped alike; if a third caller appears, that is the point to abstract them.

### JS emission

A JS member is already a module-level function taking the instance first
(`const Cell__K = (_s0) => _s0.k`), so a preamble method is the same shape:
`const Cell__get = (_s0) => _s0.x`, with the pool naming it and the same shadowing
disambiguation. A non-saturated reference becomes `(...args) => Cell__get(_s0, ...args)`.

## Semantic assumptions to confirm

1. **fsc parity on the local rule.** F# backs a `let` with a field only when a member reads
   it; otherwise it is a constructor local. Confirm with `dotnet fsi` that `ildasm`-visible
   fields match for the three tests added in `ClassTests.fs` (`PreambleCtorLocals`,
   `PreambleLambdaRead`, `PreambleStructLocal`).
2. **Lambdas in the preamble.** Commit `1084b0d5` treats a `let` read inside a preamble lambda
   as `Field`, because the lambda captures `this`. With keys the alternative is available: the
   lambda captures the local by the ordinary closure path and the `let` stays `CtorLocal`. For
   an immutable `let` the two are observationally identical. For a `let mutable` read by a
   lambda, F# permits the capture only because the storage IS a field, so that case must stay
   `Field`. The plan keeps "any lambda reference ⇒ `Field`" as the simpler rule unless the user
   prefers per-binding precision. Under tranche B a lambda BOUND by a preamble `let` is a
   method and the same verdict follows from the method rule, so this assumption narrows to
   anonymous lambdas.
3. **Struct classes.** A `[<Struct>]` class's `CtorLocal` param is read in place (`ldarg` /
   `ldarga`), as the CLR tests pin today. No change intended. A `[<Struct>]` class's preamble
   method takes `this` by reference; confirm the member path already handles that, since a
   declared `member` on the same struct does.
4. **Shadowing.** Confirm whether semantic analysis admits a shadowed preamble `let` today. If it
   does, add a test that a member reading the second `a` leaves the first `a` a `CtorLocal`,
   which only a key-based decision can pass.
## Semantic decisions taken

These were open questions on the first draft and are settled; they are recorded because the
steps below assume them.

- **Generalised preamble functions are generic methods.** A `Method` lowering carries its own
  `MethodTypars`, remapped from the binding's `LocalFunction` scheme, in every step that admits
  it. No step excludes a generalised binding.
- **`static let` functions are static methods**, `assembly` visibility, covered by the same steps
  as the instance preamble. The only difference is the absent object argument.
- **`let mutable` bound to a lambda is a mutable field.** `Method` requires
  `IsMutable = false`. `mutable` blocks generalisation, so the binding is monomorphic by the
  time the lowering is decided and the field type always encodes.

## Steps

Each step leaves the tree green. Steps 1–3 are one commit boundary; every later step can land
separately.

1. **TAST shape.** Add `ClassValueStorage`, `ClassLetLowering`, `TCtorParamG`, and the
   `BoundVar` / `Lowering` fields on `TClassLetG` in `TastDecl.fs`. Update `TastConvert.fs`
   (`recordField` → `ctorParam`, `preambleEntry` gains the key refile), `FrozenCodecDecls.fs`
   (`writeClassLet` / `readClassLet`, `writeRecordField` for ctor params → a ctor-param
   codec), `FrozenSignature.fs:161`, `PlatformTypes.fs:125`, `NominalDecl.fs:68-70`,
   `TastNodeViews`. Delete `TBaseCtorCallG.CtorParams` and read the keys from
   `TClassG.CtorParams` at `NominalEmit.fs:500`. Every `let` is `Value Field` at this step, so
   nothing downstream changes.
2. **Decide storage in `Elaborate/ClassMembers.fs`.** Add the reference walk, populate
   `Lowering` with the two `Value` cases, and restrict `instanceFieldRewrite` in the preamble to
   `Field` keys. Static preamble lets are `Value Field`. Add a SemanticAnalysis test asserting
   the TAST shape for the three CLR test sources: the lowering per binding, and `Var` versus
   `FieldGet` per reference.
3. **CLR backend consumes the shape.** Delete `LayoutNodes.ctorLocalNames`, the
   `CtorValueStorage` type in `CodegenTypes.fs` (replaced by the TAST's `ClassValueStorage`),
   `EmitEnv.CtorLocals`, `CtorLocal`, `tryCtorLocal`, `(|CtorLocalAccess|_|)`, the three
   `EmitMember` branches, `CtorParamStorage`, `InstancePreambleStep.Local`'s name field, and
   `ILInstr.Ldarga` if no other site needs it. In `buildClassPrimaryCtor` register every ctor
   param key in `args` (the `baseArgParams` loop already does this) and emit a `CtorLocal`
   `let` as an ordinary slot binding keyed by `BoundVar`. The existing `LocalSlot` addressing
   covers the struct-typed local. The CLR tests and goldens from `1084b0d5` must pass unchanged.
4. **JS backend consumes the shape.** In `emitInstancePreamble` a `CtorLocal` `let` becomes a
   `const` / `let` local named by the pool, and a `CtorLocal` param is left as the constructor
   parameter it already is. Add the JS equivalents of the three CLR tests.
5. **Decide `Method` in `Elaborate/ClassMembers.fs`.** A preamble `let`, static or instance,
   whose `Init` peels to ≥ 1 source group and which is not `mutable`, becomes
   `ClassLetLowering.Method`, carrying its `LocalFunction` typars remapped to
   `TyparScope.Member`. Feed its body's references into the field walk from step 2 before that
   walk runs, and leave a `Method` reference unrewritten in every body. Diagnose a `Method` name
   colliding with a declared member (FS0905). Extend the step 2 SemanticAnalysis test: the
   lowering per binding, that a member body holds `Var k` rather than `FieldGet`, and the typar
   scopes on a `let wrap x = (x, v)` inside a generic class. Both backends still fail on the
   surviving `Var`, so this step lands with 6 or with a temporary `Method`-to-`Value Field`
   fallback in each backend.
6. **CLR emits the method.** Add `MethodKey.PreambleFn`, its row, its signature through the
   member encoder, and the per-body eta pass. Goldens: `preamble-generic.clr.cs` loses the
   `Fun<ValueTuple, T>` field and the `<closure>$0` class; `preamble-let-rec.clr.cs` becomes a
   direct recursive `call instance`. New tests for a preamble method that escapes as a value,
   one on a `[<Struct>]` class, shadowed preamble functions, a `static let` function, and a
   generic one on a generic class called at two instantiations.
7. **JS emits the method.** `Cell__get` as a module-level function; `preamble-generic.js` and
   `preamble-fn-value.js` lose their per-instance arrows. Mirror step 6's new tests, minus the
   typar ones, which erase.
8. **Delete this document** and strip any citation of it from comments.

## Superseded findings from the review of `1084b0d5`

These were raised against the interim design and are resolved by deletion in step 3 rather
than fixed in place:

- `EmitEnv.CtorLocals` is a mutable dictionary allocated per method body and filled
  mid-emission; the doc invariant "empty in every other builder" is discipline only.
- The `CtorLocal.Slot | Arg` split leaks into three `EmitMember` call sites as opcode pairs.
- Five names share the `CtorLocal` stem across a type, a DU case, a DU and two record fields.
- `ctorLocalNames` reads `FieldGet` through the public `EFieldGet` pattern but `FieldSet`
  through `exprKind` plus `exprFieldSet`, because `EFieldSet` is private.
- The walk treats only `ELambda` as an escaping frame; an object-expression node, if added to
  the TAST, would silently break it.

## Scope and risk

Tranche A touches `TastDecl`, `TastConvert`, the frozen codec (a format change, so every cached
`.frozen` artefact invalidates), `Elaborate/ClassMembers.fs`, `Elaborate/TypeDecls.fs`, both
backends and their tests. The risk is in step 2's rewrite restriction: any preamble reference
the walk misclassifies as `CtorLocal` while a member reads it produces a member `ldfld` on a
field that was never emitted, which fails at assembly load rather than silently. The
SemanticAnalysis shape test in step 2 is the guard.

Tranche B adds a method row and an eta pass to each backend. Its risk is the surviving `Var k`
in a member body: a backend that has not been taught the key reaches its "no slot for this
bound variable" failure, which is loud. The subtler risk is the storage ordering — a `Method`
body reading a binding the field walk left `CtorLocal` produces a method `ldfld` on an absent
field, the same load-time failure as tranche A, guarded by the same test extended in step 5.

The typar remap is the part with no precedent to copy. `LocalFunction` typars have been
quantified against a binding, not a method row, and step 5 is the first site to move a set of
them into `TyparScope.Member`. `typar-scope.md` is the reference; the CLR encoder's
`ClrEncoder.fs:124` and `:256` already branch on `TyparScope.LocalFunction`, so those two
branches state what a preamble method must no longer reach.
