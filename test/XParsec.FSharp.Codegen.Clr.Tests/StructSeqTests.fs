module XParsec.FSharp.Codegen.Clr.Tests.StructSeqTests

open System
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Vertical slice toward the zero-allocation struct `Seq` module.
// These fixtures stand in for the eventual `src/Vesper.Seq` struct types until the
// codegen shape is proven, then graduate to real library source.
//
// Each test's to-be-compiled program lives as a standalone file under `data/`,
// compiled at test time via `compileSourceData` (see `TestHelpers`). The five
// map/fold-pipeline probes share one struct-seq surface — `data/_struct-seq-types.fs`
// plus the `ofArray`/`map`/`fold` combinators in `data/_struct-seq-combinators.fs` —
// so a change to that surface edits one fragment rather than five copied program blobs.
//
// The struct pipeline relies on chained `this.field.Method(args)`
// calls (e.g. `this.Source.GetEnumerator()`, `this.Source.MoveNext()`). Two fixes
// made this work, both with isolation tests below:
//   1. Front-end: a method call through a 3+-segment folded LongIdent chain is now
//      recognised (`Resolve.(|ClassChainMethod|_|)` + the ElaborateExpr `App` arms);
//      previously it mis-typed the trailing method as a property and lowered the
//      call's `()` to a spurious `Vesper.Fun::Invoke`.
//   2. Codegen: a struct-typed *field* object argument is addressed in place via
//      `ldflda`, so a mutating member call persists rather than mutating a spilled copy.

[<Tests>]
let structSeqTests =
    testList
        "StructSeq"
        [
            // The struct-closure dispatch half. The perf-mature shape of a
            // value-type closure dispatched non-allocating is a `[<Struct>]`
            // implementing the source-nameable `Vesper.Fun<'A,'B>` interface
            // (`prim-types-min.fsi`), applied through a combinator generic over
            // `'TF :> Fun<int,int>` calling `.Invoke` — which should lower to
            // `constrained. !TF callvirt Vesper.Fun::Invoke`, the constrained-dispatch
            // machinery addressing the struct by `ldloca` with NO box.
            //
            // The three-layer EXTERNAL-interface constrained-dispatch gap is
            // closed. The dispatch now flows symmetrically with the
            // project-local case:
            //   1. front-end (`InferRecordAccess.tryTyparInterfaceMember`): when the
            //      coercion target is *not* a local interface, the member is looked up
            //      via `ctx.Provider.TryLookupMember` on the external interface's
            //      qualified key and `TyparInterfaceCall` is recorded (same side-table
            //      the local path uses) — so `f.Invoke x` no longer mis-resolves as a
            //      record FIELD get;
            //   2. Elaborate: unchanged — `mkInterfaceMethodCall` already mints the
            //      `CallVia.Interface` node off `TyparInterfaceCall`, and the external
            //      interface's `Invoke(int)` has a non-`obj` param so the empty local
            //      param model is correct (no spurious boxing);
            //   3. codegen (`EmitMember.emitConstrainedInterfaceCall`): an interface not
            //      in the LOCAL `env.Interfaces` registry mints the `constrained.
            //      callvirt` slot via `env.Provider.ExternalMemberRefOn` against the
            //      interface's instantiated `TypeSpec`.
            test "a struct closure implementing Vesper.Fun dispatches via constrained callvirt with no box" {
                let _, artifact = compileSourceData "StructClosureFunDispatch"
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "struct Fun dispatch returns Invoke result"

                let il = peMethodIlWhere bytes "Program" (fun n -> n <> "Main")

                let hasConstrained =
                    il
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue hasConstrained "apply IL contains a `constrained.` prefix (typar Fun dispatch)"

                Expect.isFalse
                    (Array.contains 0x8Cuy il)
                    "apply IL contains no `box` (non-allocating struct Fun dispatch)"
            }

            // Feed a SOURCE lambda (structural TyFun) into the SAME
            // `apply` combinator whose param is a constrained `'TF :> Fun<int,int>`
            // typar. The front-end originally REJECTED this —
            // `subsumes` (Engine.fs `checkConstraint` Coercion arm) returned
            // `Unrelated` for (TyFun, Fun`2). A single `TyFun`→`Fun`
            // discharge rule was added to `subsumes`: `subsumes(TyFun(a,b), Fun`2<a,b>) =
            // Subtype` (args invariant-Equal). The typar `'TF` then binds to the
            // function type and the existing heap-closure emission (a System.Object subclass
            // implementing Vesper.Fun`2) dispatches via `callvirt Fun::Invoke`, so
            // it compiles + runs 42. The no-box/`constrained.` struct-repr IL ideal
            // is asserted in the value-struct test below, NOT here.
            test "source lambda into a constrained 'TF :> Fun slot compiles + runs" {
                let tast, artifact = compileSourceData "SourceLambdaFunSlot"

                // (1) front-end verdict: does the unifier accept a structural TyFun
                // at the `'TF :> Fun` slot? Failure here prints the rejecting diagnostic.
                Expect.isEmpty tast.Diagnostics (sprintf "front-end diagnostics: %A" tast.Diagnostics)

                // (2) runtime verdict
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "apply (fun x -> x+1) 41 = 42"

            // This only proves front-end accept + correct runtime. The IL-ideal
            // (a `constrained.` prefix `0xFE 0x16` and NO `box` `0x8C` in `apply`)
            // is the later struct-repr work — assert it there, not here.
            }

            // A NON-capturing source lambda is STATELESS, so a single
            // shared instance suffices (fsc caches it in a `static readonly` field and
            // allocates once). The lambda is emitted as a cached singleton: its
            // closure type gains a `static readonly instance` field initialised by a
            // `.cctor` (`newobj` once), and every construction site `ldsfld`s it
            // instead of `newobj`ing. This kills per-construction allocation
            // independent of the value-struct work. The heap closure shape is
            // UNCHANGED — this is only the caching.
            //
            // NOTE: `apply`'s parameter is a PLAIN function `int -> int` (an ordinary
            // higher-order function), NOT a constrained `'TF :> Fun` typar. The
            // value-struct lowering intercepts ONLY the bare-method-typar slot — the
            // value-struct shape needs a typar to instantiate `!TF` at the struct
            // `TypeDef` — so a plain-function HOF still takes the heap-singleton path. (The
            // constrained-typar shape these tests previously used now lowers to a
            // value-struct; that is the dedicated value-struct test above.)
            test "a non-capturing lambda lowers to a cached singleton (ldsfld at use, newobj in .cctor)" {
                let tast, artifact = compileSourceData "NonCapturingLambdaCachedSingleton"
                Expect.isEmpty tast.Diagnostics (sprintf "cached-singleton diagnostics: %A" tast.Diagnostics)

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "apply (fun x -> x+1) 41 = 42"

                // The construction site is `Main` (the top-level `printfn` call). It must
                // load the cached instance (`ldsfld` 0x7E) and must NOT `newobj` (0x73)
                // the closure there.
                let mainIl = peMethodIlWhere bytes "Program" (fun n -> n = "Main")
                Expect.isTrue (Array.contains 0x7Euy mainIl) "Main loads the cached closure via ldsfld (0x7E)"
                Expect.isFalse (Array.contains 0x73uy mainIl) "Main does NOT newobj the closure (0x73)"

                // The single `newobj` of the closure lives in the closure type's
                // `.cctor`, followed by a `stsfld` (0x80) into the singleton field.
                let cctorIl = peMethodIlWhere bytes "<closure>$0" (fun n -> n = ".cctor")
                Expect.isTrue (Array.contains 0x73uy cctorIl) ".cctor newobjs the closure once (0x73)"
                Expect.isTrue (Array.contains 0x80uy cctorIl) ".cctor stsflds the singleton (0x80)"
            }

            // Caching is per-closure-TYPE, so constructing the SAME
            // non-capturing lambda at TWO call sites allocates ONCE — both sites share
            // the one cached singleton. Proven by counting closure types (one) and the
            // total `newobj` in its `.cctor` (one), while both use sites `ldsfld`.
            test "the same non-capturing lambda at two sites allocates once" {
                // Plain function `int -> int` parameter ⇒ the heap-caching path (a
                // constrained typar would take the value-struct path instead).
                let tast, artifact = compileSourceData "NonCapturingLambdaTwoSites"
                Expect.isEmpty tast.Diagnostics (sprintf "two-site diagnostics: %A" tast.Diagnostics)

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "52" "(42) + (10) = 52"

                // Two distinct source lambdas, so two closure types — each cached.
                let closureCctors =
                    peMethodNames bytes
                    |> List.filter (fun (ty, m) -> ty.StartsWith "<closure>$" && m = ".cctor")

                // Each closure's `.cctor` newobjs exactly once; no per-construction
                // newobj reaches the construction sites.
                for (ty, _) in closureCctors do
                    let cctorIl = peMethodIlWhere bytes ty (fun n -> n = ".cctor")
                    let newobjs = cctorIl |> Array.filter (fun b -> b = 0x73uy) |> Array.length
                    Expect.equal newobjs 1 (sprintf "%s .cctor newobjs exactly once" ty)
            }

            // Caching guard: a CAPTURING lambda differs per construction (its
            // captured value is distinct each time), so caching would be WRONG. The
            // HEAP capturing path is UNTOUCHED — it still `newobj`s per construction
            // (no `.cctor`, no cached field). Proven by the construction site (here the
            // body of `outer`, a top-level static method) still containing `newobj`.
            //
            // NOTE: `apply`'s parameter is a PLAIN function `int -> int`, NOT a constrained
            // `'TF :> Fun` typar. The value-struct lowering now lowers a capturing lambda
            // through the CONSTRAINED slot to a by-value value-struct (no `newobj`); the
            // plain-function HOF is the genuine heap path this guard still describes (the
            // dedicated value-struct test above asserts the value-struct shape).
            test "a capturing lambda is NOT cached (still newobjs per construction)" {
                let tast, artifact = compileSourceData "CapturingLambdaNotCached"
                Expect.isEmpty tast.Diagnostics (sprintf "capturing-lambda diagnostics: %A" tast.Diagnostics)

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "outer 1 41 = 42 (capturing lambda)"

                // The capturing closure's construction site (the `outer` static method)
                // still `newobj`s (0x73) — caching did not apply.
                let outerIls = peMethodsIlWhere bytes "Program" (fun n -> n <> "Main")

                let anyNewobj = outerIls |> Array.exists (fun il -> Array.contains 0x73uy il)

                Expect.isTrue anyNewobj "a capturing lambda still newobjs per construction (not cached)"

                // No closure type gained a `.cctor` (no cached singleton was minted).
                let closureCctors =
                    peMethodNames bytes
                    |> List.filter (fun (ty, m) -> ty.StartsWith "<closure>$" && m = ".cctor")

                Expect.isEmpty closureCctors "no capturing closure was given a caching .cctor"
            }

            // The IDEAL shape: a CAPTURELESS struct closure (no field, no
            // ctor) whose only body is the interface impl — exactly what a stateless
            // source `fun x -> x + 1` lowers to. This previously could not be written
            // (a fieldless `[<Struct>]` with only an interface impl tripped parse
            // recovery, so the struct-closure test above had to add a `val N`/`new` to
            // give the closure spurious state). Now that the parser admits it, the
            // captureless form compiles and dispatches with no box.
            test "a CAPTURELESS struct closure (no field) dispatches via constrained callvirt with no box" {
                let _, artifact = compileSourceData "CapturelessStructClosure"
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "captureless struct Fun dispatch returns 42"

                let il = peMethodIlWhere bytes "Program" (fun n -> n <> "Main")

                let hasConstrained =
                    il
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue
                    hasConstrained
                    "apply IL contains a `constrained.` prefix (captureless typar Fun dispatch)"

                Expect.isFalse
                    (Array.contains 0x8Cuy il)
                    "apply IL contains no `box` (non-allocating captureless struct Fun dispatch)"
            }

            // The IDEAL — a CAPTURELESS SOURCE lambda
            // (`fun x -> x + 1`) fed into the constrained `'TF :> Fun<int,int>` slot
            // is now lowered to a zero-alloc VALUE-STRUCT closure, dispatched with
            // `constrained.` devirt and NO box. The `TyFun`→`Fun\`2` subsumes rule made
            // it typecheck and run on the heap; the heap singleton was cached. The
            // value-struct lowering synthesises the closure as a `System.ValueType` and
            // overrides the call-site `!TF` instantiation to the struct `TypeDef`, so
            // the source lambda now reaches the SAME no-box shape the hand-written
            // `[<Struct>] Add1` fixture above proves.
            test "a captureless source lambda lowers to a no-box value-struct closure" {
                let tast, artifact = compileSourceData "CapturelessLambdaValueStruct"
                Expect.isEmpty tast.Diagnostics (sprintf "captureless-lambda diagnostics: %A" tast.Diagnostics)

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "apply (fun x -> x+1) 41 = 42"

                // (1) The synthesised closure is a VALUE TYPE — base `System.ValueType`,
                // NOT a `System.Object` subclass (the heap shape).
                let closureBase = peTypeBaseTypeName bytes (fun n -> n.StartsWith "<closure>$")

                Expect.equal
                    closureBase
                    (ValueSome "System.ValueType")
                    "the captureless closure is a value type (base System.ValueType)"

                // (2) Construction is by-value: the construction site (`Main`) must NOT
                // `newobj` (0x73) the closure, and must NOT `ldsfld` (0x7E) a
                // cached singleton — it `initobj`s a local instead.
                let mainIl = peMethodIlWhere bytes "Program" (fun n -> n = "Main")
                Expect.isFalse (Array.contains 0x73uy mainIl) "Main does NOT newobj the value-struct closure (0x73)"
                Expect.isFalse (Array.contains 0x7Euy mainIl) "Main does NOT ldsfld a cached singleton (0x7E)"

                // No caching `.cctor` was minted for this closure.
                let closureCctors =
                    peMethodNames bytes
                    |> List.filter (fun (ty, m) -> ty.StartsWith "<closure>$" && m = ".cctor")

                Expect.isEmpty closureCctors "no value-struct closure was given a Step-B caching .cctor"

                // (3) `apply`'s body dispatches via `constrained.` (0xFE 0x16) with NO
                // box (0x8C): `!TF` is the struct `TypeDef`, so the JIT devirtualises.
                let applyIl = peMethodIlWhere bytes "Program" (fun n -> n <> "Main")

                let hasConstrained =
                    applyIl
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue
                    hasConstrained
                    "apply IL contains a `constrained.` prefix (value-struct typar Fun dispatch)"

                Expect.isFalse
                    (Array.contains 0x8Cuy applyIl)
                    "apply IL contains no `box` (non-allocating value-struct dispatch)"
            }

            // A CAPTURING source lambda (`fun y -> y + n`,
            // capturing `n`) fed into the SAME constrained `'TF :> Fun<int,int>` slot
            // also lowers to a zero-alloc VALUE-STRUCT closure. The captureless case
            // stored ZERO fields (`initobj`); this stores the capture by value into a
            // struct field and constructs via the value-type ctor (`ldloca; <push n>;
            // call .ctor`), NOT `initobj` (which only zeroes a fieldless struct).
            // Dispatch is still `constrained.` devirt with NO box. The helper `mk` makes
            // the capture real (the lambda's `n` is `mk`'s parameter), so the closure
            // has one genuine capture field.
            test "a capturing source lambda lowers to a no-box value-struct closure" {
                let tast, artifact = compileSourceData "CapturingLambdaValueStruct"
                Expect.isEmpty tast.Diagnostics (sprintf "capturing-lambda diagnostics: %A" tast.Diagnostics)

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "mk 1 41 = 42 (capturing lambda)"

                // (1) The synthesised closure is a VALUE TYPE — base `System.ValueType`.
                let closureBase = peTypeBaseTypeName bytes (fun n -> n.StartsWith "<closure>$")

                Expect.equal
                    closureBase
                    (ValueSome "System.ValueType")
                    "the capturing closure is a value type (base System.ValueType)"

                // (2) It has exactly ONE capture field (the captured `n`).
                use peReader = openPe bytes
                let md = peReader.GetMetadataReader()

                let closureTd =
                    md.TypeDefinitions
                    |> Seq.find (fun h ->
                        let td = md.GetTypeDefinition h
                        (md.GetString td.Name).StartsWith "<closure>$"
                    )

                let fieldCount = (md.GetTypeDefinition closureTd).GetFields() |> Seq.length
                Expect.equal fieldCount 1 "the capturing value-struct closure has exactly one capture field"

                // (3) Construction is by-value: the construction site (`mk`, a top-level
                // static method) must NOT `newobj` (0x73), NOT `ldsfld` (0x7E) a
                // cached singleton, and there must be no caching `.cctor`. The
                // capture is pushed and a `call` (0x28) to the value-type ctor stores
                // it by value.
                let mkIls = peMethodsIlWhere bytes "Program" (fun n -> n <> "Main")

                let anyNewobj = mkIls |> Array.exists (fun il -> Array.contains 0x73uy il)
                Expect.isFalse anyNewobj "no top-level method newobjs the value-struct closure (0x73)"

                let anyLdsfld = mkIls |> Array.exists (fun il -> Array.contains 0x7Euy il)
                Expect.isFalse anyLdsfld "no top-level method ldsflds a cached singleton (0x7E)"

                let anyCall = mkIls |> Array.exists (fun il -> Array.contains 0x28uy il)
                Expect.isTrue anyCall "a top-level method `call`s the value-struct ctor (0x28) with the capture pushed"

                let closureCctors =
                    peMethodNames bytes
                    |> List.filter (fun (ty, m) -> ty.StartsWith "<closure>$" && m = ".cctor")

                Expect.isEmpty closureCctors "no value-struct closure was given a Step-B caching .cctor"

                // (4) `apply`'s body dispatches via `constrained.` (0xFE 0x16) with NO
                // box (0x8C). `apply` is also a top-level static method; assert across all of them that a
                // `constrained.` prefix is present and no `box` appears anywhere.
                let hasConstrained =
                    mkIls
                    |> Array.exists (fun il ->
                        il
                        |> Array.windowed 2
                        |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)
                    )

                Expect.isTrue
                    hasConstrained
                    "apply IL contains a `constrained.` prefix (value-struct typar Fun dispatch)"

                let anyBox = mkIls |> Array.exists (fun il -> Array.contains 0x8Cuy il)

                Expect.isFalse
                    anyBox
                    "no top-level method contains a `box` (non-allocating capturing value-struct dispatch)"
            }

            // A SATURATED 2-arg SOURCE lambda (`fun x y -> x + y`)
            // fed into a constrained `'TF :> Fun<int,int,int>` slot lowers to a
            // zero-alloc VALUE-STRUCT closure with a single FLAT `Invoke(a,b)` (the
            // peeled curried body), dispatched `constrained.` with NO box — the
            // arity-2 analog of the single-arg case. Proves (a) the new
            // `subsumes(TyFun(a,TyFun(b,c)), Fun`3<a,b,c>)` arm, (b) the node-keyed
            // `Fun`-arity verdict threaded like `ClosureReprs`, (c) the 2-param flat
            // `Invoke` emission.
            test "a saturated 2-arg source lambda lowers to a no-box flat-Invoke value-struct" {
                let tast, artifact = compileSourceData "Flat2ArgLambdaValueStruct"
                Expect.isEmpty tast.Diagnostics (sprintf "flat-2 lambda diagnostics: %A" tast.Diagnostics)

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "apply2 (fun x y -> x+y) 20 22 = 42"

                // (1) The synthesised closure is a VALUE TYPE — base `System.ValueType`.
                let closureBase = peTypeBaseTypeName bytes (fun n -> n.StartsWith "<closure>$")

                Expect.equal
                    closureBase
                    (ValueSome "System.ValueType")
                    "the 2-arg closure is a value type (base System.ValueType)"

                // (2) Its `Invoke` is FLAT 2-arg: the closure type defines a single
                // `Invoke` taking two parameters (peeled from the curried lambda body),
                // and there is no nested inner closure for the second parameter.
                use peReader = openPe bytes
                let md = peReader.GetMetadataReader()

                let closureTds =
                    md.TypeDefinitions
                    |> Seq.filter (fun h ->
                        let td = md.GetTypeDefinition h
                        (md.GetString td.Name).StartsWith "<closure>$"
                    )
                    |> Seq.toList

                Expect.equal
                    (List.length closureTds)
                    1
                    "exactly one closure type (no nested inner closure for the second parameter)"

                let invokeParamCount =
                    let td = md.GetTypeDefinition closureTds.[0]

                    td.GetMethods()
                    |> Seq.pick (fun mh ->
                        let m = md.GetMethodDefinition mh

                        if md.GetString m.Name = "Invoke" then
                            Some(m.GetParameters() |> Seq.length)
                        else
                            None
                    )

                Expect.equal invokeParamCount 2 "the value-struct closure's Invoke is flat 2-arg"

                // (3) Construction is by-value (no `newobj`, no cached `ldsfld`).
                let mainIl = peMethodIlWhere bytes "Program" (fun n -> n = "Main")
                Expect.isFalse (Array.contains 0x73uy mainIl) "Main does NOT newobj the value-struct closure (0x73)"

                // (4) `apply2`'s body dispatches via `constrained.` (0xFE 0x16) with NO box.
                let applyIl = peMethodIlWhere bytes "Program" (fun n -> n <> "Main")

                let hasConstrained =
                    applyIl
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue
                    hasConstrained
                    "apply2 IL contains a `constrained.` prefix (value-struct typar Fun dispatch)"

                Expect.isFalse
                    (Array.contains 0x8Cuy applyIl)
                    "apply2 IL contains no `box` (non-allocating flat-2 value-struct dispatch)"
            }

            // The arity-3 analog: a saturated 3-arg source lambda `fun x y z -> …`
            // fed into a constrained `'TF :> Fun<int,int,int,int>` slot lowers to a
            // zero-alloc VALUE-STRUCT closure with a single FLAT `Invoke(a,b,c)` (both
            // inner lambdas peeled, NO nested inner closures), dispatched with NO box.
            // Exercises the arity-parametric peel/encoder/interface-spec path emitting
            // `Vesper.Fun`4<a,b,c,r>`.
            test "a saturated 3-arg source lambda lowers to a no-box flat-Invoke value-struct" {
                let tast, artifact = compileSourceData "Flat3ArgLambdaValueStruct"
                Expect.isEmpty tast.Diagnostics (sprintf "arity-3 front-end diagnostics: %A" tast.Diagnostics)

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "66" "apply3 (fun x y z -> x+y+z) 20 22 24 = 66"

                // (1) The synthesised closure is a VALUE TYPE — base `System.ValueType`.
                let closureBase = peTypeBaseTypeName bytes (fun n -> n.StartsWith "<closure>$")

                Expect.equal
                    closureBase
                    (ValueSome "System.ValueType")
                    "the 3-arg closure is a value type (base System.ValueType)"

                // (2) Its `Invoke` is FLAT 3-arg and there is exactly ONE closure type
                // (both inner lambdas peeled — no nested inner closures).
                use peReader = openPe bytes
                let md = peReader.GetMetadataReader()

                let closureTds =
                    md.TypeDefinitions
                    |> Seq.filter (fun h ->
                        let td = md.GetTypeDefinition h
                        (md.GetString td.Name).StartsWith "<closure>$"
                    )
                    |> Seq.toList

                Expect.equal (List.length closureTds) 1 "exactly one closure type (no nested inner closures)"

                let invokeParamCount =
                    let td = md.GetTypeDefinition closureTds.[0]

                    td.GetMethods()
                    |> Seq.pick (fun mh ->
                        let m = md.GetMethodDefinition mh

                        if md.GetString m.Name = "Invoke" then
                            Some(m.GetParameters() |> Seq.length)
                        else
                            None
                    )

                Expect.equal invokeParamCount 3 "the value-struct closure's Invoke is flat 3-arg"

                // (3) Construction is by-value (no `newobj`).
                let mainIl = peMethodIlWhere bytes "Program" (fun n -> n = "Main")
                Expect.isFalse (Array.contains 0x73uy mainIl) "Main does NOT newobj the value-struct closure (0x73)"

                // (4) `apply3`'s body dispatches via `constrained.` (0xFE 0x16) with NO box.
                let applyIl = peMethodIlWhere bytes "Program" (fun n -> n <> "Main")

                let hasConstrained =
                    applyIl
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue
                    hasConstrained
                    "apply3 IL contains a `constrained.` prefix (value-struct typar Fun dispatch)"

                Expect.isFalse
                    (Array.contains 0x8Cuy applyIl)
                    "apply3 IL contains no `box` (non-allocating flat-3 value-struct dispatch)"
            }

            // The arity-4 analog: a saturated 4-arg source lambda `fun w x y z -> …`
            // fed into a constrained `'TF :> Fun<int,int,int,int,int>` slot lowers to a
            // zero-alloc VALUE-STRUCT closure with a single FLAT `Invoke(a,b,c,d)` (all
            // three inner lambdas peeled, NO nested inner closures), NO box. Emits
            // `Vesper.Fun`5<a,b,c,d,r>` — the widest flat function value-struct.
            test "a saturated 4-arg source lambda lowers to a no-box flat-Invoke value-struct" {
                let tast, artifact = compileSourceData "Flat4ArgLambdaValueStruct"
                Expect.isEmpty tast.Diagnostics (sprintf "arity-4 front-end diagnostics: %A" tast.Diagnostics)

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "100" "apply4 (fun w x y z -> w+x+y+z) 10 20 30 40 = 100"

                // (1) The synthesised closure is a VALUE TYPE — base `System.ValueType`.
                let closureBase = peTypeBaseTypeName bytes (fun n -> n.StartsWith "<closure>$")

                Expect.equal
                    closureBase
                    (ValueSome "System.ValueType")
                    "the 4-arg closure is a value type (base System.ValueType)"

                // (2) Its `Invoke` is FLAT 4-arg and there is exactly ONE closure type
                // (all inner lambdas peeled — no nested inner closures).
                use peReader = openPe bytes
                let md = peReader.GetMetadataReader()

                let closureTds =
                    md.TypeDefinitions
                    |> Seq.filter (fun h ->
                        let td = md.GetTypeDefinition h
                        (md.GetString td.Name).StartsWith "<closure>$"
                    )
                    |> Seq.toList

                Expect.equal (List.length closureTds) 1 "exactly one closure type (no nested inner closures)"

                let invokeParamCount =
                    let td = md.GetTypeDefinition closureTds.[0]

                    td.GetMethods()
                    |> Seq.pick (fun mh ->
                        let m = md.GetMethodDefinition mh

                        if md.GetString m.Name = "Invoke" then
                            Some(m.GetParameters() |> Seq.length)
                        else
                            None
                    )

                Expect.equal invokeParamCount 4 "the value-struct closure's Invoke is flat 4-arg"

                // (3) Construction is by-value (no `newobj`).
                let mainIl = peMethodIlWhere bytes "Program" (fun n -> n = "Main")
                Expect.isFalse (Array.contains 0x73uy mainIl) "Main does NOT newobj the value-struct closure (0x73)"

                // (4) `apply4`'s body dispatches via `constrained.` (0xFE 0x16) with NO box.
                let applyIl = peMethodIlWhere bytes "Program" (fun n -> n <> "Main")

                let hasConstrained =
                    applyIl
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue
                    hasConstrained
                    "apply4 IL contains a `constrained.` prefix (value-struct typar Fun dispatch)"

                Expect.isFalse
                    (Array.contains 0x8Cuy applyIl)
                    "apply4 IL contains no `box` (non-allocating flat-4 value-struct dispatch)"
            }

            // A generic struct whose FIELD is a function typar
            // `'TFunc :> Fun<'T,'U>` (the EXTERNAL Vesper.Fun interface instantiated at
            // the struct's OWN typars `'T`/`'U`), applied via `this.F.Invoke(x)` in a
            // member body — `constrained. !TFunc callvirt` with no box. This is the
            // combination the struct-seq `'TFunc` flip rests on, and the one the landed
            // external-dispatch test did NOT cover (it used a concrete `Fun<int,int>` +
            // a *parameter* object argument). Proves the library can thread `'TFunc` by hand
            // (mirroring its explicit `'S`/`'E` typars) with no new compiler pass.
            test "generic struct field 'TFunc :> Fun<'T,'U> dispatches this.F.Invoke via constrained callvirt" {
                let _, artifact = compileSourceData "StructFieldTFuncDispatch"
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "this.F.Invoke dispatch returns 42"

                let il = peMethodIlWhere bytes "Applier`3" (fun n -> n = "Apply")

                let hasConstrained =
                    il
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue hasConstrained "Apply IL contains a `constrained.` prefix (typar-field Fun dispatch)"
                Expect.isFalse (Array.contains 0x8Cuy il) "Apply IL contains no `box`"
            }

            // A member call on a value whose type is a generic
            // typar constrained to a project-local interface (`'T :> IGetVal`).
            // The object argument is a bare TyVar carrying a `Coercion` constraint; resolution
            // looks the member up through the interface's members and mints a
            // `CallVia.Interface` node, and codegen emits `constrained. <typar> callvirt`
            // so it RUNS end-to-end.
            test "typar object argument constrained to a local interface dispatches via constrained callvirt" {
                let _, artifact = compileSourceData "TyparInterfaceDispatch"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "7"
                    "constrained typar interface dispatch returns the impl value"
            }

            // The non-allocating payoff: the SAME generic `callIt` applied to a
            // `[<Struct>]` argument. The constrained-typar dispatch addresses the struct
            // (`ldloca`) and `constrained. !!T callvirt`s the interface slot, so the JIT
            // resolves the struct's impl directly — no boxing. Asserted on `callIt`'s IL:
            // a `constrained.` prefix (0xFE 0x16) is present and there is NO `box` (0x8C).
            test "constrained typar dispatch on a struct arg does not box" {
                let _, artifact = compileSourceData "TyparInterfaceStructDispatch"
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "9" "constrained struct dispatch returns the impl value"

                // The Program-class top-level `callIt` is emitted on the "Program" type
                // under its source name on the Program class, so target it structurally.
                let il = peMethodIlWhere bytes "Program" (fun n -> n <> "Main")

                let hasConstrained =
                    il
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue hasConstrained "callIt IL contains a `constrained.` prefix (typar interface dispatch)"
                Expect.isFalse (Array.contains 0x8Cuy il) "callIt IL contains no `box` (non-allocating struct dispatch)"
            }

            // A typar constrained to a *generic*
            // interface instantiated at a CONCRETE arg (`'T :> IBox<int>`). Forces
            // the `CallVia.Interface` slot to be minted on the instantiated interface
            // `TypeSpec` (`IBox`1<int>`) rather than the bare definition — the case
            // the struct-dispatch test deferred (`iface.Typars` non-empty).
            test "constrained typar dispatch on a generic interface (concrete arg) does not box" {
                let _, artifact = compileSourceData "GenericIfaceConcreteDispatch"
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "5"
                    "generic-interface constrained dispatch returns the impl value"

                let il = peMethodIlWhere bytes "Program" (fun n -> n <> "Main")

                let hasConstrained =
                    il
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue hasConstrained "callIt IL contains a `constrained.` prefix"
                Expect.isFalse (Array.contains 0x8Cuy il) "callIt IL contains no `box`"
            }

            // A generic struct whose field is a typar
            // (`'S`) constrained to a generic interface whose arg is ANOTHER typar of
            // the enclosing struct (`'S :> IBox<'T>`). The constrained dispatch must
            // mint the slot on `IBox\`1<!T>` where `!T` is the struct's own typar —
            // the interface instantiation is itself a generic parameter, not concrete.
            test "generic struct dispatches through a typar field constrained to a generic interface (typar arg)" {
                let _, artifact = compileSourceData "GenericStructTyparIface"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "7"
                    "generic-struct typar-arg interface dispatch returns the impl value"
            }

            // A [<Struct>] implementing an
            // interface that declares an abstract *property* (`Current`). The impl
            // property getter must be wired (MethodImpl / get_-getter) to the
            // interface's getter slot, else TypeLoadException "Method 'Current' ...
            // does not have an implementation".
            test "struct implements an interface with an abstract property and dispatches" {
                let _, artifact = compileSourceData "StructIfaceProperty"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "10"
                    "struct interface property dispatch returns the impl value"
            }

            // A *generic* struct implementing a *generic*
            // local interface AT ITS OWN TYPAR (`Box<'T> : IBox<'T>`). Every other
            // interface-impl fixture instantiates the interface at a CONCRETE arg
            // (`IBox<int>`, `IStructSeq<ArrayEnumerator>`); here the impl member's
            // return type `'T` is the enclosing struct's own type parameter, which
            // must be threaded into the impl member's scope (not diagnosed free) AND
            // emitted as a generic MethodImpl so the dispatch round-trips at any
            // instantiation. Boxing the struct to the interface and calling `Unwrap`
            // is the producer-side proof.
            test "a generic struct implements a generic local interface at its own typar and dispatches" {
                let _, artifact = compileSourceData "GenericStructGenericIface"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "42"
                    "generic struct implementing a generic interface at its own typar dispatches the impl value"
            }

            // A project-local class implementing a project-local
            // interface, dispatched through the interface. Existing interface-impl
            // tests all use BCL interfaces; `resolveInterfaceImpls` only recognises an
            // interface via the external provider, so a local interface errors with
            // "Type 'IGetVal' is not an interface".
            test "project-local class implements a project-local interface and dispatches" {
                let _, artifact = compileSourceData "LocalInterfaceImpl"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "local interface dispatch returns the impl value"
            }

            // Minimal repro of the chained-method-call gap: `this.I.Get()` parses as
            // `App(LongIdent[this; I; Get], ())`. The 3-segment chain isn't recognised
            // as a method call, so `Get` is mis-typed as a property and `()` becomes a
            // `Fun::Invoke` over-application → ExecutionEngine at JIT.
            test "chained this.field.Method() call (3-segment) resolves and runs" {
                let _, artifact = compileSourceData "StructFieldGet"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "7" "chained method call on a struct field works"
            }

            // A 4-segment chain `this.A.B.Bump()` through two intermediate struct
            // fields, mutating the innermost — exercises the recursive `ldflda`
            // addressing (`this` → `ldflda A` → `ldflda B` → call by address).
            test "4-segment chained method call through nested struct fields mutates in place" {
                let _, artifact = compileSourceData "NestedStructChain"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "11 12"
                    "nested struct-field mutation persists across calls"
            }

            // A chain anchored on an ordinary local (not `this`): `o.I.Get()`.
            test "chained method call anchored on a local variable resolves" {
                let _, artifact = compileSourceData "LocallyAnchoredChain"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "locally anchored chained method call works"
            }

            // `for y in s` over a GENERIC typar source
            // (`'S :> ISeq`) whose `GetEnumerator` is reached through a project-local
            // *custom* (non-`IEnumerable`) interface. The source object argument is a typar, so
            // `GetEnumerator` must dispatch via `constrained. !S callvirt ISeq::GetEnumerator`.
            // The enumerator `E` is here a CONCRETE struct exposing public pattern
            // `MoveNext`/`Current`, so the loop body stays the existing by-address struct
            // walk — isolating the new for-in-over-typar-source dispatch.
            test "for-in over a generic typar source via a custom interface (concrete enumerator)" {
                let _, artifact = compileSourceData "TyparSeqSource"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "6"
                    "for-in over a typar seq source sums via constrained dispatch"
            }

            // `for y in s` over a generic typar source
            // whose seq interface is GENERIC instantiated at a CONCRETE enumerator
            // (`'S :> IStructSeq<ArrayEnumerator>`). `GetEnumerator` dispatches via
            // `constrained. !S callvirt IStructSeq`1<ArrayEnumerator>::GetEnumerator` —
            // the slot minted on the instantiated interface `TypeSpec`. The enumerator
            // is concrete so its members stay the by-address struct walk.
            test "for-in over a generic typar source via a generic interface (concrete enumerator)" {
                let _, artifact = compileSourceData "GenericIfaceTyparSeqSource"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "6"
                    "for-in over a typar source via a generic seq interface sums"
            }

            // A fully generic
            // `sumSeq` over ANY struct sequence — both the seq interface arg `'E` and
            // the enumerator are typars (`'S :> IStructSeq<'E> and 'E :> IStructEnumerator`).
            // `GetEnumerator` and the enumerator's `MoveNext`/`Current` ALL dispatch via
            // `constrained. callvirt`, with `'E` inferred from `ArraySeq`'s interface impl.
            test "for-in over a fully generic struct seq source" {
                let _, artifact = compileSourceData "FullyGenericStructSeq"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "6"
                    "fully generic struct seq sums via constrained dispatch"
            }

            // A concrete `[<Struct>] MapSeq` holding a concrete
            // `[<Struct>] ArraySeq` field + a reference-type closure, walked by
            // `for y in s` — the value-type-source + chained struct-field
            // dispatch (`this.Source.GetEnumerator()` / `this.Source.MoveNext()`).
            test "concrete struct MapSeq pipeline maps and folds" {
                let _, artifact = compileSourceData "StructMapSeq"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "2\n4\n6\ndone" "maps the struct pipeline in order"
            }

            // A GENERIC `MapSeq<'S>` whose source field is a
            // typar (`'S :> IStructSeq<ArrayEnumerator>`), instantiated at a CONCRETE
            // `ArraySeq` at the use site, walked by `for y in s`. Two capabilities meet:
            //   - inside `MapSeq.GetEnumerator`, `this.Source.GetEnumerator()` is a
            //     constrained-typar dispatch (the Wrap.Fetch capability), and
            //   - `for y in s` sources a CONCRETE instantiation of a generic struct
            //     (`MapSeq`1<ArraySeq>`), so the for-in pattern walk addresses a
            //     generic-struct value by address and calls its pattern `GetEnumerator`.
            // The enumerator (`MapEnumerator`) stays concrete to isolate the generic
            // *source* from a generic *enumerator*.
            test "for-in over a generic MapSeq wrapping a concrete ArraySeq" {
                let _, artifact = compileSourceData "GenericMapSeqConcreteSource"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "2\n4\n6\ndone"
                    "generic MapSeq over concrete ArraySeq maps in order"
            }

            // The FULLY GENERIC map pipeline. Both `MapSeq` and
            // `MapEnumerator` are generic; the enumerator chains a generic inner
            // enumerator `'E :> IStructEnumerator` — so `MapEnumerator<'E>.MoveNext` /
            // `.Current` dispatch on a typar field via `constrained. !E callvirt`. The
            // for-in source `s` is a concrete instantiation `MapSeq`2<ArraySeq,
            // ArrayEnumerator>`; its `GetEnumerator` yields a concrete-but-generic
            // `MapEnumerator`1<ArrayEnumerator>`. This is the `ArraySeq → map` tree, all
            // generic, the keystone of the fully generic struct seq.
            test "fully generic struct map pipeline chains a generic enumerator" {
                let _, artifact = compileSourceData "FullyGenericMapPipeline"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "12" "fully generic map pipeline sums the mapped values"
            }

            // The consuming TERMINAL `fold`. Every
            // prior fixture sums inline with `total <- total + y`; none drives a
            // generic struct seq through a `fold` that threads a STATE accumulator
            // and applies a passed reference-type closure `(fun acc x -> acc + x)`
            // per element. This is the shape `src/Vesper.Seq` `Seq.fold` will have:
            // a free generic function `fold f seed s`, generic over the struct seq
            // `'S`/enumerator `'E`, walking `for y in s` and folding. Proves the
            // terminal codegens + runs before the library graduation.
            // Escape hatch: a generic struct sequence/enumerator (generic over
            // `'T`) ALSO implements the BCL `IEnumerable<'T>` / `IEnumerator<'T>` /
            // `IEnumerator` / `IDisposable` so it boxes transparently when handed to a
            // standard .NET API. Proves the generic-struct-implements-generic-BCL-interface
            // declaration + the `IEnumerator<'T> :> IEnumerator` upcast
            // round-trip: the struct upcast to `IEnumerable<int>` enumerates 1,2,3.
            test "generic struct seq implements IEnumerable<'T> escape hatch and enumerates" {
                let _, artifact = compileSourceData "StructSeqEscapeHatch"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "6" "escape-hatch enumerates via IEnumerable<'T>"
            }

            test "fold over a fully generic struct seq threads state through a closure" {
                let _, artifact = compileSourceData "StructSeqFold"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "10"
                    "fold threads state through the closure over a generic struct seq"
            }

            // Fun wall: the flat arity-2 fn type + its flat<->curried adapters
            // (`Fun<'A,'B,'C>` / `Curried` / `flatten` in Vesper.Core) must not
            // just BUILD but RUN end-to-end. This exercises (1) a `[<Struct>]`
            // implementing the 2-arg interface and a saturated `Invoke(a,b)`
            // dispatch, (2) `curryFun`'s `:> Fun<_,_>` upcast over the `Curried`
            // adapter whose body re-dispatches `f.Invoke(a, b)`, and (3) `flatten`
            // forcing a genuinely curried `Fun<int, Fun<int,int>>` (built from two
            // project-local classes) into a flat slot whose `Invoke(a,b)` walks the
            // curried chain `f.Invoke(a).Invoke(b)`.
            test "Fun flat dispatch + curryFun/flatten adapters round-trip" {
                let tast, artifact = compileSourceData "FunAdapters"
                let bytes = Codegen.toBytes artifact
                Expect.isEmpty tast.Diagnostics (sprintf "no diagnostics: %A" tast.Diagnostics)
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "42 42 42"
                    "flat Invoke(a,b), curryFun round-trip, and flatten of a curried value all yield 42"
            }

            // The WHOLE struct-seq pipeline dispatched via
            // constrained struct-closure typars — the `src/Vesper.Seq/struct-seq`
            // flip in miniature. Hand-written struct closures (an `AddN : Fun<int,int>`
            // for `map` and a `SumAcc : Fun<int,int,int>` for `fold`) drive
            // `ofArray |> map |> fold`. `map`'s `MapEnumerator.Current` dispatches
            // `this.F.Invoke(this.Source.Current)` through the `'TFunc :> Fun<int,int>`
            // field (`constrained. !TFunc callvirt`); `fold` dispatches
            // `f.Invoke(state, y)` through the `'TFunc :> Fun<int,int,int>` PARAMETER
            // in ONE flat 2-arg constrained call. The hot fold loop is asserted
            // non-allocating: a `constrained.` prefix present, no `box`.
            //
            // The fixtures here re-declare the struct-seq surface inline (mirroring
            // `src/Vesper.Seq/struct-seq.clr.fs`) so the proof is self-contained against
            // `compileSource` (which tolerates the Seq contract not being stacked);
            // the library form is proven separately by `buildPackage "Vesper.Seq"`.
            test "struct-closure-typar map/fold pipeline runs non-allocating" {
                let tast, artifact = compileSourceData "StructSeqTyparClosurePipeline"
                let bytes = Codegen.toBytes artifact
                Expect.isEmpty tast.Diagnostics (sprintf "no diagnostics: %A" tast.Diagnostics)
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                // (1+1)+(2+1)+(3+1)+(4+1) = 2+3+4+5 = 14
                Expect.equal (output.Replace("\r", "").Trim()) "14" "map (+1) then fold (+) yields 14"

                // The map node's per-element work is `MapEnumerator.Current`, which
                // dispatches `this.F.Invoke(...)` via `constrained. !TFunc callvirt
                // Fun::Invoke` — non-allocating.
                let curIl = peMethodIlWhere bytes "MapEnumerator`4" (fun n -> n.EndsWith "Current")

                let curConstrained =
                    curIl
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue
                    curConstrained
                    "MapEnumerator.Current IL contains a `constrained.` prefix (Fun typar dispatch)"

                Expect.isFalse (Array.contains 0x8Cuy curIl) "MapEnumerator.Current IL contains no `box`"

                // The fold loop drives `f.Invoke(state, y)` via `constrained. !TFunc
                // callvirt Fun::Invoke` — the hot accumulator, also non-allocating.
                // Several free top-level fns land on "Program"; pick the
                // one whose body carries a `constrained.` prefix (the fold loop) and
                // assert it has no `box`.
                let programFoldIl =
                    peMethodsIlWhere bytes "Program" (fun n -> n <> "Main")
                    |> Array.filter (fun il ->
                        il
                        |> Array.windowed 2
                        |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)
                    )

                Expect.isNonEmpty
                    programFoldIl
                    "a top-level fn (the fold loop) contains a `constrained.` prefix (Fun typar dispatch)"

                Expect.isFalse
                    (programFoldIl |> Array.exists (Array.contains 0x8Cuy))
                    "the constrained fold loop IL contains no `box` (non-allocating)"
            }

            // A STORED binding whose type CARRIES the function typar,
            // fed by a value-struct source lambda, must lay out its `'TFunc` slot as the
            // `<closure>$` value-struct, NOT the `Vesper.Fun`2` INTERFACE (reference).
            // The reduced repro of the capstone gap: `mk : ('TF:>Fun<int,int>) ->
            // Holder<'TF>`; `let h = mk (fun x -> x+1)`. Originally the module field `h`
            // was `valuetype Holder`1<class Fun`2<int,int>>` (sig blob ends `15 12 05 …`,
            // GENERICINST CLASS) and the stored struct↔reference layout disagreement
            // corrupted the read (`h.F.Invoke 41`). With the fix the field's `'TF` arg is
            // GENERICINST VALUETYPE `<closure>$…` (`15 11 …`) and the round-trip yields 42.
            test "a stored binding's Fun typar slot is laid out as the <closure>$ value-struct" {
                let tast, artifact = compileSourceData "StoredFunTyparSlotHolder"
                Expect.isEmpty tast.Diagnostics (sprintf "stored-module class diagnostics: %A" tast.Diagnostics)

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "h.F.Invoke 41 = 42"

                // The module field `h`'s signature: walk its blob and assert the
                // `Holder<…>` instantiation's single type-argument is GENERICINST
                // VALUETYPE (ELEMENT_TYPE_VALUETYPE = 0x11), NOT ELEMENT_TYPE_CLASS
                // (0x12). The broken layout encoded the typar slot as the `Fun` class.
                use peReader = openPe bytes
                let md = peReader.GetMetadataReader()

                let hFieldSig =
                    md.TypeDefinitions
                    |> Seq.collect (fun tdh -> (md.GetTypeDefinition tdh).GetFields())
                    |> Seq.tryPick (fun fh ->
                        let fd = md.GetFieldDefinition fh
                        // A top-level (Program-class) value's field name carries its
                        // source offset (`h` → `h$<offset>`).
                        let fn = md.GetString fd.Name

                        if fn = "h" || fn.StartsWith "h$" then
                            Some fd.Signature
                        else
                            None
                    )
                    |> function
                        | Some s -> s
                        | None -> failwith "module field 'h' not found"

                // Decode the FIELD sig properly: `h : valuetype Holder`1<arg0>` is
                //   FIELD(0x06) GENERICINST(0x15) VALUETYPE(0x11) <Holder token>
                //   <argCount=1> <arg0 element type> …
                // The DISCRIMINATING byte is arg0's element type constructor, NOT the byte
                // after the first 0x15 (that is Holder's own VALUETYPE marker — always
                // 0x11 since Holder is `[<Struct>]`, identical in the broken encoding).
                // Use BlobReader so the compressed Holder token / arg-count are skipped
                // correctly regardless of their width. arg0 must be VALUETYPE (0x11 =
                // the `<closure>$` struct); the bug encoded it GENERICINST(0x15) CLASS
                // (0x12) `Fun`2`. (0x11 = ELEMENT_TYPE_VALUETYPE, 0x15 = GENERICINST.)
                let mutable br = md.GetBlobReader hFieldSig
                Expect.equal (br.ReadByte()) 0x06uy "FIELD sig header"
                Expect.equal (br.ReadByte()) 0x15uy "Holder is a generic instance (GENERICINST)"
                Expect.equal (br.ReadByte()) 0x11uy "Holder itself is a value type (VALUETYPE)"
                br.ReadCompressedInteger() |> ignore // Holder TypeDef/TypeRef coded token
                Expect.equal (br.ReadCompressedInteger()) 1 "Holder`1 has one type argument"
                let arg0TyCtor = br.ReadByte()

                Expect.equal
                    arg0TyCtor
                    0x11uy
                    (sprintf
                        "h's Holder<…> type-arg is a value-struct closure (VALUETYPE 0x11); the bug encoded it GENERICINST(0x15) CLASS Fun`2. arg0 type constructor = 0x%02X"
                        arg0TyCtor)
            }

            // The SAME `ofArray |>
            // map |> fold` pipeline as the proof above, but the two hand-written
            // struct closures (`AddN`/`SumAcc`) are replaced by SOURCE lambdas —
            // `map (fun x -> x + 1)` (a saturated 1-arg `Fun` slot, verdict
            // arity 1) and `fold (fun acc x -> acc + x)` (a saturated 2-arg `Fun`
            // slot, verdict arity 2). Proves the whole epic composes: the node-keyed
            // verdict fires per application site regardless of how the combinators nest,
            // and BOTH lambdas lower to zero-alloc value-struct closures with no box;
            // output identical to the hand-written proof (14).
            //
            // Two distinct verdict-propagation rewrites compose
            // here (codegen-time substitution — see
            // `project_seq_struct_pipeline_ladder` memory + `ClosureVerdictRewrite.fs`):
            //  (1) the stored binding's `'TFunc` FIELD/`Var` type
            //      (`s1 : MapSeq<…,'TFunc,…>`) is laid out as the `<closure>$` value-
            //      struct by position (`substituteVerdictClosures`), so the `fold`
            //      call's `'S` MethodSpec instantiates `MapSeq<…,<closure>$,…>`.
            //  (2) the CONSUMING combinator's body — `fold`'s
            //      `for y in source` — is GENERIC over its phantom enumerator typar
            //      `'E`; the `<closure>$` is no longer baked into a grounded body.
            //      The `fold` call's MethodSpec solves `'E` from its
            //      `'S :> IStructSeq<'T,'E>` bound by walking the (already value-
            //      struct-instantiated) `'S` arg's seq impl, so the `constrained.
            //      callvirt GetEnumerator` token's nested `'TFunc` is the
            //      `<closure>$` value-struct and matches the object argument's impl. No
            //      type-equality rewrite (the old `rewriteClosureLeaves` is gone).
            test "SOURCE-lambda map/fold pipeline runs non-allocating (end-to-end)" {
                let tast, artifact = compileSourceData "StructSeqSourceLambdaPipeline"
                let bytes = Codegen.toBytes artifact
                Expect.isEmpty tast.Diagnostics (sprintf "source-lambda pipeline diagnostics: %A" tast.Diagnostics)

                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                // (1+1)+(2+1)+(3+1)+(4+1) = 2+3+4+5 = 14 — identical to the hand-written proof.
                Expect.equal (output.Replace("\r", "").Trim()) "14" "map (+1) then fold (+) yields 14"

                let curIl = peMethodIlWhere bytes "MapEnumerator`4" (fun n -> n.EndsWith "Current")

                let curConstrained =
                    curIl
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue curConstrained "MapEnumerator.Current dispatches via `constrained.`"
                Expect.isFalse (Array.contains 0x8Cuy curIl) "MapEnumerator.Current IL contains no `box`"

                let programFoldIl =
                    peMethodsIlWhere bytes "Program" (fun n -> n <> "Main")
                    |> Array.filter (fun il ->
                        il
                        |> Array.windowed 2
                        |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)
                    )

                Expect.isNonEmpty programFoldIl "the fold loop contains a `constrained.` prefix"

                Expect.isFalse
                    (programFoldIl |> Array.exists (Array.contains 0x8Cuy))
                    "the constrained fold loop IL contains no `box` (non-allocating)"

                let closureBases = peClosureBaseTypeNames bytes

                Expect.isTrue
                    (closureBases |> List.forall (fun b -> b = "ValueType"))
                    (sprintf "both source-lambda closures are value types: %A" closureBases)
            }

            // The SAME `ofArray |> map |> fold` source-lambda pipeline,
            // but FULLY NESTED into one expression with NO stored `let s1` — `fold (fun
            // acc x -> acc + x) 0 (map (fun x -> x + 1) (ofArray xs))`. The mapped seq
            // is a Main-local / temp slot (the `total` initialiser's sub-expression),
            // not a module-value field, so it exercises the temp-slot side of the
            // verdict propagation: `map`'s value-struct result flows directly into
            // `fold`'s `'S` MethodSpec (the call-site instantiation) and `fold`'s
            // `for y in source` for-in still carries the function-type-as-`'TFunc` seq types,
            // both rewritten to the `<closure>$` value-struct. Same
            // output (14), same no-box constrained dispatch.
            test "nested-temp source-lambda map/fold pipeline runs non-allocating" {
                let tast, artifact = compileSourceData "StructSeqNestedTempPipeline"
                let bytes = Codegen.toBytes artifact
                Expect.isEmpty tast.Diagnostics (sprintf "nested pipeline diagnostics: %A" tast.Diagnostics)

                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "14" "nested map (+1) then fold (+) yields 14"

                let curIl = peMethodIlWhere bytes "MapEnumerator`4" (fun n -> n.EndsWith "Current")

                let curConstrained =
                    curIl
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue curConstrained "MapEnumerator.Current dispatches via `constrained.`"
                Expect.isFalse (Array.contains 0x8Cuy curIl) "MapEnumerator.Current IL contains no `box`"

                let programFoldIl =
                    peMethodsIlWhere bytes "Program" (fun n -> n <> "Main")
                    |> Array.filter (fun il ->
                        il
                        |> Array.windowed 2
                        |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)
                    )

                Expect.isNonEmpty programFoldIl "the fold loop contains a `constrained.` prefix"

                Expect.isFalse
                    (programFoldIl |> Array.exists (Array.contains 0x8Cuy))
                    "the constrained fold loop IL contains no `box` (non-allocating)"

                let closureBases = peClosureBaseTypeNames bytes

                Expect.isTrue
                    (closureBases |> List.forall (fun b -> b = "ValueType"))
                    (sprintf "both nested source-lambda closures are value types: %A" closureBases)
            }

            // External-function sibling of the project-local `apply2` test:
            // the SAME node-keyed verdict mechanism must lower a SOURCE lambda
            // fed into `fold`'s `'TFunc :> Fun<'State,'T,'State>` parameter — the
            // combinator here stands in for the eventual external `StructSeq.fold`.
            // No `collectStackLambdaArgs` extension is needed for the head: the verdict
            // is recorded at the application site by `subsumes`' caller regardless of
            // whether the applied function is project-local or external.
            test "a SOURCE lambda through fold's Fun slot lowers to a no-box value-struct" {
                let tast, artifact = compileSourceData "FoldSourceLambdaValueStruct"
                Expect.isEmpty tast.Diagnostics (sprintf "fold source-lambda diagnostics: %A" tast.Diagnostics)
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "10" "fold (+) 0 [1;2;3;4] = 10"

                let closureBase = peTypeBaseTypeName bytes (fun n -> n.StartsWith "<closure>$")
                Expect.equal closureBase (ValueSome "System.ValueType") "the fold source lambda is a value-struct"
            }

            // A MULTI-`map` chain — two transformers before the terminal,
            // whose lambdas have STRUCTURALLY IDENTICAL frozen types (`int -> int`):
            //   s1 = map (fun x -> x + 1) s0   // closure A
            //   s2 = map (fun x -> x * 2) s1   // closure B — SAME function type as A
            //   total = fold (fun acc x -> acc + x) 0 s2
            // This is the case the earlier program-wide function-TYPE-keyed table could not
            // handle: A and B collide on the type key, so `s2`'s outer `'TFunc` slot (and
            // the nested-`s1` slot inside it) would both bind to whichever closure the
            // table picked FIRST. Expected output:
            // ((1+1)*2)+((2+1)*2)+((3+1)*2)+((4+1)*2) = 4+6+8+10 = 28.
            //
            // `fold`'s phantom enumerator typar `'E` is
            // no longer grounded into its shared body; it is a real generic method slot the
            // call site solves from the `'S :> IStructSeq<'T,'E>` bound by walking the
            // (already `<closure>$`-rewritten) source arg's seq interface impl. So the body
            // is genuinely generic over `'E`, each `fold` instantiation carries its own
            // enumerator via the normal `MethodSpec`, and the collision-prone for-in
            // type-equality rewrite (which raised `EntryPointNotFoundException` on the
            // doubly-nested `MapSeq<MapSeq<…>,…>` object argument) is gone — no shared baked body
            // to disambiguate. The closure identity rides through `'S`'s rewritten arg, so
            // the two same-typed `int->int` maps stay distinct.
            test "multi-map chain lowers each closure to its OWN value-struct slot" {
                let tast, artifact = compileSourceData "StructSeqMultiMapChain"
                let bytes = Codegen.toBytes artifact
                Expect.isEmpty tast.Diagnostics (sprintf "multi-map diagnostics: %A" tast.Diagnostics)
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "28" "(x+1)*2 mapped then summed = 28"
            }

            // A THREE-`map` chain whose three transformer
            // lambdas all share the STRUCTURALLY IDENTICAL frozen type (`int -> int`):
            //   s1 = map (fun x -> x + 1) s0   // closure A
            //   s2 = map (fun x -> x * 2) s1   // closure B — SAME function type as A
            //   s3 = map (fun x -> x + 3) s2   // closure C — SAME function type as A, B
            //   total = fold (fun acc x -> acc + x) 0 s3
            // The 2-map test passes even with a structural type→closure table by sheer
            // luck (only one nested level). At THREE maps the table COLLIDES: rewriting
            // `s3`'s field type, a structural `(int->int) → closure` lookup cannot
            // tell `s3`'s outer `'TFunc` (closure C), the once-nested `s2` source slot
            // (closure B), and the twice-nested `s1` source slot (closure A) apart — they
            // are all `int->int`. A first/last-match structural rewrite picks ONE closure
            // for all three positions, corrupting the stored seq signature
            // (`EntryPointNotFoundException` class). Only NODE identity disambiguates.
            // Expected output: for [1;2;3;4], each e -> ((e+1)*2)+3 = 7,9,11,13; sum = 40.
            test "three-map chain keeps each same-typed closure in its OWN slot" {
                let tast, artifact = compileSourceData "StructSeqThreeMapChain"
                let bytes = Codegen.toBytes artifact
                Expect.isEmpty tast.Diagnostics (sprintf "three-map diagnostics: %A" tast.Diagnostics)
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "40" "((x+1)*2)+3 mapped then summed = 40"

                let curIl = peMethodIlWhere bytes "MapEnumerator`4" (fun n -> n.EndsWith "Current")

                let curConstrained =
                    curIl
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue curConstrained "MapEnumerator.Current dispatches via `constrained.`"
                Expect.isFalse (Array.contains 0x8Cuy curIl) "MapEnumerator.Current IL contains no `box`"

                let programFoldIl =
                    peMethodsIlWhere bytes "Program" (fun n -> n <> "Main")
                    |> Array.filter (fun il ->
                        il
                        |> Array.windowed 2
                        |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)
                    )

                Expect.isNonEmpty programFoldIl "the fold loop contains a `constrained.` prefix"

                Expect.isFalse
                    (programFoldIl |> Array.exists (Array.contains 0x8Cuy))
                    "the constrained fold loop IL contains no `box` (non-allocating)"

                let closureBases = peClosureBaseTypeNames bytes

                Expect.isTrue
                    (closureBases |> List.forall (fun b -> b = "ValueType"))
                    (sprintf "all three source-lambda closures are value types: %A" closureBases)
            }

            // Library GRADUATION, the headline epic test.
            // `ofArray |> map |> fold` from SOURCE LAMBDAS against the REAL, separately
            // built `Vesper.Seq` package (an EXTERNAL combinator) through the strict
            // `buildPackage` path — not the inline single-`compileSource` slice the
            // tests above use. The external analogue of the project-local
            // phantom-typar solve was wired into `ClrRecipes.emitExternalCall`:
            // `StructSeq.fold`'s enumerator typar `'E` (in `'S :> IStructSeq<'T,'E>` only —
            // no param/result, so `recoverOpenTypars` can't see it) is recovered from the
            // source `'S`'s EXTERNAL seq impl (`ExternalClassShape.FrozenInterfaces`, via
            // `tryExternalInterfaceWitness`) before the `MethodSpec` is minted, so the
            // external call type-checks AND the value-struct closures ride by value — the
            // same payoff the inline test proves, now across the package boundary.
            test "ofArray |> map |> fold from source lambdas against the external Vesper.Seq package (no box)" {
                let src = dataSource "external-vesper-seq-pipeline"
                let (exitCode, output), bytes = runPackagesInspect [ "Vesper.Seq" ] src
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "14" "(x+1) over [1;2;3;4] summed = 14"

                // The driver's source lambdas lower to VALUE-STRUCT closures even though
                // the combinator is EXTERNAL — `substituteVerdictClosures` rewrote
                // their frozen types so the external-call `MethodSpec` instantiates
                // `'TFunc` at the struct `TypeDef`, not the `Fun` interface.
                let closureBases = peClosureBaseTypeNames bytes

                Expect.isNonEmpty closureBases "the driver emits source-lambda closures"

                Expect.isTrue
                    (closureBases |> List.forall (fun b -> b = "ValueType"))
                    (sprintf "both source-lambda closures are value types: %A" closureBases)
            }
        ]
