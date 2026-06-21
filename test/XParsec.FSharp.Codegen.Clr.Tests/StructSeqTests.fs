module XParsec.FSharp.Codegen.Clr.Tests.StructSeqTests

open System
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Vertical slice toward the zero-allocation struct `Seq` module
// (`brainstorm-seq-module.md`). These fixtures stand in for the eventual
// `src/Vesper.Seq` struct types; they stay inline F# until the codegen shape is
// proven, then graduate to real library source.
//
// RUNG 2 — landed. The struct pipeline relies on chained `this.field.Method(args)`
// calls (e.g. `this.Source.GetEnumerator()`, `this.Source.MoveNext()`). Two fixes
// made this work, both with isolation tests below:
//   1. Front-end: a method call through a 3+-segment folded LongIdent chain is now
//      recognised (`Resolve.(|ClassChainMethod|_|)` + the FreezeExpr `App` arms);
//      previously it mis-typed the trailing method as a property and lowered the
//      call's `()` to a spurious `Vesper.Fun::Invoke`.
//   2. Codegen: a struct-typed *field* receiver is addressed in place via `ldflda`
//      (`EmitMember.loadStructReceiverAddr`), so a mutating member call persists
//      rather than mutating a spilled copy.

[<Tests>]
let structSeqTests =
    testList
        "StructSeq"
        [
            // Rung 4 (struct closures) — the dispatch half. The perf-mature shape of a
            // value-type closure dispatched non-allocating is a `[<Struct>]`
            // implementing the source-nameable `Vesper.Fun<'A,'B>` interface
            // (`prim-types-min.fsi`), applied through a combinator generic over
            // `'TF :> Fun<int,int>` calling `.Invoke` — which should lower to
            // `constrained. !TF callvirt Vesper.Fun::Invoke`, the rung-3 machinery
            // addressing the struct by `ldloca` with NO box.
            //
            // LANDED — the three-layer EXTERNAL-interface constrained-dispatch gap is
            // closed (rung 3 deferred it). The dispatch now flows symmetrically with the
            // project-local case:
            //   1. front-end (`InferRecordAccess.tryTyparInterfaceMember`): when the
            //      coercion target is *not* a local interface, the member is looked up
            //      via `ctx.Provider.TryLookupMember` on the external interface's
            //      qualified key and `TyparInterfaceCall` is recorded (same side-table
            //      the local path uses) — so `f.Invoke x` no longer mis-resolves as a
            //      record FIELD get;
            //   2. Freeze: unchanged — `mkInterfaceMethodCall` already mints the
            //      `CallVia.Interface` node off `TyparInterfaceCall`, and the external
            //      interface's `Invoke(int)` has a non-`obj` param so the empty local
            //      param model is correct (no spurious boxing);
            //   3. codegen (`EmitMember.emitConstrainedInterfaceCall`): an interface not
            //      in the LOCAL `env.Interfaces` registry mints the `constrained.
            //      callvirt` slot via `env.Provider.ExternalMemberRefOn` against the
            //      interface's instantiated `TypeSpec`.
            test
                "a struct closure implementing Vesper.Fun dispatches via constrained callvirt with no box (rung 4 target shape)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type Add1 ="
                            "    val N : int"
                            "    new(n: int) = { N = n }"
                            "    interface Fun<int, int> with"
                            "        member this.Invoke(x: int) : int = x + this.N"
                            "let apply (f: 'TF when 'TF :> Fun<int, int>) (x: int) : int = f.Invoke x"
                            "printfn \"%d\" (apply (Add1 1) 41)"
                        ]

                let _, artifact = compileSource "StructClosureFunDispatch" src
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "struct Fun dispatch returns Invoke result"

                let il = peMethodIlWhere bytes "Program" (fun n -> n.StartsWith "fn$")

                let hasConstrained =
                    il
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue hasConstrained "apply IL contains a `constrained.` prefix (typar Fun dispatch)"

                Expect.isFalse
                    (Array.contains 0x8Cuy il)
                    "apply IL contains no `box` (non-allocating struct Fun dispatch)"
            }

            // rung-4 Step A: feed a SOURCE lambda (structural TyFun) into the SAME
            // `apply` combinator whose param is a constrained `'TF :> Fun<int,int>`
            // typar. The M0 probe established that the front-end REJECTED this —
            // `subsumes` (Engine.fs `checkConstraint` Coercion arm) returned
            // `Unrelated` for (TyFun, Fun`2). Step A added the single arrow→`Fun`
            // discharge rule to `subsumes`: `subsumes(TyFun(a,b), Fun`2<a,b>) =
            // Subtype` (args invariant-Equal). The typar `'TF` then binds to the
            // arrow and the existing heap-closure emission (a System.Object subclass
            // implementing Vesper.Fun`2) dispatches via `callvirt Fun::Invoke`, so
            // it compiles + runs 42. The no-box/`constrained.` struct-repr IL ideal
            // is Step C — NOT asserted here.
            test "rung4 Step A: source lambda into a constrained 'TF :> Fun slot compiles + runs" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let apply (f: 'TF when 'TF :> Fun<int, int>) (x: int) : int = f.Invoke x"
                            "printfn \"%d\" (apply (fun x -> x + 1) 41)"
                        ]

                let tast, artifact = compileSource "M0SourceLambdaFun" src

                // (1) front-end verdict: does the unifier accept a structural TyFun
                // at the `'TF :> Fun` slot? Failure here prints the rejecting diagnostic.
                Expect.isEmpty tast.Diagnostics (sprintf "M0 front-end diagnostics: %A" tast.Diagnostics)

                // (2) runtime verdict
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "M0 Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "M0 apply (fun x -> x+1) 41 = 42"

            // Step A only proves front-end accept + correct runtime. The IL-ideal
            // (a `constrained.` prefix `0xFE 0x16` and NO `box` `0x8C` in `apply`)
            // is the later struct-repr milestone — assert it there, not here.
            }

            // rung-4 Step B: a NON-capturing source lambda is STATELESS, so a single
            // shared instance suffices (fsc caches it in a `static readonly` field and
            // allocates once). The lambda is emitted as a cached singleton: its
            // closure type gains a `static readonly instance` field initialised by a
            // `.cctor` (`newobj` once), and every construction site `ldsfld`s it
            // instead of `newobj`ing. This kills per-construction allocation
            // independent of the value-struct work (Step C). The heap closure shape is
            // UNCHANGED — Step B is only the caching.
            //
            // NOTE: `apply`'s parameter is a PLAIN arrow `int -> int` (an ordinary
            // higher-order function), NOT a constrained `'TF :> Fun` typar. Step C
            // (M1) intercepts ONLY the bare-method-typar slot — the value-struct shape
            // needs a typar to instantiate `!TF` at the struct `TypeDef` — so a plain
            // arrow HOF still takes the Step-B heap-singleton path. (The
            // constrained-typar shape these tests previously used now lowers to a
            // value-struct; that is the dedicated "Step C (M1)" test above.)
            test "rung4 Step B: a non-capturing lambda lowers to a cached singleton (ldsfld at use, newobj in .cctor)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let apply (f: int -> int) (x: int) : int = f x"
                            "printfn \"%d\" (apply (fun x -> x + 1) 41)"
                        ]

                let tast, artifact = compileSource "StepBCachedSingleton" src
                Expect.isEmpty tast.Diagnostics (sprintf "Step B front-end diagnostics: %A" tast.Diagnostics)

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

            // rung-4 Step B: caching is per-closure-TYPE, so constructing the SAME
            // non-capturing lambda at TWO call sites allocates ONCE — both sites share
            // the one cached singleton. Proven by counting closure types (one) and the
            // total `newobj` in its `.cctor` (one), while both use sites `ldsfld`.
            test "rung4 Step B: the same non-capturing lambda at two sites allocates once" {
                // Plain arrow `int -> int` parameter ⇒ the Step-B heap-caching path (a
                // constrained typar would take the Step C value-struct path instead).
                let src =
                    String.concat
                        "\n"
                        [
                            "let apply (f: int -> int) (x: int) : int = f x"
                            "let a = apply (fun x -> x + 1) 41"
                            "let b = apply (fun x -> x + 1) 9"
                            "printfn \"%d\" (a + b)"
                        ]

                let tast, artifact = compileSource "StepBTwoSites" src
                Expect.isEmpty tast.Diagnostics (sprintf "Step B two-site diagnostics: %A" tast.Diagnostics)

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

            // rung-4 Step B guard: a CAPTURING lambda differs per construction (its
            // captured value is distinct each time), so caching would be WRONG. The
            // HEAP capturing path is UNTOUCHED — it still `newobj`s per construction
            // (no `.cctor`, no cached field). Proven by the construction site (here the
            // body of `outer`, a `fn$` static method) still containing `newobj`.
            //
            // NOTE: `apply`'s parameter is a PLAIN arrow `int -> int`, NOT a constrained
            // `'TF :> Fun` typar. Step C (M2) now lowers a capturing lambda through the
            // CONSTRAINED slot to a by-value value-struct (no `newobj`); the plain-arrow
            // HOF is the genuine heap path this guard still describes (the dedicated
            // "Step C (M2)" test above asserts the value-struct shape).
            test "rung4 Step B: a capturing lambda is NOT cached (still newobjs per construction)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let apply (f: int -> int) (x: int) : int = f x"
                            "let outer (n: int) (x: int) : int = apply (fun y -> y + n) x"
                            "printfn \"%d\" (outer 1 41)"
                        ]

                let tast, artifact = compileSource "StepBCapturingNotCached" src
                Expect.isEmpty tast.Diagnostics (sprintf "Step B capturing diagnostics: %A" tast.Diagnostics)

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "outer 1 41 = 42 (capturing lambda)"

                // The capturing closure's construction site (the `outer` static method)
                // still `newobj`s (0x73) — caching did not apply.
                let outerIls = peMethodsIlWhere bytes "Program" (fun n -> n.StartsWith "fn$")

                let anyNewobj = outerIls |> Array.exists (fun il -> Array.contains 0x73uy il)

                Expect.isTrue anyNewobj "a capturing lambda still newobjs per construction (not cached)"

                // No closure type gained a `.cctor` (no cached singleton was minted).
                let closureCctors =
                    peMethodNames bytes
                    |> List.filter (fun (ty, m) -> ty.StartsWith "<closure>$" && m = ".cctor")

                Expect.isEmpty closureCctors "no capturing closure was given a caching .cctor"
            }

            // The IDEAL rung-4 shape: a CAPTURELESS struct closure (no field, no
            // ctor) whose only body is the interface impl — exactly what a stateless
            // source `fun x -> x + 1` lowers to. This previously could not be written
            // (a fieldless `[<Struct>]` with only an interface impl tripped parse
            // recovery, so the rung-4 test above had to add a `val N`/`new` to give
            // the closure spurious state). Now that the parser admits it, the
            // captureless form compiles and dispatches with no box.
            test "a CAPTURELESS struct closure (no field) dispatches via constrained callvirt with no box" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type Add1 ="
                            "    interface Fun<int, int> with"
                            "        member _.Invoke(x: int) : int = x + 1"
                            "let apply (f: 'TF when 'TF :> Fun<int, int>) (x: int) : int = f.Invoke x"
                            "printfn \"%d\" (apply (Add1()) 41)"
                        ]

                let _, artifact = compileSource "CapturelessStructClosure" src
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "captureless struct Fun dispatch returns 42"

                let il = peMethodIlWhere bytes "Program" (fun n -> n.StartsWith "fn$")

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

            // rung-4 Step C (M1): the IDEAL — a CAPTURELESS SOURCE lambda
            // (`fun x -> x + 1`) fed into the constrained `'TF :> Fun<int,int>` slot
            // is now lowered to a zero-alloc VALUE-STRUCT closure, dispatched with
            // `constrained.` devirt and NO box. Step A made it typecheck (arrow →
            // `Fun\`2` subsumes) and run on the heap; Step B cached the heap
            // singleton. Step C synthesises the closure as a `System.ValueType` and
            // overrides the call-site `!TF` instantiation to the struct `TypeDef`, so
            // the source lambda now reaches the SAME no-box shape the hand-written
            // `[<Struct>] Add1` fixture above proves.
            test "rung4 Step C (M1): a captureless source lambda lowers to a no-box value-struct closure" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let apply (f: 'TF when 'TF :> Fun<int, int>) (x: int) : int = f.Invoke x"
                            "printfn \"%d\" (apply (fun x -> x + 1) 41)"
                        ]

                let tast, artifact = compileSource "StepCValueStructClosure" src
                Expect.isEmpty tast.Diagnostics (sprintf "Step C front-end diagnostics: %A" tast.Diagnostics)

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
                // `newobj` (0x73) the closure, and must NOT `ldsfld` (0x7E) a Step-B
                // cached singleton — it `initobj`s a local instead.
                let mainIl = peMethodIlWhere bytes "Program" (fun n -> n = "Main")
                Expect.isFalse (Array.contains 0x73uy mainIl) "Main does NOT newobj the value-struct closure (0x73)"
                Expect.isFalse (Array.contains 0x7Euy mainIl) "Main does NOT ldsfld a cached singleton (0x7E)"

                // No Step-B caching `.cctor` was minted for this closure.
                let closureCctors =
                    peMethodNames bytes
                    |> List.filter (fun (ty, m) -> ty.StartsWith "<closure>$" && m = ".cctor")

                Expect.isEmpty closureCctors "no value-struct closure was given a Step-B caching .cctor"

                // (3) `apply`'s body dispatches via `constrained.` (0xFE 0x16) with NO
                // box (0x8C): `!TF` is the struct `TypeDef`, so the JIT devirtualises.
                let applyIl = peMethodIlWhere bytes "Program" (fun n -> n.StartsWith "fn$")

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

            // rung-4 Step C (M2): a CAPTURING source lambda (`fun y -> y + n`,
            // capturing `n`) fed into the SAME constrained `'TF :> Fun<int,int>` slot
            // also lowers to a zero-alloc VALUE-STRUCT closure. M1 stored ZERO fields
            // (`initobj`); M2 stores the capture by value into a struct field and
            // constructs via the value-type ctor (`ldloca; <push n>; call .ctor`), NOT
            // `initobj` (which only zeroes a fieldless struct). Dispatch is still
            // `constrained.` devirt with NO box. The helper `mk` makes the capture
            // real (the lambda's `n` is `mk`'s parameter), so the closure has one
            // genuine capture field.
            test "rung4 Step C (M2): a capturing source lambda lowers to a no-box value-struct closure" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let apply (f: 'TF when 'TF :> Fun<int, int>) (x: int) : int = f.Invoke x"
                            "let mk (n: int) (x: int) : int = apply (fun y -> y + n) x"
                            "printfn \"%d\" (mk 1 41)"
                        ]

                let tast, artifact = compileSource "StepCM2CapturingValueStruct" src
                Expect.isEmpty tast.Diagnostics (sprintf "Step C M2 front-end diagnostics: %A" tast.Diagnostics)

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

                // (3) Construction is by-value: the construction site (`mk`, a `fn$`
                // static method) must NOT `newobj` (0x73), NOT `ldsfld` (0x7E) a
                // cached singleton, and there must be no Step-B caching `.cctor`. The
                // capture is pushed and a `call` (0x28) to the value-type ctor stores
                // it by value.
                let mkIls = peMethodsIlWhere bytes "Program" (fun n -> n.StartsWith "fn$")

                let anyNewobj = mkIls |> Array.exists (fun il -> Array.contains 0x73uy il)
                Expect.isFalse anyNewobj "no fn$ method newobjs the value-struct closure (0x73)"

                let anyLdsfld = mkIls |> Array.exists (fun il -> Array.contains 0x7Euy il)
                Expect.isFalse anyLdsfld "no fn$ method ldsflds a cached singleton (0x7E)"

                let anyCall = mkIls |> Array.exists (fun il -> Array.contains 0x28uy il)
                Expect.isTrue anyCall "a fn$ method `call`s the value-struct ctor (0x28) with the capture pushed"

                let closureCctors =
                    peMethodNames bytes
                    |> List.filter (fun (ty, m) -> ty.StartsWith "<closure>$" && m = ".cctor")

                Expect.isEmpty closureCctors "no value-struct closure was given a Step-B caching .cctor"

                // (4) `apply`'s body dispatches via `constrained.` (0xFE 0x16) with NO
                // box (0x8C). `apply` is also a `fn$`; assert across all of them that a
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
                Expect.isFalse anyBox "no fn$ method contains a `box` (non-allocating capturing value-struct dispatch)"
            }

            // rung-4 Step C (M3): a SATURATED 2-arg SOURCE lambda (`fun x y -> x + y`)
            // fed into a constrained `'TF :> Fun2<int,int,int>` slot lowers to a
            // zero-alloc VALUE-STRUCT closure with a single FLAT `Invoke(a,b)` (the
            // peeled curried body), dispatched `constrained.` with NO box — the
            // arity-2 analog of the M1/M2 single-arg case. Proves (a) the new
            // `subsumes(TyFun(a,TyFun(b,c)), Fun2`3<a,b,c>)` arm, (b) the node-keyed
            // `Fun`-arity verdict threaded like `ClosureReprs`, (c) the 2-param flat
            // `Invoke` emission.
            test "rung4 Step C (M3): a saturated 2-arg source lambda lowers to a no-box flat-Invoke value-struct" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let apply2 (f: 'TF when 'TF :> Fun2<int, int, int>) (a: int) (b: int) : int = f.Invoke(a, b)"
                            "printfn \"%d\" (apply2 (fun x y -> x + y) 20 22)"
                        ]

                let tast, artifact = compileSource "StepCM3Flat2ValueStruct" src
                Expect.isEmpty tast.Diagnostics (sprintf "Step C M3 front-end diagnostics: %A" tast.Diagnostics)

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
                // and there is no nested inner closure for the second arrow.
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
                    "exactly one closure type (no nested inner closure for the second arrow)"

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
                let applyIl = peMethodIlWhere bytes "Program" (fun n -> n.StartsWith "fn$")

                let hasConstrained =
                    applyIl
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue
                    hasConstrained
                    "apply2 IL contains a `constrained.` prefix (value-struct typar Fun2 dispatch)"

                Expect.isFalse
                    (Array.contains 0x8Cuy applyIl)
                    "apply2 IL contains no `box` (non-allocating flat-2 value-struct dispatch)"
            }

            // Rung-4 foundation: a generic struct whose FIELD is a function typar
            // `'TFunc :> Fun<'T,'U>` (the EXTERNAL Vesper.Fun interface instantiated at
            // the struct's OWN typars `'T`/`'U`), applied via `this.F.Invoke(x)` in a
            // member body — `constrained. !TFunc callvirt` with no box. This is the
            // combination the struct-seq `'TFunc` flip rests on, and the one the landed
            // external-dispatch test did NOT cover (it used a concrete `Fun<int,int>` +
            // a *parameter* receiver). Proves the library can thread `'TFunc` by hand
            // (mirroring its explicit `'S`/`'E` typars) with no new compiler pass.
            test "generic struct field 'TFunc :> Fun<'T,'U> dispatches this.F.Invoke via constrained callvirt" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type Add1 ="
                            "    val N : int"
                            "    new(n: int) = { N = n }"
                            "    interface Fun<int, int> with"
                            "        member this.Invoke(x: int) : int = x + this.N"
                            "[<Struct>]"
                            "type Applier<'TFunc, 'T, 'U when 'TFunc :> Fun<'T, 'U>> ="
                            "    val F : 'TFunc"
                            "    new(f: 'TFunc) = { F = f }"
                            "    member this.Apply(x: 'T) : 'U = this.F.Invoke(x)"
                            "let a = Applier<Add1, int, int>(Add1 1)"
                            "printfn \"%d\" (a.Apply 41)"
                        ]

                let _, artifact = compileSource "StructFieldTFuncDispatch" src
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

            // Wall B+C (rung 3): a member call on a value whose type is a generic
            // typar constrained to a project-local interface (`'T :> IGetVal`).
            // The receiver is a bare TyVar carrying a `Coercion` constraint; Wall B
            // resolves the member through the interface's members and mints a
            // `CallVia.Interface` node, Wall C emits `constrained. <typar> callvirt`
            // so it RUNS end-to-end.
            test "typar receiver constrained to a local interface dispatches via constrained callvirt (Wall C)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IGetVal ="
                            "    abstract member GetVal : unit -> int"
                            "type Holder(n: int) ="
                            "    interface IGetVal with"
                            "        member _.GetVal() = n"
                            "let callIt (x: 'T when 'T :> IGetVal) : int = x.GetVal()"
                            "printfn \"%d\" (callIt (Holder 7))"
                        ]

                let _, artifact = compileSource "TyparInterfaceDispatch" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "7"
                    "constrained typar interface dispatch returns the impl value"
            }

            // Wall C, the non-allocating payoff: the SAME generic `callIt` applied to a
            // `[<Struct>]` argument. The constrained-typar dispatch addresses the struct
            // (`ldloca`) and `constrained. !!T callvirt`s the interface slot, so the JIT
            // resolves the struct's impl directly — no boxing. Asserted on `callIt`'s IL:
            // a `constrained.` prefix (0xFE 0x16) is present and there is NO `box` (0x8C).
            test "constrained typar dispatch on a struct arg does not box (Wall C)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IGetVal ="
                            "    abstract member GetVal : unit -> int"
                            "[<Struct>]"
                            "type SBox ="
                            "    val N : int"
                            "    new(n: int) = { N = n }"
                            "    interface IGetVal with"
                            "        member this.GetVal() = this.N"
                            "let callIt (x: 'T when 'T :> IGetVal) : int = x.GetVal()"
                            "printfn \"%d\" (callIt (SBox 9))"
                        ]

                let _, artifact = compileSource "TyparInterfaceStructDispatch" src
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "9" "constrained struct dispatch returns the impl value"

                // The holder-less top-level `callIt` is emitted on the "Program" type
                // under a synthetic `fn$<n>` name, so target it structurally.
                let il = peMethodIlWhere bytes "Program" (fun n -> n.StartsWith "fn$")

                let hasConstrained =
                    il
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue hasConstrained "callIt IL contains a `constrained.` prefix (typar interface dispatch)"
                Expect.isFalse (Array.contains 0x8Cuy il) "callIt IL contains no `box` (non-allocating struct dispatch)"
            }

            // Rung-3 generic payoff, step 0: a typar constrained to a *generic*
            // interface instantiated at a CONCRETE arg (`'T :> IBox<int>`). Forces
            // the `CallVia.Interface` slot to be minted on the instantiated interface
            // `TypeSpec` (`IBox`1<int>`) rather than the bare definition — the case
            // Wall C deferred (`iface.Typars` non-empty).
            test "constrained typar dispatch on a generic interface (concrete arg) does not box" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IBox<'T> ="
                            "    abstract member Get : unit -> 'T"
                            "[<Struct>]"
                            "type IntBox ="
                            "    val N : int"
                            "    new(n: int) = { N = n }"
                            "    interface IBox<int> with"
                            "        member this.Get() = this.N"
                            "let callIt (x: 'T when 'T :> IBox<int>) : int = x.Get()"
                            "printfn \"%d\" (callIt (IntBox 5))"
                        ]

                let _, artifact = compileSource "GenericIfaceConcreteDispatch" src
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "5"
                    "generic-interface constrained dispatch returns the impl value"

                let il = peMethodIlWhere bytes "Program" (fun n -> n.StartsWith "fn$")

                let hasConstrained =
                    il
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue hasConstrained "callIt IL contains a `constrained.` prefix"
                Expect.isFalse (Array.contains 0x8Cuy il) "callIt IL contains no `box`"
            }

            // Rung-3 generic payoff, step 1: a generic struct whose field is a typar
            // (`'S`) constrained to a generic interface whose arg is ANOTHER typar of
            // the enclosing struct (`'S :> IBox<'T>`). The constrained dispatch must
            // mint the slot on `IBox\`1<!T>` where `!T` is the struct's own typar —
            // the interface instantiation is itself a generic parameter, not concrete.
            test "generic struct dispatches through a typar field constrained to a generic interface (typar arg)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IBox<'T> ="
                            "    abstract member Get : unit -> 'T"
                            "[<Struct>]"
                            "type IntBox ="
                            "    val N : int"
                            "    new(n: int) = { N = n }"
                            "    interface IBox<int> with"
                            "        member this.Get() = this.N"
                            "[<Struct>]"
                            "type Wrap<'S, 'T when 'S :> IBox<'T>> ="
                            "    val Inner : 'S"
                            "    new(inner: 'S) = { Inner = inner }"
                            "    member this.Fetch() : 'T = this.Inner.Get()"
                            "let w = Wrap<IntBox, int>(IntBox 7)"
                            "printfn \"%d\" (w.Fetch())"
                        ]

                let _, artifact = compileSource "GenericStructTyparIface" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "7"
                    "generic-struct typar-arg interface dispatch returns the impl value"
            }

            // Rung-3 sub-task 1 (north-star probe gap): a [<Struct>] implementing an
            // interface that declares an abstract *property* (`Current`). The impl
            // property getter must be wired (MethodImpl / get_-getter) to the
            // interface's getter slot, else TypeLoadException "Method 'Current' ...
            // does not have an implementation".
            test "struct implements an interface with an abstract property and dispatches" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IStructEnumerator ="
                            "    abstract member MoveNext : unit -> bool"
                            "    abstract member Current : int"
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    interface IStructEnumerator with"
                            "        member this.MoveNext() : bool ="
                            "            this.Idx <- this.Idx + 1"
                            "            this.Idx < this.Arr.Length"
                            "        member this.Current : int = this.Arr.[this.Idx]"
                            "let e = ArrayEnumerator([| 10; 20 |])"
                            "let i = (e :> IStructEnumerator)"
                            "i.MoveNext() |> ignore"
                            "printfn \"%d\" i.Current"
                        ]

                let _, artifact = compileSource "StructIfaceProperty" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "10"
                    "struct interface property dispatch returns the impl value"
            }

            // rung-3 §2.1 sub-gap 1: a *generic* struct implementing a *generic*
            // local interface AT ITS OWN TYPAR (`Box<'T> : IBox<'T>`). Every other
            // interface-impl fixture instantiates the interface at a CONCRETE arg
            // (`IBox<int>`, `IStructSeq<ArrayEnumerator>`); here the impl member's
            // return type `'T` is the enclosing struct's own type parameter, which
            // must be threaded into the impl member's scope (not diagnosed free) AND
            // emitted as a generic MethodImpl so the dispatch round-trips at any
            // instantiation. Boxing the struct to the interface and calling `Unwrap`
            // is the producer-side proof.
            test
                "a generic struct implements a generic local interface at its own typar and dispatches (rung 3 §2.1 sub-gap 1)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IBox<'E> ="
                            "    abstract member Unwrap : unit -> 'E"
                            "[<Struct>]"
                            "type Box<'T> ="
                            "    val Value : 'T"
                            "    new(value: 'T) = { Value = value }"
                            "    interface IBox<'T> with"
                            "        member this.Unwrap() : 'T = this.Value"
                            "let b = Box<int>(42)"
                            "let i = (b :> IBox<int>)"
                            "printfn \"%d\" (i.Unwrap())"
                        ]

                let _, artifact = compileSource "GenericStructGenericIface" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "42"
                    "generic struct implementing a generic interface at its own typar dispatches the impl value"
            }

            // Wall A (rung 3): a project-local class implementing a project-local
            // interface, dispatched through the interface. Existing interface-impl
            // tests all use BCL interfaces; `resolveInterfaceImpls` only recognises an
            // interface via the external provider, so a local interface errors with
            // "Type 'IGetVal' is not an interface".
            test "project-local class implements a project-local interface and dispatches" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IGetVal ="
                            "    abstract member GetVal : unit -> int"
                            "type Holder(n: int) ="
                            "    interface IGetVal with"
                            "        member _.GetVal() = n"
                            "let h = Holder(42)"
                            "let v = (h :> IGetVal).GetVal()"
                            "printfn \"%d\" v"
                        ]

                let _, artifact = compileSource "LocalInterfaceImpl" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "local interface dispatch returns the impl value"
            }

            // Minimal repro of the chained-method-call gap: `this.I.Get()` parses as
            // `App(LongIdent[this; I; Get], ())`. The 3-segment chain isn't recognised
            // as a method call, so `Get` is mis-typed as a property and `()` becomes a
            // `Fun::Invoke` over-application → ExecutionEngine at JIT.
            test "chained this.field.Method() call (3-segment) resolves and runs" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type Inner ="
                            "    val Cur : int"
                            "    new(c: int) = { Cur = c }"
                            "    member this.Get() : int = this.Cur"
                            "[<Struct>]"
                            "type Outer ="
                            "    val I : Inner"
                            "    new(i: Inner) = { I = i }"
                            "    member this.StepGet() : int = this.I.Get()"
                            "let run () ="
                            "    let o = Outer(Inner(7))"
                            "    printfn \"%d\" (o.StepGet())"
                            "run ()"
                        ]

                let _, artifact = compileSource "StructFieldGet" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "7" "chained method call on a struct field works"
            }

            // A 4-segment chain `this.A.B.Bump()` through two intermediate struct
            // fields, mutating the innermost — exercises the recursive `ldflda`
            // addressing (`this` → `ldflda A` → `ldflda B` → call by address).
            test "4-segment chained method call through nested struct fields mutates in place" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type Leaf ="
                            "    val mutable N : int"
                            "    new(n: int) = { N = n }"
                            "    member this.Bump() : int ="
                            "        this.N <- this.N + 1"
                            "        this.N"
                            "[<Struct>]"
                            "type Mid ="
                            "    val mutable L : Leaf"
                            "    new(l: Leaf) = { L = l }"
                            "[<Struct>]"
                            "type Top ="
                            "    val mutable M : Mid"
                            "    new(m: Mid) = { M = m }"
                            "    member this.Step() : int = this.M.L.Bump()"
                            "let run () ="
                            "    let mutable t = Top(Mid(Leaf(10)))"
                            "    let a = t.Step()"
                            "    let b = t.Step()"
                            "    printfn \"%d %d\" a b"
                            "run ()"
                        ]

                let _, artifact = compileSource "NestedStructChain" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "11 12"
                    "nested struct-field mutation persists across calls"
            }

            // A chain headed by an ordinary local (not `this`): `o.I.Get()`.
            test "chained method call headed by a local variable resolves" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type Inner ="
                            "    val Cur : int"
                            "    new(c: int) = { Cur = c }"
                            "    member this.Get() : int = this.Cur"
                            "[<Struct>]"
                            "type Outer ="
                            "    val I : Inner"
                            "    new(i: Inner) = { I = i }"
                            "let run () ="
                            "    let o = Outer(Inner(42))"
                            "    printfn \"%d\" (o.I.Get())"
                            "run ()"
                        ]

                let _, artifact = compileSource "LocalHeadedChain" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "local-headed chained method call works"
            }

            // Rung-3 payoff, for-in step 1: `for y in s` over a GENERIC typar source
            // (`'S :> ISeq`) whose `GetEnumerator` is reached through a project-local
            // *custom* (non-`IEnumerable`) interface. The source receiver is a typar, so
            // `GetEnumerator` must dispatch via `constrained. !S callvirt ISeq::GetEnumerator`.
            // The enumerator `E` is here a CONCRETE struct exposing public pattern
            // `MoveNext`/`Current`, so the loop body stays the existing by-address struct
            // walk — isolating the new for-in-over-typar-source dispatch.
            test "for-in over a generic typar source via a custom interface (concrete enumerator)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    member this.MoveNext() : bool ="
                            "        this.Idx <- this.Idx + 1"
                            "        this.Idx < this.Arr.Length"
                            "    member this.Current : int = this.Arr.[this.Idx]"
                            "type ISeq ="
                            "    abstract member GetEnumerator : unit -> ArrayEnumerator"
                            "[<Struct>]"
                            "type ArraySeq ="
                            "    val Arr : int[]"
                            "    new(arr: int[]) = { Arr = arr }"
                            "    interface ISeq with"
                            "        member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)"
                            "let sumSeq (s: 'S when 'S :> ISeq) : int ="
                            "    let mutable total = 0"
                            "    for y in s do"
                            "        total <- total + y"
                            "    total"
                            "printfn \"%d\" (sumSeq (ArraySeq([| 1; 2; 3 |])))"
                        ]

                let _, artifact = compileSource "TyparSeqSource" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "6"
                    "for-in over a typar seq source sums via constrained dispatch"
            }

            // Rung-3 payoff, for-in step 2a: `for y in s` over a generic typar source
            // whose seq interface is GENERIC instantiated at a CONCRETE enumerator
            // (`'S :> IStructSeq<ArrayEnumerator>`). `GetEnumerator` dispatches via
            // `constrained. !S callvirt IStructSeq`1<ArrayEnumerator>::GetEnumerator` —
            // the slot minted on the instantiated interface `TypeSpec`. The enumerator
            // is concrete so its members stay the by-address struct walk.
            test "for-in over a generic typar source via a generic interface (concrete enumerator)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    member this.MoveNext() : bool ="
                            "        this.Idx <- this.Idx + 1"
                            "        this.Idx < this.Arr.Length"
                            "    member this.Current : int = this.Arr.[this.Idx]"
                            "type IStructSeq<'E> ="
                            "    abstract member GetEnumerator : unit -> 'E"
                            "[<Struct>]"
                            "type ArraySeq ="
                            "    val Arr : int[]"
                            "    new(arr: int[]) = { Arr = arr }"
                            "    interface IStructSeq<ArrayEnumerator> with"
                            "        member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)"
                            "let sumSeq (s: 'S when 'S :> IStructSeq<ArrayEnumerator>) : int ="
                            "    let mutable total = 0"
                            "    for y in s do"
                            "        total <- total + y"
                            "    total"
                            "printfn \"%d\" (sumSeq (ArraySeq([| 1; 2; 3 |])))"
                        ]

                let _, artifact = compileSource "GenericIfaceTyparSeqSource" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "6"
                    "for-in over a typar source via a generic seq interface sums"
            }

            // Rung-3 payoff, for-in step 2b (the §2 north-star): a fully generic
            // `sumSeq` over ANY struct sequence — both the seq interface arg `'E` and
            // the enumerator are typars (`'S :> IStructSeq<'E> and 'E :> IStructEnumerator`).
            // `GetEnumerator` and the enumerator's `MoveNext`/`Current` ALL dispatch via
            // `constrained. callvirt`, with `'E` inferred from `ArraySeq`'s interface impl.
            test "for-in over a fully generic struct seq source (north-star)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IStructEnumerator ="
                            "    abstract member MoveNext : unit -> bool"
                            "    abstract member Current : int"
                            "type IStructSeq<'E when 'E :> IStructEnumerator> ="
                            "    abstract member GetEnumerator : unit -> 'E"
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    interface IStructEnumerator with"
                            "        member this.MoveNext() : bool ="
                            "            this.Idx <- this.Idx + 1"
                            "            this.Idx < this.Arr.Length"
                            "        member this.Current : int = this.Arr.[this.Idx]"
                            "[<Struct>]"
                            "type ArraySeq ="
                            "    val Arr : int[]"
                            "    new(arr: int[]) = { Arr = arr }"
                            "    interface IStructSeq<ArrayEnumerator> with"
                            "        member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)"
                            "let sumSeq (s: 'S when 'S :> IStructSeq<'E> and 'E :> IStructEnumerator) : int ="
                            "    let mutable total = 0"
                            "    for y in s do"
                            "        total <- total + y"
                            "    total"
                            "printfn \"%d\" (sumSeq (ArraySeq([| 1; 2; 3 |])))"
                        ]

                let _, artifact = compileSource "FullyGenericStructSeq" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "6"
                    "fully generic struct seq sums via constrained dispatch"
            }

            // Rung 2 target: a concrete `[<Struct>] MapSeq` holding a concrete
            // `[<Struct>] ArraySeq` field + a reference-type closure, walked by
            // `for y in s` — the value-type-source (rung 1) + chained struct-field
            // dispatch (`this.Source.GetEnumerator()` / `this.Source.MoveNext()`).
            test "concrete struct MapSeq pipeline maps and folds (rung 2)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    member this.MoveNext() : bool ="
                            "        this.Idx <- this.Idx + 1"
                            "        this.Idx < this.Arr.Length"
                            "    member this.Current : int = this.Arr.[this.Idx]"
                            "[<Struct>]"
                            "type ArraySeq ="
                            "    val Arr : int[]"
                            "    new(arr: int[]) = { Arr = arr }"
                            "    member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)"
                            "[<Struct>]"
                            "type MapEnumerator ="
                            "    val mutable Source : ArrayEnumerator"
                            "    val F : int -> int"
                            "    new(source: ArrayEnumerator, f: int -> int) = { Source = source; F = f }"
                            "    member this.MoveNext() : bool = this.Source.MoveNext()"
                            "    member this.Current : int = this.F (this.Source.Current)"
                            "[<Struct>]"
                            "type MapSeq ="
                            "    val Source : ArraySeq"
                            "    val F : int -> int"
                            "    new(source: ArraySeq, f: int -> int) = { Source = source; F = f }"
                            "    member this.GetEnumerator() : MapEnumerator = MapEnumerator(this.Source.GetEnumerator(), this.F)"
                            "let xs = [| 1; 2; 3 |]"
                            "let s = MapSeq(ArraySeq(xs), fun x -> x * 2)"
                            "for y in s do"
                            "    printfn \"%d\" y"
                            "printfn \"done\""
                        ]

                let _, artifact = compileSource "StructMapSeq" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "2\n4\n6\ndone" "maps the struct pipeline in order"
            }

            // Rung-3 payoff, step 2a: a GENERIC `MapSeq<'S>` whose source field is a
            // typar (`'S :> IStructSeq<ArrayEnumerator>`), instantiated at a CONCRETE
            // `ArraySeq` at the use site, walked by `for y in s`. Two capabilities meet:
            //   - inside `MapSeq.GetEnumerator`, `this.Source.GetEnumerator()` is a
            //     constrained-typar dispatch (the Wrap.Fetch capability), and
            //   - `for y in s` sources a CONCRETE instantiation of a generic struct
            //     (`MapSeq`1<ArraySeq>`), so the for-in pattern walk addresses a
            //     generic-struct value by address and calls its pattern `GetEnumerator`.
            // The enumerator (`MapEnumerator`) stays concrete to isolate the generic
            // *source* from a generic *enumerator* (step 2b).
            test "for-in over a generic MapSeq wrapping a concrete ArraySeq (rung 3 step 2a)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    member this.MoveNext() : bool ="
                            "        this.Idx <- this.Idx + 1"
                            "        this.Idx < this.Arr.Length"
                            "    member this.Current : int = this.Arr.[this.Idx]"
                            "type IStructSeq<'E> ="
                            "    abstract member GetEnumerator : unit -> 'E"
                            "[<Struct>]"
                            "type ArraySeq ="
                            "    val Arr : int[]"
                            "    new(arr: int[]) = { Arr = arr }"
                            "    interface IStructSeq<ArrayEnumerator> with"
                            "        member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)"
                            "[<Struct>]"
                            "type MapEnumerator ="
                            "    val mutable Source : ArrayEnumerator"
                            "    val F : int -> int"
                            "    new(source: ArrayEnumerator, f: int -> int) = { Source = source; F = f }"
                            "    member this.MoveNext() : bool = this.Source.MoveNext()"
                            "    member this.Current : int = this.F (this.Source.Current)"
                            "[<Struct>]"
                            "type MapSeq<'S when 'S :> IStructSeq<ArrayEnumerator>> ="
                            "    val Source : 'S"
                            "    val F : int -> int"
                            "    new(source: 'S, f: int -> int) = { Source = source; F = f }"
                            "    member this.GetEnumerator() : MapEnumerator = MapEnumerator(this.Source.GetEnumerator(), this.F)"
                            "let xs = [| 1; 2; 3 |]"
                            "let s = MapSeq<ArraySeq>(ArraySeq(xs), fun x -> x * 2)"
                            "for y in s do"
                            "    printfn \"%d\" y"
                            "printfn \"done\""
                        ]

                let _, artifact = compileSource "GenericMapSeqConcreteSource" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "2\n4\n6\ndone"
                    "generic MapSeq over concrete ArraySeq maps in order"
            }

            // Rung-3 payoff, step 2b: the FULLY GENERIC map pipeline. Both `MapSeq` and
            // `MapEnumerator` are generic; the enumerator chains a generic inner
            // enumerator `'E :> IStructEnumerator` — so `MapEnumerator<'E>.MoveNext` /
            // `.Current` dispatch on a typar field via `constrained. !E callvirt`. The
            // for-in source `s` is a concrete instantiation `MapSeq`2<ArraySeq,
            // ArrayEnumerator>`; its `GetEnumerator` yields a concrete-but-generic
            // `MapEnumerator`1<ArrayEnumerator>`. This is the `ArraySeq → map` tree, all
            // generic, the keystone of the §2 north-star.
            test "fully generic struct map pipeline chains a generic enumerator (rung 3 step 2b)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IStructEnumerator ="
                            "    abstract member MoveNext : unit -> bool"
                            "    abstract member Current : int"
                            "type IStructSeq<'E when 'E :> IStructEnumerator> ="
                            "    abstract member GetEnumerator : unit -> 'E"
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    interface IStructEnumerator with"
                            "        member this.MoveNext() : bool ="
                            "            this.Idx <- this.Idx + 1"
                            "            this.Idx < this.Arr.Length"
                            "        member this.Current : int = this.Arr.[this.Idx]"
                            "[<Struct>]"
                            "type ArraySeq ="
                            "    val Arr : int[]"
                            "    new(arr: int[]) = { Arr = arr }"
                            "    interface IStructSeq<ArrayEnumerator> with"
                            "        member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)"
                            "[<Struct>]"
                            "type MapEnumerator<'E when 'E :> IStructEnumerator> ="
                            "    val mutable Source : 'E"
                            "    val F : int -> int"
                            "    new(source: 'E, f: int -> int) = { Source = source; F = f }"
                            "    interface IStructEnumerator with"
                            "        member this.MoveNext() : bool = this.Source.MoveNext()"
                            "        member this.Current : int = this.F (this.Source.Current)"
                            "[<Struct>]"
                            "type MapSeq<'S, 'E when 'S :> IStructSeq<'E> and 'E :> IStructEnumerator> ="
                            "    val Source : 'S"
                            "    val F : int -> int"
                            "    new(source: 'S, f: int -> int) = { Source = source; F = f }"
                            "    interface IStructSeq<MapEnumerator<'E>> with"
                            "        member this.GetEnumerator() : MapEnumerator<'E> = MapEnumerator<'E>(this.Source.GetEnumerator(), this.F)"
                            "let sumSeq (s: 'S when 'S :> IStructSeq<'E> and 'E :> IStructEnumerator) : int ="
                            "    let mutable total = 0"
                            "    for y in s do"
                            "        total <- total + y"
                            "    total"
                            "let xs = [| 1; 2; 3 |]"
                            "let s = MapSeq<ArraySeq, ArrayEnumerator>(ArraySeq(xs), fun x -> x * 2)"
                            "printfn \"%d\" (sumSeq s)"
                        ]

                let _, artifact = compileSource "FullyGenericMapPipeline" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "12" "fully generic map pipeline sums the mapped values"
            }

            // §2.2 graduation prerequisite: the consuming TERMINAL `fold`. Every
            // prior fixture sums inline with `total <- total + y`; none drives a
            // generic struct seq through a `fold` that threads a STATE accumulator
            // and applies a passed reference-type closure `(fun acc x -> acc + x)`
            // per element. This is the shape `src/Vesper.Seq` `Seq.fold` will have:
            // a free generic function `fold f seed s`, generic over the struct seq
            // `'S`/enumerator `'E`, walking `for y in s` and folding. Proves the
            // terminal codegens + runs before the library graduation.
            // §7.2 escape hatch: a generic struct sequence/enumerator (generic over
            // `'T`) ALSO implements the BCL `IEnumerable<'T>` / `IEnumerator<'T>` /
            // `IEnumerator` / `IDisposable` so it boxes transparently when handed to a
            // standard .NET API. Proves the generic-struct-implements-generic-BCL-interface
            // declaration + the `IEnumerator<'T> :> IEnumerator` upcast (§2.1 sub-gap 2)
            // round-trip: the struct upcast to `IEnumerable<int>` enumerates 1,2,3.
            test "generic struct seq implements IEnumerable<'T> escape hatch and enumerates (§7.2)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "open System.Collections.Generic"
                            "open System.Collections"
                            "[<Struct>]"
                            "type ArrayEnumerator<'T> ="
                            "    val Arr : 'T[]"
                            "    val mutable Idx : int"
                            "    new(arr: 'T[]) = { Arr = arr; Idx = -1 }"
                            "    interface IEnumerator<'T> with"
                            "        member this.Current : 'T = this.Arr.[this.Idx]"
                            "    interface IEnumerator with"
                            "        member this.MoveNext() : bool ="
                            "            this.Idx <- this.Idx + 1"
                            "            this.Idx < this.Arr.Length"
                            "        member this.Current : obj = box (this.Arr.[this.Idx])"
                            "        member this.Reset() : unit = ()"
                            "    interface System.IDisposable with"
                            "        member this.Dispose() : unit = ()"
                            "[<Struct>]"
                            "type ArraySeq<'T> ="
                            "    val Arr : 'T[]"
                            "    new(arr: 'T[]) = { Arr = arr }"
                            "    interface IEnumerable<'T> with"
                            "        member this.GetEnumerator() : IEnumerator<'T> = (ArrayEnumerator<'T>(this.Arr) :> IEnumerator<'T>)"
                            "    interface IEnumerable with"
                            "        member this.GetEnumerator() : IEnumerator = (ArrayEnumerator<'T>(this.Arr) :> IEnumerator)"
                            "let sum3 (xs: IEnumerable<int>) : int ="
                            "    let e = xs.GetEnumerator()"
                            "    let mutable total = 0"
                            "    while e.MoveNext() do"
                            "        total <- total + e.Current"
                            "    total"
                            "let s = ArraySeq<int>([| 1; 2; 3 |])"
                            "printfn \"%d\" (sum3 (s :> IEnumerable<int>))"
                        ]

                let _, artifact = compileSource "StructSeqEscapeHatch" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "6" "escape-hatch enumerates via IEnumerable<'T>"
            }

            test "fold over a fully generic struct seq threads state through a closure (rung 3 §2.2)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IStructEnumerator ="
                            "    abstract member MoveNext : unit -> bool"
                            "    abstract member Current : int"
                            "type IStructSeq<'E when 'E :> IStructEnumerator> ="
                            "    abstract member GetEnumerator : unit -> 'E"
                            "[<Struct>]"
                            "type ArrayEnumerator ="
                            "    val Arr : int[]"
                            "    val mutable Idx : int"
                            "    new(arr: int[]) = { Arr = arr; Idx = -1 }"
                            "    interface IStructEnumerator with"
                            "        member this.MoveNext() : bool ="
                            "            this.Idx <- this.Idx + 1"
                            "            this.Idx < this.Arr.Length"
                            "        member this.Current : int = this.Arr.[this.Idx]"
                            "[<Struct>]"
                            "type ArraySeq ="
                            "    val Arr : int[]"
                            "    new(arr: int[]) = { Arr = arr }"
                            "    interface IStructSeq<ArrayEnumerator> with"
                            "        member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)"
                            "let fold (f: 'State -> int -> 'State) (seed: 'State) (s: 'S when 'S :> IStructSeq<'E> and 'E :> IStructEnumerator) : 'State ="
                            "    let mutable state = seed"
                            "    for y in s do"
                            "        state <- f state y"
                            "    state"
                            "let xs = [| 1; 2; 3; 4 |]"
                            "printfn \"%d\" (fold (fun acc x -> acc + x) 0 (ArraySeq(xs)))"
                        ]

                let _, artifact = compileSource "StructSeqFold" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "10"
                    "fold threads state through the closure over a generic struct seq"
            }

            // Fun2 wall: the flat arity-2 fn type + its flat<->curried adapters
            // (`Fun2<'A,'B,'C>` / `Curried` / `flatten` in Vesper.Core) must not
            // just BUILD but RUN end-to-end. This exercises (1) a `[<Struct>]`
            // implementing the 2-arg interface and a saturated `Invoke(a,b)`
            // dispatch, (2) `curryFun`'s `:> Fun<_,_>` upcast over the `Curried`
            // adapter whose body re-dispatches `f.Invoke(a, b)`, and (3) `flatten`
            // forcing a genuinely curried `Fun<int, Fun<int,int>>` (built from two
            // project-local classes) into a flat slot whose `Invoke(a,b)` walks the
            // curried chain `f.Invoke(a).Invoke(b)`.
            test "Fun2 flat dispatch + curryFun/flatten adapters round-trip" {
                let src =
                    String.concat
                        "\n"
                        [
                            // A value-type flat 2-arg closure. A fieldless `[<Struct>]`
                            // whose body is ONLY an interface impl trips parse recovery
                            // ("Skipped tokens at module level"); a `val`/`new` preamble
                            // (the rung-4 `Add1` shape) parses, so carry a dummy field.
                            "[<Struct>]"
                            "type Add2 ="
                            "    val Z : int"
                            "    new(z: int) = { Z = z }"
                            "    interface Fun2<int, int, int> with"
                            "        member this.Invoke(a: int, b: int) : int = a + b + this.Z"
                            // a genuinely curried value: AddB captures `a`, returns b -> a+b
                            "type AddB(a: int) ="
                            "    interface Fun<int, int> with"
                            "        member this.Invoke(b: int) : int = a + b"
                            "type AddCurried() ="
                            "    interface Fun<int, Fun<int, int>> with"
                            "        member this.Invoke(a: int) : Fun<int, int> = AddB(a) :> Fun<int, int>"
                            "let flat = (Add2(0) :> Fun2<int, int, int>).Invoke(20, 22)"
                            "let curried = ((curryFun (Add2(0) :> Fun2<int, int, int>) 20) :> Fun<int, int>).Invoke(22)"
                            "let flattened = (flatten (AddCurried() :> Fun<int, Fun<int, int>>)).Invoke(20, 22)"
                            "printfn \"%d %d %d\" flat curried flattened"
                        ]

                let tast, artifact = compileSource "Fun2Adapters" src
                let bytes = Codegen.toBytes artifact
                Expect.isEmpty tast.Diagnostics (sprintf "no diagnostics: %A" tast.Diagnostics)
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "42 42 42"
                    "flat Invoke(a,b), curryFun round-trip, and flatten of a curried value all yield 42"
            }

            // RUNG 4 PROOF (wall iv): the WHOLE struct-seq pipeline dispatched via
            // constrained struct-closure typars — the `src/Vesper.Seq/struct-seq`
            // flip in miniature. Hand-written struct closures (an `AddN : Fun<int,int>`
            // for `map` and a `SumAcc : Fun2<int,int,int>` for `fold`) drive
            // `ofArray |> map |> fold`. `map`'s `MapEnumerator.Current` dispatches
            // `this.F.Invoke(this.Source.Current)` through the `'TFunc :> Fun<int,int>`
            // field (`constrained. !TFunc callvirt`); `fold` dispatches
            // `f.Invoke(state, y)` through the `'TFunc :> Fun2<int,int,int>` PARAMETER
            // in ONE flat 2-arg constrained call. The hot fold loop is asserted
            // non-allocating: a `constrained.` prefix present, no `box`.
            //
            // The fixtures here re-declare the struct-seq surface inline (mirroring
            // `src/Vesper.Seq/struct-seq.fs`) so the proof is self-contained against
            // `compileSource` (which tolerates the Seq contract not being stacked);
            // the library form is proven separately by `buildPackage "Vesper.Seq"`.
            test "rung 4: struct-closure-typar map/fold pipeline runs non-allocating (wall iv proof)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IStructEnumerator<'T> ="
                            "    abstract member MoveNext : unit -> bool"
                            "    abstract member Current : 'T"
                            "type IStructSeq<'T, 'E when 'E :> IStructEnumerator<'T>> ="
                            "    abstract member GetEnumerator : unit -> 'E"
                            "[<Struct>]"
                            "type ArrayEnumerator<'T> ="
                            "    val Arr : 'T[]"
                            "    val mutable Idx : int"
                            "    new(arr: 'T[]) = { Arr = arr; Idx = -1 }"
                            "    interface IStructEnumerator<'T> with"
                            "        member this.MoveNext() : bool ="
                            "            this.Idx <- this.Idx + 1"
                            "            this.Idx < this.Arr.Length"
                            "        member this.Current : 'T = this.Arr.[this.Idx]"
                            "[<Struct>]"
                            "type ArraySeq<'T> ="
                            "    val Arr : 'T[]"
                            "    new(arr: 'T[]) = { Arr = arr }"
                            "    interface IStructSeq<'T, ArrayEnumerator<'T>> with"
                            "        member this.GetEnumerator() : ArrayEnumerator<'T> = ArrayEnumerator<'T>(this.Arr)"
                            "[<Struct>]"
                            "type MapEnumerator<'E, 'TFunc, 'T, 'U when 'E :> IStructEnumerator<'T> and 'TFunc :> Fun<'T, 'U>> ="
                            "    val mutable Source : 'E"
                            "    val F : 'TFunc"
                            "    new(source: 'E, f: 'TFunc) = { Source = source; F = f }"
                            "    interface IStructEnumerator<'U> with"
                            "        member this.MoveNext() : bool = this.Source.MoveNext()"
                            "        member this.Current : 'U = this.F.Invoke(this.Source.Current)"
                            "[<Struct>]"
                            "type MapSeq<'S, 'E, 'TFunc, 'T, 'U when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T> and 'TFunc :> Fun<'T, 'U>> ="
                            "    val Source : 'S"
                            "    val F : 'TFunc"
                            "    new(source: 'S, f: 'TFunc) = { Source = source; F = f }"
                            "    interface IStructSeq<'U, MapEnumerator<'E, 'TFunc, 'T, 'U>> with"
                            "        member this.GetEnumerator() : MapEnumerator<'E, 'TFunc, 'T, 'U> = MapEnumerator<'E, 'TFunc, 'T, 'U>(this.Source.GetEnumerator(), this.F)"
                            // hand-written struct closures (each carries a `val`/`new`
                            // so the [<Struct>] parses — a fieldless impl-only struct
                            // trips parse recovery).
                            "[<Struct>]"
                            "type AddN ="
                            "    val N : int"
                            "    new(n: int) = { N = n }"
                            "    interface Fun<int, int> with"
                            "        member this.Invoke(x: int) : int = x + this.N"
                            "[<Struct>]"
                            "type SumAcc ="
                            "    val Z : int"
                            "    new(z: int) = { Z = z }"
                            "    interface Fun2<int, int, int> with"
                            "        member this.Invoke(state: int, y: int) : int = state + y + this.Z"
                            "let ofArray (arr: 'T[]) : ArraySeq<'T> = ArraySeq<'T>(arr)"
                            "let map (f: 'TFunc when 'TFunc :> Fun<'T, 'U>) (source: 'S when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>) : MapSeq<'S, 'E, 'TFunc, 'T, 'U> ="
                            "    MapSeq<'S, 'E, 'TFunc, 'T, 'U>(source, f)"
                            "let fold (f: 'TFunc when 'TFunc :> Fun2<'State, 'T, 'State>) (seed: 'State) (source: 'S when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>) : 'State ="
                            "    let mutable state = seed"
                            "    for y in source do"
                            "        state <- f.Invoke(state, y)"
                            "    state"
                            "let xs = [| 1; 2; 3; 4 |]"
                            "let s0 = ofArray xs"
                            "let s1 = map (AddN 1) s0"
                            "let total = fold (SumAcc 0) 0 s1"
                            "printfn \"%d\" total"
                        ]

                let tast, artifact = compileSource "StructSeqRung4Pipeline" src
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
                // callvirt Fun2::Invoke` — the hot accumulator, also non-allocating.
                // Several free top-level fns land on "Program" as `fn$<n>`; pick the
                // one whose body carries a `constrained.` prefix (the fold loop) and
                // assert it has no `box`.
                let programFoldIl =
                    peMethodsIlWhere bytes "Program" (fun n -> n.StartsWith "fn$")
                    |> Array.filter (fun il ->
                        il
                        |> Array.windowed 2
                        |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)
                    )

                Expect.isNonEmpty
                    programFoldIl
                    "a top-level fn$ (the fold loop) contains a `constrained.` prefix (Fun2 typar dispatch)"

                Expect.isFalse
                    (programFoldIl |> Array.exists (Array.contains 0x8Cuy))
                    "the constrained fold loop IL contains no `box` (non-allocating)"
            }

            // rung-4 M6 P-a: a STORED binding whose type CARRIES the function typar,
            // fed by a value-struct source lambda, must lay out its `'TFunc` slot as the
            // `<closure>$` value-struct, NOT the `Vesper.Fun`2` INTERFACE (reference).
            // The reduced repro from the M6 ptest comment: `mk : ('TF:>Fun<int,int>) ->
            // Holder<'TF>`; `let h = mk (fun x -> x+1)`. Before P-a the module field `h`
            // is `valuetype Holder`1<class Fun`2<int,int>>` (sig blob ends `15 12 05 …`,
            // GENERICINST CLASS) and the stored struct↔reference layout disagreement
            // corrupts the read (`h.F.Invoke 41`). After P-a the field's `'TF` arg is
            // GENERICINST VALUETYPE `<closure>$…` (`15 11 …`) and the round-trip yields 42.
            test "rung4 (M6 P-a): a stored binding's Fun typar slot is laid out as the <closure>$ value-struct" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type Holder<'TFunc when 'TFunc :> Fun<int, int>> ="
                            "    val F : 'TFunc"
                            "    new(f: 'TFunc) = { F = f }"
                            "let mk (f: 'TFunc when 'TFunc :> Fun<int, int>) : Holder<'TFunc> = Holder<'TFunc>(f)"
                            "let apply (f: 'TF when 'TF :> Fun<int, int>) (x: int) : int = f.Invoke x"
                            // The STORED binding `h` is the P-a target: its `Holder` field
                            // must lay out `'TFunc` as the `<closure>$` value-struct. The
                            // closure is then dispatched by passing `h.F` through a typar
                            // combinator (`apply`) — the M1/M2 constrained-dispatch path —
                            // which reads `h.F`'s (rewritten) value-struct type for the
                            // `!TF` MethodSpec, so the read of the stored struct is correct.
                            "let h = mk (fun x -> x + 1)"
                            "printfn \"%d\" (apply h.F 41)"
                        ]

                let tast, artifact = compileSource "M6PaStoredHolder" src
                Expect.isEmpty tast.Diagnostics (sprintf "M6 P-a diagnostics: %A" tast.Diagnostics)

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

                        if md.GetString fd.Name = "h" then
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
                // The DISCRIMINATING byte is arg0's element-type head, NOT the byte
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
                let arg0Head = br.ReadByte()

                Expect.equal
                    arg0Head
                    0x11uy
                    (sprintf
                        "h's Holder<…> type-arg is a value-struct closure (VALUETYPE 0x11); the bug encoded it GENERICINST(0x15) CLASS Fun`2. arg0 head = 0x%02X"
                        arg0Head)
            }

            // rung-4 M6 (end-to-end integration capstone): the SAME `ofArray |> map
            // |> fold` pipeline as the wall-iv proof above, but the two hand-written
            // struct closures (`AddN`/`SumAcc`) are replaced by SOURCE lambdas —
            // `map (fun x -> x + 1)` (a saturated 1-arg `Fun` slot, M1/M2 verdict
            // arity 1) and `fold (fun acc x -> acc + x)` (a saturated 2-arg `Fun2`
            // slot, M3 verdict arity 2). Should prove the whole epic composes: the
            // node-keyed verdict fires per application site regardless of how the
            // combinators nest, and BOTH lambdas lower to zero-alloc value-struct
            // closures with no box; output identical to wall-iv (14).
            //
            // PENDING — blocked on a verdict-propagation gap the M3 mechanism does not
            // cover (see `docs/rung4-lambda-lowering-design.md` §2.4 / §5). The §2.4
            // node-keyed verdict rewrites only the *call site* (`EmitCall`'s `!TF`
            // MethodSpec override binds `'TFunc` to the closure value-type). It does NOT
            // flow into the **type of a binding that stores the combinator's result**:
            // `let s1 = map (fun x -> x+1) s0` freezes `s1 : MapSeq<…,'TFunc,…>` with
            // `'TFunc` still grounded to the ARROW (which encodes to the `Fun`/`Fun2`
            // INTERFACE — a reference type), so the Program static field for `s1` is laid
            // out as `MapSeq<…, class Fun<int,int>, …>` while `map` returns the value-
            // struct-instantiated `MapSeq<…, <closure>$, …>`. The struct↔reference layout
            // mismatch corrupts the stored value → a `NullReferenceException` when `fold`
            // reads `this.F` (captureless closure) / `InvalidProgram` (capturing).
            //
            // Reduced repro (verified): `let h = mk (fun x -> x + 1)` where
            // `mk : ('TFunc :> Fun<int,int>) -> Holder<'TFunc>` — the module field `h`
            // is emitted `valuetype Holder`1<class Fun`2<int,int>>` (field-sig blob
            // `06 15 11 08 01 15 12 05 02 08 08`), not `Holder`1<<closure>$>`. The same
            // pipeline WITHOUT a stored result (construct + dispatch inside one generic
            // body, or a `let g = f` local copy) runs correctly — confirming the gap is
            // specifically result-type propagation into the binding, not the closure
            // emission, the constrained dispatch, or the call-site MethodSpec (all of
            // which the M1/M2/M3 tests prove green).
            //
            // The fix is NOT a localized integration patch: it requires the verdict to
            // propagate through the combinator's RETURN type into the frozen type of the
            // value that binds it (the deferred per-axis constraint table of §5, or an
            // equivalent freeze-time substitution). Left PENDING per the no-speculative-
            // broadening guardrail; the orchestrator decides the follow-up milestone.
            ptest "rung 4 (M6): SOURCE-lambda map/fold pipeline runs non-allocating (end-to-end)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IStructEnumerator<'T> ="
                            "    abstract member MoveNext : unit -> bool"
                            "    abstract member Current : 'T"
                            "type IStructSeq<'T, 'E when 'E :> IStructEnumerator<'T>> ="
                            "    abstract member GetEnumerator : unit -> 'E"
                            "[<Struct>]"
                            "type ArrayEnumerator<'T> ="
                            "    val Arr : 'T[]"
                            "    val mutable Idx : int"
                            "    new(arr: 'T[]) = { Arr = arr; Idx = -1 }"
                            "    interface IStructEnumerator<'T> with"
                            "        member this.MoveNext() : bool ="
                            "            this.Idx <- this.Idx + 1"
                            "            this.Idx < this.Arr.Length"
                            "        member this.Current : 'T = this.Arr.[this.Idx]"
                            "[<Struct>]"
                            "type ArraySeq<'T> ="
                            "    val Arr : 'T[]"
                            "    new(arr: 'T[]) = { Arr = arr }"
                            "    interface IStructSeq<'T, ArrayEnumerator<'T>> with"
                            "        member this.GetEnumerator() : ArrayEnumerator<'T> = ArrayEnumerator<'T>(this.Arr)"
                            "[<Struct>]"
                            "type MapEnumerator<'E, 'TFunc, 'T, 'U when 'E :> IStructEnumerator<'T> and 'TFunc :> Fun<'T, 'U>> ="
                            "    val mutable Source : 'E"
                            "    val F : 'TFunc"
                            "    new(source: 'E, f: 'TFunc) = { Source = source; F = f }"
                            "    interface IStructEnumerator<'U> with"
                            "        member this.MoveNext() : bool = this.Source.MoveNext()"
                            "        member this.Current : 'U = this.F.Invoke(this.Source.Current)"
                            "[<Struct>]"
                            "type MapSeq<'S, 'E, 'TFunc, 'T, 'U when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T> and 'TFunc :> Fun<'T, 'U>> ="
                            "    val Source : 'S"
                            "    val F : 'TFunc"
                            "    new(source: 'S, f: 'TFunc) = { Source = source; F = f }"
                            "    interface IStructSeq<'U, MapEnumerator<'E, 'TFunc, 'T, 'U>> with"
                            "        member this.GetEnumerator() : MapEnumerator<'E, 'TFunc, 'T, 'U> = MapEnumerator<'E, 'TFunc, 'T, 'U>(this.Source.GetEnumerator(), this.F)"
                            "let ofArray (arr: 'T[]) : ArraySeq<'T> = ArraySeq<'T>(arr)"
                            "let map (f: 'TFunc when 'TFunc :> Fun<'T, 'U>) (source: 'S when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>) : MapSeq<'S, 'E, 'TFunc, 'T, 'U> ="
                            "    MapSeq<'S, 'E, 'TFunc, 'T, 'U>(source, f)"
                            "let fold (f: 'TFunc when 'TFunc :> Fun2<'State, 'T, 'State>) (seed: 'State) (source: 'S when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>) : 'State ="
                            "    let mutable state = seed"
                            "    for y in source do"
                            "        state <- f.Invoke(state, y)"
                            "    state"
                            "let xs = [| 1; 2; 3; 4 |]"
                            "let s0 = ofArray xs"
                            "let s1 = map (fun x -> x + 1) s0"
                            "let total = fold (fun acc x -> acc + x) 0 s1"
                            "printfn \"%d\" total"
                        ]

                let tast, artifact = compileSource "StructSeqRung4M6SourceLambda" src
                let bytes = Codegen.toBytes artifact
                Expect.isEmpty tast.Diagnostics (sprintf "M6 source-lambda pipeline diagnostics: %A" tast.Diagnostics)
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                // (1+1)+(2+1)+(3+1)+(4+1) = 2+3+4+5 = 14 — identical to the wall-iv proof.
                Expect.equal (output.Replace("\r", "").Trim()) "14" "map (+1) then fold (+) yields 14"

                let curIl = peMethodIlWhere bytes "MapEnumerator`4" (fun n -> n.EndsWith "Current")

                let curConstrained =
                    curIl
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue curConstrained "MapEnumerator.Current dispatches via `constrained.`"
                Expect.isFalse (Array.contains 0x8Cuy curIl) "MapEnumerator.Current IL contains no `box`"

                let programFoldIl =
                    peMethodsIlWhere bytes "Program" (fun n -> n.StartsWith "fn$")
                    |> Array.filter (fun il ->
                        il
                        |> Array.windowed 2
                        |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)
                    )

                Expect.isNonEmpty programFoldIl "the fold loop contains a `constrained.` prefix"

                Expect.isFalse
                    (programFoldIl |> Array.exists (Array.contains 0x8Cuy))
                    "the constrained fold loop IL contains no `box` (non-allocating)"

                let closureBases =
                    use pr = openPe bytes
                    let m = pr.GetMetadataReader()

                    m.TypeDefinitions
                    |> Seq.choose (fun tdh ->
                        let td = m.GetTypeDefinition tdh

                        if (m.GetString td.Name).StartsWith "<closure>$" then
                            match td.BaseType.Kind with
                            | HandleKind.TypeReference ->
                                Some(
                                    m.GetString (m.GetTypeReference(TypeReferenceHandle.op_Explicit td.BaseType)).Name
                                )
                            | _ -> Some "<none>"
                        else
                            None
                    )
                    |> Seq.toList

                Expect.isTrue
                    (closureBases |> List.forall (fun b -> b = "ValueType"))
                    (sprintf "both source-lambda closures are value types: %A" closureBases)
            }

            // rung-4 M3 (external-head sibling of the project-local `apply2` test):
            // the SAME node-keyed verdict mechanism (§2.4) must lower a SOURCE lambda
            // fed into `fold`'s `'TFunc :> Fun2<'State,'T,'State>` parameter — the
            // combinator here stands in for the eventual external `StructSeq.fold`.
            // No `collectStackLambdaArgs` extension is needed for the head: the verdict
            // is recorded at the application site by `subsumes`' caller regardless of
            // whether the head is project-local or external.
            test "rung4 Step C (M3): a SOURCE lambda through fold's Fun2 slot lowers to a no-box value-struct" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type IStructEnumerator<'T> ="
                            "    abstract member MoveNext : unit -> bool"
                            "    abstract member Current : 'T"
                            "type IStructSeq<'T, 'E when 'E :> IStructEnumerator<'T>> ="
                            "    abstract member GetEnumerator : unit -> 'E"
                            "[<Struct>]"
                            "type ArrayEnumerator<'T> ="
                            "    val Arr : 'T[]"
                            "    val mutable Idx : int"
                            "    new(arr: 'T[]) = { Arr = arr; Idx = -1 }"
                            "    interface IStructEnumerator<'T> with"
                            "        member this.MoveNext() : bool ="
                            "            this.Idx <- this.Idx + 1"
                            "            this.Idx < this.Arr.Length"
                            "        member this.Current : 'T = this.Arr.[this.Idx]"
                            "[<Struct>]"
                            "type ArraySeq<'T> ="
                            "    val Arr : 'T[]"
                            "    new(arr: 'T[]) = { Arr = arr }"
                            "    interface IStructSeq<'T, ArrayEnumerator<'T>> with"
                            "        member this.GetEnumerator() : ArrayEnumerator<'T> = ArrayEnumerator<'T>(this.Arr)"
                            "let ofArray (arr: 'T[]) : ArraySeq<'T> = ArraySeq<'T>(arr)"
                            "let fold (f: 'TFunc when 'TFunc :> Fun2<'State, 'T, 'State>) (seed: 'State) (source: 'S when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>) : 'State ="
                            "    let mutable state = seed"
                            "    for y in source do"
                            "        state <- f.Invoke(state, y)"
                            "    state"
                            "let total = fold (fun acc x -> acc + x) 0 (ofArray [| 1; 2; 3; 4 |])"
                            "printfn \"%d\" total"
                        ]

                let tast, artifact = compileSource "StepCM3FoldSourceLambda" src
                Expect.isEmpty tast.Diagnostics (sprintf "M3 fold source-lambda diagnostics: %A" tast.Diagnostics)
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "10" "fold (+) 0 [1;2;3;4] = 10"

                let closureBase = peTypeBaseTypeName bytes (fun n -> n.StartsWith "<closure>$")
                Expect.equal closureBase (ValueSome "System.ValueType") "the fold source lambda is a value-struct"
            }
        ]
