module XParsec.FSharp.Codegen.Clr.Tests.StructSeqTests

open System
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection
open XParsec.FSharp.Codegen.Clr.Tests.PackageHarness

// Vertical slice toward the zero-allocation struct `Seq` module, standing in for
// `src/Vesper.Seq`. Each test's program is a standalone file under `data/`; the map/fold
// probes share `data/_struct-seq-types.fs` + `data/_struct-seq-combinators.fs`.

[<Tests>]
let structSeqTests =
    testList
        "StructSeq"
        [
            // A `[<Struct>]` implementing the EXTERNAL `Vesper.Fun<int,int>` interface, passed
            // to a combinator generic over `'TF :> Fun<int,int>`: `.Invoke` lowers to
            // `constrained. !TF callvirt Vesper.Fun::Invoke`, addressing the struct with no box.
            test "a struct closure implementing Vesper.Fun dispatches via constrained callvirt with no box" {
                let artifact = compileSourceData "StructClosureFunDispatch"
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

            // A SOURCE lambda `fun x -> x + 1` at the same `'TF :> Fun<int,int>` slot: `'TF`
            // binds to the function type `int -> int` and the heap closure (a System.Object
            // subclass) dispatches `callvirt Fun::Invoke`. Accept + run only; no IL assertions.
            test "source lambda into a constrained 'TF :> Fun slot compiles + runs" {
                let artifact = compileSourceData "SourceLambdaFunSlot"

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "apply (fun x -> x+1) 41 = 42"
            }

            // A non-capturing lambda is stateless, so its closure type gains a static
            // singleton field set by a `.cctor` and every construction site `ldsfld`s it.
            // `apply` takes a plain `int -> int`, not a `'TF :> Fun` typar: hence the heap path.
            test "a non-capturing lambda lowers to a cached singleton (ldsfld at use, newobj in .cctor)" {
                let artifact = compileSourceData "NonCapturingLambdaCachedSingleton"

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "apply (fun x -> x+1) 41 = 42"

                // The construction site is `Main`, the top-level `printfn` call.
                let mainIl = peMethodIlWhere bytes "Program" (fun n -> n = "Main")
                Expect.isTrue (Array.contains 0x7Euy mainIl) "Main loads the cached closure via ldsfld (0x7E)"
                Expect.isFalse (Array.contains 0x73uy mainIl) "Main does NOT newobj the closure (0x73)"

                let cctorIl = peMethodIlWhere bytes "<closure>$0" (fun n -> n = ".cctor")
                Expect.isTrue (Array.contains 0x73uy cctorIl) ".cctor newobjs the closure once (0x73)"
                Expect.isTrue (Array.contains 0x80uy cctorIl) ".cctor stsflds the singleton (0x80)"
            }

            // Caching is per-closure-TYPE: `fun x -> x + 1` at two construction sites
            // allocates only in a `.cctor`. Asserted as: every emitted closure type's
            // `.cctor` newobjs exactly once.
            test "the same non-capturing lambda at two sites allocates once" {
                let artifact = compileSourceData "NonCapturingLambdaTwoSites"

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "52" "(42) + (10) = 52"

                let closureCctors =
                    peMethodNames bytes
                    |> List.filter (fun (ty, m) -> ty.StartsWith "<closure>$" && m = ".cctor")

                for (ty, _) in closureCctors do
                    let cctorIl = peMethodIlWhere bytes ty (fun n -> n = ".cctor")
                    let newobjs = cctorIl |> Array.filter (fun b -> b = 0x73uy) |> Array.length
                    Expect.equal newobjs 1 (sprintf "%s .cctor newobjs exactly once" ty)
            }

            // A capturing lambda differs per construction, so caching it would be wrong: on
            // the heap path (`apply` takes a plain `int -> int`) it still `newobj`s per
            // construction, with no `.cctor` and no cached field.
            test "a capturing lambda is NOT cached (still newobjs per construction)" {
                let artifact = compileSourceData "CapturingLambdaNotCached"

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "outer 1 41 = 42 (capturing lambda)"

                let outerIls = peMethodsIlWhere bytes "Program" (fun n -> n <> "Main")

                let anyNewobj = outerIls |> Array.exists (fun il -> Array.contains 0x73uy il)

                Expect.isTrue anyNewobj "a capturing lambda still newobjs per construction (not cached)"

                let closureCctors =
                    peMethodNames bytes
                    |> List.filter (fun (ty, m) -> ty.StartsWith "<closure>$" && m = ".cctor")

                Expect.isEmpty closureCctors "no capturing closure was given a caching .cctor"
            }

            // A fieldless, ctor-less `[<Struct>]` whose only body is the `Fun` interface impl,
            // the hand-written shape a stateless `fun x -> x + 1` lowers to. The struct
            // closure above carries a `val N`/`new`; this one has nothing to address.
            test "a CAPTURELESS struct closure (no field) dispatches via constrained callvirt with no box" {
                let artifact = compileSourceData "CapturelessStructClosure"
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

            // A captureless SOURCE lambda `fun x -> x + 1` at a `'TF :> Fun<int,int>` slot is
            // synthesised as a `System.ValueType` closure and the call site instantiates `!TF`
            // at that struct, the same no-box shape as the hand-written `[<Struct>]` above.
            test "a captureless source lambda lowers to a no-box value-struct closure" {
                let artifact = compileSourceData "CapturelessLambdaValueStruct"

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "apply (fun x -> x+1) 41 = 42"

                let closureBase = peTypeBaseTypeName bytes (fun n -> n.StartsWith "<closure>$")

                Expect.equal
                    closureBase
                    (ValueSome "System.ValueType")
                    "the captureless closure is a value type (base System.ValueType)"

                // Construction is by-value: `Main` `initobj`s a local instead.
                let mainIl = peMethodIlWhere bytes "Program" (fun n -> n = "Main")
                Expect.isFalse (Array.contains 0x73uy mainIl) "Main does NOT newobj the value-struct closure (0x73)"
                Expect.isFalse (Array.contains 0x7Euy mainIl) "Main does NOT ldsfld a cached singleton (0x7E)"

                let closureCctors =
                    peMethodNames bytes
                    |> List.filter (fun (ty, m) -> ty.StartsWith "<closure>$" && m = ".cctor")

                Expect.isEmpty closureCctors "no value-struct closure was given a caching .cctor"

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

            // A capturing source lambda `fun y -> y + n` at the same `'TF :> Fun<int,int>`
            // slot. Unlike the captureless case it has one field, so construction is
            // `ldloca; <push n>; call .ctor` rather than `initobj`. `n` is `mk`'s parameter.
            test "a capturing source lambda lowers to a no-box value-struct closure" {
                let artifact = compileSourceData "CapturingLambdaValueStruct"

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "mk 1 41 = 42 (capturing lambda)"

                let closureBase = peTypeBaseTypeName bytes (fun n -> n.StartsWith "<closure>$")

                Expect.equal
                    closureBase
                    (ValueSome "System.ValueType")
                    "the capturing closure is a value type (base System.ValueType)"

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

                // `mk` is the construction site; `apply` also lands on "Program", so these
                // assertions range over every top-level method.
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

                Expect.isEmpty closureCctors "no value-struct closure was given a caching .cctor"

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

            // A tupled group AHEAD of the closure slot: the lambda is source group 1 but flat
            // parameter 2, so instantiating `!TF` off the group index picks `b: int` and the
            // closure lands on the `Fun`2` interface (a box) instead of its own struct.
            test "a value-struct closure behind a tupled group instantiates its own struct" {
                let artifact = compileSourceData "TupledGroupBeforeLambdaValueStruct"

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "applyAfterPair (20, 21) (fun x -> x + 1) = 42"

                let ils = peMethodsIlWhere bytes "Program" (fun _ -> true)
                let anyBox = ils |> Array.exists (fun il -> Array.contains 0x8Cuy il)
                Expect.isFalse anyBox "no top-level method boxes the value-struct closure"

                let anyNewobj = ils |> Array.exists (fun il -> Array.contains 0x73uy il)
                Expect.isFalse anyNewobj "no top-level method newobjs the value-struct closure (0x73)"
            }

            // The arity-2 analog: a saturated `fun x y -> x + y` at a
            // `'TF :> Fun<int,int,int>` slot peels to ONE value-struct closure with a single
            // flat `Invoke(a,b)`, and no nested inner closure for the second parameter.
            test "a saturated 2-arg source lambda lowers to a no-box flat-Invoke value-struct" {
                let artifact = compileSourceData "Flat2ArgLambdaValueStruct"

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "apply2 (fun x y -> x+y) 20 22 = 42"

                let closureBase = peTypeBaseTypeName bytes (fun n -> n.StartsWith "<closure>$")

                Expect.equal
                    closureBase
                    (ValueSome "System.ValueType")
                    "the 2-arg closure is a value type (base System.ValueType)"

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

                let mainIl = peMethodIlWhere bytes "Program" (fun n -> n = "Main")
                Expect.isFalse (Array.contains 0x73uy mainIl) "Main does NOT newobj the value-struct closure (0x73)"

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

            // The arity-3 analog: `fun x y z -> …` at a `'TF :> Fun<int,int,int,int>` slot
            // peels both inner lambdas into one value-struct closure with a flat
            // `Invoke(a,b,c)`, implementing `Vesper.Fun`4<a,b,c,r>`.
            test "a saturated 3-arg source lambda lowers to a no-box flat-Invoke value-struct" {
                let artifact = compileSourceData "Flat3ArgLambdaValueStruct"

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "66" "apply3 (fun x y z -> x+y+z) 20 22 24 = 66"

                let closureBase = peTypeBaseTypeName bytes (fun n -> n.StartsWith "<closure>$")

                Expect.equal
                    closureBase
                    (ValueSome "System.ValueType")
                    "the 3-arg closure is a value type (base System.ValueType)"

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

                let mainIl = peMethodIlWhere bytes "Program" (fun n -> n = "Main")
                Expect.isFalse (Array.contains 0x73uy mainIl) "Main does NOT newobj the value-struct closure (0x73)"

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

            // The arity-4 analog: `fun w x y z -> …` at a `'TF :> Fun<int,int,int,int,int>`
            // slot peels all three inner lambdas into one value-struct closure with a flat
            // `Invoke(a,b,c,d)`, implementing `Vesper.Fun`5<a,b,c,d,r>`, the widest flat form.
            test "a saturated 4-arg source lambda lowers to a no-box flat-Invoke value-struct" {
                let artifact = compileSourceData "Flat4ArgLambdaValueStruct"

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "100" "apply4 (fun w x y z -> w+x+y+z) 10 20 30 40 = 100"

                let closureBase = peTypeBaseTypeName bytes (fun n -> n.StartsWith "<closure>$")

                Expect.equal
                    closureBase
                    (ValueSome "System.ValueType")
                    "the 4-arg closure is a value type (base System.ValueType)"

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

                let mainIl = peMethodIlWhere bytes "Program" (fun n -> n = "Main")
                Expect.isFalse (Array.contains 0x73uy mainIl) "Main does NOT newobj the value-struct closure (0x73)"

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

            // A generic struct whose FIELD is a function typar `'TFunc :> Fun<'T,'U>` (the
            // external `Vesper.Fun` at the struct's own typars), applied as `this.F.Invoke(x)`
            // in a member body. Unlike the tests above the object argument is a field, not a param.
            test "generic struct field 'TFunc :> Fun<'T,'U> dispatches this.F.Invoke via constrained callvirt" {
                let artifact = compileSourceData "StructFieldTFuncDispatch"
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

            // A member call whose object argument is a typar constrained to a PROJECT-LOCAL
            // interface (`'T :> IGetVal`), so the member is looked up through the interface and
            // emitted as `constrained. <typar> callvirt`.
            test "typar object argument constrained to a local interface dispatches via constrained callvirt" {
                let artifact = compileSourceData "TyparInterfaceDispatch"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "7"
                    "constrained typar interface dispatch returns the impl value"
            }

            // The same generic `callIt`, now applied to a `[<Struct>]` argument: the dispatch
            // addresses the struct (`ldloca`) and `constrained. !!T callvirt`s the interface
            // slot, so the JIT resolves the impl directly and nothing boxes.
            test "constrained typar dispatch on a struct arg does not box" {
                let artifact = compileSourceData "TyparInterfaceStructDispatch"
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "9" "constrained struct dispatch returns the impl value"

                // `callIt` is emitted on "Program" under its source name; select it as the
                // one non-`Main` method rather than by name.
                let il = peMethodIlWhere bytes "Program" (fun n -> n <> "Main")

                let hasConstrained =
                    il
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue hasConstrained "callIt IL contains a `constrained.` prefix (typar interface dispatch)"
                Expect.isFalse (Array.contains 0x8Cuy il) "callIt IL contains no `box` (non-allocating struct dispatch)"
            }

            // A typar constrained to a GENERIC interface at a concrete arg (`'T :> IBox<int>`),
            // so the interface slot must be minted on the instantiated `IBox`1<int>` TypeSpec
            // rather than on the bare definition.
            test "constrained typar dispatch on a generic interface (concrete arg) does not box" {
                let artifact = compileSourceData "GenericIfaceConcreteDispatch"
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

            // As above but the interface arg is itself a typar of the enclosing struct
            // (`'S :> IBox<'T>`), so the slot is minted on `IBox`1<!T>`, whose argument is a
            // generic parameter of the instantiation, not a concrete type.
            test "generic struct dispatches through a typar field constrained to a generic interface (typar arg)" {
                let artifact = compileSourceData "GenericStructTyparIface"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "7"
                    "generic-struct typar-arg interface dispatch returns the impl value"
            }

            // A `[<Struct>]` implementing an interface that declares an abstract PROPERTY
            // (`Current`): the impl's getter must be wired by MethodImpl to the interface's
            // getter slot, else the runtime raises TypeLoadException at load.
            test "struct implements an interface with an abstract property and dispatches" {
                let artifact = compileSourceData "StructIfaceProperty"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "10"
                    "struct interface property dispatch returns the impl value"
            }

            // A generic struct implementing a generic local interface AT ITS OWN TYPAR
            // (`Box<'T> : IBox<'T>`): the impl member's return type `'T` must stay in scope
            // and be emitted as a generic MethodImpl. Boxing to `IBox` and calling `Unwrap`.
            test "a generic struct implements a generic local interface at its own typar and dispatches" {
                let artifact = compileSourceData "GenericStructGenericIface"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "42"
                    "generic struct implementing a generic interface at its own typar dispatches the impl value"
            }

            // A project-local class implementing a PROJECT-LOCAL interface (the interface-impl
            // fixtures elsewhere use BCL interfaces, which are found through the external
            // provider), dispatched through the interface.
            test "project-local class implements a project-local interface and dispatches" {
                let artifact = compileSourceData "LocalInterfaceImpl"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "local interface dispatch returns the impl value"
            }

            // The minimal chained method call: `this.I.Get()` parses as
            // `App(LongIdent[this; I; Get], ())`, so the 3-segment chain must be recognised as
            // a method call and not as a property `Get` over-applied to `()`.
            test "chained this.field.Method() call (3-segment) resolves and runs" {
                let artifact = compileSourceData "StructFieldGet"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "7" "chained method call on a struct field works"
            }

            // A 4-segment chain `this.A.B.Bump()` through two intermediate struct
            // fields, mutating the innermost, which exercises the recursive `ldflda`
            // addressing (`this` → `ldflda A` → `ldflda B` → call by address).
            test "4-segment chained method call through nested struct fields mutates in place" {
                let artifact = compileSourceData "NestedStructChain"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "11 12"
                    "nested struct-field mutation persists across calls"
            }

            // A chain anchored on an ordinary local (not `this`): `o.I.Get()`.
            test "chained method call anchored on a local variable resolves" {
                let artifact = compileSourceData "LocallyAnchoredChain"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "locally anchored chained method call works"
            }

            // `for y in s` over a typar source (`'S :> ISeq`, a project-local non-`IEnumerable`
            // interface): `GetEnumerator` dispatches `constrained. !S callvirt ISeq::GetEnumerator`.
            // The enumerator is a concrete struct, so the loop body stays the by-address walk.
            test "for-in over a generic typar source via a custom interface (concrete enumerator)" {
                let artifact = compileSourceData "TyparSeqSource"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "6"
                    "for-in over a typar seq source sums via constrained dispatch"
            }

            // The same, with a GENERIC seq interface at a concrete enumerator
            // (`'S :> IStructSeq<ArrayEnumerator>`): the slot is minted on the instantiated
            // `IStructSeq`1<ArrayEnumerator>` TypeSpec.
            test "for-in over a generic typar source via a generic interface (concrete enumerator)" {
                let artifact = compileSourceData "GenericIfaceTyparSeqSource"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "6"
                    "for-in over a typar source via a generic seq interface sums"
            }

            // A `sumSeq` generic in both source and enumerator
            // (`'S :> IStructSeq<'E> and 'E :> IStructEnumerator`), so `GetEnumerator` AND the
            // enumerator's `MoveNext`/`Current` all dispatch `constrained. callvirt`.
            test "for-in over a fully generic struct seq source" {
                let artifact = compileSourceData "FullyGenericStructSeq"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "6"
                    "fully generic struct seq sums via constrained dispatch"
            }

            // A concrete `[<Struct>] MapSeq` holding a concrete `[<Struct>] ArraySeq` field plus
            // a reference-type closure, walked by `for y in s`, a value-type source with
            // chained struct-field dispatch (`this.Source.GetEnumerator()`/`.MoveNext()`).
            test "concrete struct MapSeq pipeline maps and folds" {
                let artifact = compileSourceData "StructMapSeq"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "2\n4\n6\ndone" "maps the struct pipeline in order"
            }

            // A generic `MapSeq<'S>` (`'S :> IStructSeq<ArrayEnumerator>`) instantiated at a
            // concrete `ArraySeq`: `this.Source.GetEnumerator()` is a constrained-typar
            // dispatch, while `for y in s` walks the concrete `MapSeq`1<ArraySeq>` by address.
            test "for-in over a generic MapSeq wrapping a concrete ArraySeq" {
                let artifact = compileSourceData "GenericMapSeqConcreteSource"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "2\n4\n6\ndone"
                    "generic MapSeq over concrete ArraySeq maps in order"
            }

            // Both `MapSeq` and `MapEnumerator` generic: the enumerator chains an inner
            // enumerator typar `'E`, so `MapEnumerator<'E>.MoveNext`/`.Current` dispatch on a
            // TYPAR FIELD via `constrained. !E callvirt`. The for-in source is concrete.
            test "fully generic struct map pipeline chains a generic enumerator" {
                let artifact = compileSourceData "FullyGenericMapPipeline"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "12" "fully generic map pipeline sums the mapped values"
            }

            // The escape hatch: a generic struct seq/enumerator ALSO implements the BCL
            // `IEnumerable<'T>`/`IEnumerator<'T>`/`IEnumerator`/`IDisposable`, so it boxes
            // transparently when upcast to `IEnumerable<int>` and handed to a .NET API.
            test "generic struct seq implements IEnumerable<'T> escape hatch and enumerates" {
                let artifact = compileSourceData "StructSeqEscapeHatch"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "6" "escape-hatch enumerates via IEnumerable<'T>"
            }

            // The consuming TERMINAL: a free `fold f seed s`, generic over the struct seq `'S`
            // and enumerator `'E`, threading a state accumulator through a reference-type
            // closure per element. Every fixture above instead sums inline.
            test "fold over a fully generic struct seq threads state through a closure" {
                let artifact = compileSourceData "StructSeqFold"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "10"
                    "fold threads state through the closure over a generic struct seq"
            }

            // Vesper.Core's flat<->curried adapters run end-to-end: a `[<Struct>]` with a
            // saturated `Invoke(a,b)`; `curryFun`'s upcast over `Curried`, re-dispatching
            // `f.Invoke(a,b)`; and `flatten`, whose `Invoke(a,b)` walks `f.Invoke(a).Invoke(b)`.
            test "Fun flat dispatch + curryFun/flatten adapters round-trip" {
                let artifact = compileSourceData "FunAdapters"
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "42 42 42"
                    "flat Invoke(a,b), curryFun round-trip, and flatten of a curried value all yield 42"
            }

            // `ofArray |> map |> fold` driven by HAND-WRITTEN struct closures (`AddN :
            // Fun<int,int>` for map, `SumAcc : Fun<int,int,int>` for fold): map dispatches
            // through a `'TFunc` FIELD, fold through a `'TFunc` PARAMETER in one flat 2-arg call.
            test "struct-closure-typar map/fold pipeline runs non-allocating" {
                let artifact = compileSourceData "StructSeqTyparClosurePipeline"
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                // (1+1)+(2+1)+(3+1)+(4+1) = 2+3+4+5 = 14
                Expect.equal (output.Replace("\r", "").Trim()) "14" "map (+1) then fold (+) yields 14"

                // The map node's per-element work: `this.F.Invoke(this.Source.Current)`.
                let curIl = peMethodIlWhere bytes "MapEnumerator`4" (fun n -> n.EndsWith "Current")

                let curConstrained =
                    curIl
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue
                    curConstrained
                    "MapEnumerator.Current IL contains a `constrained.` prefix (Fun typar dispatch)"

                Expect.isFalse (Array.contains 0x8Cuy curIl) "MapEnumerator.Current IL contains no `box`"

                // The fold loop drives `f.Invoke(state, y)`. Several free top-level fns land on
                // "Program", so pick the ones carrying a `constrained.` prefix.
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

            // A STORED binding whose type carries the function typar (`mk : ('TF :>
            // Fun<int,int>) -> Holder<'TF>`, `let h = mk (fun x -> x+1)`) must lay its `'TF`
            // slot out as the `<closure>$` value-struct, not as the `Fun`2` interface.
            test "a stored binding's Fun typar slot is laid out as the <closure>$ value-struct" {
                let artifact = compileSourceData "StoredFunTyparSlotHolder"

                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "42" "h.F.Invoke 41 = 42"

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

                // `h : valuetype Holder`1<arg0>` encodes as FIELD(0x06) GENERICINST(0x15)
                // VALUETYPE(0x11) <token> <argCount> <arg0>. Only arg0 discriminates, because
                // the 0x11 after 0x15 is Holder's own struct marker. 0x11 = VALUETYPE, 0x12 = CLASS.
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

            // The same `ofArray |> map |> fold`, with the hand-written struct closures replaced
            // by SOURCE lambdas (`map (fun x -> x + 1)`, `fold (fun acc x -> acc + x)`), via a
            // STORED `let s1`. Both lower to value-struct closures; output is still 14.
            test "SOURCE-lambda map/fold pipeline runs non-allocating (end-to-end)" {
                let artifact = compileSourceData "StructSeqSourceLambdaPipeline"
                let bytes = Codegen.toBytes artifact

                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                // (1+1)+(2+1)+(3+1)+(4+1) = 2+3+4+5 = 14, identical to the hand-written proof.
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

            // The same source-lambda pipeline fully NESTED into one expression, no stored
            // `let s1`: `fold (fun acc x -> acc + x) 0 (map (fun x -> x + 1) (ofArray xs))`.
            // The mapped seq is a temp slot rather than a module-value field. Still 14, no box.
            test "nested-temp source-lambda map/fold pipeline runs non-allocating" {
                let artifact = compileSourceData "StructSeqNestedTempPipeline"
                let bytes = Codegen.toBytes artifact

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

            // A source lambda at `fold`'s own `'TFunc :> Fun<'State,'T,'State>` PARAMETER,
            // isolated from the map/fold pipeline above: the combinator here stands in for the
            // eventual external `StructSeq.fold`.
            test "a SOURCE lambda through fold's Fun slot lowers to a no-box value-struct" {
                let artifact = compileSourceData "FoldSourceLambdaValueStruct"
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "10" "fold (+) 0 [1;2;3;4] = 10"

                let closureBase = peTypeBaseTypeName bytes (fun n -> n.StartsWith "<closure>$")
                Expect.equal closureBase (ValueSome "System.ValueType") "the fold source lambda is a value-struct"
            }

            // Two chained `map`s whose lambdas have the SAME frozen type `int -> int`
            // (`fun x -> x + 1` then `fun x -> x * 2`), so a type-keyed closure table would
            // bind both `'TFunc` slots to one closure. Only node identity keeps them apart.
            test "multi-map chain lowers each closure to its OWN value-struct slot" {
                let artifact = compileSourceData "StructSeqMultiMapChain"
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "28" "(x+1)*2 mapped then summed = 28"
            }

            // THREE chained `map`s, all `int -> int`: the two-map case above has only one
            // nesting level, so a type-keyed table can survive it by luck. At three, `s3`'s
            // own slot and the once- and twice-nested source slots are indistinguishable by type.
            test "three-map chain keeps each same-typed closure in its OWN slot" {
                let artifact = compileSourceData "StructSeqThreeMapChain"
                let bytes = Codegen.toBytes artifact
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

            // The same source-lambda `ofArray |> map |> fold`, but the combinators are EXTERNAL:
            // the separately built `Vesper.Seq` package, not the inline fixture the tests
            // above compile. `fold`'s enumerator typar `'E` appears only in its `'S` bound.
            test "ofArray |> map |> fold from source lambdas against the external Vesper.Seq package (no box)" {
                let src = dataSource "external-vesper-seq-pipeline"
                let (exitCode, output), bytes = runPackagesInspect [ "Vesper.Seq" ] src
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "14" "(x+1) over [1;2;3;4] summed = 14"

                // The driver's lambdas stay value-structs across the package boundary: the
                // external call instantiates `'TFunc` at the struct, not at the `Fun` interface.
                let closureBases = peClosureBaseTypeNames bytes

                Expect.isNonEmpty closureBases "the driver emits source-lambda closures"

                Expect.isTrue
                    (closureBases |> List.forall (fun b -> b = "ValueType"))
                    (sprintf "both source-lambda closures are value types: %A" closureBases)
            }
        ]
