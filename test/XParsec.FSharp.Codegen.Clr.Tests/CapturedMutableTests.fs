module XParsec.FSharp.Codegen.Clr.Tests.CapturedMutableTests

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Runtime.Loader
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// A `let mutable` captured by an escaping closure (Regions verdict `HeapShared`) is
// rewritten to a `Vesper.Ref<'T>` heap cell the closure and the outer frame share. The
// cell ships in `Vesper.Core.dll`; the consumer PE references it and declares no copy.

[<Tests>]
let tests =
    testList
        "CapturedMutable"
        [
            test "TAST: a captured `let mutable` does NOT prepend a Ref decl (cell lives in Vesper.Core.dll)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let mkCounter z ="
                            "    let mutable n = z"
                            "    fun y ->"
                            "        n <- n + 1"
                            "        n"
                        ]

                let tast = analyse src

                let hasLocalRefDecl =
                    tast.Decls
                    |> EqArray.exists (fun d ->
                        match d with
                        | TDecl.Type td when td.Name = "Ref" || td.Name = "Vesper.Ref" -> true
                        | _ -> false
                    )

                Expect.isFalse
                    hasLocalRefDecl
                    "no Ref<'T> decl synthesised locally, because the cell lives in Vesper.Core.dll"
            }

            test "TAST: a non-captured `let mutable` is not promoted (no rewrite either)" {
                // The cell never crosses a closure boundary, so Regions reports
                // `CallerStack` and the promotion pass skips it.
                let src =
                    String.concat "\n" [ "let useLocal z ="; "    let mutable n = z"; "    n <- 7"; "    n" ]

                let tast = analyse src

                let hasRefDecl =
                    tast.Decls
                    |> EqArray.exists (fun d ->
                        match d with
                        | TDecl.Type td when td.Name = "Ref" || td.Name = "Vesper.Ref" -> true
                        | _ -> false
                    )

                Expect.isFalse hasRefDecl "no Ref<'T> decl synthesised, because the cell stayed local"
            }

            test "TAST: a captured cell's use/write/init sites all lower through Vesper.Ref<_>" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let mkCounter z ="
                            "    let mutable n = z"
                            "    fun y ->"
                            "        n <- n + 1"
                            "        n"
                        ]

                let tast = analyse src

                let rec scanExpr (predicate: TExpr -> bool) (e: TExpr) : bool =
                    if predicate e then
                        true
                    else
                        match e with
                        | TExpr.Lambda(_, b, _, _) -> scanExpr predicate b
                        | TExpr.Let(_, v, b, _, _) -> scanExpr predicate v || scanExpr predicate b
                        | TExpr.App(f, a, _, _) -> scanExpr predicate f || scanExpr predicate a
                        | TExpr.Sequential(items, _, _) -> items |> EqArray.exists (scanExpr predicate)
                        | TExpr.IfThenElse(c, t, e, _, _) ->
                            scanExpr predicate c || scanExpr predicate t || scanExpr predicate e
                        | TExpr.FieldGet(r, _, _, _) -> scanExpr predicate r
                        | TExpr.FieldSet(r, _, v, _, _) -> scanExpr predicate r || scanExpr predicate v
                        | TExpr.RecordCons(fields, _, _) ->
                            fields |> EqArray.exists (fun (_, v) -> scanExpr predicate v)
                        | TExpr.Match(sc, arms, _, _) ->
                            scanExpr predicate sc
                            || arms |> EqArray.exists (fun a -> scanExpr predicate a.Body)
                        | _ -> false

                let isContentsFieldSet =
                    function
                    | TExpr.FieldSet(_, "contents", _, _, _) -> true
                    | _ -> false

                let isContentsFieldGet =
                    function
                    | TExpr.FieldGet(_, "contents", _, _) -> true
                    | _ -> false

                let isRecordConsOfRef =
                    function
                    | TExpr.RecordCons(_, TyRecord("Vesper.Ref`1", _), _) -> true
                    | _ -> false

                let scan p =
                    tast.Decls
                    |> EqArray.exists (fun d ->
                        match d with
                        | TDecl.Let(_, v, _, _) -> scanExpr p v
                        | TDecl.Expression(e, _) -> scanExpr p e
                        | _ -> false
                    )

                Expect.isTrue (scan isContentsFieldSet) "`n <- n + 1` lowered to a FieldSet on `contents`"
                Expect.isTrue (scan isContentsFieldGet) "every bare `n` read lowered to a FieldGet on `contents`"

                Expect.isTrue (scan isRecordConsOfRef) "`let mutable n = z` lowered to a Vesper.Ref<_> RecordCons"
            }

            test "Codegen: consumer PE referencing Vesper.Ref<_> does NOT declare its own copy" {
                // A one-shot inner function applied immediately: both reads and writes
                // lower to `Vesper.Ref<int>::contents`, and the closure value itself is
                // consumed synchronously.
                let src =
                    String.concat
                        "\n"
                        [
                            "let useCounter (z: int) : int ="
                            "    let mutable n = z"
                            "    let bump (v: int) ="
                            "        n <- n + v"
                            "    bump 7"
                            "    n"
                        ]

                // Force the shared Vesper.Core.dll into the Default ALC up front
                // so the consumer's `Vesper.Ref\`1` AssemblyRef resolves at load.
                vesperCoreDll.Value |> ignore

                let _, artifact = compileSource "VesperRefConsumer" src
                let bytes = Codegen.toBytes artifact
                let asm = loadAssembly bytes

                let localRef = asm.GetType "Vesper.Ref`1"

                Expect.isNull
                    localRef
                    "the consumer PE must NOT declare a local Vesper.Ref`1, because the cell lives in Vesper.Core.dll"

                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                Expect.contains
                    refs
                    "Vesper.Core"
                    (sprintf "consumer PE must reference Vesper.Core for Vesper.Ref`1 (refs: %A)" refs)
            }

            test "mkCounter () counter: three invocations see the shared Vesper.Ref<int> cell" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let mkCounter () ="
                            "    let mutable n = 0"
                            "    fun () ->"
                            "        n <- n + 1"
                            "        n"
                            "let c = mkCounter ()"
                            "printfn \"%d\" (c ())"
                            "printfn \"%d\" (c ())"
                            "printfn \"%d\" (c ())"
                        ]

                vesperCoreDll.Value |> ignore

                let _, artifact = compileSource "MkCounter" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 (sprintf "Main returns 0 (output: %s)" output)

                Expect.equal
                    (output.Trim().Replace("\r\n", "\n"))
                    "1\n2\n3"
                    "the three invocations read 1, 2, 3 from the shared Ref<int> cell"
            }

            // Wrapped in an outer function so the promotion fires: a module-level
            // `let mutable` lives in a static field instead and never promotes.
            test "two closures share a single Vesper.Ref<int> cell" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let useTwoClosures () ="
                            "    let mutable n = 0"
                            "    let inc () = n <- n + 1"
                            "    let read () = n"
                            "    inc ()"
                            "    inc ()"
                            "    inc ()"
                            "    read ()"
                            "printfn \"%d\" (useTwoClosures ())"
                        ]

                vesperCoreDll.Value |> ignore

                let _, artifact = compileSource "TwoClosuresShareCell" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 (sprintf "Main returns 0 (output: %s)" output)
                Expect.equal (output.Trim()) "3" "three increments, observed through the read closure, sum to 3"
            }

            // ---- Generic closures (TAST-level acceptance) ----
            // A `Closure`'s `Typars` count is the enclosing static method's, or 0 in `Main`, a
            // top-level value let, or a monomorphic fn. Inner closures inherit it verbatim.

            let discover (src: string) : Emit.Closure list =
                let ctx, tast = analyseWithCtx src
                let pools = Freeze.run ctx tast
                let pool = TastPoolBuilder.openOver pools
                let lowered0 = Emit.lower (List.ofArray (TastAccessor.roots pool))
                // No lambda here carries a value-struct verdict, but discovery takes the
                // pooled table, so index it node-keyed the way the real layout path does.
                let funVerdicts =
                    pools.FunVerdicts
                    |> Array.map (fun (id, v) -> ({ Pool = pool; Id = id }: TastAccessor.ExprId), v)
                    |> DenseTable.index

                let moduleMembers = Map.ofArray pools.ModuleMembers
                let closureReprs = Map.ofArray pools.ClosureReprs
                let genericFnSchemes = Map.ofArray pools.GenericFnSchemes

                // Only discovered closures are asserted, so the module class name is
                // immaterial, because any valid `ModuleKey` yields the same set.
                let programClass =
                    SymbolKeyOps.moduleKeyOf (ModuleContainer.InNamespace NamespaceKey.Global) "Program"

                let emissions = Emit.emissions moduleMembers programClass lowered0
                let moduleValues = Emit.collectModuleValues emissions lowered0

                let moduleValueKeys =
                    HashSet<BoundVarId>(moduleValues |> List.map (fun mv -> mv.Key))

                let fns0 = CompiledFns.gather lowered0
                let eligible = Emit.staticEligible moduleValueKeys fns0
                let lowered = Emit.bridgeStaticFnEscapes eligible fns0 lowered0

                let staticFns =
                    Emit.collectStaticFns emissions genericFnSchemes eligible (CompiledFns.gather lowered)

                let typarsMap = Dictionary<BoundVarId, int>()

                for fn in staticFns do
                    typarsMap.[fn.Key] <- Emit.staticFnTypars fn

                let closures, _ =
                    Emit.discoverClosures
                        (Emit.ClosureNamer())
                        eligible
                        moduleValueKeys
                        typarsMap
                        funVerdicts
                        closureReprs
                        lowered
                        []

                closures

            test "a closure inside a generic static fn carries that fn's typars" {
                // `mkConst` is generic (`'a -> unit -> 'a`) and its inner `fun () -> x`
                // captures `x: 'a`. The inner `let f = …` breaks the lambda peel, so
                // `mkConst` is a 1-arg static fn and the inner lambda is left as a closure.
                let src =
                    String.concat "\n" [ "let mkConst x ="; "    let f = fun () -> x"; "    f" ]

                let closures = discover src

                Expect.equal (List.length closures) 1 "exactly one closure: `fun () -> x` inside `mkConst`"
                let c = List.head closures

                Expect.equal c.Typars 1 "the inner closure inherits `mkConst`'s one typar (`'a`)"
            }

            test "a closure inside a monomorphic static fn has empty Typars" {
                // `x + 1` forces `x: int`, so `f` is `int -> int` with no typars. `g`
                // captures `x` and is a closure, because static-method candidates are top-level only.
                let src = String.concat "\n" [ "let f x ="; "    let g () = x + 1"; "    g ()" ]

                let closures = discover src

                Expect.isNonEmpty closures "the `let g () = ...` lambda is a closure"

                for c in closures do
                    Expect.equal
                        c.Typars
                        0
                        (sprintf "closure %s should have zero Typars (enclosing fn is monomorphic)" c.Name)
            }

            // `Repr` is the front-end verdict that a stack (readonly-struct) shape is
            // ADMISSIBLE: Axis 1 `LocalStack` ∧ Axis 2 `StackOnlyEligible`. These assert
            // the classification; whether codegen takes it is a separate gate.
            test "a frame-local applied closure carries Repr = Stack" {
                // `f` is `LocalStack` (confined to `useLocal`) and its only use is
                // the direct callee of `f 3`, so Axis 2 is `StackOnlyEligible`.
                let src =
                    String.concat "\n" [ "let useLocal () ="; "    let f x = x + 1"; "    f 3" ]

                let closures = discover src

                Expect.equal (List.length closures) 1 "exactly one closure: `f` inside `useLocal`"
                Expect.equal (List.head closures).Repr ClosureRepr.Stack "frame-local + no heap channel ⇒ Stack"
            }

            test "a closure stored in a ValueTuple carries Repr = Heap" {
                // `g` is frame-local, but `(g, g)` is a `ValueTuple`, which can't hold a
                // ref-struct field, so Axis 2 is pinned to `RequiresHeapRepr`.
                let src =
                    String.concat "\n" [ "let f () ="; "    let g = fun x -> x"; "    (g, g)" ]

                let closures = discover src

                Expect.equal (List.length closures) 1 "exactly one closure: `g`"
                Expect.equal (List.head closures).Repr ClosureRepr.Heap "tuple containment ⇒ Heap"
            }

            test "a generic closure also carries the Repr field" {
                // Capturing a generic typar value rides the `Vesper.Fun<_,_>` channel, so
                // Axis 2 is `RequiresHeapRepr` and the conjunction is `Heap`, in contrast to
                // the monomorphic `f` above, which is `Stack`.
                let src =
                    String.concat "\n" [ "let mkConst x ="; "    let f = fun () -> x"; "    f" ]

                let closures = discover src

                Expect.equal (List.length closures) 1 "exactly one closure: `f` inside `mkConst`"
                let c = List.head closures
                Expect.equal c.Typars 1 "the closure is generic over `mkConst`'s `'a`"
                Expect.equal c.Repr ClosureRepr.Heap "generic typar capture ⇒ RequiresHeapRepr ⇒ Heap"
            }

            test "an anonymous lambda with no bound variable defaults to Repr = Heap" {
                // In `(fun x -> x + 1) 5` the lambda has no `SelfKey`, so the
                // snapshot can't key it; it falls back to the emitted `Heap` shape.
                let src = "printfn \"%d\" ((fun x -> x + 1) 5)"

                let closures = discover src

                Expect.isNonEmpty closures "the inline lambda is discovered"

                for c in closures do
                    Expect.equal c.Repr ClosureRepr.Heap (sprintf "anonymous closure %s defaults to Heap" c.Name)
            }

            test "an inner closure inherits the enclosing closure's Typars" {
                // `mkPair` is generic (`'a -> ('b -> 'a)`); the `let mid = …; mid` body stops
                // the lambda peel after `x`, and `let mid y = let inner = fun z -> x in inner`
                // produces two nested closures, both inheriting mkPair's typars.
                let src =
                    String.concat
                        "\n"
                        [
                            "let mkPair x ="
                            "    let mid y ="
                            "        let inner = fun z -> x"
                            "        inner"
                            "    mid"
                        ]

                let closures = discover src

                Expect.equal (List.length closures) 2 "two closures: `mid` and `inner`"

                for c in closures do
                    Expect.isTrue
                        (c.Typars > 0)
                        (sprintf "closure %s should inherit mkPair's typars (count > 0)" c.Name)

                let inner = closures.[0] // registered first (leaves-first walk)
                let outer = closures.[1]

                Expect.equal outer.Typars inner.Typars "both closures carry the same typar count (inherited verbatim)"
            }

            test "a closure in Main / top-level expression has empty Typars" {
                // The lambda resides in `Main`, which is not a generic static method.
                let src = "printfn \"%d\" ((fun x -> x + 1) 5)"

                let closures = discover src

                Expect.isNonEmpty closures "the inline `fun x -> x + 1` is a closure"

                for c in closures do
                    Expect.equal c.Typars 0 (sprintf "closure %s in Main should have zero Typars" c.Name)
            }

            // `mkConst` is the genuine trigger: no operator applies to `x`, so it stays
            // `'a -> unit -> 'a` and the typar reaches the capture-field signature.
            // (`let mkAdder x = fun y -> y + x` is NOT, because `(+)`'s `default ^T1: int` forces int.)
            test "Higher-order returning a closure with an un-pinned typar" {
                // The inner `fun () -> x` is a generic closure over `'a`: capture field `!0`,
                // ctor + Invoke through `MemberRef`s on `<closure>$0<!!0>` at the construction
                // site inside `mkConst`, and `<closure>$0<!0>` from inside its own `Invoke`.
                let src =
                    String.concat
                        "\n"
                        [
                            "let mkConst x ="
                            "    let f = fun () -> x"
                            "    f"
                            "let always10 = mkConst 10"
                            "printfn \"%d\" (always10 ())"
                        ]

                vesperCoreDll.Value |> ignore

                let _, artifact = compileSource "MkConst" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 (sprintf "Main returns 0 (output: %s)" output)

                Expect.equal
                    (output.Trim())
                    "10"
                    "the inner closure carries its capture's type through the static-method typar"
            }

            // One closure `TypeDef` with two `TypeSpec` parents at the construction sites
            // (`<int>` and `<string>`): polymorphic, not monomorphised per call site.
            test "Generic closure used at two distinct instantiations (int + string)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let mkConst x ="
                            "    let f = fun () -> x"
                            "    f"
                            "let always10 = mkConst 10"
                            "let alwaysHi = mkConst \"hi\""
                            "printfn \"%d\" (always10 ())"
                            "printfn \"%s\" (alwaysHi ())"
                        ]

                vesperCoreDll.Value |> ignore

                let _, artifact = compileSource "MkConstTwoInsts" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 (sprintf "Main returns 0 (output: %s)" output)

                Expect.equal
                    (output.Trim().Replace("\r\n", "\n"))
                    "10\nhi"
                    "two instantiations of the same generic closure print their respective captures"
            }

            test "Generic closure `TypeDefinition` carries `GenericParam` rows" {
                let src =
                    String.concat "\n" [ "let mkConst x ="; "    let f = fun () -> x"; "    f" ]

                vesperCoreDll.Value |> ignore

                let _, artifact = compileSource "MkConstIL" src
                let bytes = Codegen.toBytes artifact

                use peReader = openPe bytes
                let md = peReader.GetMetadataReader()

                let closureTd =
                    md.TypeDefinitions
                    |> Seq.tryFind (fun h ->
                        let td = md.GetTypeDefinition h
                        let name = md.GetString td.Name
                        name.StartsWith "<closure>$"
                    )

                match closureTd with
                | None -> failtest "no <closure>$… TypeDefinition emitted"
                | Some h ->
                    let td = md.GetTypeDefinition h
                    let name = md.GetString td.Name

                    Expect.stringContains
                        name
                        "`1"
                        (sprintf "closure metadata name carries the arity suffix `1; got '%s'" name)

                    let gpCount = td.GetGenericParameters().Count

                    Expect.equal
                        gpCount
                        1
                        (sprintf "closure `TypeDefinition` declares exactly one generic parameter (got %d)" gpCount)
            }

            // The blob is the closure's own typar, not the concrete type the construction
            // sites supply.
            test "Generic closure capture field signature encodes as `!0`" {
                let src =
                    String.concat "\n" [ "let mkConst x ="; "    let f = fun () -> x"; "    f" ]

                vesperCoreDll.Value |> ignore

                let _, artifact = compileSource "MkConstILField" src
                let bytes = Codegen.toBytes artifact

                use peReader = openPe bytes
                let md = peReader.GetMetadataReader()

                let closureTd =
                    md.TypeDefinitions
                    |> Seq.find (fun h ->
                        let td = md.GetTypeDefinition h
                        let name = md.GetString td.Name
                        name.StartsWith "<closure>$"
                    )

                let td = md.GetTypeDefinition closureTd

                let captureField =
                    td.GetFields()
                    |> Seq.tryFind (fun fh ->
                        let fd = md.GetFieldDefinition fh
                        (md.GetString fd.Name) = "capture0"
                    )

                match captureField with
                | None -> failtest "no `capture0` field on the closure type"
                | Some fh ->
                    let fd = md.GetFieldDefinition fh
                    let sigBlob = md.GetBlobBytes fd.Signature

                    // ECMA-335 II.23.2.4: FIELD (0x06) | ELEMENT_TYPE_VAR (0x13) | index (compressed int 0x00)
                    Expect.equal
                        (sigBlob.Length)
                        3
                        (sprintf "capture0 signature is 3 bytes (got %d: %s)" sigBlob.Length (formatIlBytes sigBlob))

                    Expect.equal sigBlob.[0] 0x06uy "byte 0 = FIELD calling convention (0x06)"

                    Expect.equal
                        sigBlob.[1]
                        0x13uy
                        "byte 1 = ELEMENT_TYPE_VAR (0x13) — a *type* (not method) generic parameter"

                    Expect.equal sigBlob.[2] 0x00uy "byte 2 = generic parameter index 0 (the closure's own `!0`)"
            }

            // For `mkConst` the `Fun\`2` instantiation is `(unit, 'a)`: ParamTy encodes
            // concretely and ResultTy is the closure's own `!0`. Asserted by scanning the
            // `InterfaceImpl` TypeSpec blob for an ELEMENT_TYPE_VAR 0 marker.
            test "Generic closure `Fun\`2` InterfaceImpl uses the closure's own typar marker" {
                let src =
                    String.concat "\n" [ "let mkConst x ="; "    let f = fun () -> x"; "    f" ]

                vesperCoreDll.Value |> ignore

                let _, artifact = compileSource "MkConstILIface" src
                let bytes = Codegen.toBytes artifact

                use peReader = openPe bytes
                let md = peReader.GetMetadataReader()

                let closureTd =
                    md.TypeDefinitions
                    |> Seq.find (fun h ->
                        let td = md.GetTypeDefinition h
                        let name = md.GetString td.Name
                        name.StartsWith "<closure>$"
                    )

                let td = md.GetTypeDefinition closureTd

                let ifaceImpls = td.GetInterfaceImplementations()

                Expect.isGreaterThan ifaceImpls.Count 0 "closure declares at least one InterfaceImpl"

                let containsSelfTypar =
                    ifaceImpls
                    |> Seq.exists (fun iih ->
                        let ii = md.GetInterfaceImplementation iih

                        if ii.Interface.Kind = HandleKind.TypeSpecification then
                            let ts = md.GetTypeSpecification(TypeSpecificationHandle.op_Explicit ii.Interface)
                            let blob = md.GetBlobBytes ts.Signature
                            // 0x13 = VAR (a type generic parameter); the next byte is its
                            // compressed index.
                            let mutable found = false
                            let mutable i = 0

                            while not found && i < blob.Length - 1 do
                                if blob.[i] = 0x13uy && blob.[i + 1] = 0x00uy then
                                    found <- true

                                i <- i + 1

                            found
                        else
                            false
                    )

                Expect.isTrue
                    containsSelfTypar
                    "the `Fun\`2` InterfaceImpl signature contains an ELEMENT_TYPE_VAR 0 marker (the closure's own `!0`)"
            }

            // `a` and `b` each capture `x: 'a` independently and route through a third closure
            // (`fun cond ->`) that selects between them, so neither can be DCE'd: three
            // generic closures in `mkPair`'s body, all inheriting its single typar.
            test "Sibling closures inside a generic static fn each carry the typar set" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let mkPair x ="
                            "    let a = fun () -> x"
                            "    let b = fun () -> x"
                            "    fun cond -> if cond then a () else b ()"
                            "let pickFrom10 = mkPair 10"
                            "printfn \"%d\" (pickFrom10 true)"
                            "printfn \"%d\" (pickFrom10 false)"
                        ]

                vesperCoreDll.Value |> ignore

                let _, artifact = compileSource "MkPair" src
                let bytes = Codegen.toBytes artifact

                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 (sprintf "Main returns 0 (output: %s)" output)

                Expect.equal
                    (output.Trim().Replace("\r\n", "\n"))
                    "10\n10"
                    "both branches of the result closure read the captured `'a` (= 10)"

                use peReader = openPe bytes
                let md = peReader.GetMetadataReader()

                let closureTds =
                    md.TypeDefinitions
                    |> Seq.choose (fun h ->
                        let td = md.GetTypeDefinition h
                        let name = md.GetString td.Name

                        if name.StartsWith "<closure>$" then
                            Some(name, td.GetGenericParameters().Count)
                        else
                            None
                    )
                    |> List.ofSeq

                Expect.equal
                    (List.length closureTds)
                    3
                    (sprintf "three closure TypeDefs emitted (got %A)" (closureTds |> List.map fst))

                for (name, gpCount) in closureTds do
                    Expect.stringContains name "`1" (sprintf "%s carries arity suffix" name)

                    Expect.equal gpCount 1 (sprintf "%s declares exactly one generic parameter (got %d)" name gpCount)
            }
        ]
