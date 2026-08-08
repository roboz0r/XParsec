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

// Captured-mutable promotion: `let mutable` captured by an
// escaping closure is rewritten to a `Vesper.Ref<'T>` cell so the closure and
// outer frame share the same heap-allocated reference. The cell type lives in
// `Vesper.Core.dll`; the codegen resolves it through the cross-package record
// path; the consumer PE declares no copy. The promotion fires only for
// bindings whose Regions verdict is `HeapShared` — uncaptured `let mutable`
// (CallerStack) stays a stack-local.
//
// These tests cover the promotion's TAST-level guarantees: the consumer's
// `Decls` carries no `Ref` type, the binding-site rewrite, and the `n <- v`
// ↦ FieldSet + `n` ↦ FieldGet lowering. The generic-closure synthesis
// runtime acceptance lives at the bottom of this file.

[<Tests>]
let tests =
    testList
        "CapturedMutable"
        [
            test "TAST: a captured `let mutable` does NOT prepend a Ref decl (cell lives in Vesper.Core.dll)" {
                // Previously the pass prepended a synthetic `type Ref<'T> = { mutable
                // contents: 'T }` so the codegen's user-record path could find it.
                // That stopgap was retired: the cell type ships in
                // `Vesper.Core.dll` and the codegen resolves it through
                // `externalRecordRef`. The consumer PE's `Decls` carries no
                // `Ref` declaration regardless of whether the promotion fires.
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

                Expect.isFalse hasLocalRefDecl "no Ref<'T> decl synthesised locally — the cell lives in Vesper.Core.dll"
            }

            test "TAST: a non-captured `let mutable` is not promoted (no rewrite either)" {
                // The cell never crosses a closure boundary, so Regions reports
                // CallerStack and the promotion pass skips it. No rewrite, and
                // (as before) no Ref decl synthesised.
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

                Expect.isFalse hasRefDecl "no Ref<'T> decl synthesised — the cell stayed local"
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

            // When the captured-mutable pass fires, the consumer PE *references*
            // `Vesper.Ref\`1` from `Vesper.Core.dll` (mints a `TypeRef`) and
            // does *not* declare its own copy. The IL-level reflection check
            // below is the acceptance; the runtime
            // end-to-end variants (`mkCounter ()` counter, two-closures-
            // share-the-cell) run further down — they were unblocked once
            // unit-param closures and generic-closure synthesis landed.
            test "Codegen: consumer PE referencing Vesper.Ref<_> does NOT declare its own copy" {
                // The simplest cell-promoting program whose codegen path doesn't
                // hit the deferred closure-emit gaps: a single `let mutable n`
                // captured by a one-shot inner function that's then *applied
                // immediately and discarded*. Both reads/writes lower to
                // `Vesper.Ref<int>::contents`; the closure's invocation completes
                // before the outer function returns, so escape analysis flags it
                // — but the closure value itself is consumed synchronously.
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

                // Acceptance: no local `Vesper.Ref\`1` TypeDef.
                let localRef = asm.GetType "Vesper.Ref`1"

                Expect.isNull
                    localRef
                    "the consumer PE must NOT declare a local Vesper.Ref`1 — the cell lives in Vesper.Core.dll"

                // The reference table must include `Vesper.Core` (the
                // `externalRecordRef` path mints a TypeRef whose AssemblyRef
                // resolves to it).
                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                Expect.contains
                    refs
                    "Vesper.Core"
                    (sprintf "consumer PE must reference Vesper.Core for Vesper.Ref`1 (refs: %A)" refs)
            }

            // Runtime: the captured-mutable cell
            // survives across multiple invocations of the escaping closure. The
            // `unit -> int` Invoke + `mkCounter ()` static-method call both
            // require the unit-parameter closure peel (peelLambda /
            // discoverClosures); the cell itself is the `Vesper.Ref\`1` from
            // `Vesper.Core.dll`.
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

            // Runtime: two closures captured
            // by the same `let mutable` cell observe each other's writes — the
            // `Vesper.Ref\`1` is shared. Both closures take a unit parameter.
            // Wrapped in an outer
            // function (`useTwoClosures`) so RefCellPromotion fires for the
            // local `let mutable n` (module-level mutables don't promote — they
            // live in a static field, a different mechanism).
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
            //
            // Each `Closure` carries a `Typars` set: the enclosing static
            // method's typars (`StaticMethodRef.Typars`) at discovery, or `[]`
            // for closures resident in `Main` / a top-level value let, or a
            // closure inside a monomorphic static method. Inner closures
            // inherit the enclosing closure's set verbatim.
            //
            // The helper bundles the `lower + collectModuleValues +
            // collectStaticFns + typar map + discoverClosures` recipe each
            // test uses.

            let discover (src: string) : Emit.Closure list =
                let ctx, tast = analyseWithCtx src
                let pools = Freeze.run ctx tast
                let pool = TastPoolBuilder.openOver pools
                let lowered0 = Emit.lower (List.ofArray (TastAccessor.roots pool))
                // No lambda in these sources carries a value-struct verdict, but the
                // discovery signature is the pooled one, so feed it the pooled table —
                // node-keyed, as `Layout.buildFile` does.
                let funVerdicts =
                    pools.FunVerdicts
                    |> Array.map (fun (id, v) -> ({ Pool = pool; Id = id }: TastAccessor.ExprId), v)
                    |> DenseTable.index

                // The bound-variable-keyed tables at the dense id the CLR emit's API takes, indexed
                // exactly as `Layout.buildFile` indexes them, so this harness cannot drift
                // from the real path.
                let moduleMembers = Map.ofArray pools.ModuleMembers
                let closureReprs = Map.ofArray pools.ClosureReprs
                let genericFnSchemes = Map.ofArray pools.GenericFnSchemes

                // A holderless static fn keys on the Program holder, mirroring
                // `Layout.build` (this test asserts only discovered closures, so the
                // holder name is immaterial — any valid `ModuleKey` yields the same set).
                let programHolder =
                    SymbolKeyOps.moduleKeyOf (ModuleHolder.InNamespace NamespaceKey.Global) "Program"

                let emissions = Emit.emissions moduleMembers programHolder lowered0
                let moduleValues = Emit.collectModuleValues emissions lowered0

                let moduleValueKeys =
                    HashSet<BoundVarId>(moduleValues |> List.map (fun mv -> mv.Key))

                // Mirror `HolderPlan.create`: the capture-only eligible set drives
                // bridging, then `collectStaticFns` projects it onto the bridged decls.
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
                // The canonical generic-closure case: `mkConst`
                // is generic (`'a -> unit -> 'a`);
                // its inner `fun () -> x` captures `x: 'a` and must become
                // generic over `mkConst`'s `'a`. An inner `let f = …` breaks
                // `peelLambda`'s otherwise-straight chain so `mkConst` is a
                // 1-arg static fn (not 2-arg) and the inner lambda is left as
                // a closure for `discoverClosures` to find.
                let src =
                    String.concat "\n" [ "let mkConst x ="; "    let f = fun () -> x"; "    f" ]

                let closures = discover src

                Expect.equal (List.length closures) 1 "exactly one closure: `fun () -> x` inside `mkConst`"
                let c = List.head closures

                Expect.equal c.Typars 1 "the inner closure inherits `mkConst`'s one typar (`'a`) — count 1"
            }

            test "a closure inside a monomorphic static fn has empty Typars" {
                // `f x = let g () = x + 1 in g ()` — `x + 1` forces `x: int`,
                // so `f` is `int -> int` (no typars). `g` is a local function
                // that captures `x` and is therefore a closure (not a
                // static-method candidate — those are top-level only).
                let src = String.concat "\n" [ "let f x ="; "    let g () = x + 1"; "    g ()" ]

                let closures = discover src

                Expect.isNonEmpty closures "the `let g () = ...` lambda is a closure"

                for c in closures do
                    Expect.equal
                        c.Typars
                        0
                        (sprintf "closure %s should have zero Typars (enclosing fn is monomorphic)" c.Name)
            }

            // The Regions stack/heap verdict (Axis 1 `LocalStack` ∧ Axis 2
            // `StackOnlyEligible`) reaches `Emit.Closure.Repr` via
            // `TastFile.ClosureReprs`. The field is inert (emission ignores it),
            // so these assert the classification only — IL is unchanged.
            // The shapes mirror the RegionsTests.
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
                // Even though `g` is frame-local, the `(g, g)` tuple containment
                // (a `ValueTuple`, which can't hold a ref-struct field) pins Axis 2
                // to `RequiresHeapRepr`.
                let src =
                    String.concat "\n" [ "let f () ="; "    let g = fun x -> x"; "    (g, g)" ]

                let closures = discover src

                Expect.equal (List.length closures) 1 "exactly one closure: `g`"
                Expect.equal (List.head closures).Repr ClosureRepr.Heap "tuple containment ⇒ Heap"
            }

            test "a generic closure also carries the Repr field" {
                // The checkpoint requires both monomorphic and generic closures to
                // carry the verdict. `mkConst`'s inner `fun () -> x` is generic over
                // `'a` (Typars = 1); capturing a generic typar value rides the
                // `Vesper.Fun<_,_>` channel, so Axis 2 is `RequiresHeapRepr` and the
                // conjunction is `Heap` — the field is keyed for generic closures
                // (contrast the monomorphic `f` above, which is `Stack`).
                let src =
                    String.concat "\n" [ "let mkConst x ="; "    let f = fun () -> x"; "    f" ]

                let closures = discover src

                Expect.equal (List.length closures) 1 "exactly one closure: `f` inside `mkConst`"
                let c = List.head closures
                Expect.equal c.Typars 1 "the closure is generic over `mkConst`'s `'a`"
                Expect.equal c.Repr ClosureRepr.Heap "generic typar capture ⇒ RequiresHeapRepr ⇒ Heap"
            }

            test "an anonymous lambda with no bound variable defaults to Repr = Heap" {
                // `(fun x -> x + 1) 5` — the lambda has no `SelfKey`, so the
                // snapshot can't key it; it falls back to the emitted `Heap` shape.
                let src = "printfn \"%d\" ((fun x -> x + 1) 5)"

                let closures = discover src

                Expect.isNonEmpty closures "the inline lambda is discovered"

                for c in closures do
                    Expect.equal c.Repr ClosureRepr.Heap (sprintf "anonymous closure %s defaults to Heap" c.Name)
            }

            test "an inner closure inherits the enclosing closure's Typars" {
                // `mkPair` is generic (`'a -> ('b -> 'a)`). The body is a
                // `let mid = …; mid` so peelLambda stops after `x`; the body's
                // `let mid y = let inner = fun z -> x in inner` produces *two*
                // nested closures, both of which must inherit mkPair's typars
                // (inner closures inherit the parent closure's typars verbatim).
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

                // Both closures carry *the same* typar count — the inner closure's
                // typars are not re-derived but inherited verbatim (closures carry
                // the enclosing method's typar count).
                let inner = closures.[0] // registered first (leaves-first walk)
                let outer = closures.[1]

                Expect.equal outer.Typars inner.Typars "both closures carry the same typar count (inherited verbatim)"
            }

            test "a closure in Main / top-level expression has empty Typars" {
                // An anonymous lambda in a top-level expression (`printfn` arg)
                // resides in `Main`, so its `Typars` is empty — `Main` is not
                // a generic static method.
                let src = "printfn \"%d\" ((fun x -> x + 1) 5)"

                let closures = discover src

                Expect.isNonEmpty closures "the inline `fun x -> x + 1` is a closure"

                for c in closures do
                    Expect.equal c.Typars 0 (sprintf "closure %s in Main should have zero Typars" c.Name)
            }

            // Generic-closure synthesis:
            // a higher-order function that *returns* a closure with an
            // un-pinned typar in its capture-field signature emits the inner
            // closure as a generic `TypeDefinition` over the enclosing static
            // method's typars, instantiated per call site. The genuine
            // generic-closure case: the inner closure captures a value of an
            // *unconstrained* typar (`'a`). No operators apply to `x` inside
            // the inner closure, so F#'s static-member-default mechanism never
            // kicks in; `mkConst` stays `'a -> unit -> 'a` and the static
            // method's typar flows through into the closure's capture-field
            // signature.
            //
            // (`let mkAdder x = fun y -> y + x` is NOT a generic-closure case
            // in F# — the `+` operator's `default ^T1: int` clause in
            // `Vesper.Core/ops-platform.fsi` forces `int` in the absence of
            // other type direction. `mkConst` cannot be defaulted; it is the
            // genuine trigger.)
            test "Higher-order returning a closure with an un-pinned typar" {
                // Generic-closure runtime acceptance: `mkConst` is
                // a generic 1-arg static method (the inner `let f = …; f`
                // breaks `peelLambda`'s chain — same shape the TAST test
                // above uses). The inner `fun () -> x` is a *generic closure*
                // over `'a` — its capture-field signature is `!0`, its ctor +
                // Invoke route through `MemberRef`s on `<closure>$0<!!0>` at
                // the construction site (inside `mkConst`'s body, `!!i` is the
                // method's typar) and `<closure>$0<!0>` from inside its own
                // `Invoke` body.
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

            // One generic closure `TypeDef`,
            // two `TypeSpec` parents at the construction sites (`<int>` and
            // `<string>`) — proving the closure type is genuinely polymorphic,
            // not silently monomorphised per call site.
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

            // The emitted PE carries a
            // real generic `TypeDefinition` for the closure (one or more
            // `GenericParam` rows) — the prior monomorphic-only path would
            // either have failed encoding (the `cannot encode SemType: TyVar`
            // error) or emitted a non-generic closure type.
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
                        (sprintf "closure metadata name carries the arity suffix (`1) — got '%s'" name)

                    let gpCount = td.GetGenericParameters().Count

                    Expect.equal
                        gpCount
                        1
                        (sprintf "closure `TypeDefinition` declares exactly one generic parameter (got %d)" gpCount)
            }

            // The capture field's signature
            // blob is `FIELD (0x06) VAR (0x13) 0` — the closure's own typar,
            // not the concrete BCL type the construction sites supply. This
            // pins the "generic by construction, not monomorphised" property
            // at the signature level.
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

            // The closure's `Vesper.Fun\`2`
            // `InterfaceImpl` signature carries the closure type's own typar
            // markers (`!0` for `ParamTy = unit`-isn't-a-typar, `!0` for
            // `ResultTy = 'a`). For mkConst, ParamTy is the F# unit type
            // (concrete encoding via `eUnit`), and ResultTy is the typar `!0`.
            // We assert by scanning the TypeSpec blob for an ELEMENT_TYPE_VAR 0
            // marker — the prior monomorphic-only path would have hit
            // `cannot encode SemType: TyVar` and never reached IL.
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

                // The `Fun\`2` interface is a TypeSpec (generic instantiation).
                // Find an impl whose interface handle is a TypeSpec and read its
                // signature blob — scan for an ELEMENT_TYPE_VAR (0x13) byte
                // followed by index 0 (the closure's `!0`).
                let containsSelfTypar =
                    ifaceImpls
                    |> Seq.exists (fun iih ->
                        let ii = md.GetInterfaceImplementation iih

                        if ii.Interface.Kind = HandleKind.TypeSpecification then
                            let ts = md.GetTypeSpecification(TypeSpecificationHandle.op_Explicit ii.Interface)
                            let blob = md.GetBlobBytes ts.Signature
                            // Walk the blob: every 0x13 means VAR (type generic
                            // parameter); the byte immediately after is the
                            // (compressed) index.
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

            // Two sibling closures in the same
            // generic static method's body each carry the typar set
            // independently (and share the captured value). The
            // original `mkPair` returns a tuple; Vesper's tuple emission isn't
            // on the critical path, so the test routes both siblings
            // through a third closure (`fun cond ->`) that selects between
            // them — exercising three generic closures in `mkPair`'s body, all
            // inheriting its single typar.
            test "Sibling closures inside a generic static fn each carry the typar set" {
                // `mkPair` is generic over `'a`; `a` and `b` each capture
                // `x: 'a` independently and route through a third closure
                // (the result `fun cond ->`) so both siblings are live —
                // neither can be DCE'd. The result closure's `Invoke` body
                // selects between `a` and `b` by the boolean parameter and
                // returns the captured `'a`. The whole chain is instantiated
                // at `int` by `mkPair 10`.
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

                // Three closures total: `a`, `b`, and `fun cond -> …`. All three
                // are generic over `mkPair`'s typar — pin the count + arity
                // suffix + GenericParam shape for each.
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
