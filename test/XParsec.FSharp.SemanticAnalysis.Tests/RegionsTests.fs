module XParsec.FSharp.SemanticAnalysis.Tests.RegionsTests

open Expecto
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    let ctx, _ = Pipeline.analyseSemWithContext MockBuiltins.provider input lexed file
    ctx, file

/// Find the headPat NodeKey of a module-level binding by name.
let private headKeyOf (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) (name: string) : NodeKey =
    let rec tryBindings (bindings: System.Collections.Immutable.ImmutableArray<Binding<SyntaxToken>>) =
        let mutable found = ValueNone
        let mutable i = 0

        while found.IsNone && i < bindings.Length do
            let b = bindings.[i]

            match b.headPat with
            | Pat.NamedSimple t when ctx.NameOf t = name -> found <- ValueSome(CstKeys.ofPat b.headPat)
            | _ -> ()

            i <- i + 1

        found

    let tryElems (elems: ModuleElems<SyntaxToken>) =
        let mutable found = ValueNone
        let mutable i = 0

        while found.IsNone && i < elems.Length do
            match elems.[i] with
            | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
                found <- tryBindings bindings
            | _ -> ()

            i <- i + 1

        found

    let result =
        match file with
        | ImplementationFile.AnonymousModule elems -> tryElems elems
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) -> tryElems elems
        | _ -> ValueNone

    match result with
    | ValueSome k -> k
    | ValueNone -> failwithf "binding %s not found at module level" name

let private escapeOf (input: string) (name: string) : EscapeState option =
    let ctx, file = analyse input
    let key = headKeyOf ctx file name

    match ctx.Bindings.Escape.TryGetValue key with
    | ValueSome s -> Some s
    | ValueNone -> None

/// Region of a binding (via its headPat TyVar). Used by tests that need
/// to check identity (e.g. two names sharing a region) rather than just
/// the escape state.
let private regionOf (input: string) (name: string) : RegionId option =
    let ctx, file = analyse input
    let key = headKeyOf ctx file name

    match ctx.Bindings.TypeVar.TryGetValue key with
    | ValueSome tv ->
        let root = UnionFind.find tv

        if root.Region.Raw >= 0 then Some root.Region else None
    | ValueNone -> None

/// Axis-2 representation verdict of a *module-level* binding.
/// Mirrors `escapeOf` over the `Repr` side table.
let private reprOf (input: string) (name: string) : RegionRepr option =
    let ctx, file = analyse input
    let key = headKeyOf ctx file name

    match ctx.Bindings.Repr.TryGetValue key with
    | ValueSome r -> Some r
    | ValueNone -> None

/// Find the headPat NodeKey of the first `let`-binding named `name` reachable
/// from `e` (searching binding RHSs, let bodies, and lambda bodies). Lets the
/// repr tests key a *nested* closure (`let g = fun x -> x` inside a function).
let rec private findLetKey (ctx: PassContext) (name: string) (e: Expr<SyntaxToken>) : NodeKey voption =
    match e with
    | Expr.LetOrUse(bindings = bs; body = body) ->
        let mutable found = ValueNone
        let mutable i = 0

        while found.IsNone && i < bs.Length do
            let b = bs.[i]

            match b.headPat with
            | Pat.NamedSimple t when ctx.NameOf t = name -> found <- ValueSome(CstKeys.ofPat b.headPat)
            | _ -> found <- findLetKey ctx name b.expr

            i <- i + 1

        match found with
        | ValueSome _ -> found
        | ValueNone ->
            match body with
            | ValueSome b -> findLetKey ctx name b
            | ValueNone -> ValueNone
    | Expr.Fun(expr = body) -> findLetKey ctx name body
    | _ -> ValueNone

/// Axis-2 verdict of a *nested* binding named `name` (under the first
/// module-level binding's RHS).
let private reprOfNested (input: string) (name: string) : RegionRepr option =
    let ctx, file = analyse input

    let rhs =
        let elems =
            match file with
            | ImplementationFile.AnonymousModule e -> e
            | _ -> failwith "expected anonymous module"

        match elems.[0] with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bs)) -> bs.[0].expr
        | _ -> failwith "expected let"

    match findLetKey ctx name rhs with
    | ValueSome key ->
        match ctx.Bindings.Repr.TryGetValue key with
        | ValueSome r -> Some r
        | ValueNone -> None
    | ValueNone -> failwithf "binding %s not found" name

[<Tests>]
let tests =
    testList
        "Regions"
        [
            test "local closure doesn't escape" {
                // `f` is defined and called within `useLocal`'s body; it
                // never flows out of the call frame. `useLocal` itself
                // returns the `int` result of `f 3`, so it doesn't escape
                // either.
                let input = "let useLocal () = let f x = x + 1 in f 3"
                let useLocalEscape = escapeOf input "useLocal"
                Expect.equal useLocalEscape (Some LocalStack) "useLocal returns int → LocalStack"

                let fEscape =
                    let ctx, file = analyse input

                    let rec findInExpr e =
                        match e with
                        | Expr.LetOrUse(bindings = bs; body = body) ->
                            let mutable found = ValueNone

                            for b in bs do
                                match b.headPat with
                                | Pat.NamedSimple t when ctx.NameOf t = "f" ->
                                    found <- ValueSome(CstKeys.ofPat b.headPat)
                                | _ -> ()

                            if found.IsNone then
                                match body with
                                | ValueSome b -> findInExpr b
                                | ValueNone -> ValueNone
                            else
                                found
                        | Expr.Fun(expr = body) -> findInExpr body
                        | _ -> ValueNone

                    let key =
                        let elems =
                            match file with
                            | ImplementationFile.AnonymousModule e -> e
                            | _ -> failwith "expected anonymous module"

                        let rhs =
                            match elems.[0] with
                            | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bs)) -> bs.[0].expr
                            | _ -> failwith "expected let"

                        match findInExpr rhs with
                        | ValueSome k -> k
                        | ValueNone -> failwith "f not found"

                    match ctx.Bindings.Escape.TryGetValue key with
                    | ValueSome s -> Some s
                    | ValueNone -> None

                Expect.equal fEscape (Some LocalStack) "f is LocalStack"
            }

            test "returned closure escapes" {
                let escape = escapeOf "let mkAdder n = fun x -> x + n\nlet a = mkAdder 5" "mkAdder"

                Expect.equal escape (Some CallerStack) "mkAdder returns a closure"
            }

            test "local tuple in a function-bound let is CallerStack" {
                let escape = escapeOf "let useLocal () = let p = (1, 2) in p" "useLocal"
                // useLocal's body returns the tuple p, so useLocal escapes.
                // The bare tuple `p` at module level (`let r = let p = ...`)
                // doesn't, but we test that variant in a separate test.
                Expect.equal escape (Some CallerStack) "useLocal returns the tuple"
            }

            test "module-level tuple binding is LocalStack" {
                // No enclosing function — the tuple lives at module level
                // and doesn't escape any frame.
                let escape = escapeOf "let p = (1, 2)" "p"
                Expect.equal escape (Some LocalStack) "p is LocalStack"
            }

            test "returned tuple is CallerStack" {
                let escape = escapeOf "let f () = (1, 2)" "f"
                Expect.equal escape (Some CallerStack) "f returns a tuple"
            }

            test "doubly-captured closure becomes HeapShared" {
                // x is captured by both fun y and fun z (which is itself
                // returned by fun y). x reaches ≥ 2 distinct lambda regions
                // → HeapShared.
                let input = "let mk x = fun y -> fun z -> x + y + z"
                let ctx, file = analyse input
                // mk itself classifies as CallerStack (returns a closure).
                let mkEscape =
                    let k = headKeyOf ctx file "mk"

                    match ctx.Bindings.Escape.TryGetValue k with
                    | ValueSome s -> Some s
                    | ValueNone -> None

                Expect.equal mkEscape (Some CallerStack) "mk returns nested closures → CallerStack"

                let xKey =
                    let elems =
                        match file with
                        | ImplementationFile.AnonymousModule e -> e
                        | _ -> failwith "expected anonymous module"

                    let bindings =
                        match elems.[0] with
                        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bs)) -> bs
                        | _ -> failwith "expected let"

                    let mkB = bindings.[0]
                    let xPat = mkB.argumentPats.[0]
                    CstKeys.ofPat xPat

                let xEscape =
                    match ctx.Bindings.Escape.TryGetValue xKey with
                    | ValueSome s -> Some s
                    | ValueNone -> None

                Expect.equal xEscape (Some HeapShared) "x captured across 2 lambda boundaries → HeapShared"
            }

            test "pure arithmetic has no escape entry" {
                let escape = escapeOf "let r = 1 + 2" "r"
                Expect.equal escape None "r has no escape entry — it's a primitive value"
            }

            test "branching joins regions: function returning tuple from either arm" {
                let escape = escapeOf "let f b = if b then (1, 2) else (3, 4)" "f"
                Expect.equal escape (Some CallerStack) "f returns a tuple from either arm"
            }

            test "identifier reuse: bindings share a region" {
                // `let r = let x = (1, 2) in let y = x in y` — x and y
                // should map to the same RegionId via the Ident pass-through
                // rule. The headPats are inside r's RHS, so we walk the
                // CST to find them.
                let input = "let r = let x = (1, 2) in let y = x in y"
                let ctx, file = analyse input

                let rec scanLet (e: Expr<SyntaxToken>) : (NodeKey * RegionId) option * (NodeKey * RegionId) option =
                    let mutable xR = None
                    let mutable yR = None

                    let regionOfHead (b: Binding<SyntaxToken>) =
                        let k = CstKeys.ofPat b.headPat

                        match ctx.Bindings.TypeVar.TryGetValue k with
                        | ValueSome tv ->
                            let root = UnionFind.find tv

                            if root.Region.Raw >= 0 then Some(k, root.Region) else None
                        | ValueNone -> None

                    let rec walk e =
                        match e with
                        | Expr.LetOrUse(bindings = bs; body = body) ->
                            for b in bs do
                                match b.headPat with
                                | Pat.NamedSimple t ->
                                    match ctx.NameOf t with
                                    | "x" -> xR <- regionOfHead b
                                    | "y" -> yR <- regionOfHead b
                                    | _ -> ()
                                | _ -> ()

                            match body with
                            | ValueSome b -> walk b
                            | ValueNone -> ()
                        | _ -> ()

                    walk e
                    xR, yR

                let rRhs =
                    let elems =
                        match file with
                        | ImplementationFile.AnonymousModule e -> e
                        | _ -> failwith "expected anonymous module"

                    match elems.[0] with
                    | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bs)) -> bs.[0].expr
                    | _ -> failwith "expected let"

                let xR, yR = scanLet rRhs

                match xR, yR with
                | Some(_, rx), Some(_, ry) -> Expect.equal rx ry "x and y share the same RegionId"
                | _ -> failwithf "expected x and y to have regions, got x=%A y=%A" xR yR
            }

            test "recursive binding doesn't crash the solver" {
                // `let rec f x = f x` — region(f) edges may form a self-loop
                // through the App rule; the fixpoint solver should converge.
                let ctx, file = analyse "let rec f x = f x"
                let k = headKeyOf ctx file "f"

                let escape =
                    match ctx.Bindings.Escape.TryGetValue k with
                    | ValueSome s -> Some s
                    | ValueNone -> None
                // Either a classification or no entry is acceptable — what
                // matters is that the pass terminates and doesn't throw.
                ignore escape
                Expect.isTrue true "solver converged on recursive binding"
            }

            test "Fun param region uses the lambda's own frame depth" {
                // `let mk = fun a -> a` at module level. The lambda's
                // parameter `a` must mint with MintFunctionLevel equal to
                // the lambda's own frame (not the outer scope's), so the
                // non-strict level rule (1<=1) seeds it CallerStack and
                // the closure→body edge lifts `mk` to CallerStack.
                // Regression: previously lambdaRegion registered params
                // BEFORE enterFun, leaving a's MintFunctionLevel at 0
                // and mk classified as LocalStack.
                let escape = escapeOf "let mk = fun a -> a" "mk"
                Expect.equal escape (Some CallerStack) "mk returns its argument → CallerStack"
            }

            test "as-pattern parameter surfaces its inner binder with a region" {
                // `let f (x as y) = x` — Freeze's `translatePat` drops the `as`
                // node and surfaces only the inner binder `x` (the alias `y`
                // isn't a `TPat` binder yet; downstream `Var`s find it via the
                // side tables). Regions now walks the
                // post-Freeze `TExpr`, so it stamps the surviving inner binder's
                // region — the `as`-node key no longer exists to stamp.
                let input = "let f (x as y) = x"
                let ctx, file = analyse input

                let elems =
                    match file with
                    | ImplementationFile.AnonymousModule e -> e
                    | _ -> failwith "expected anonymous module"

                let fBinding =
                    match elems.[0] with
                    | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bs)) -> bs.[0]
                    | _ -> failwith "expected let"

                let rec unwrap p =
                    match p with
                    | Pat.EnclosedBlock(pat = inner)
                    | Pat.Typed(pat = inner) -> unwrap inner
                    | _ -> p

                let asPat = unwrap fBinding.argumentPats.[0]

                let innerKey =
                    match asPat with
                    | Pat.As(pat = inner) -> CstKeys.ofPat inner
                    | _ -> failwithf "expected As pattern, got %A" asPat

                let regionOfKey k =
                    match ctx.Bindings.TypeVar.TryGetValue k with
                    | ValueSome tv ->
                        let root = UnionFind.find tv
                        if root.Region.Raw >= 0 then Some root.Region else None
                    | ValueNone -> None

                Expect.isSome (regionOfKey innerKey) "the as-pattern's surfaced inner binder has a region"
            }

            test "tuple-pattern parameter binders share a region" {
                // `let f (a, b) = a` — `a` and `b` project parts of the
                // same tuple parameter; both should land on the parameter's
                // single region.
                let input = "let f (a, b) = a"
                let ctx, file = analyse input

                let elems =
                    match file with
                    | ImplementationFile.AnonymousModule e -> e
                    | _ -> failwith "expected anonymous module"

                let fBinding =
                    match elems.[0] with
                    | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bs)) -> bs.[0]
                    | _ -> failwith "expected let"

                let rec unwrap p =
                    match p with
                    | Pat.EnclosedBlock(pat = inner)
                    | Pat.Typed(pat = inner) -> unwrap inner
                    | _ -> p

                let tuplePat = unwrap fBinding.argumentPats.[0]

                let aKey, bKey =
                    match tuplePat with
                    | Pat.Tuple(patterns = pats) -> CstKeys.ofPat pats.[0], CstKeys.ofPat pats.[1]
                    | _ -> failwithf "expected Tuple pattern, got %A" tuplePat

                let regionOfKey k =
                    match ctx.Bindings.TypeVar.TryGetValue k with
                    | ValueSome tv ->
                        let root = UnionFind.find tv
                        if root.Region.Raw >= 0 then Some root.Region else None
                    | ValueNone -> None

                let rA = regionOfKey aKey
                let rB = regionOfKey bKey
                Expect.isSome rA "tuple-pattern element should have a region"
                Expect.equal rA rB "tuple-pattern elements share one region"
            }

            test "mutual recursion: sibling region is visible during body walk" {
                // `let rec a () = b and b () = (1, 2)`. a's body is
                // `Ident b`, so a's closure→body edge points to b. With
                // sibling regions pre-recorded in processBindingGroup,
                // BindingRegions[b] is set when a's body is walked → the
                // edge exists → CallerStack propagates from b's tuple
                // back through r_b to r_a. Without the pre-record pass,
                // a's body would resolve `b` as Unknown, the
                // closure→body edge would be dropped, and a would stay
                // LocalStack.
                let escape = escapeOf "let rec a () = b\nand b () = (1, 2)" "a"
                Expect.equal escape (Some CallerStack) "a returns b, which returns a tuple"
            }

            test "let mutable at module level is LocalStack" {
                // No enclosing function frame; no closure capture. The cell
                // mints at MintFunctionLevel = 0, so the level rule doesn't
                // fire, and the cell doesn't reach any lambda → LocalStack.
                let escape = escapeOf "let mutable r = (1, 2)" "r"
                Expect.equal escape (Some LocalStack) "module-top mutable cell is LocalStack"
            }

            test "uncaptured mutable cell inside a function is CallerStack" {
                // `let useLocal () = let mutable n = (1, 2) in n` — the cell
                // is minted inside useLocal's frame; useLocal returns it, so
                // the level rule fires and the cell goes CallerStack. The
                // tuple `(1, 2)` separately escapes too (RHS-to-cell edge
                // propagates from the cell upward).
                let input = "let useLocal () = let mutable n = (1, 2) in n"
                let escape = escapeOf input "useLocal"
                Expect.equal escape (Some CallerStack) "useLocal returns the mutable cell's value"
            }

            test "mutable cell captured by an escaping closure is HeapShared" {
                // `let mkCounter () = let mutable n = 0 in fun () -> n` —
                // the cell is captured by the returned closure. With
                // threshold-of-1, any closure capture forces HeapShared.
                let input = "let mkCounter () = let mutable n = 0 in fun () -> n"
                let ctx, file = analyse input

                let nKey =
                    let elems =
                        match file with
                        | ImplementationFile.AnonymousModule e -> e
                        | _ -> failwith "expected anonymous module"

                    let mkBinding =
                        match elems.[0] with
                        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bs)) -> bs.[0]
                        | _ -> failwith "expected let"

                    let rec findN e =
                        match e with
                        | Expr.LetOrUse(bindings = bs; body = body) ->
                            let mutable found = ValueNone

                            for b in bs do
                                match b.headPat with
                                | Pat.NamedSimple t when ctx.NameOf t = "n" ->
                                    found <- ValueSome(CstKeys.ofPat b.headPat)
                                | _ -> ()

                            if found.IsSome then
                                found
                            else
                                match body with
                                | ValueSome b -> findN b
                                | ValueNone -> ValueNone
                        | _ -> ValueNone

                    match findN mkBinding.expr with
                    | ValueSome k -> k
                    | ValueNone -> failwith "n not found"

                match ctx.Bindings.Escape.TryGetValue nKey with
                | ValueSome HeapShared -> ()
                | other -> failwithf "expected HeapShared for captured mutable, got %A" other
            }

            test "list literal at module top is precisely analysed (no spurious HeapShared)" {
                // `let xs = [ 1; 2 ]` lowers to nested `UnionCons("Cons", …)`
                // before Regions runs (the pass walks the
                // post-Freeze `TExpr`), so the list allocation is modelled
                // precisely as a module-top composite — `LocalStack`, not the
                // old pessimistic `HeapShared` fallback the CST pass produced for
                // an unrecognised list-literal node. (A `None` no-region posture
                // is also acceptable.)
                let escape = escapeOf "let xs = [ 1; 2 ]" "xs"

                match escape with
                | Some LocalStack
                | None -> ()
                | other -> failwithf "expected LocalStack or None, got %A" other
            }

            test "module-level record literal is LocalStack" {
                let escape = escapeOf "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }" "r"

                Expect.equal escape (Some LocalStack) "module-top record is LocalStack"
            }

            test "record returned from a function is CallerStack" {
                let escape =
                    escapeOf "type R = { X: int; Y: int }\nlet mk () = { X = 1; Y = 2 }" "mk"

                Expect.equal escape (Some CallerStack) "mk returns a record"
            }

            test "module-level ctor application is LocalStack" {
                let escape = escapeOf "type S = | Circle of float\nlet c = Circle 1.0" "c"

                Expect.equal escape (Some LocalStack) "module-top ctor app is LocalStack"
            }

            test "ctor returned from a function is CallerStack" {
                let escape = escapeOf "type S = | Circle of float\nlet mk () = Circle 1.0" "mk"

                Expect.equal escape (Some CallerStack) "mk returns a ctor value"
            }

            test "generic record literal at module top is LocalStack" {
                let escape = escapeOf "type Box<'a> = { Value: 'a }\nlet b = { Value = 1 }" "b"

                Expect.equal escape (Some LocalStack) "generic record at module-top is LocalStack"
            }

            test "generic ctor at module top is LocalStack" {
                let escape = escapeOf "type Option<'a> = | Some of 'a | None\nlet s = Some 1" "s"

                Expect.equal escape (Some LocalStack) "generic ctor at module-top is LocalStack"
            }

            test "abbreviation to a record at module top is LocalStack" {
                // Abbreviations vanish at translateType, so the binding's
                // region shape is identical to a direct `Box<int>` literal.
                let escape =
                    escapeOf "type Box<'a> = { Value: 'a }\ntype IntBox = Box<int>\nlet b : IntBox = { Value = 1 }" "b"

                Expect.equal escape (Some LocalStack) "abbreviation to record at module-top is LocalStack"
            }

            // --- Axis 1 lattice -------------------------------------------
            // The `ReturnOnly` tier and the two coarsening maps. v1 lays the
            // tier down but `solve` does not mint it yet (a returned closure
            // stays `CallerStack` per the tests above — the `ReturnOnly`
            // refinement is Consumer B). These tests guard the lattice's CLR /
            // native projections so the documented tables can't silently drift.

            test "toClrRefSafe maps each tier to its Roslyn safe-context" {
                Expect.equal
                    (EscapeState.toClrRefSafe LocalStack)
                    SafeContext.CurrentMethod
                    "LocalStack → CurrentMethod"

                Expect.equal (EscapeState.toClrRefSafe ReturnOnly) SafeContext.ReturnOnly "ReturnOnly → ReturnOnly"

                Expect.equal
                    (EscapeState.toClrRefSafe CallerStack)
                    SafeContext.CallingMethod
                    "CallerStack → CallingMethod"

                Expect.equal (EscapeState.toClrRefSafe HeapShared) SafeContext.Heap "HeapShared → Heap"
            }

            test "toNativeRegionTier coarsens to the Tofte–Talpin tiers" {
                Expect.equal (EscapeState.toNativeRegionTier LocalStack) NativeRegionTier.Stack "LocalStack → Stack"

                Expect.equal
                    (EscapeState.toNativeRegionTier ReturnOnly)
                    NativeRegionTier.ReturnSlot
                    "ReturnOnly → ReturnSlot (sret)"

                Expect.equal
                    (EscapeState.toNativeRegionTier CallerStack)
                    NativeRegionTier.ReturnSlot
                    "CallerStack → ReturnSlot (out-param)"

                Expect.equal (EscapeState.toNativeRegionTier HeapShared) NativeRegionTier.Heap "HeapShared → Heap"
            }

            test "ReturnOnly slots between CallerStack and LocalStack in the CLR projection" {
                // The lattice order is `HeapShared > CallerStack > ReturnOnly >
                // LocalStack`. `lub` is private, but the safe-context image
                // preserves the ordering distinction (ReturnOnly is its own
                // Roslyn tier, strictly more permissive than CallingMethod and
                // strictly less than CurrentMethod), guarding against the tier
                // being collapsed into a neighbour when Consumer B starts
                // minting it.
                Expect.notEqual
                    (EscapeState.toClrRefSafe ReturnOnly)
                    (EscapeState.toClrRefSafe CallerStack)
                    "ReturnOnly is distinct from CallerStack"

                Expect.notEqual
                    (EscapeState.toClrRefSafe ReturnOnly)
                    (EscapeState.toClrRefSafe LocalStack)
                    "ReturnOnly is distinct from LocalStack"
            }

            // --- Axis 2 representation fixpoint ----------------------------------
            // Orthogonal to Axis 1: a closure can be frame-local by lifetime yet
            // pinned to a heap representation by a containment / boxing channel.
            // The ref-struct-eligibility predicate is the conjunction
            // `LocalStack ∧ StackOnlyEligible`; these tests pin the second
            // conjunct. Codegen is untouched.

            test "frame-local applied closure is StackOnlyEligible" {
                // `let useLocal () = let f x = x + 1 in f 3` — `f` is only ever
                // the direct callee of an application: no aggregate, no box, no
                // heap escape reaches it, so its representation is stack-eligible
                // (it is also LocalStack by Axis 1 — the unconditional green-light).
                let repr = reprOfNested "let useLocal () = let f x = x + 1 in f 3" "f"
                Expect.equal repr (Some RegionRepr.StackOnlyEligible) "f has no heap-repr channel"
            }

            test "closure stored in a ValueTuple requires heap repr" {
                // `g` is frame-local by lifetime, but `(g, g)` puts it in a
                // `System.ValueTuple` — which cannot carry a ref-struct field —
                // so the aggregate-containment channel pins it to the heap. This
                // is exactly the lifetime/representation split: Axis 1 and Axis 2
                // disagree on the same region.
                let repr = reprOfNested "let f () = let g = fun x -> x in (g, g)" "g"
                Expect.equal repr (Some RegionRepr.RequiresHeapRepr) "tuple containment pins g to the heap"
            }

            test "returned closure stays StackOnlyEligible — only Axis 1 disqualifies it" {
                // `mkAdder` returns `fun x -> x + n` but is never itself applied
                // and stored: no aggregate / box / heap channel reaches it, so
                // Axis 2 is `StackOnlyEligible`; it is the Axis-1 `CallerStack`
                // lifetime that fails the ref-struct conjunction. Guards the
                // orthogonality — the repr fixpoint must NOT fold escape into
                // itself (the `ReturnOnly` by-value-return refinement that would
                // re-admit such a closure is Consumer B's job). Note the *applied
                // and stored* form `let a = mkAdder 5` does reach the heap
                // (`a` is a static field holding a closure that reaches two
                // lambdas → `HeapShared` → `RequiresHeapRepr`); that is a
                // genuine heap-escape channel, not an Axis-1 leak.
                let input = "let mkAdder n = fun x -> x + n"
                Expect.equal (reprOf input "mkAdder") (Some RegionRepr.StackOnlyEligible) "no heap-repr channel"
                Expect.equal (escapeOf input "mkAdder") (Some CallerStack) "but it escapes by lifetime"
            }

            test "non-aggregated module-level closure is StackOnlyEligible" {
                // A plain top-level function binding with no containment or box
                // channel — the baseline stack-eligible case.
                let repr = reprOf "let add x = x + 1" "add"
                Expect.equal repr (Some RegionRepr.StackOnlyEligible) "add rides no heap-repr channel"
            }
        ]
