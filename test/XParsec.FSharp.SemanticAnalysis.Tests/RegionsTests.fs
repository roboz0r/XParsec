module XParsec.FSharp.SemanticAnalysis.Tests.RegionsTests

open Expecto
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input

    let ctx, _ =
        Pipeline.analyseSemWithContext realProvider.Value (Hashing.originSourceOfText lexed) file

    ctx, file

/// Find the binding-pattern NodeKey of a module-level binding by name.
let private patternKeyOf (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) (name: string) : NodeKey =
    let rec tryBindings (bindings: System.Collections.Immutable.ImmutableArray<Binding<SyntaxToken>>) =
        let mutable found = ValueNone
        let mutable i = 0

        while found.IsNone && i < bindings.Length do
            let b = bindings.[i]

            match b.pattern with
            | Pat.NamedSimple t when ctx.NameOf t = name -> found <- ValueSome(CstKeys.ofPat b.pattern)
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
    let key = patternKeyOf ctx file name

    match ctx.Bindings.Escape.TryGetValue key with
    | ValueSome s -> Some s
    | ValueNone -> None

/// Region of a binding, via its binding-pattern type variable.
let private regionOf (input: string) (name: string) : RegionId option =
    let ctx, file = analyse input
    let key = patternKeyOf ctx file name

    match ctx.Bindings.TypeVar.TryGetValue key with
    | ValueSome tv ->
        let root = UnionFind.find ctx.Store tv

        if (ctx.Store.Region root.Id).Raw >= 0 then
            Some(ctx.Store.Region root.Id)
        else
            None
    | ValueNone -> None

/// Axis-2 representation verdict of a MODULE-LEVEL binding, over the `Repr` side table.
let private reprOf (input: string) (name: string) : RegionRepr option =
    let ctx, file = analyse input
    let key = patternKeyOf ctx file name

    match ctx.Bindings.Repr.TryGetValue key with
    | ValueSome r -> Some r
    | ValueNone -> None

/// The binding-pattern NodeKey of the first `let` named `name` reachable from `e`, through
/// binding RHSs, let bodies and lambda bodies — e.g. `let g = fun x -> x` inside a function.
let rec private findLetKey (ctx: PassContext) (name: string) (e: Expr<SyntaxToken>) : NodeKey voption =
    match e with
    | Expr.LetOrUse(bindings = bs; body = body) ->
        let mutable found = ValueNone
        let mutable i = 0

        while found.IsNone && i < bs.Length do
            let b = bs.[i]

            match b.pattern with
            | Pat.NamedSimple t when ctx.NameOf t = name -> found <- ValueSome(CstKeys.ofPat b.pattern)
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

/// Axis-2 verdict of a NESTED binding, under the first module-level binding's RHS.
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
                // `f` is called inside `useLocal`'s body and never escapes; `useLocal`
                // returns the `int` result of `f 3`.
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
                                match b.pattern with
                                | Pat.NamedSimple t when ctx.NameOf t = "f" ->
                                    found <- ValueSome(CstKeys.ofPat b.pattern)
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
                Expect.equal escape (Some CallerStack) "useLocal returns the tuple"
            }

            test "module-level tuple binding is LocalStack" {
                // No enclosing function frame for the tuple to escape.
                let escape = escapeOf "let p = (1, 2)" "p"
                Expect.equal escape (Some LocalStack) "p is LocalStack"
            }

            test "returned tuple is CallerStack" {
                let escape = escapeOf "let f () = (1, 2)" "f"
                Expect.equal escape (Some CallerStack) "f returns a tuple"
            }

            test "doubly-captured closure becomes HeapShared" {
                // `x` is captured by both `fun y` and `fun z`, so it reaches 2 distinct
                // lambda regions → HeapShared.
                let input = "let mk x = fun y -> fun z -> x + y + z"
                let ctx, file = analyse input

                let mkEscape =
                    let k = patternKeyOf ctx file "mk"

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
                // `x` and `y` must map to one RegionId through the Ident pass-through rule;
                // their binding patterns are inside `r`'s RHS, so the walk goes via the CST.
                let input = "let r = let x = (1, 2) in let y = x in y"
                let ctx, file = analyse input

                let rec scanLet (e: Expr<SyntaxToken>) : (NodeKey * RegionId) option * (NodeKey * RegionId) option =
                    let mutable xR = None
                    let mutable yR = None

                    let regionOfPattern (b: Binding<SyntaxToken>) =
                        let k = CstKeys.ofPat b.pattern

                        match ctx.Bindings.TypeVar.TryGetValue k with
                        | ValueSome tv ->
                            let root = UnionFind.find ctx.Store tv

                            if (ctx.Store.Region root.Id).Raw >= 0 then
                                Some(k, ctx.Store.Region root.Id)
                            else
                                None
                        | ValueNone -> None

                    let rec walk e =
                        match e with
                        | Expr.LetOrUse(bindings = bs; body = body) ->
                            for b in bs do
                                match b.pattern with
                                | Pat.NamedSimple t ->
                                    match ctx.NameOf t with
                                    | "x" -> xR <- regionOfPattern b
                                    | "y" -> yR <- regionOfPattern b
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
                // region(f) edges may form a self-loop through the App rule; the fixpoint
                // solver must still converge.
                let ctx, file = analyse "let rec f x = f x"
                let k = patternKeyOf ctx file "f"

                let escape =
                    match ctx.Bindings.Escape.TryGetValue k with
                    | ValueSome s -> Some s
                    | ValueNone -> None
                // Either verdict is acceptable; the test is that the pass terminates.
                ignore escape
                Expect.isTrue true "solver converged on recursive binding"
            }

            test "Fun param region uses the lambda's own frame depth" {
                // `a` mints with MintFunctionLevel equal to the LAMBDA's own frame, not the
                // outer scope's, so the non-strict level rule (1 <= 1) seeds it CallerStack
                // and the closure→body edge lifts `mk` to CallerStack.
                let escape = escapeOf "let mk = fun a -> a" "mk"
                Expect.equal escape (Some CallerStack) "mk returns its argument → CallerStack"
            }

            test "as-pattern parameter surfaces its inner bound variable with a region" {
                // `translatePat` drops the `as` node and surfaces only the inner bound
                // variable `x`; the alias `y` is not a `TPat` bound variable, and downstream
                // `Var`s find it via the side tables. So only `x`'s region is stamped.
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
                        let root = UnionFind.find ctx.Store tv

                        if (ctx.Store.Region root.Id).Raw >= 0 then
                            Some(ctx.Store.Region root.Id)
                        else
                            None
                    | ValueNone -> None

                Expect.isSome (regionOfKey innerKey) "the as-pattern's surfaced inner bound variable has a region"
            }

            test "tuple-pattern parameter bound variables share a region" {
                // `a` and `b` project parts of the same tuple parameter, so both land on
                // the parameter's single region.
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
                        let root = UnionFind.find ctx.Store tv

                        if (ctx.Store.Region root.Id).Raw >= 0 then
                            Some(ctx.Store.Region root.Id)
                        else
                            None
                    | ValueNone -> None

                let rA = regionOfKey aKey
                let rB = regionOfKey bKey
                Expect.isSome rA "tuple-pattern element should have a region"
                Expect.equal rA rB "tuple-pattern elements share one region"
            }

            test "mutual recursion: sibling region is visible during body walk" {
                // `a`'s body is `Ident b`, so its closure→body edge points at `b`. Sibling
                // regions are pre-minted before the group's bodies are walked, so the edge
                // exists and CallerStack propagates from `b`'s tuple back to `a`.
                let escape = escapeOf "let rec a () = b\nand b () = (1, 2)" "a"
                Expect.equal escape (Some CallerStack) "a returns b, which returns a tuple"
            }

            test "let mutable at module level is LocalStack" {
                // The cell mints at MintFunctionLevel = 0, so the level rule does not fire,
                // and it reaches no lambda → LocalStack.
                let escape = escapeOf "let mutable r = (1, 2)" "r"
                Expect.equal escape (Some LocalStack) "module-top mutable cell is LocalStack"
            }

            test "uncaptured mutable cell inside a function is CallerStack" {
                // The cell is minted inside `useLocal`'s frame and returned, so the level
                // rule fires; the tuple escapes with it through the RHS-to-cell edge.
                let input = "let useLocal () = let mutable n = (1, 2) in n"
                let escape = escapeOf input "useLocal"
                Expect.equal escape (Some CallerStack) "useLocal returns the mutable cell's value"
            }

            test "mutable cell captured by an escaping closure is HeapShared" {
                // The cell is captured by the returned closure; with a threshold of 1, any
                // closure capture forces HeapShared.
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
                                match b.pattern with
                                | Pat.NamedSimple t when ctx.NameOf t = "n" ->
                                    found <- ValueSome(CstKeys.ofPat b.pattern)
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
                // The literal lowers to nested `UnionCons("Cons", …)` before Regions walks
                // it, so the allocation is modelled as an ordinary module-top composite:
                // LocalStack, or no region entry at all.
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
                // Abbreviations vanish at translateType, so the region shape is identical
                // to a direct `Box<int>` literal.
                let escape =
                    escapeOf "type Box<'a> = { Value: 'a }\ntype IntBox = Box<int>\nlet b : IntBox = { Value = 1 }" "b"

                Expect.equal escape (Some LocalStack) "abbreviation to record at module-top is LocalStack"
            }

            // --- Axis 1 lattice -------------------------------------------
            // `solve` does not mint `ReturnOnly`: a returned closure stays `CallerStack`,
            // per the tests above. These pin the tier's CLR and native projections.

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
                // Lattice order: `HeapShared > CallerStack > ReturnOnly > LocalStack`. `lub`
                // is private, but the safe-context image keeps `ReturnOnly` its own Roslyn
                // tier, so it cannot be collapsed into a neighbour.
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
            // Ref-struct eligibility is `LocalStack ∧ StackOnlyEligible`; these pin the
            // second conjunct. A closure can be frame-local yet heap-pinned by containment.

            test "frame-local applied closure is StackOnlyEligible" {
                // `f` is only ever the direct callee of an application: no aggregate, box
                // or heap escape reaches it, so its representation is stack-eligible.
                let repr = reprOfNested "let useLocal () = let f x = x + 1 in f 3" "f"
                Expect.equal repr (Some RegionRepr.StackOnlyEligible) "f has no heap-repr channel"
            }

            test "closure stored in a ValueTuple requires heap repr" {
                // `g` is frame-local by lifetime, but `(g, g)` puts it in a
                // `System.ValueTuple`, which cannot carry a ref-struct field — so aggregate
                // containment pins it to the heap. Axis 1 and Axis 2 disagree here.
                let repr = reprOfNested "let f () = let g = fun x -> x in (g, g)" "g"
                Expect.equal repr (Some RegionRepr.RequiresHeapRepr) "tuple containment pins g to the heap"
            }

            test "returned closure stays StackOnlyEligible — only Axis 1 disqualifies it" {
                // Never applied and stored here, so no repr channel reaches it and Axis 2
                // says `StackOnlyEligible`; the Axis-1 `CallerStack` lifetime is what fails
                // the conjunction. Add `let a = mkAdder 5` and `a` → `RequiresHeapRepr`.
                let input = "let mkAdder n = fun x -> x + n"
                Expect.equal (reprOf input "mkAdder") (Some RegionRepr.StackOnlyEligible) "no heap-repr channel"
                Expect.equal (escapeOf input "mkAdder") (Some CallerStack) "but it escapes by lifetime"
            }

            test "non-aggregated module-level closure is StackOnlyEligible" {
                // A top-level function binding with no containment or box channel.
                let repr = reprOf "let add x = x + 1" "add"
                Expect.equal repr (Some RegionRepr.StackOnlyEligible) "add rides no heap-repr channel"
            }
        ]
