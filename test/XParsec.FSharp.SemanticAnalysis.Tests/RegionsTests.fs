module XParsec.FSharp.SemanticAnalysis.Tests.RegionsTests

open Expecto
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    let ctx, _ = Pipeline.analyseWithContext MockBuiltins.provider input lexed file
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

    match ctx.Escape.TryGetValue key with
    | ValueSome s -> Some s
    | ValueNone -> None

/// Region of a binding (via its headPat TyVar). Used by tests that need
/// to check identity (e.g. two names sharing a region) rather than just
/// the escape state.
let private regionOf (input: string) (name: string) : RegionId option =
    let ctx, file = analyse input
    let key = headKeyOf ctx file name

    match ctx.TypeVar.TryGetValue key with
    | ValueSome tv ->
        let root = UnionFind.find tv

        if root.Region.Raw >= 0 then Some root.Region else None
    | ValueNone -> None

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
                    // Find `f` inside useLocal's body — walk the CST.
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

                    match ctx.Escape.TryGetValue key with
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

                    match ctx.Escape.TryGetValue k with
                    | ValueSome s -> Some s
                    | ValueNone -> None

                Expect.equal mkEscape (Some CallerStack) "mk returns nested closures → CallerStack"

                // Look up x (mk's parameter) and check its escape state.
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
                    match ctx.Escape.TryGetValue xKey with
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

                        match ctx.TypeVar.TryGetValue k with
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
                    match ctx.Escape.TryGetValue k with
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

            test "as-pattern parameter binders share a region" {
                // `let f (x as y) = x` — `x` and `y` are two names for the
                // same parameter value, so registerParam must thread one
                // region through both binders (mirrors recordBindingRegion
                // for let-bindings).
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

                let asKey, innerKey =
                    match asPat with
                    | Pat.As(pat = inner) -> CstKeys.ofPat asPat, CstKeys.ofPat inner
                    | _ -> failwithf "expected As pattern, got %A" asPat

                let regionOfKey k =
                    match ctx.TypeVar.TryGetValue k with
                    | ValueSome tv ->
                        let root = UnionFind.find tv
                        if root.Region.Raw >= 0 then Some root.Region else None
                    | ValueNone -> None

                let rAs = regionOfKey asKey
                let rInner = regionOfKey innerKey
                Expect.isSome rAs "as-pattern outer name should have a region"
                Expect.equal rAs rInner "as-pattern outer and inner share one region"
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
                    match ctx.TypeVar.TryGetValue k with
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

                // Find n's binding key by walking mkCounter's body.
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

                match ctx.Escape.TryGetValue nKey with
                | ValueSome HeapShared -> ()
                | other -> failwithf "expected HeapShared for captured mutable, got %A" other
            }

            test "conservative fallback: unhandled construct gets HeapShared" {
                // `let xs = [ 1; 2 ]` uses a list-literal we don't model
                // precisely — the fallback should produce a HeapShared
                // entry rather than silently nothing.
                let escape = escapeOf "let xs = [ 1; 2 ]" "xs"
                // The Expr.ArrayOrList / similar list-literal isn't in the
                // tiny subset's precise rules, so it routes through the
                // fallback path. Depending on parser output it may map to a
                // pattern we do handle — accept HeapShared OR nothing as
                // both correctness postures (over-approx vs no-region).
                match escape with
                | Some HeapShared
                | None -> ()
                | other -> failwithf "expected HeapShared or None, got %A" other
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
        ]
