module XParsec.FSharp.SemanticAnalysis.Tests.RegionsTests

open Expecto
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// A compile composing no platform, so nothing is laid out flat and every type is tracked.
// What holds here holds for any target's layout verdict; the rows that turn on that verdict
// are pinned per backend, in each codegen suite's `RegionLayoutTests`.

let private probe (input: string) =
    RegionProbe.analyse testCompiling realProvider.Value input

/// Down to the pattern a parameter's parentheses and annotation wrap.
let rec private unwrapPat (p: Pat<SyntaxToken>) : Pat<SyntaxToken> =
    match p with
    | Pat.EnclosedBlock(pat = inner)
    | Pat.Typed(pat = inner) -> unwrapPat inner
    | _ -> p

let private escapeOf (input: string) (name: string) : EscapeState option = RegionProbe.escapeOf (probe input) name

let private reprOf (input: string) (name: string) : RegionRepr option = RegionProbe.reprOf (probe input) name

let private reprOfNested (input: string) (name: string) : RegionRepr option =
    RegionProbe.reprOfNested (probe input) name

[<Tests>]
let tests =
    testList
        "Regions"
        [
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
                // lambda regions → HeapShared. What `mk` itself settles at turns on whether
                // the target tracks the `int` the bodies compute, so it is pinned per backend.
                let p = probe "let mk x = fun y -> fun z -> x + y + z"
                let xKey = CstKeys.ofPat (RegionProbe.firstBinding p).argumentPats.[0]

                Expect.equal
                    (RegionProbe.escapeAt p xKey)
                    (Some HeapShared)
                    "x captured across 2 lambda boundaries → HeapShared"
            }

            test "branching joins regions: function returning tuple from either arm" {
                let escape = escapeOf "let f b = if b then (1, 2) else (3, 4)" "f"
                Expect.equal escape (Some CallerStack) "f returns a tuple from either arm"
            }

            test "identifier reuse: bindings share a region" {
                // `x` and `y` must map to one RegionId through the Ident pass-through rule;
                // their binding patterns are inside `r`'s RHS, so the walk goes via the CST.
                let p = probe "let r = let x = (1, 2) in let y = x in y"

                let regionOfName name =
                    RegionProbe.regionAt p (RegionProbe.nestedKeyOf p name)

                let xR = regionOfName "x"
                let yR = regionOfName "y"
                Expect.isSome xR "x has a region"
                Expect.equal xR yR "x and y share the same RegionId"
            }

            test "recursive binding doesn't crash the solver" {
                // region(f) edges may form a self-loop through the App rule; the fixpoint
                // solver must still converge.
                // Either verdict is acceptable; the test is that the pass terminates.
                escapeOf "let rec f x = f x" "f" |> ignore
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
                let p = probe "let f (x as y) = x"

                let innerKey =
                    match unwrapPat (RegionProbe.firstBinding p).argumentPats.[0] with
                    | Pat.As(pat = inner) -> CstKeys.ofPat inner
                    | other -> failwithf "expected As pattern, got %A" other

                Expect.isSome
                    (RegionProbe.regionAt p innerKey)
                    "the as-pattern's surfaced inner bound variable has a region"
            }

            test "tuple-pattern parameter bound variables share a region" {
                // `a` and `b` project parts of the same tuple parameter, so both land on
                // the parameter's single region.
                let p = probe "let f (a, b) = a"

                let aKey, bKey =
                    match unwrapPat (RegionProbe.firstBinding p).argumentPats.[0] with
                    | Pat.Tuple(patterns = pats) -> CstKeys.ofPat pats.[0], CstKeys.ofPat pats.[1]
                    | other -> failwithf "expected Tuple pattern, got %A" other

                let rA = RegionProbe.regionAt p aKey
                let rB = RegionProbe.regionAt p bKey
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
                let p = probe "let mkCounter () = let mutable n = 0 in fun () -> n"

                Expect.equal
                    (RegionProbe.escapeOfNested p "n")
                    (Some HeapShared)
                    "a captured mutable cell is HeapShared"
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
                Expect.equal repr (Some RegionRepr.StackOnlyEligible) "add sits on no heap-repr channel"
            }
        ]
