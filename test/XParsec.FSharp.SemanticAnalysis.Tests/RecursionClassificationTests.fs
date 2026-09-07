module XParsec.FSharp.SemanticAnalysis.Tests.RecursionClassificationTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// One `let` as the pool records it: a lone binding, or a lexical `let rec … and …` group
/// with its members in source order and its components over their indices.
type private LetRecursion =
    | Let of name: string * Recursion
    | Group of members: (string * Recursion) list * components: int list list

/// Every `let` in `src`, module-level then local in pool walk order.
let private letRecursions (src: string) : LetRecursion list =
    let pool = TastPoolBuilder.openOver (freezeFor src)

    let name (p: TastAccessor.PatId) =
        match p with
        | TastAccessor.PNamedNaming(BoundVarNaming.Source n) -> n
        | _ -> "_"

    let group (members: TastAccessor.LetMemberView[]) (components: SccPartition) =
        Group(
            [ for m in members -> name m.Pattern, m.Recursion ],
            [ for c in components.Components -> EqArray.toList c.Members ]
        )

    let acc = ResizeArray<LetRecursion>()

    let rec walk (e: TastAccessor.ExprId) =
        match e with
        | TastAccessor.ELet l -> acc.Add(Let(name l.Binding.Pattern, l.Binding.Recursion))
        | TastAccessor.ELetGroup g -> acc.Add(group g.Members g.Components)
        | _ -> ()

        for c in TastAccessor.exprChildren e do
            walk c

    for d in TastAccessor.roots pool do
        match d with
        | TastAccessor.DLet dl ->
            acc.Add(Let(name dl.Binding.Pattern, dl.Binding.Recursion))
            walk dl.Binding.Value
        | TastAccessor.DLetGroup g ->
            acc.Add(group g.Members g.Components)

            for m in g.Members do
                walk m.Value
        | _ -> ()

    List.ofSeq acc

[<Tests>]
let recursionTests =
    testList
        "TastPools records each let's Recursion"
        [
            test "a module let is Recursive, TailRecursive or NonRecursive by what its value applies" {
                let src =
                    "let rec fact n = if n = 0 then 1 else n * fact (n - 1)\n"
                    + "let rec loop n acc = if n = 0 then acc else loop (n - 1) (acc * n)\n"
                    + "let rec partial n acc = if n = 0 then acc else partial (n - 1)\n"
                    + "let rec viaMatch n = match n with 0 -> 42 | _ -> viaMatch (n - 1)\n"
                    + "let rec unused x = x + 1\n"
                    + "let plain x = x + 1\n"

                Expect.equal
                    (letRecursions src)
                    [
                        Let("fact", Recursion.Recursive)
                        Let("loop", Recursion.TailRecursive)
                        Let("partial", Recursion.Recursive)
                        Let("viaMatch", Recursion.TailRecursive)
                        Let("unused", Recursion.NonRecursive)
                        Let("plain", Recursion.NonRecursive)
                    ]
                    "a saturated tail self-call is TailRecursive; an unsaturated or non-tail one is Recursive; `rec` alone is NonRecursive"
            }

            test "a local let classifies exactly as a module let" {
                let src =
                    "let f () =\n"
                    + "    let rec go i = if i = 0 then 0 else go (i - 1)\n"
                    + "    let rec count i = if i = 0 then 0 else 1 + count (i - 1)\n"
                    + "    let rec k = 5\n"
                    + "    let y = go k\n"
                    + "    count y\n"

                Expect.equal
                    (letRecursions src)
                    [
                        Let("f", Recursion.NonRecursive)
                        Let("go", Recursion.TailRecursive)
                        Let("count", Recursion.Recursive)
                        Let("k", Recursion.NonRecursive)
                        Let("y", Recursion.NonRecursive)
                    ]
                    "`let rec` without a self-reference is NonRecursive, the same as a plain let"
            }

            test "a `let rec` whose lambdas are not a direct chain is Recursive, not TailRecursive" {
                let src =
                    "let rec h = if true then (fun x -> h x) else (fun x -> x)\n"
                    + "let rec f = fun x -> if x = 0 then 0 else f (x - 1)\n"

                Expect.equal
                    (letRecursions src)
                    [ Let("h", Recursion.Recursive); Let("f", Recursion.TailRecursive) ]
                    "only the body after the value's leading lambdas is in tail position"
            }

            test "a nested `let rec` classifies against its own binding, its parent against the parent's" {
                let src =
                    "let rec outer n =\n"
                    + "    let rec inner m = if m = 0 then outer (n - 1) else inner (m - 1)\n"
                    + "    if n = 0 then 0 else inner n\n"

                Expect.equal
                    (letRecursions src)
                    [ Let("outer", Recursion.Recursive); Let("inner", Recursion.TailRecursive) ]
                    "`outer (n - 1)` is in tail position of `inner`, not of `outer`, so it is a reference and not a tail self-call"
            }

            test "every member of a mutually recursive module group is Recursive" {
                let src =
                    "let rec isEven n = if n = 0 then true else isOdd (n - 1)\n"
                    + "and isOdd n = if n = 0 then false else isEven (n - 1)\n"

                Expect.equal
                    (letRecursions src)
                    [
                        Group([ "isEven", Recursion.Recursive; "isOdd", Recursion.Recursive ], [ [ 0; 1 ] ])
                    ]
                    "a member referencing a sibling is recursive, exactly as one referencing itself"
            }

            test "every member of a mutually recursive local group is Recursive" {
                let src =
                    "let run () =\n"
                    + "    let rec a x = if x = 0 then 0 else b (x - 1)\n"
                    + "    and b x = a x\n"
                    + "    a 3\n"

                Expect.equal
                    (letRecursions src)
                    [
                        Let("run", Recursion.NonRecursive)
                        Group([ "a", Recursion.Recursive; "b", Recursion.Recursive ], [ [ 0; 1 ] ])
                    ]
                    "a local group classifies exactly as a module group"
            }

            test "a group member's saturated tail self-call is TailRecursive" {
                let src =
                    "let rec walk n = if n = 0 then stop 0 else walk (n - 1)\n"
                    + "and stop n = walk n\n"

                Expect.equal
                    (letRecursions src)
                    [
                        Group([ "walk", Recursion.TailRecursive; "stop", Recursion.Recursive ], [ [ 0; 1 ] ])
                    ]
                    "a tail call to a sibling is an ordinary call, so only `walk` trampolines"
            }

            test "a `let rec … and …` group whose members reference only themselves is two singleton components" {
                let src = "let rec p x = x\nand q y = y\n"

                Expect.equal
                    (letRecursions src)
                    [
                        Group([ "p", Recursion.NonRecursive; "q", Recursion.NonRecursive ], [ [ 0 ]; [ 1 ] ])
                    ]
                    "one lexical group with two singleton components, and a singleton without a self-edge is not recursive"
            }

            test "a member referencing a sibling in an earlier component is not recursive" {
                let src = "let rec f x = if x = 0 then 0 else f (x - 1)\n" + "and g x = f x\n"

                Expect.equal
                    (letRecursions src)
                    [
                        Group([ "f", Recursion.TailRecursive; "g", Recursion.NonRecursive ], [ [ 0 ]; [ 1 ] ])
                    ]
                    "`g` references `f` across a component boundary, which is an ordinary call"
            }

            test "a format-literal alias member is folded out of its group and the components renumbered" {
                let src =
                    "let rec fmt : Vesper.Format<int -> string, unit, string, string> = \"%d\"\n"
                    + "and a n = if n = 0 then sprintf fmt n else b (n - 1)\n"
                    + "and b n = a n\n"

                Expect.equal
                    (letRecursions src)
                    [ Group([ "a", Recursion.Recursive; "b", Recursion.Recursive ], [ [ 0; 1 ] ]) ]
                    "`fmt` is const-propagated into its uses and dropped, so the group is `a` and `b` at indices 0 and 1"
            }

            test "a group left with one member after folding is a plain let" {
                let src =
                    "let rec fmt : Vesper.Format<int -> string, unit, string, string> = \"%d\"\n"
                    + "and show n = sprintf fmt n\n"

                Expect.equal
                    (letRecursions src)
                    [ Let("show", Recursion.NonRecursive) ]
                    "one surviving member is a `Let`"
            }
        ]
