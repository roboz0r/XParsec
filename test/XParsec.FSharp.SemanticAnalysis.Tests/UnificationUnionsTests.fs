module XParsec.FSharp.SemanticAnalysis.Tests.UnificationUnionsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers
open XParsec.FSharp.SemanticAnalysis.Tests.UnificationTestHelpers

[<Tests>]
let tests =
    testList
        "Unification.Unions"
        [
            test "int | string translates to the canonical TyOr [int; string]" {
                let dom = unionDomainOf "let f (x: int | string) = x"
                Expect.equal dom (mkUnion [ BuiltinTypes.tyInt; BuiltinTypes.tyString ]) "f domain is int | string"
            }

            test "string | int translates to the SAME canonical union (order-insensitive)" {
                let a = unionDomainOf "let f (x: int | string) = x"
                let b = unionDomainOf "let f (x: string | int) = x"
                Expect.equal b a "string | int ≡ int | string after translate"

                match b with
                | TyOr ms -> Expect.equal ms.Disjuncts.Length 2 "two distinct disjuncts"
                | other -> failtestf "expected a TyOr, got %A" other
            }

            test "int | null translates with the reserved `null` disjunct present" {
                let dom = unionDomainOf "let f (x: int | null) = x"

                match dom with
                | TyOr ms ->
                    Expect.equal ms.Disjuncts.Length 2 "two disjuncts"

                    Expect.contains
                        (EqSet.toList ms.Disjuncts)
                        (TyConst(RuntimeNames.nullKey, EqArray.empty))
                        "the reserved `null` literal type is a disjunct"
                | other -> failtestf "expected int | null to be a TyOr, got %A" other
            }

            test "int | string | bool translates to a 3-disjunct canonical TyOr" {
                let dom = unionDomainOf "let f (x: int | string | bool) = x"

                Expect.equal
                    dom
                    (mkUnion [ BuiltinTypes.tyInt; BuiltinTypes.tyString; BuiltinTypes.tyBool ])
                    "f domain is int | string | bool"

                match dom with
                | TyOr ms -> Expect.equal ms.Disjuncts.Length 3 "three distinct disjuncts"
                | other -> failtestf "expected a TyOr, got %A" other
            }

            test "disjunct → union: a disjunct is Equal to the union it belongs to" {
                let ctx = subsumeCtx ()

                Expect.equal
                    (UnificationSubsume.subsumes ctx intTy (mkUnion [ intTy; strTy ]))
                    UnificationSubsume.SubsumeOutcome.Equal
                    "int ≤ (int | string) is Equal (int is a disjunct)"
            }

            test "disjunct → union: a non-disjunct is Unrelated" {
                let ctx = subsumeCtx ()

                Expect.equal
                    (UnificationSubsume.subsumes ctx boolTy (mkUnion [ intTy; strTy ]))
                    UnificationSubsume.SubsumeOutcome.Unrelated
                    "bool ⋠ (int | string)"
            }

            test "union → union: a narrower union is a Subtype of a wider one" {
                let ctx = subsumeCtx ()

                Expect.equal
                    (UnificationSubsume.subsumes ctx (mkUnion [ intTy; strTy ]) (mkUnion [ intTy; strTy; boolTy ]))
                    UnificationSubsume.SubsumeOutcome.Subtype
                    "(int | string) ≤ (int | string | bool)"
            }

            test "union → union: identical canonical disjunct sets are Equal" {
                let ctx = subsumeCtx ()

                Expect.equal
                    (UnificationSubsume.subsumes ctx (mkUnion [ intTy; strTy ]) (mkUnion [ strTy; intTy ]))
                    UnificationSubsume.SubsumeOutcome.Equal
                    "(int | string) ≤ (string | int) is Equal (order-insensitive)"
            }

            test "union → union: a disjunct outside the target makes it Unrelated" {
                let ctx = subsumeCtx ()

                Expect.equal
                    (UnificationSubsume.subsumes ctx (mkUnion [ intTy; strTy ]) (mkUnion [ intTy; boolTy ]))
                    UnificationSubsume.SubsumeOutcome.Unrelated
                    "(int | string) ⋠ (int | bool)"
            }

            test "union → disjunct: a union does NOT subsume one of its disjuncts" {
                let ctx = subsumeCtx ()

                Expect.equal
                    (UnificationSubsume.subsumes ctx (mkUnion [ intTy; strTy ]) intTy)
                    UnificationSubsume.SubsumeOutcome.Unrelated
                    "(int | string) ⋠ int — the consumer must narrow first"
            }

            test "let binding annotated with a union accepts a disjunct value" {
                let ctx = analyse "let x: int | string = 1"
                Expect.isEmpty ctx.Diagnostics "an int is accepted by an (int | string) annotation"
            }

            test "let binding annotated with a reordered union accepts the other disjunct" {
                let ctx = analyse "let x: string | int = \"a\""
                Expect.isEmpty ctx.Diagnostics "a string is accepted by a (string | int) annotation"
            }

            test "a union-typed parameter accepts arguments of each disjunct" {
                let ctx = analyse "let f (x: int | string) = 0\nlet a = f 1\nlet b = f \"a\""
                Expect.isEmpty ctx.Diagnostics "f 1 and f \"a\" both check against (int | string)"
            }

            test "passing a disjunct into a union slot does NOT narrow the slot" {
                // `f 1` returns the *union* `int | string`, not `int`: the slot accepted `1`
                // by assignability without unifying the parameter down to the actual. (The
                // body is `x`, so the return type is the parameter's.)
                let ctx = analyse "let f (x: int | string) = x\nlet y = f 1"

                let yKey =
                    NodeKey.ofSource (("let f (x: int | string) = x\nlet y = f 1").IndexOf "y =") NodeKind.PatIdent

                Expect.equal (typeOf ctx yKey) (mkUnion [ intTy; strTy ]) "f 1 : int | string"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "a non-disjunct value is still rejected against a union annotation" {
                let ctx = analyse "let x: int | string = true"

                let hasMismatch = ctx.Diagnostics |> Seq.exists Diagnostic.isError

                Expect.isTrue hasMismatch "bool ⋠ (int | string) — annotation rejects it"
            }

            test "assignment stays symmetric: `x <- 1` on a string is an error" {
                // There is no annotation here, so inference must not silently widen the
                // binding to `string | int`.
                let ctx = analyse "let mutable x = \"\"\nx <- 1"

                let hasMismatch =
                    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "mismatch")

                Expect.isTrue hasMismatch "`x <- 1` against a string binding stays a hard error"
            }

            test "a union slot accepts a value by assignability WITHOUT pinning its typar" {
                // A value that subsumes into a disjunct is accepted without `unify`, so the
                // actual's typar stays free — a plain `unify` against the union would link it.
                let ctx = subsumeCtx ()
                let tv = ctx.Store.NewTypeVar()
                let actual = TyVar tv
                let target = mkUnion [ TyVar tv; intTy ]
                let accepted = UnificationEngine.tryCoerceUpcast ctx dummyTok actual target
                Expect.isTrue accepted "the union slot accepts the value"

                Expect.equal
                    (ctx.Store.Link(UnionFind.find ctx.Store tv))
                    ValueNone
                    "the actual's typar is left free (no pin)"
            }

            test "equality on (int | string) is Satisfied — every disjunct is equatable" {
                let ctx = subsumeCtx ()

                Expect.equal
                    (checkConstraintKind ctx SemanticConstraintKind.Equality (mkUnion [ intTy; strTy ]))
                    UnificationEngine.ConstraintOutcome.Satisfied
                    "int | string supports equality (both disjuncts do)"
            }

            test "equality on a union with a function disjunct is Violated" {
                // A `TyFun` arm supports no structural equality, so the all-disjuncts
                // reduction fails for the whole union.
                let ctx = subsumeCtx ()

                Expect.equal
                    (checkConstraintKind ctx SemanticConstraintKind.Equality (mkUnion [ intTy; TyFun(intTy, intTy) ]))
                    UnificationEngine.ConstraintOutcome.Violated
                    "int | (int -> int) — the function arm breaks equality"
            }

            test "equality on a union with an unresolved disjunct Defers" {
                // A free disjunct is "unknown yet": the reduction defers so the
                // constraint re-fires when that disjunct's TyVar Links.
                let ctx = subsumeCtx ()

                Expect.equal
                    (checkConstraintKind
                        ctx
                        SemanticConstraintKind.Equality
                        (mkUnion [ intTy; TyVar(ctx.Store.NewTypeVar()) ]))
                    UnificationEngine.ConstraintOutcome.Defer
                    "int | 'a — defers on the free disjunct"
            }

            test "comparison on (int | string) is Violated though each disjunct is comparable" {
                // Unlike equality, comparison does NOT reduce disjunct-wise: generic `compare`
                // throws across distinct runtime types, so admitting it would let `List.sort`
                // on a `(int | string) list` type-check and then throw.
                let ctx = subsumeCtx ()

                Expect.equal
                    (checkConstraintKind ctx SemanticConstraintKind.Comparison (mkUnion [ intTy; strTy ]))
                    UnificationEngine.ConstraintOutcome.Violated
                    "int | string fails comparison even though int and string each support it"
            }

            test "an exhaustive type-test match on a union checks with no warning" {
                // Both disjuncts tested, so the match is provably
                // exhaustive and the bound variables `i`/`s` narrow to `int`/`string`.
                let ctx =
                    analyse
                        "let f (x: int | string) =\n    match x with\n    | :? int as i -> i\n    | :? string as s -> 0"

                Expect.isEmpty ctx.Diagnostics "exhaustive union match — no diagnostics"
                Expect.isFalse (hasUnionExhaustivenessWarning ctx) "no non-exhaustiveness warning"
            }

            test "a union match missing a disjunct warns (closed-union exhaustiveness)" {
                let ctx =
                    analyse "let f (x: int | string) =\n    match x with\n    | :? int as i -> i"

                Expect.isTrue (hasUnionExhaustivenessWarning ctx) "missing `string` arm warns"

                let warning =
                    ctx.Diagnostics
                    |> Seq.find (fun d -> d.Severity = Severity.Warning && d.Message.Contains "anonymous union")

                Expect.isTrue (warning.Message.Contains "string") "the warning names the uncovered disjunct"
            }

            test "a fall-through catch-all binds the narrowed residual union" {
                // After `:? int` catches `int`, the residual is `int | string \ int`
                // = `string`, so `other` binds at `string` (a collapsed singleton),
                // not the full union — and the catch-all makes the match exhaustive.
                let input =
                    "let f (x: int | string) =\n    match x with\n    | :? int as i -> i\n    | other -> 0"

                let ctx = analyse input
                let otherKey = NodeKey.ofSource (input.IndexOf "other") NodeKind.PatIdent
                Expect.equal (typeOf ctx otherKey) BuiltinTypes.tyString "other : string (residual)"
                Expect.isFalse (hasUnionExhaustivenessWarning ctx) "catch-all makes it exhaustive"
            }

            test "an annotated `int | string` binding freezes to a canonical FTOr signature" {
                // Domain and return are both `int | string`, so the decl freezes to
                // `FTFun(FTOr, FTOr)`. Expected is built by freezing the SAME canonical
                // SemType, not by guessing the disjunct order.
                let file = freezeDecls "let f (x: int | string) : int | string = x"
                let union = mkUnion [ BuiltinTypes.tyInt; BuiltinTypes.tyString ]
                let expected = toFrozen (TyFun(union, union))
                Expect.equal (frozenLetTy file) expected "f freezes to (int | string) -> (int | string)"
            }

            test "the frozen union carries both disjuncts as FTOr in canonical order" {
                // Pin the `FTOr` shape directly (not just via the round-trip equality
                // above): the domain is an `FTOr` of exactly the two `FTConst`
                // leaves, sorted to the canonical order `mkUnion` produces.
                let file = freezeDecls "let f (x: int | string) : int | string = x"

                match frozenLetTy file with
                | FTFun(FTOr disjuncts, _) ->
                    let canonical =
                        match toFrozen (mkUnion [ BuiltinTypes.tyInt; BuiltinTypes.tyString ]) with
                        | FTOr ds -> ds
                        | other -> failtestf "expected the canonical union to freeze to FTOr, got %A" other

                    Expect.equal disjuncts canonical "domain disjuncts are the canonical FTOr [int; string]"
                | other -> failtestf "expected f's domain to freeze to an FTOr, got %A" other
            }

            test "the frozen FTOr round-trips through the SemType bridge" {
                // `ofFrozen`/`toFrozen` are mutual inverses on synthetic samples; assert it
                // also holds for a union that travelled the *real* freeze.
                let frozen = frozenLetTy (freezeDecls "let f (x: int | string) : int | string = x")

                Expect.equal
                    (toFrozen (ofFrozen (MeasuredThaw.noneOver (TypeStore())) frozen))
                    frozen
                    "ofFrozen >> toFrozen = id on the frozen signature"
            }
        ]
