module XParsec.FSharp.SemanticAnalysis.Tests.MkUnionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes

// Stage 2 of the anonymous-union plan (docs/anon-unions-plan.md): the `mkUnion`
// smart constructor and the total order it sorts members by. `mkUnion` is the
// ONLY sanctioned producer of `TyOr`, and the equality layer's `n1 = n2`
// discipline relies on its canonical form — so `string | int` and `int | string`
// must build the *same* value. These pin flatten / dedup / collapse / sort /
// order-insensitivity / idempotence directly on the constructor, with no parser
// or unifier in the loop.

let private tc (n: string) : SemType = TyConst(n, EqArray.empty)
let private tInt = tc "int"
let private tString = tc "string"
let private tBool = tc "bool"

[<Tests>]
let tests =
    testList
        "mkUnion canonicalisation"
        [
            test "collapse: a single member is that member, not a union" {
                Expect.equal (mkUnion [ tInt ]) tInt "TyOr [A] ≡ A"
            }

            test "empty is never (TyOr [])" { Expect.equal (mkUnion []) (TyOr EqArray.empty) "mkUnion [] ≡ never" }

            test "dedup: repeated members collapse" {
                Expect.equal (mkUnion [ tInt; tInt ]) tInt "A | A ≡ A"
                Expect.equal (mkUnion [ tInt; tString; tInt ]) (mkUnion [ tInt; tString ]) "A | B | A ≡ A | B"
            }

            test "order-insensitive: members sort to a canonical form" {
                Expect.equal (mkUnion [ tString; tInt ]) (mkUnion [ tInt; tString ]) "string | int ≡ int | string"

                Expect.equal
                    (mkUnion [ tBool; tString; tInt ])
                    (mkUnion [ tInt; tBool; tString ])
                    "three members, any input order, one canonical form"
            }

            test "flatten: a nested union splices its members" {
                Expect.equal
                    (mkUnion [ mkUnion [ tInt; tString ]; tBool ])
                    (mkUnion [ tInt; tString; tBool ])
                    "(A | B) | C ≡ A | B | C"
            }

            test "flatten + dedup interact: nested duplicates collapse" {
                Expect.equal
                    (mkUnion [ mkUnion [ tInt; tString ]; mkUnion [ tString; tBool ] ])
                    (mkUnion [ tInt; tString; tBool ])
                    "(A | B) | (B | C) ≡ A | B | C"
            }

            test "idempotent: re-wrapping a canonical union changes nothing" {
                let u = mkUnion [ tInt; tString; tBool ]
                Expect.equal (mkUnion [ u ]) u "mkUnion [mkUnion xs] ≡ mkUnion xs"
                Expect.equal (mkUnion [ u; tInt ]) u "absorbing an existing member is a no-op"
            }

            test "a genuine two-member union stays a TyOr" {
                match mkUnion [ tInt; tString ] with
                | TyOr ms -> Expect.equal ms.Length 2 "two distinct members ⇒ arity-2 TyOr"
                | other -> failtestf "expected TyOr, got %A" other
            }

            test "members are stored sorted (the canonical member vector)" {
                // Two different input orders yield the SAME member vector, in sorted
                // order — the property freeze/unify identity rests on.
                let a = mkUnion [ tString; tInt; tBool ]
                let b = mkUnion [ tBool; tInt; tString ]

                match a, b with
                | TyOr ma, TyOr mb -> Expect.equal ma mb "identical canonical member vectors"
                | _ -> failtest "both should be unions"
            }

            // The Stage-2 follow-up (fix #1): substitution / resolution traversals
            // rebuild a `TyOr` through `mkUnion`, not a bare `EqArray.map`, so a
            // member resolving onto another member re-canonicalises (collapses) the
            // set rather than leaving a stale `string | string`. `zonk` is the
            // exemplar; `substituteWith` / `Inline.substType` share the path.
            test "zonk collapses a union when a member resolves to another member" {
                let tv = TypeVar()
                tv.Link <- ValueSome tString
                // `'a | string` with `'a ↦ string` — a raw pre-resolution union.
                let u = TyOr(EqArray.ofList [ TyVar tv; tString ])
                Expect.equal (Unification.zonk u) tString "('a | string)[a:=string] ≡ string"
            }

            test "zonk keeps distinct resolved members and re-canonicalises" {
                let tv = TypeVar()
                tv.Link <- ValueSome tInt
                let u = TyOr(EqArray.ofList [ TyVar tv; tString ])
                Expect.equal (Unification.zonk u) (mkUnion [ tInt; tString ]) "('a | string)[a:=int] ≡ int | string"
            }
        ]
