module XParsec.FSharp.SemanticAnalysis.Tests.MkUnionTests

open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes

// `mkUnion` rests on SET-semantic disjunct identity (`EqSet`, not a canonical sort — a
// total order on `SemType` does not exist), so `string | int` and `int | string` are
// the *same* value. Pinned on the constructor, with no parser or unifier in the loop.

let private tc (n: string) : SemType =
    TyConst(RuntimeNames.primitiveKey n, Block.empty)

let private tInt = tc "int"
let private tString = tc "string"
let private tBool = tc "bool"

// A raw, pre-resolution union: `TyDisjuncts.OfSeq` flattens and dedups but (unlike
// `mkUnion`) does NOT collapse, so a 2-disjunct set holding an unresolved `TyVar` stays
// a `TyOr` — the only way to hand `zonk` a union that still has resolving to do.
let private rawOr (xs: SemType list) : SemType = TyOr(TyDisjuncts.OfSeq xs)

[<Tests>]
let tests =
    testList
        "mkUnion canonicalisation"
        [
            test "collapse: a single disjunct is that disjunct, not a union" {
                Expect.equal (mkUnion [ tInt ]) tInt "TyOr [A] ≡ A"
            }

            test "empty is never (TyOr [])" {
                match mkUnion [] with
                | TyOr ms -> Expect.equal ms.Disjuncts.Length 0 "mkUnion [] ≡ never (an empty TyOr)"
                | other -> failtestf "expected an empty TyOr, got %A" other
            }

            test "dedup: repeated disjuncts collapse" {
                Expect.equal (mkUnion [ tInt; tInt ]) tInt "A | A ≡ A"
                Expect.equal (mkUnion [ tInt; tString; tInt ]) (mkUnion [ tInt; tString ]) "A | B | A ≡ A | B"
            }

            test "order-insensitive: disjuncts compare set-equal regardless of order" {
                Expect.equal (mkUnion [ tString; tInt ]) (mkUnion [ tInt; tString ]) "string | int ≡ int | string"

                Expect.equal
                    (mkUnion [ tBool; tString; tInt ])
                    (mkUnion [ tInt; tBool; tString ])
                    "three disjuncts, any input order, one canonical form"
            }

            test "flatten: a nested union splices its disjuncts" {
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
                Expect.equal (mkUnion [ u; tInt ]) u "absorbing an existing disjunct is a no-op"
            }

            test "a genuine two-disjunct union stays a TyOr" {
                match mkUnion [ tInt; tString ] with
                | TyOr ms -> Expect.equal ms.Disjuncts.Length 2 "two distinct disjuncts ⇒ arity-2 TyOr"
                | other -> failtestf "expected TyOr, got %A" other
            }

            test "disjuncts compare set-equal across input orders (the canonical set)" {
                // Storage keeps insertion order, but identity is set-based: two input
                // orders yield SET-EQUAL disjunct sets under `EqSet`'s equality.
                let a = mkUnion [ tString; tInt; tBool ]
                let b = mkUnion [ tBool; tInt; tString ]

                match a, b with
                | TyOr ma, TyOr mb -> Expect.equal ma.Disjuncts mb.Disjuncts "set-equal disjunct sets"
                | _ -> failtest "both should be unions"
            }

            // Substitution and resolution rebuild a `TyOr` through `mkUnion`, not a
            // bare map, so a disjunct resolving ONTO another re-canonicalises
            // rather than leaving a stale `string | string`.
            test "zonk collapses a union when a disjunct resolves to another" {
                let store = TypeStore()
                let tv = store.NewTypeVar()
                store.SetLink(UnionFind.find store tv, ValueSome tString)
                // `'a | string` with `'a ↦ string` — a raw pre-resolution union.
                let u = rawOr [ TyVar tv; tString ]
                Expect.equal (Unification.zonk store u) tString "('a | string)[a:=string] ≡ string"
            }

            test "zonk keeps distinct resolved disjuncts and re-canonicalises" {
                let store = TypeStore()
                let tv = store.NewTypeVar()
                store.SetLink(UnionFind.find store tv, ValueSome tInt)
                let u = rawOr [ TyVar tv; tString ]

                Expect.equal
                    (Unification.zonk store u)
                    (mkUnion [ tInt; tString ])
                    "('a | string)[a:=int] ≡ int | string"
            }

            // `TyDisjuncts`' constructor is private, so `OfSeq` is the only way to
            // build one, and it always normalises.
            test "TyDisjuncts.OfSeq normalises regardless of input order" {
                let a = TyDisjuncts.OfSeq [ tString; tInt ]
                let b = TyDisjuncts.OfSeq [ tInt; tString ]
                Expect.equal a b "OfSeq yields one set-equal canonical form regardless of order"
                Expect.equal a.Disjuncts.Length 2 "two distinct disjuncts kept"

                Expect.equal
                    (TyDisjuncts.OfSeq [ tInt; tInt ]).Disjuncts.Length
                    1
                    "OfSeq dedups (collapse is mkUnion's job)"
            }
        ]
