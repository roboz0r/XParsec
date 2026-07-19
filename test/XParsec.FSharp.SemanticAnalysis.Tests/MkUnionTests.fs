module XParsec.FSharp.SemanticAnalysis.Tests.MkUnionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes

// The `mkUnion` smart constructor and the SET-semantic member identity it rests on
// (`EqSet`, NOT a canonical sort — a total order on `SemType` does not exist). `mkUnion`
// is the ONLY sanctioned producer of `TyOr`, and the equality layer's `n1 = n2`
// discipline relies on its canonical set form — so `string | int` and `int | string`
// are the *same* value. These pin flatten / dedup / collapse / order-insensitivity /
// idempotence directly on the constructor, with no parser or unifier in the loop.

let private tc (n: string) : SemType =
    TyConst(RuntimeNames.primitiveKey n, EqArray.empty)

let private tInt = tc "int"
let private tString = tc "string"
let private tBool = tc "bool"

// A raw, pre-resolution union: `UnionMembers.OfSeq` flattens/dedups but (unlike
// `mkUnion`) does NOT collapse, so a 2-member set holding an unresolved `TyVar`
// stays a `TyOr` — the only way to hand `zonk` a union to resolve, since `mkUnion`
// is fed ground members in canonical use. The raw DU ctor is private, so this is
// the sole construction path; that privacy is the type-enforced invariant.
let private rawOr (xs: SemType list) : SemType = TyOr(UnionMembers.OfSeq xs)

[<Tests>]
let tests =
    testList
        "mkUnion canonicalisation"
        [
            test "collapse: a single member is that member, not a union" {
                Expect.equal (mkUnion [ tInt ]) tInt "TyOr [A] ≡ A"
            }

            test "empty is never (TyOr [])" {
                match mkUnion [] with
                | TyOr ms -> Expect.equal ms.Members.Length 0 "mkUnion [] ≡ never (an empty TyOr)"
                | other -> failtestf "expected an empty TyOr, got %A" other
            }

            test "dedup: repeated members collapse" {
                Expect.equal (mkUnion [ tInt; tInt ]) tInt "A | A ≡ A"
                Expect.equal (mkUnion [ tInt; tString; tInt ]) (mkUnion [ tInt; tString ]) "A | B | A ≡ A | B"
            }

            test "order-insensitive: members compare set-equal regardless of order" {
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
                | TyOr ms -> Expect.equal ms.Members.Length 2 "two distinct members ⇒ arity-2 TyOr"
                | other -> failtestf "expected TyOr, got %A" other
            }

            test "members compare set-equal across input orders (the canonical set)" {
                // Two different input orders yield SET-EQUAL member sets (EqSet's
                // order-insensitive equality) — the property freeze/unify identity rests
                // on. Storage keeps insertion order, but identity is set-based.
                let a = mkUnion [ tString; tInt; tBool ]
                let b = mkUnion [ tBool; tInt; tString ]

                match a, b with
                | TyOr ma, TyOr mb -> Expect.equal ma.Members mb.Members "set-equal member sets"
                | _ -> failtest "both should be unions"
            }

            // The Stage-2 follow-up (fix #1): substitution / resolution traversals
            // rebuild a `TyOr` through `mkUnion`, not a bare `EqArray.map`, so a
            // member resolving onto another member re-canonicalises (collapses) the
            // set rather than leaving a stale `string | string`. `zonk` is the
            // exemplar; `substituteWith` / `Inline.substType` share the path.
            test "zonk collapses a union when a member resolves to another member" {
                let store = TypeStore()
                let tv = store.NewTypeVar()
                store.SetLink(tv, ValueSome tString)
                // `'a | string` with `'a ↦ string` — a raw pre-resolution union.
                let u = rawOr [ TyVar tv; tString ]
                Expect.equal (Unification.zonk store u) tString "('a | string)[a:=string] ≡ string"
            }

            test "zonk keeps distinct resolved members and re-canonicalises" {
                let store = TypeStore()
                let tv = store.NewTypeVar()
                store.SetLink(tv, ValueSome tInt)
                let u = rawOr [ TyVar tv; tString ]

                Expect.equal
                    (Unification.zonk store u)
                    (mkUnion [ tInt; tString ])
                    "('a | string)[a:=int] ≡ int | string"
            }

            // Stage 3a: the canonical-set form is type-enforced, not convention.
            // `UnionMembers`' constructor is private, so `OfSeq` is the only way to
            // build one and it always normalises — a non-canonical `UnionMembers`
            // cannot exist (the privacy is a compile-time guarantee; here we pin that
            // the one public path canonicalises).
            test "UnionMembers.OfSeq normalises regardless of input order" {
                let a = UnionMembers.OfSeq [ tString; tInt ]
                let b = UnionMembers.OfSeq [ tInt; tString ]
                Expect.equal a b "OfSeq yields one set-equal canonical form regardless of order"
                Expect.equal a.Members.Length 2 "two distinct members kept"

                Expect.equal
                    (UnionMembers.OfSeq [ tInt; tInt ]).Members.Length
                    1
                    "OfSeq dedups (collapse is mkUnion's job)"
            }
        ]
