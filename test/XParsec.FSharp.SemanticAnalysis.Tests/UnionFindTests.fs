module XParsec.FSharp.SemanticAnalysis.Tests.UnionFindTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// Metavars must be minted through an arena so each carries a dense id; one shared
// store for the module suffices (these tests only exercise union-find structure).
let private store = TypeStore()

[<Tests>]
let tests =
    testList
        "UnionFind"
        [
            test "fresh TypeVar is its own root" {
                let tv = store.NewTypeVar()
                Expect.isTrue (System.Object.ReferenceEquals(UnionFind.find store tv, tv)) "self-root"
            }

            test "union puts two vars in the same class" {
                let a = store.NewTypeVar()
                let b = store.NewTypeVar()
                UnionFind.union store a b
                Expect.isTrue (UnionFind.inSameClass store a b) "a and b unified"
            }

            test "union is transitive" {
                let a = store.NewTypeVar()
                let b = store.NewTypeVar()
                let c = store.NewTypeVar()
                UnionFind.union store a b
                UnionFind.union store b c
                Expect.isTrue (UnionFind.inSameClass store a c) "a and c transitively unified"
            }

            test "unrelated vars stay in different classes" {
                let a = store.NewTypeVar()
                let b = store.NewTypeVar()
                let c = store.NewTypeVar()
                UnionFind.union store a b
                Expect.isFalse (UnionFind.inSameClass store a c) "c isolated"
                Expect.isFalse (UnionFind.inSameClass store b c) "c isolated"
            }

            test "path compression flattens chains" {
                let a = store.NewTypeVar()
                let b = store.NewTypeVar()
                let c = store.NewTypeVar()
                let d = store.NewTypeVar()
                // Wire by hand to bypass rank-based union's auto-balancing.
                store.SetParent(b, ValueSome a)
                store.SetParent(c, ValueSome b)
                store.SetParent(d, ValueSome c)

                let root = UnionFind.find store d
                Expect.isTrue (System.Object.ReferenceEquals(root, a)) "found a"
                Expect.equal (store.Parent d) (ValueSome a) "d compressed"
                Expect.equal (store.Parent c) (ValueSome a) "c compressed"
                Expect.equal (store.Parent b) (ValueSome a) "b compressed"
            }

            test "rank-based union: smaller tree hangs off larger" {
                let a = store.NewTypeVar()
                let b = store.NewTypeVar()
                let c = store.NewTypeVar()
                UnionFind.union store a b
                UnionFind.union store a c

                let rootC = UnionFind.find store c
                Expect.isTrue (System.Object.ReferenceEquals(rootC, a)) "c's root is a"
                Expect.equal (store.Rank a) 1 "rank stays 1"
            }
        ]
